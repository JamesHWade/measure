"""Versioned, owner-scoped experiment records; no optimization or instrument actuation."""
from __future__ import annotations

from datetime import datetime, timezone
import json
import math
import sqlite3
from typing import Any, Literal
import uuid

from pydantic import (
    AwareDatetime, BaseModel, ConfigDict, Field, StrictBool, StrictFloat,
    StrictInt, StrictStr, field_validator, model_validator,
)

from storage import canonical, now, registry_connection, registry_mutation, sha256
from method_registry import method_link

SCHEMA_VERSION = "1.0"
MAX_RECORD_BYTES = 64_000
MAX_REGISTRY_PAGE = 50
OutcomeStatus = Literal["observed", "not_detected", "below_limit", "censored", "missing", "failed", "infeasible"]
AttemptStatus = Literal["planned", "running", "completed", "cancelled", "failed", "unknown"]


class RecordModel(BaseModel):
    model_config = ConfigDict(extra="forbid", allow_inf_nan=False)

    @field_validator("*")
    @classmethod
    def reject_whitespace_only(cls, value: Any) -> Any:
        if isinstance(value, str) and value and not value.strip():
            raise ValueError("Text fields must not consist only of whitespace")
        return value


def finite_number(value: Any) -> bool:
    try:
        return isinstance(value, (int, float)) and not isinstance(value, bool) and math.isfinite(value)
    except OverflowError:
        return False


class Condition(RecordModel):
    name: str = Field(min_length=1, max_length=100)
    value: StrictInt | StrictFloat | StrictStr | StrictBool
    unit: str | None = Field(default=None, min_length=1, max_length=40)

    @model_validator(mode="after")
    def validate_value(self) -> Condition:
        if isinstance(self.value, str) and (not self.value.strip() or len(self.value) > 300):
            raise ValueError("Condition text must contain 1 to 300 characters")
        if isinstance(self.value, (int, float)) and not isinstance(self.value, bool):
            if not finite_number(self.value) or self.unit is None:
                raise ValueError("Numeric conditions require a finite value and an explicit unit (1 for dimensionless)")
        elif self.unit is not None:
            raise ValueError("Categorical and boolean conditions must not have a numeric unit")
        return self


def unique_conditions(values: list[Condition]) -> list[Condition]:
    names = [v.name for v in values]
    if len(set(names)) != len(names):
        raise ValueError("Condition names must be unique within a condition set")
    return values


class ExperimentSpec(RecordModel):
    title: str = Field(min_length=1, max_length=200)
    source: str = Field(min_length=1, max_length=200, description="Caller-declared origin, for example manual entry or an instrument system.")
    external_reference: str | None = Field(default=None, min_length=1, max_length=200)
    proposal_reference: str | None = Field(default=None, min_length=1, max_length=200, description="Optional external proposal ID; not a service-validated optimizer campaign.")
    executable_method_id: str | None = Field(default=None, min_length=1, max_length=100, description="Owned immutable executable method for the intended design; validation does not imply instrument execution.")
    intended_conditions: list[Condition] = Field(default_factory=list, max_length=64)
    description: str = Field(default="", max_length=2000)

    _unique = field_validator("intended_conditions")(unique_conditions)


class MeasurementLink(RecordModel):
    dataset_id: str = Field(min_length=1, max_length=100)
    trace_id: str = Field(min_length=1, max_length=100)


class AttemptSpec(RecordModel):
    status: AttemptStatus
    evaluator_kind: Literal["manual", "instrument", "simulator", "replay"] = "manual"
    evaluator_reference: str | None = Field(default=None, min_length=1, max_length=200)
    evaluator_revision: str | None = Field(default=None, min_length=1, max_length=200)
    training_snapshot_reference: str | None = Field(default=None, min_length=1, max_length=200, description="Optional caller-declared model training snapshot reference; not validated as a registry snapshot.")
    fidelity: StrictFloat | None = Field(default=None, ge=0, le=1, description="Caller-declared fidelity coordinate, not measured accuracy or uncertainty.")
    actual_conditions: list[Condition] = Field(default_factory=list, max_length=64)
    measurements: list[MeasurementLink] = Field(default_factory=list, max_length=32)
    started_at: AwareDatetime | None = None
    ended_at: AwareDatetime | None = None
    external_reference: str | None = Field(default=None, min_length=1, max_length=200)
    instrument_reference: str | None = Field(default=None, min_length=1, max_length=200)
    executable_method_id: str | None = Field(default=None, min_length=1, max_length=100, description="Owned immutable executable method reported for this attempt; actual instrument settings remain separately reported.")
    method_reference: str | None = Field(default=None, min_length=1, max_length=200)
    replicate_group: str | None = Field(default=None, min_length=1, max_length=100)
    replicate_kind: Literal["technical", "biological", "independent", "unspecified"] | None = None
    replicate_index: StrictInt | None = Field(default=None, ge=1)
    explanation: str = Field(default="", max_length=2000)

    _unique = field_validator("actual_conditions")(unique_conditions)

    @field_validator("started_at", "ended_at")
    @classmethod
    def utc_times(cls, value: datetime | None) -> datetime | None:
        return None if value is None else value.astimezone(timezone.utc)

    @model_validator(mode="after")
    def validate_attempt(self) -> AttemptSpec:
        keys = [(v.dataset_id, v.trace_id) for v in self.measurements]
        if len(set(keys)) != len(keys):
            raise ValueError("Measurement links must be unique")
        if self.started_at and self.ended_at and self.ended_at < self.started_at:
            raise ValueError("ended_at must not precede started_at")
        if self.status == "planned" and (self.started_at or self.ended_at):
            raise ValueError("A planned attempt cannot have actual start/end timestamps")
        if self.status == "running" and self.ended_at:
            raise ValueError("A running attempt cannot have an end timestamp")
        if self.status in ("failed", "cancelled", "unknown") and not self.explanation.strip():
            raise ValueError("Failed, cancelled and unknown attempts require an explanation")
        if (self.replicate_kind is not None or self.replicate_index is not None) and self.replicate_group is None:
            raise ValueError("Replicate kind/index require an explicit replicate group")
        return self


class ResultSpec(RecordModel):
    metric: str = Field(min_length=1, max_length=100)
    analyte_reference: str | None = Field(default=None, min_length=1, max_length=200, description="Caller-declared analyte label, not a verified chemical identity.")
    status: OutcomeStatus
    unit: str = Field(min_length=1, max_length=40, description="Caller-declared unit; use 1 for dimensionless. This registry does not verify or convert units.")
    value: StrictInt | StrictFloat | None = None
    lower_bound: StrictInt | StrictFloat | None = None
    upper_bound: StrictInt | StrictFloat | None = None
    analysis_id: str | None = Field(default=None, min_length=1, max_length=100)
    summary_field: str | None = Field(default=None, min_length=1, max_length=100, description="For an observed analysis result, a numeric key directly in the stored analysis summary; value is copied by the server.")
    supersedes_result_id: str | None = Field(default=None, min_length=1, max_length=100)
    explanation: str = Field(default="", max_length=2000)

    @model_validator(mode="after")
    def validate_result(self) -> ResultSpec:
        if any(v is not None and not finite_number(v) for v in (self.value, self.lower_bound, self.upper_bound)):
            raise ValueError("Result values/bounds must be finite numeric values within floating-point range")
        if self.status == "observed":
            if self.analysis_id is not None:
                if self.value is not None or self.summary_field is None:
                    raise ValueError("An analysis-derived observation requires summary_field and omits value")
            elif self.value is None or self.summary_field is not None:
                raise ValueError("A caller-reported observation requires value and no summary_field")
            if self.lower_bound is not None or self.upper_bound is not None:
                raise ValueError("Observed values do not use censoring bounds")
        else:
            if self.value is not None or self.summary_field is not None or not self.explanation.strip():
                raise ValueError("Non-observed outcomes omit value/summary_field and require an explanation")
            if self.status == "below_limit":
                if self.upper_bound is None or self.lower_bound is not None:
                    raise ValueError("below_limit requires an upper bound only")
            elif self.status == "censored":
                if self.lower_bound is None and self.upper_bound is None:
                    raise ValueError("A censored outcome requires at least one bound")
            elif self.lower_bound is not None or self.upper_bound is not None:
                raise ValueError("Bounds apply only to censored or below_limit outcomes")
        if self.lower_bound is not None and self.upper_bound is not None and self.lower_bound >= self.upper_bound:
            raise ValueError("Censoring bounds must be increasing")
        if self.supersedes_result_id and not self.explanation.strip():
            raise ValueError("Superseding a result requires an explanation")
        return self


def bounded(value: Any) -> Any:
    if len(canonical(value).encode("utf-8")) > MAX_RECORD_BYTES:
        raise ValueError("Registry record or result batch exceeds the byte limit")
    return value


def _owned(con: sqlite3.Connection, table: str, owner: str, record_id: str) -> sqlite3.Row:
    if table not in ("experiments", "experiment_attempts", "experiment_results"):
        raise ValueError("Invalid registry record type")
    row = con.execute(f"SELECT * FROM {table} WHERE id=? AND owner=?", (record_id, owner)).fetchone()
    if row is None:
        raise ValueError("Registry record was not found or is not accessible")
    return row


def _experiment(row: sqlite3.Row) -> dict[str, Any]:
    payload = json.loads(row["payload"])
    method = payload.pop("executable_method", None)
    return {"schema_version": SCHEMA_VERSION, "experiment_id": row["id"], "spec": payload,
            "executable_method": method, "input_sha256": row["digest"], "created_at": row["created_at"]}


def _requested_spec(spec: ExperimentSpec | AttemptSpec) -> dict:
    # Omit the new optional field when absent to preserve pre-method retry hashes.
    payload = spec.model_dump(mode="json")
    if payload["executable_method_id"] is None:
        payload.pop("executable_method_id")
    return bounded(payload)


def create_experiment(owner: str, spec: ExperimentSpec, key: str) -> dict[str, Any]:
    payload = _requested_spec(spec)

    def write(con: sqlite3.Connection) -> dict:
        retained = dict(payload)
        if payload.get("executable_method_id") is not None:
            retained["executable_method"] = method_link(con, owner, payload["executable_method_id"])
        bounded(retained)
        record_id = str(uuid.uuid4())
        con.execute("INSERT INTO experiments VALUES (?, ?, ?, ?, ?)", (record_id, owner, canonical(retained), sha256(retained), now()))
        return _experiment(_owned(con, "experiments", owner, record_id))

    return registry_mutation(owner, key, {"operation": "create_experiment", "spec": payload}, write)


def get_experiment(owner: str, experiment_id: str) -> dict[str, Any]:
    with registry_connection() as con:
        row = _owned(con, "experiments", owner, experiment_id)
        return _experiment(row)


def _page(offset: int, limit: int) -> None:
    if offset < 0 or not 1 <= limit <= MAX_REGISTRY_PAGE:
        raise ValueError("Invalid registry page; limit must be between 1 and 50")


def _paged(items: list[dict], count: int, offset: int, limit: int) -> dict[str, Any]:
    return {"items": items, "count": count, "offset": offset,
            "next_offset": offset + limit if offset + limit < count else None}


def list_experiments(owner: str, offset: int = 0, limit: int = 20) -> dict[str, Any]:
    _page(offset, limit)
    with registry_connection() as con:
        count = con.execute("SELECT COUNT(*) FROM experiments WHERE owner=?", (owner,)).fetchone()[0]
        rows = con.execute("SELECT * FROM experiments WHERE owner=? ORDER BY created_at, id LIMIT ? OFFSET ?", (owner, limit, offset)).fetchall()
        items = [{"experiment_id": r["id"], "title": json.loads(r["payload"])["title"], "created_at": r["created_at"]} for r in rows]
        return _paged(items, count, offset, limit)


def _measurement(con: sqlite3.Connection, owner: str, link: dict) -> dict:
    row = con.execute("SELECT payload, digest FROM datasets WHERE id=? AND owner=?", (link["dataset_id"], owner)).fetchone()
    if row is None:
        raise ValueError("Measurement dataset was not found or is not accessible")
    traces = json.loads(row["payload"])["traces"]
    trace = next((v for v in traces if v["trace_id"] == link["trace_id"]), None)
    if trace is None:
        raise ValueError("Measurement trace was not found in its dataset")
    return {**link, "input_sha256": row["digest"],
            "metadata": {k: v for k, v in trace.items() if k not in ("axis", "signal")}}


def _attempt(con: sqlite3.Connection, owner: str, attempt_id: str, revision: int | None = None) -> dict[str, Any]:
    attempt = _owned(con, "experiment_attempts", owner, attempt_id)
    selected = attempt["latest_revision"] if revision is None else revision
    row = con.execute("SELECT * FROM attempt_revisions WHERE attempt_id=? AND revision=?", (attempt_id, selected)).fetchone()
    if row is None:
        raise ValueError("Attempt revision was not found")
    payload = json.loads(row["payload"])
    method = payload.pop("executable_method", None)
    return {"schema_version": SCHEMA_VERSION, "attempt_id": attempt_id, "experiment_id": attempt["experiment_id"],
            "revision": row["revision"], "latest_revision": attempt["latest_revision"], "spec": payload,
            "executable_method": method, "input_sha256": row["digest"], "reason": row["reason"], "created_at": row["created_at"]}


def _attempt_payload(con: sqlite3.Connection, owner: str, spec: dict) -> dict:
    payload = {**spec, "measurements": [_measurement(con, owner, link) for link in spec["measurements"]]}
    if spec.get("executable_method_id") is not None:
        payload["executable_method"] = method_link(con, owner, spec["executable_method_id"])
    return bounded(payload)


def record_attempt(owner: str, experiment_id: str, spec: AttemptSpec, key: str) -> dict[str, Any]:
    requested = _requested_spec(spec)

    def write(con: sqlite3.Connection) -> dict:
        _owned(con, "experiments", owner, experiment_id)
        payload = _attempt_payload(con, owner, requested)
        attempt_id, created = str(uuid.uuid4()), now()
        con.execute("INSERT INTO experiment_attempts VALUES (?, ?, ?, ?, ?)", (attempt_id, owner, experiment_id, 1, created))
        con.execute("INSERT INTO attempt_revisions VALUES (?, ?, ?, ?, ?, ?)", (attempt_id, 1, canonical(payload), sha256(payload), "Initial record", created))
        return _attempt(con, owner, attempt_id)

    return registry_mutation(owner, key, {"operation": "record_attempt", "experiment_id": experiment_id, "spec": requested}, write)


def revise_attempt(owner: str, attempt_id: str, expected_revision: int, spec: AttemptSpec, reason: str, key: str) -> dict[str, Any]:
    if not reason.strip() or len(reason) > 2000:
        raise ValueError("A revision explanation of 1 to 2000 characters is required")
    requested = _requested_spec(spec)

    def write(con: sqlite3.Connection) -> dict:
        current = _owned(con, "experiment_attempts", owner, attempt_id)
        if current["latest_revision"] != expected_revision:
            raise ValueError("Attempt revision conflict; retrieve the current revision before retrying")
        payload = _attempt_payload(con, owner, requested)
        revision = expected_revision + 1
        con.execute("INSERT INTO attempt_revisions VALUES (?, ?, ?, ?, ?, ?)", (attempt_id, revision, canonical(payload), sha256(payload), reason, now()))
        con.execute("UPDATE experiment_attempts SET latest_revision=? WHERE id=? AND owner=?", (revision, attempt_id, owner))
        return _attempt(con, owner, attempt_id)

    return registry_mutation(owner, key, {"operation": "revise_attempt", "attempt_id": attempt_id,
                              "expected_revision": expected_revision, "spec": requested, "reason": reason}, write)


def get_attempt(owner: str, attempt_id: str, revision: int | None = None) -> dict[str, Any]:
    with registry_connection() as con:
        return _attempt(con, owner, attempt_id, revision)


def list_attempts(owner: str, experiment_id: str, offset: int = 0, limit: int = 20) -> dict[str, Any]:
    _page(offset, limit)
    with registry_connection() as con:
        _owned(con, "experiments", owner, experiment_id)
        count = con.execute("SELECT COUNT(*) FROM experiment_attempts WHERE experiment_id=? AND owner=?", (experiment_id, owner)).fetchone()[0]
        rows = con.execute("""SELECT a.id, a.latest_revision, a.created_at, r.payload FROM experiment_attempts a
            JOIN attempt_revisions r ON r.attempt_id=a.id AND r.revision=a.latest_revision
            WHERE a.experiment_id=? AND a.owner=? ORDER BY a.created_at, a.id LIMIT ? OFFSET ?""", (experiment_id, owner, limit, offset)).fetchall()
        items = [{"attempt_id": r["id"], "revision": r["latest_revision"], "status": json.loads(r["payload"])["status"],
                  "created_at": r["created_at"]} for r in rows]
        return _paged(items, count, offset, limit)


def _result(con: sqlite3.Connection, owner: str, result_id: str) -> dict[str, Any]:
    row = _owned(con, "experiment_results", owner, result_id)
    successor = con.execute("SELECT id FROM experiment_results WHERE supersedes_id=? AND owner=?", (result_id, owner)).fetchone()
    return {"schema_version": SCHEMA_VERSION, "result_id": result_id, "attempt_id": row["attempt_id"],
            "attempt_revision": row["attempt_revision"], "outcome": json.loads(row["payload"]),
            "superseded_by_result_id": None if successor is None else successor[0],
            "input_sha256": row["digest"], "created_at": row["created_at"]}


def _outcome(con: sqlite3.Connection, owner: str, attempt: dict, spec: ResultSpec) -> dict:
    payload = spec.model_dump(mode="json")
    provenance: dict[str, Any] = {"value_origin": "caller_reported", "unit_origin": "caller_declared",
                                  "attempt_sha256": attempt["input_sha256"],
                                  "evaluator": {k: attempt["spec"][k] for k in (
                                      "evaluator_kind", "evaluator_reference", "evaluator_revision",
                                      "training_snapshot_reference", "fidelity")}}
    if spec.analysis_id is not None:
        row = con.execute("SELECT * FROM analyses WHERE id=? AND owner=?", (spec.analysis_id, owner)).fetchone()
        if row is None:
            raise ValueError("Result analysis was not found or is not accessible")
        links = {(m["dataset_id"], m["trace_id"]) for m in attempt["spec"]["measurements"]}
        if (row["dataset_id"], row["trace_id"]) not in links:
            raise ValueError("Result analysis must belong to a measurement linked to this attempt revision")
        analysis_result = json.loads(row["result"])
        provenance.update({"analysis_id": spec.analysis_id, "dataset_id": row["dataset_id"], "trace_id": row["trace_id"],
                           "analysis_kind": row["kind"], "analysis_created_at": row["created_at"],
                           "analysis_provenance": json.loads(row["provenance"]),
                           "analysis_warnings": analysis_result.get("warnings", [])})
        if spec.status == "observed":
            summary = analysis_result.get("summary", {"peak_count": analysis_result.get("peak_count")})
            value = summary.get(spec.summary_field) if isinstance(summary, dict) else None
            if not finite_number(value):
                raise ValueError("The selected analysis summary field is not a finite numeric value")
            payload["value"] = value
            provenance["value_origin"] = "stored_analysis_summary"
            provenance["summary_field"] = spec.summary_field
    return bounded({**payload, "provenance": provenance})


def record_results(owner: str, attempt_id: str, expected_attempt_revision: int, specs: list[ResultSpec], key: str) -> dict[str, Any]:
    if not 1 <= len(specs) <= 50:
        raise ValueError("A result batch must contain between 1 and 50 outcomes")
    requested = bounded([v.model_dump(mode="json") for v in specs])

    def write(con: sqlite3.Connection) -> dict:
        attempt = _attempt(con, owner, attempt_id)
        if attempt["revision"] != expected_attempt_revision:
            raise ValueError("Attempt revision conflict; retrieve the current revision before recording outcomes")
        results = []
        for spec in specs:
            current = con.execute("""SELECT r.id FROM experiment_results r WHERE r.owner=? AND r.attempt_id=?
                AND r.metric=? AND r.analyte_reference IS ?
                AND NOT EXISTS(SELECT 1 FROM experiment_results next WHERE next.supersedes_id=r.id)""",
                (owner, attempt_id, spec.metric, spec.analyte_reference)).fetchone()
            if spec.supersedes_result_id is not None:
                previous = _owned(con, "experiment_results", owner, spec.supersedes_result_id)
                successor = con.execute("SELECT id FROM experiment_results WHERE supersedes_id=?", (previous["id"],)).fetchone()
                if previous["attempt_id"] != attempt_id or successor is not None:
                    raise ValueError("Result supersession conflict; specify a current result from the same attempt")
                if current is not None and current["id"] != previous["id"]:
                    raise ValueError("Result supersession would collide with another current metric/analyte result")
            elif current is not None:
                raise ValueError("A current result already exists; explicitly supersede it to record a correction")
            payload = _outcome(con, owner, attempt, spec)
            result_id = str(uuid.uuid4())
            con.execute("INSERT INTO experiment_results VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
                (result_id, owner, attempt_id, expected_attempt_revision, spec.metric, spec.analyte_reference,
                 spec.supersedes_result_id, canonical(payload), sha256(payload), now()))
            results.append(_result(con, owner, result_id))
        return bounded({"schema_version": SCHEMA_VERSION, "attempt_id": attempt_id,
                        "attempt_revision": expected_attempt_revision, "results": results})

    return registry_mutation(owner, key, {"operation": "record_results", "attempt_id": attempt_id,
                              "expected_attempt_revision": expected_attempt_revision, "results": requested}, write)


def get_result(owner: str, result_id: str) -> dict[str, Any]:
    with registry_connection() as con:
        return _result(con, owner, result_id)


def list_results(owner: str, attempt_id: str, include_superseded: bool = False, offset: int = 0, limit: int = 20) -> dict[str, Any]:
    _page(offset, limit)
    predicate = "" if include_superseded else " AND NOT EXISTS(SELECT 1 FROM experiment_results next WHERE next.supersedes_id=r.id)"
    with registry_connection() as con:
        _owned(con, "experiment_attempts", owner, attempt_id)
        where = " FROM experiment_results r WHERE r.owner=? AND r.attempt_id=?" + predicate
        count = con.execute("SELECT COUNT(*)" + where, (owner, attempt_id)).fetchone()[0]
        rows = con.execute("SELECT r.id" + where + " ORDER BY r.created_at, r.id LIMIT ? OFFSET ?", (owner, attempt_id, limit, offset)).fetchall()
        items = []
        for row in rows:
            record = _result(con, owner, row["id"])
            outcome = record["outcome"]
            items.append({k: record[k] for k in ("result_id", "attempt_revision", "superseded_by_result_id", "created_at")} |
                         {k: outcome[k] for k in ("metric", "analyte_reference", "status", "value", "unit")})
        return _paged(items, count, offset, limit)
