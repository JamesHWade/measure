"""A small, constrained MCP service for measurement and separations analysis.

This prototype deliberately accepts structured, bounded traces rather than
arbitrary server paths or executable code. Connect deployment requires the
explicit proxy/identity configuration documented in README.md.
"""
from __future__ import annotations

import hashlib
import csv
import io
import json
import math
import os
import subprocess
import uuid
from pathlib import Path
from typing import Annotated, Any, Literal

from mcp.server.fastmcp import Context, FastMCP
from mcp.types import ToolAnnotations
from pydantic import BaseModel, ConfigDict, Field, model_validator

import experiments as registry
import method_registry
from methods import (MAX_CONSTRAINTS_BYTES, MAX_GRADIENT_POINTS, MAX_SOLVENTS, MAX_VIOLATIONS,
                     MethodConstraints, MethodDeclaration, evaluate_method)
from experiments import AttemptSpec, ExperimentSpec, ResultSpec
from storage import canonical, connect, db_path, now, sha256

ROOT = Path(__file__).resolve().parent
MAX_TRACES = 32
MAX_POINTS = 50_000
MAX_DATASET_POINTS = 150_000
MAX_PAYLOAD_BYTES = 4_000_000
MAX_PAGE = 1000


class StrictModel(BaseModel):
    model_config = ConfigDict(extra="forbid", allow_inf_nan=False)


class TraceMetadata(StrictModel):
    trace_id: str = Field(min_length=1, max_length=100)
    sample_id: str = Field(min_length=1, max_length=100)
    injection_id: str = Field(min_length=1, max_length=100)
    channel: str = Field(min_length=1, max_length=100)
    separation: Literal["SEC", "LC", "GC", "CE", "other"]
    detector: str = Field(min_length=1, max_length=100)
    axis_unit: str = Field(min_length=1, max_length=40)
    signal_unit: str = Field(min_length=1, max_length=40)
    sample_type: Literal["unknown", "blank", "standard", "qc", "reference"] = "unknown"
    batch_id: str | None = Field(default=None, max_length=100)
    run_order: int | None = Field(default=None, ge=0)


class Trace(TraceMetadata):
    axis: list[float] = Field(min_length=3, max_length=MAX_POINTS, description="Finite, strictly increasing elution times or volumes in axis_unit.")
    signal: list[float] = Field(min_length=3, max_length=MAX_POINTS, description="Finite detector response values in signal_unit; same length as axis.")

    @model_validator(mode="after")
    def validate_trace(self) -> Trace:
        if len(self.axis) != len(self.signal):
            raise ValueError("axis and signal must have the same number of points")
        if any(not math.isfinite(v) for v in self.axis + self.signal):
            raise ValueError("axis and signal must contain only finite numbers")
        if any(b <= a for a, b in zip(self.axis, self.axis[1:])):
            raise ValueError("axis values must be strictly increasing and unique")
        return self


class CSVChannel(StrictModel):
    column: str = Field(min_length=1, max_length=200, description="Name of the numeric signal column in the table.")
    metadata: TraceMetadata


class CSVGroup(StrictModel):
    values: list[str] = Field(min_length=1, max_length=4, description="Group values in the same order as group_columns.")
    metadata: TraceMetadata


class Dataset(StrictModel):
    title: str = Field(min_length=1, max_length=200)
    traces: list[Trace] = Field(min_length=1, max_length=MAX_TRACES)

    @model_validator(mode="after")
    def validate_dataset(self) -> Dataset:
        if sum(len(t.axis) for t in self.traces) > MAX_DATASET_POINTS:
            raise ValueError("dataset exceeds the total point limit")
        if len({t.trace_id for t in self.traces}) != len(self.traces):
            raise ValueError("trace_id values must be unique within a dataset")
        injection_samples: dict[str, tuple] = {}
        injection_channels: set[tuple[str, str]] = set()
        for trace in self.traces:
            details = (trace.sample_id, trace.sample_type, trace.batch_id, trace.run_order, trace.separation)
            if trace.injection_id in injection_samples and injection_samples[trace.injection_id] != details:
                raise ValueError("traces of an injection must share sample, type, batch, run order and separation")
            injection_samples[trace.injection_id] = details
            key = (trace.injection_id, trace.channel)
            if key in injection_channels:
                raise ValueError("each injection/channel combination must be unique")
            injection_channels.add(key)
        return self


class ChromatogramMethod(StrictModel):
    baseline: Literal["none", "als"] = "none"
    baseline_lambda: float = Field(default=1e6, gt=0, le=1e12, description="ALS baseline smoothness on the sample-index grid.")
    baseline_p: float = Field(default=0.01, gt=0, lt=1, description="ALS asymmetry parameter.")
    smoothing: Literal["none", "gaussian"] = "none"
    smoothing_sigma: float = Field(default=1, gt=0, le=100, description="Gaussian kernel standard deviation in sample points, not axis units.")
    min_height: float = Field(default=0, ge=0, description="Minimum peak height in signal units.")
    min_prominence: float = Field(default=0, ge=0, description="Minimum peak prominence in signal units.")
    min_distance: float = Field(default=0, ge=0, description="Minimum separation between peaks in axis units.")
    integration_baseline: Literal["local", "none", "global"] = "local"


class Standard(StrictModel):
    location: float
    molecular_weight_g_mol: float = Field(gt=0)


class SECMethod(StrictModel):
    standards: list[Standard] = Field(min_length=2, max_length=100)
    fit_type: Literal["linear", "quadratic", "cubic"] = "linear"
    extrapolation: Literal["none", "linear", "warn"] = "none"
    integration_range: tuple[float, float] | None = Field(default=None, description="Inclusive limits on the original elution axis; uses sampled points without interpolating boundaries.")
    standard_description: str = Field(min_length=1, max_length=300, description="Polymer/standard identity and relevant conditions for interpreting relative calibration.")
    baseline_corrected: Literal[True]
    signal_proportional_to_weight_concentration: Literal[True]

    @model_validator(mode="after")
    def validate_method(self) -> SECMethod:
        required = {"linear": 2, "quadratic": 3, "cubic": 4}[self.fit_type]
        locations = [s.location for s in self.standards]
        if len(locations) < required or len(set(locations)) != len(locations):
            raise ValueError(f"{self.fit_type} calibration requires at least {required} distinct standard locations")
        if len({s.molecular_weight_g_mol for s in self.standards}) < 2:
            raise ValueError("calibration standards must contain at least two distinct molecular weights")
        if self.integration_range and self.integration_range[0] >= self.integration_range[1]:
            raise ValueError("integration_range must contain increasing limits")
        return self


class ResponseStandard(StrictModel):
    concentration: float = Field(ge=0, description="Known concentration in the method's concentration_unit.")
    response: float = Field(description="Integrated peak area measured using the same response basis as the unknown.")


class PeakQuantitationMethod(StrictModel):
    analyte: str = Field(min_length=1, max_length=200, description="Caller-supplied analyte identity; not verified by the service.")
    concentration_unit: str = Field(min_length=1, max_length=40)
    standards: list[ResponseStandard] = Field(min_length=3, max_length=100)
    weighting: Literal["none", "1/x", "1/x2"] = "none"
    force_origin: bool = False
    peak_assigned_to_analyte: Literal[True] = Field(description="Declare that the selected peak and standards correspond to this analyte.")
    standards_use_same_area_basis: Literal[True] = Field(description="Declare that standards and unknown use comparable integration, detector and response units.")

    @model_validator(mode="after")
    def validate_standards(self) -> PeakQuantitationMethod:
        concentrations = [s.concentration for s in self.standards]
        if len(set(concentrations)) < 3:
            raise ValueError("At least three distinct concentration levels are required")
        if self.weighting != "none" and any(x == 0 for x in concentrations):
            raise ValueError("Inverse-concentration weighting requires positive standard concentrations")
        return self


def identity(ctx: Context) -> str:
    """Use a local stdio principal, or an explicitly trusted Connect GUID."""
    mode = os.environ.get("MEASURE_MCP_IDENTITY", "local")
    request = ctx.request_context.request
    if mode == "local":
        if request is not None:
            raise ValueError("Local identity is permitted only over stdio")
        return "local"
    if mode != "connect" or os.environ.get("MEASURE_MCP_TRUST_CONNECT_HEADERS") != "1":
        raise ValueError("A trusted identity mode must be configured")
    if request is None:
        raise ValueError("Connect identity requires an authenticated HTTP request")
    headers = request.headers.getlist("rstudio-connect-credentials")
    if len(headers) != 1:
        raise ValueError("Connect credentials are missing or ambiguous")
    try:
        credentials = json.loads(headers[0])
        principal = str(uuid.UUID(credentials["user"]))
    except (ValueError, TypeError, KeyError, json.JSONDecodeError) as exc:
        raise ValueError("Connect credentials must contain a unique user GUID") from exc
    return "connect:" + principal


def dataset_for(owner: str, dataset_id: str) -> tuple[dict, str]:
    with connect() as con:
        row = con.execute("SELECT payload, digest FROM datasets WHERE id=? AND owner=?", (dataset_id, owner)).fetchone()
    if row is None:
        raise ValueError("Dataset was not found or is not accessible")
    return json.loads(row[0]), row[1]


def source_for(owner: str, dataset_id: str) -> dict[str, Any] | None:
    with connect() as con:
        row = con.execute("""SELECT s.kind, s.digest, s.content, s.mapping FROM dataset_sources s
            JOIN datasets d ON s.dataset_id=d.id WHERE d.id=? AND d.owner=?""", (dataset_id, owner)).fetchone()
    if row is None:
        return None
    return {"kind": row[0], "source_sha256": row[1], "content": row[2], "mapping": json.loads(row[3])}


def register_dataset(owner: str, dataset: Dataset, source: dict[str, Any] | None = None) -> dict[str, Any]:
    payload = dataset.model_dump(mode="json")
    serialized = canonical(payload)
    if len(serialized.encode("utf-8")) > MAX_PAYLOAD_BYTES:
        raise ValueError("Dataset exceeds the request-size limit")
    dataset_id, digest, created = str(uuid.uuid4()), sha256(payload), now()
    with connect() as con:
        con.execute("INSERT INTO datasets VALUES (?, ?, ?, ?, ?)", (dataset_id, owner, digest, serialized, created))
        if source is not None:
            con.execute("INSERT INTO dataset_sources VALUES (?, ?, ?, ?, ?)",
                (dataset_id, source["kind"], source["source_sha256"], source["content"], canonical(source["mapping"])))
    return {"dataset_id": dataset_id, "input_sha256": digest,
            "source_sha256": None if source is None else source["source_sha256"], "created_at": created,
            "trace_count": len(dataset.traces), "point_count": sum(len(t.axis) for t in dataset.traces)}


def parse_csv(csv_text: str, axis_column: str, channels: list[CSVChannel], delimiter: str) -> tuple[list[Trace], dict[str, Any]]:
    raw = csv_text.encode("utf-8")
    if len(raw) > MAX_PAYLOAD_BYTES:
        raise ValueError("CSV exceeds the request-size limit")
    reader = csv.DictReader(io.StringIO(csv_text.lstrip("\ufeff"), newline=""), delimiter=delimiter)
    fields = reader.fieldnames or []
    columns = [channel.column for channel in channels]
    if len(set(fields)) != len(fields) or len(set(columns)) != len(columns) or axis_column in columns or any(c not in fields for c in [axis_column, *columns]):
        raise ValueError("CSV needs unique headers and distinct existing axis and signal columns")
    axis: list[float] = []
    signals: list[list[float]] = [[] for _ in channels]
    for row in reader:
        if len(axis) >= MAX_POINTS or (len(axis) + 1) * len(channels) > MAX_DATASET_POINTS:
            raise ValueError("CSV exceeds the point limit")
        if None in row:
            raise ValueError("CSV contains a row with extra fields")
        try:
            axis.append(float(row[axis_column]))
            for channel, values in zip(channels, signals):
                values.append(float(row[channel.column]))
        except (TypeError, KeyError, ValueError) as exc:
            raise ValueError("CSV rows must have numeric values in all selected columns") from exc
    traces = [Trace.model_validate({**channel.metadata.model_dump(), "axis": axis, "signal": signal})
              for channel, signal in zip(channels, signals)]
    source = {"kind": "delimited_text", "source_sha256": hashlib.sha256(raw).hexdigest(), "content": csv_text,
              "mapping": {"delimiter": delimiter, "axis_column": axis_column,
                          "signal_columns": [{"column": c.column, "trace_id": c.metadata.trace_id} for c in channels]}}
    return traces, source


def parse_long_csv(csv_text: str, axis_column: str, signal_column: str, group_columns: list[str],
                   groups: list[CSVGroup], delimiter: str) -> tuple[list[Trace], dict[str, Any]]:
    raw = csv_text.encode("utf-8")
    if len(raw) > MAX_PAYLOAD_BYTES:
        raise ValueError("CSV exceeds the request-size limit")
    reader = csv.DictReader(io.StringIO(csv_text.lstrip("\ufeff"), newline=""), delimiter=delimiter)
    fields = reader.fieldnames or []
    selected = [axis_column, signal_column, *group_columns]
    if len(set(fields)) != len(fields) or len(set(selected)) != len(selected) or any(c not in fields for c in selected):
        raise ValueError("CSV needs unique headers and distinct existing axis, signal and group columns")
    keys = [tuple(group.values) for group in groups]
    if any(len(key) != len(group_columns) for key in keys) or len(set(keys)) != len(keys):
        raise ValueError("Each group needs one unique value per group column")
    indices = {key: i for i, key in enumerate(keys)}
    axes: list[list[float]] = [[] for _ in groups]
    signals: list[list[float]] = [[] for _ in groups]
    total = 0
    for row in reader:
        if None in row:
            raise ValueError("CSV contains a row with extra fields")
        key = tuple(row.get(column) for column in group_columns)
        if key not in indices:
            raise ValueError("CSV contains an unmapped group; provide metadata for every group")
        index = indices[key]
        if len(axes[index]) >= MAX_POINTS or total >= MAX_DATASET_POINTS:
            raise ValueError("CSV exceeds the point limit")
        try:
            axes[index].append(float(row[axis_column]))
            signals[index].append(float(row[signal_column]))
        except (TypeError, KeyError, ValueError) as exc:
            raise ValueError("CSV rows must have numeric values in the selected axis and signal columns") from exc
        total += 1
    traces = [Trace.model_validate({**group.metadata.model_dump(), "axis": axis, "signal": signal})
              for group, axis, signal in zip(groups, axes, signals)]
    source = {"kind": "delimited_text", "source_sha256": hashlib.sha256(raw).hexdigest(), "content": csv_text,
              "mapping": {"delimiter": delimiter, "axis_column": axis_column, "signal_column": signal_column,
                          "group_columns": group_columns,
                          "groups": [{"values": group.values, "trace_id": group.metadata.trace_id} for group in groups]}}
    return traces, source


def trace_for(dataset: dict, trace_id: str) -> dict[str, Any]:
    for trace in dataset["traces"]:
        if trace["trace_id"] == trace_id:
            return trace
    raise ValueError("Trace was not found in the dataset")


def analysis_for(owner: str, analysis_id: str) -> dict[str, Any]:
    with connect() as con:
        row = con.execute("SELECT dataset_id, trace_id, kind, method, result, provenance, created_at FROM analyses WHERE id=? AND owner=?", (analysis_id, owner)).fetchone()
    if row is None:
        raise ValueError("Analysis was not found or is not accessible")
    return {"analysis_id": analysis_id, "dataset_id": row[0], "trace_id": row[1], "kind": row[2],
            "method": json.loads(row[3]), "result": json.loads(row[4]), "provenance": json.loads(row[5]), "created_at": row[6]}


def run_worker(payload: dict) -> dict[str, Any]:
    command = os.environ.get("MEASURE_MCP_R", "R")
    try:
        process = subprocess.run([command, "--vanilla", "--slave", "-f", str(ROOT / "worker.R")],
            input=canonical(payload), text=True, capture_output=True, timeout=90, check=False)
    except (OSError, subprocess.TimeoutExpired) as exc:
        raise ValueError("The R analysis worker could not start or timed out") from exc
    if process.returncode != 0:
        raise ValueError("The R analysis worker failed; check the service environment")
    try:
        output = json.loads(process.stdout)
    except json.JSONDecodeError as exc:
        raise ValueError("The R analysis worker returned an invalid response") from exc
    if not output.get("ok"):
        raise ValueError("Analysis failed: " + str(output.get("error", "unknown worker error")))
    warnings = output.get("warnings", [])
    if isinstance(warnings, str):
        warnings = [warnings]
    result = output["result"]
    result["warnings"] = warnings
    return result


def save_analysis(owner: str, dataset_id: str, trace_id: str, kind: str, method: dict, result: dict, digest: str) -> dict[str, Any]:
    analysis_id = str(uuid.uuid4())
    worker_digest = hashlib.sha256((ROOT / "worker.R").read_bytes()).hexdigest()
    source = source_for(owner, dataset_id)
    provenance = {"input_sha256": digest, "method_sha256": sha256(method), "worker_sha256": worker_digest,
                  "source_sha256": None if source is None else source["source_sha256"],
                  "packages": result.get("packages"), "installed_r_code_md5": result.get("installed_r_code_md5"),
                  "r_version": result.get("r_version"),
                  "measure_revision": os.environ.get("MEASURE_MCP_MEASURE_REVISION"),
                  "measure_sec_revision": os.environ.get("MEASURE_MCP_SEC_REVISION")}
    created = now()
    with connect() as con:
        con.execute("INSERT INTO analyses VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)",
            (analysis_id, owner, dataset_id, trace_id, kind, canonical(method), canonical(result), canonical(provenance), created))
    return {"analysis_id": analysis_id, "dataset_id": dataset_id, "trace_id": trace_id,
            "kind": kind, "status": "completed_with_warnings" if result.get("warnings") else "completed",
            "qualification": "experimental", "summary": result.get("summary", {"peak_count": result.get("peak_count")}),
            "diagnostics": result.get("diagnostics"), "warnings": result.get("warnings", []), "notes": result.get("notes", []),
            "provenance": provenance, "created_at": created, "artifact": True}


mcp = FastMCP("measure-separations", stateless_http=True, json_response=True, streamable_http_path="/mcp")
READ = ToolAnnotations(readOnlyHint=True, destructiveHint=False, idempotentHint=True, openWorldHint=False)
WRITE = ToolAnnotations(readOnlyHint=False, destructiveHint=False, idempotentHint=False, openWorldHint=False)
RETRY_WRITE = ToolAnnotations(readOnlyHint=False, destructiveHint=False, idempotentHint=True, openWorldHint=False)
RequestKey = Annotated[str, Field(min_length=1, max_length=100)]
RecordId = Annotated[str, Field(min_length=1, max_length=100)]


@mcp.tool(annotations=READ, structured_output=True)
def measurement_registry_capabilities(ctx: Context) -> dict[str, Any]:
    """Describe the local experiment registry without requiring an R backend."""
    identity(ctx)
    return {"schema_version": registry.SCHEMA_VERSION, "qualification": "experimental",
            "storage": "single_node_sqlite", "experiment_specs": "immutable",
            "attempts": "append_only_revisions", "results": "immutable_with_explicit_supersession",
            "conditions": ["numeric_with_units", "categorical", "boolean"],
            "outcome_statuses": ["observed", "not_detected", "below_limit", "censored", "missing", "failed", "infeasible"],
            "idempotency": "required_key_per_owner_for_registry_writes",
            "bo_suggestions": False, "instrument_actuation": False,
            "executable_method_links": "owned_immutable_methods_with_digests",
            "limits": {"record_bytes": registry.MAX_RECORD_BYTES, "page_size": registry.MAX_REGISTRY_PAGE,
                       "conditions_per_set": 64, "measurements_per_attempt": 32, "results_per_batch": 50}}


@mcp.tool(annotations=READ, structured_output=True)
def measurement_method_capabilities(ctx: Context) -> dict[str, Any]:
    """Describe executable LC method validation and storage without an R backend."""
    identity(ctx)
    return {"schema_version": "1.0", "qualification": "experimental",
            "programs": ["isocratic", "gradient_absolute", "gradient_duration"],
            "interpolation": "piecewise_linear_flow_and_solvent_fractions",
            "canonical_units": {"time": "min", "flow": "mL/min", "volume": "mL"},
            "solvent_use": "exact_integral_of_linear_flow_times_linear_fraction",
            "overheads": "separate_constant_pre_and_post_acquisition_phases",
            "equipment_compatibility": "caller_declared_references_and_allowed_values",
            "records": "immutable_owner_scoped_with_retry_receipts",
            "optimizer_search_space": False, "pressure_prediction": False,
            "instrument_actuation": False, "hardware_qualification": False,
            "limits": {"record_bytes": method_registry.MAX_METHOD_BYTES,
                       "page_size": method_registry.MAX_METHOD_PAGE,
                       "solvents": MAX_SOLVENTS, "gradient_points": MAX_GRADIENT_POINTS,
                       "constraint_bytes": MAX_CONSTRAINTS_BYTES, "violations": MAX_VIOLATIONS}}


@mcp.tool(annotations=READ, structured_output=True)
def measurement_validate_method(declaration: MethodDeclaration, ctx: Context,
                                constraints: MethodConstraints | None = None) -> dict[str, Any]:
    """Normalize a declared LC method, calculate time/solvent use and report feasibility; performs no execution or storage."""
    identity(ctx)
    evaluation = evaluate_method(declaration, constraints)
    return evaluation.model_dump(mode="json")


@mcp.tool(annotations=RETRY_WRITE, structured_output=True)
def measurement_register_method(title: Annotated[str, Field(min_length=1, max_length=200)],
                                declaration: MethodDeclaration, idempotency_key: RequestKey,
                                ctx: Context, constraints: MethodConstraints | None = None) -> dict[str, Any]:
    """Retain an immutable method that meets its declared constraints, with original units, canonical program and digests; does not run an instrument."""
    return method_registry.register_method(identity(ctx), title, declaration, constraints, idempotency_key)


@mcp.tool(annotations=READ, structured_output=True)
def measurement_get_method(method_id: RecordId, ctx: Context) -> dict[str, Any]:
    """Retrieve an owned immutable executable method and its declared limits."""
    return method_registry.get_method(identity(ctx), method_id)


@mcp.tool(annotations=READ, structured_output=True)
def measurement_list_methods(ctx: Context, offset: Annotated[int, Field(ge=0)] = 0,
                             limit: Annotated[int, Field(ge=1, le=50)] = 20) -> dict[str, Any]:
    """List compact method identities in a bounded owner-scoped page."""
    return method_registry.list_methods(identity(ctx), offset, limit)


@mcp.tool(annotations=RETRY_WRITE, structured_output=True)
def measurement_create_experiment(spec: ExperimentSpec, idempotency_key: RequestKey, ctx: Context) -> dict[str, Any]:
    """Record an immutable experiment design with intended conditions; does not execute it or create an optimizer campaign."""
    return registry.create_experiment(identity(ctx), spec, idempotency_key)


@mcp.tool(annotations=READ, structured_output=True)
def measurement_get_experiment(experiment_id: RecordId, ctx: Context) -> dict[str, Any]:
    """Retrieve an experiment's intended conditions and caller-declared origin."""
    return registry.get_experiment(identity(ctx), experiment_id)


@mcp.tool(annotations=READ, structured_output=True)
def measurement_list_experiments(ctx: Context, offset: Annotated[int, Field(ge=0)] = 0,
                                 limit: Annotated[int, Field(ge=1, le=50)] = 20) -> dict[str, Any]:
    """List a bounded page of the current owner's experiments."""
    return registry.list_experiments(identity(ctx), offset, limit)


@mcp.tool(annotations=RETRY_WRITE, structured_output=True)
def measurement_record_attempt(experiment_id: RecordId, spec: AttemptSpec, idempotency_key: RequestKey, ctx: Context) -> dict[str, Any]:
    """Record one attempted experiment, its reported status/actual conditions and owned dataset/trace links; no instrument action occurs."""
    return registry.record_attempt(identity(ctx), experiment_id, spec, idempotency_key)


@mcp.tool(annotations=RETRY_WRITE, structured_output=True)
def measurement_revise_attempt(attempt_id: RecordId, expected_revision: Annotated[int, Field(ge=1)], spec: AttemptSpec,
                                reason: Annotated[str, Field(min_length=1, max_length=2000)],
                                idempotency_key: RequestKey, ctx: Context) -> dict[str, Any]:
    """Append a corrected or updated reported attempt snapshot with an explanation and optimistic version check; earlier revisions remain available."""
    return registry.revise_attempt(identity(ctx), attempt_id, expected_revision, spec, reason, idempotency_key)


@mcp.tool(annotations=READ, structured_output=True)
def measurement_get_attempt(attempt_id: RecordId, ctx: Context,
                            revision: Annotated[int | None, Field(ge=1)] = None) -> dict[str, Any]:
    """Read the latest or a specific historical attempt revision, including sample/injection metadata from its linked traces."""
    return registry.get_attempt(identity(ctx), attempt_id, revision)


@mcp.tool(annotations=READ, structured_output=True)
def measurement_list_attempts(experiment_id: RecordId, ctx: Context, offset: Annotated[int, Field(ge=0)] = 0,
                              limit: Annotated[int, Field(ge=1, le=50)] = 20) -> dict[str, Any]:
    """List a bounded page of attempts and latest reported statuses for an owned experiment."""
    return registry.list_attempts(identity(ctx), experiment_id, offset, limit)


@mcp.tool(annotations=RETRY_WRITE, structured_output=True)
def measurement_record_results(attempt_id: RecordId, expected_attempt_revision: Annotated[int, Field(ge=1)],
                               results: Annotated[list[ResultSpec], Field(min_length=1, max_length=50)],
                               idempotency_key: RequestKey, ctx: Context) -> dict[str, Any]:
    """Atomically record outcomes against an exact attempt revision; copy numeric analysis-summary values or record explicit caller-reported outcomes. Units are caller-declared."""
    return registry.record_results(identity(ctx), attempt_id, expected_attempt_revision, results, idempotency_key)


@mcp.tool(annotations=READ, structured_output=True)
def measurement_get_result(result_id: RecordId, ctx: Context) -> dict[str, Any]:
    """Retrieve an immutable outcome with evidence provenance, attempt revision and any later superseding result."""
    return registry.get_result(identity(ctx), result_id)


@mcp.tool(annotations=READ, structured_output=True)
def measurement_list_results(attempt_id: RecordId, ctx: Context, include_superseded: bool = False,
                             offset: Annotated[int, Field(ge=0)] = 0,
                             limit: Annotated[int, Field(ge=1, le=50)] = 20) -> dict[str, Any]:
    """List compact current outcome heads or the full result history for an owned attempt; no missing outcome is converted to zero."""
    return registry.list_results(identity(ctx), attempt_id, include_superseded, offset, limit)


@mcp.tool(annotations=READ, structured_output=True)
def measurement_capabilities(ctx: Context) -> dict[str, Any]:
    """Report installed R backends and the experimental operations available."""
    identity(ctx)
    result = run_worker({"operation": "capabilities"})
    packages = result.get("packages", {})
    return {"service": "measure-separations", "qualification": "experimental", "backend": result,
            "input_formats": ["structured_traces", "delimited_single_trace", "delimited_wide", "delimited_long"],
            "techniques": {"generic_1d_trace": bool(packages.get("measure") and packages.get("recipes")),
                           "linear_response_quantitation": bool(packages.get("measure") and packages.get("recipes")),
                           "relative_sec": bool(result.get("sec_calibrated_curve_support") and packages.get("measure") and packages.get("recipes")),
                           "relative_sec_requires_corrected_development_revision": True},
            "limits": {"traces_per_dataset": MAX_TRACES, "points_per_trace": MAX_POINTS,
                       "points_per_dataset": MAX_DATASET_POINTS, "payload_bytes": MAX_PAYLOAD_BYTES, "page_size": MAX_PAGE}}


@mcp.tool(annotations=WRITE, structured_output=True)
def measurement_register_dataset(dataset: Dataset, ctx: Context) -> dict[str, Any]:
    """Register bounded one-dimensional traces and their sample, injection, detector and unit metadata."""
    return register_dataset(identity(ctx), dataset)


@mcp.tool(annotations=WRITE, structured_output=True)
def measurement_register_csv(title: str, csv_text: str, axis_column: str, signal_column: str,
                             metadata: TraceMetadata, ctx: Context,
                             delimiter: Literal[",", "\t", ";"] = ",") -> dict[str, Any]:
    """Register one explicitly mapped numeric trace from bounded CSV/TSV text; metadata are the same as for a Trace, excluding axis and signal."""
    owner = identity(ctx)
    traces, source = parse_csv(csv_text, axis_column, [CSVChannel(column=signal_column, metadata=metadata)], delimiter)
    return register_dataset(owner, Dataset(title=title, traces=traces), source)


@mcp.tool(annotations=WRITE, structured_output=True)
def measurement_register_wide_csv(title: str, csv_text: str, axis_column: str,
                                  channels: Annotated[list[CSVChannel], Field(min_length=1, max_length=MAX_TRACES)],
                                  ctx: Context, delimiter: Literal[",", "\t", ";"] = ",") -> dict[str, Any]:
    """Register multiple explicitly mapped detector columns that share one numeric axis; retain the source text and mapping."""
    owner = identity(ctx)
    traces, source = parse_csv(csv_text, axis_column, channels, delimiter)
    return register_dataset(owner, Dataset(title=title, traces=traces), source)


@mcp.tool(annotations=WRITE, structured_output=True)
def measurement_register_long_csv(title: str, csv_text: str, axis_column: str, signal_column: str,
                                  group_columns: Annotated[list[str], Field(min_length=1, max_length=4)],
                                  groups: Annotated[list[CSVGroup], Field(min_length=1, max_length=MAX_TRACES)],
                                  ctx: Context, delimiter: Literal[",", "\t", ";"] = ",") -> dict[str, Any]:
    """Register row-oriented traces grouped by explicit column values, with a separate axis and metadata for each group."""
    owner = identity(ctx)
    traces, source = parse_long_csv(csv_text, axis_column, signal_column, group_columns, groups, delimiter)
    return register_dataset(owner, Dataset(title=title, traces=traces), source)


@mcp.tool(annotations=READ, structured_output=True)
def measurement_inspect_dataset(dataset_id: str, ctx: Context) -> dict[str, Any]:
    """Inspect available traces, sample/injection identities, axes and units without returning raw arrays."""
    owner = identity(ctx)
    dataset, digest = dataset_for(owner, dataset_id)
    source = source_for(owner, dataset_id)
    traces = []
    for trace in dataset["traces"]:
        summary = {k: v for k, v in trace.items() if k not in ("axis", "signal")}
        summary.update({"point_count": len(trace["axis"]), "axis_range": [trace["axis"][0], trace["axis"][-1]],
                        "signal_range": [min(trace["signal"]), max(trace["signal"])]})
        traces.append(summary)
    source_info = None if source is None else {k: v for k, v in source.items() if k != "content"}
    return {"dataset_id": dataset_id, "input_sha256": digest, "title": dataset["title"], "traces": traces, "source": source_info}


@mcp.tool(annotations=READ, structured_output=True)
def measurement_read_source(dataset_id: str, ctx: Context, offset: Annotated[int, Field(ge=0)] = 0,
                            limit: Annotated[int, Field(ge=1, le=MAX_PAGE)] = 1000) -> dict[str, Any]:
    """Read a bounded character page of the retained original CSV/TSV text and its import mapping."""
    owner = identity(ctx)
    dataset_for(owner, dataset_id)
    source = source_for(owner, dataset_id)
    if source is None:
        raise ValueError("This dataset has no retained text source")
    content = source.pop("content")
    return {"dataset_id": dataset_id, **source, "offset": offset, "count": len(content),
            "next_offset": offset + limit if offset + limit < len(content) else None,
            "text": content[offset:offset + limit]}


@mcp.tool(annotations=WRITE, structured_output=True)
def measurement_analyze_chromatogram(dataset_id: str, trace_id: str, method: ChromatogramMethod, ctx: Context) -> dict[str, Any]:
    """Process one trace and detect/integrate peaks using measure; peak areas are not compound identities or concentrations."""
    owner = identity(ctx)
    dataset, digest = dataset_for(owner, dataset_id)
    trace = trace_for(dataset, trace_id)
    spec = method.model_dump(mode="json")
    result = run_worker({"operation": "chromatogram", "trace": trace, "method": spec})
    if method.baseline == "als" or method.smoothing == "gaussian":
        steps = [b - a for a, b in zip(trace["axis"], trace["axis"][1:])]
        if max(steps) - min(steps) > 1e-6 * max(steps):
            result["warnings"].append("ALS baseline and Gaussian smoothing use sample indices; spacing is irregular, so their physical width varies along the axis.")
    return save_analysis(owner, dataset_id, trace_id, "chromatogram", spec, result, digest)


@mcp.tool(annotations=WRITE, structured_output=True)
def measurement_analyze_sec(dataset_id: str, trace_id: str, method: SECMethod, ctx: Context) -> dict[str, Any]:
    """Calculate relative SEC molecular-weight moments from a baseline-corrected concentration trace and explicit standards."""
    owner = identity(ctx)
    dataset, digest = dataset_for(owner, dataset_id)
    trace = trace_for(dataset, trace_id)
    if trace["separation"] != "SEC" or trace["axis_unit"] not in ("min", "mL"):
        raise ValueError("SEC analysis requires an SEC trace with an axis in min or mL")
    spec = method.model_dump(mode="json")
    result = run_worker({"operation": "sec", "trace": trace, "method": spec})
    return save_analysis(owner, dataset_id, trace_id, "sec", spec, result, digest)


@mcp.tool(annotations=WRITE, structured_output=True)
def measurement_quantify_peak(analysis_id: str, peak_id: int, method: PeakQuantitationMethod, ctx: Context) -> dict[str, Any]:
    """Quantify an explicitly assigned chromatogram peak with supplied linear response standards; does not establish peak identity."""
    owner = identity(ctx)
    source = analysis_for(owner, analysis_id)
    if source["kind"] != "chromatogram":
        raise ValueError("Peak quantitation requires a chromatogram analysis")
    peaks = source["result"].get("peaks", [])
    matches = [peak for peak in peaks if peak.get("peak_id") == peak_id]
    if len(matches) != 1 or not isinstance(matches[0].get("area"), (int, float)):
        raise ValueError("Peak was not found or does not have a finite integrated area")
    dataset, digest = dataset_for(owner, source["dataset_id"])
    trace = trace_for(dataset, source["trace_id"])
    spec = method.model_dump(mode="json") | {"source_analysis_id": analysis_id, "peak_id": peak_id,
            "response_unit": f'{trace["signal_unit"]} * {trace["axis_unit"]}'}
    result = run_worker({"operation": "quantitation", "method": spec, "response": matches[0]["area"]})
    return save_analysis(owner, source["dataset_id"], source["trace_id"], "peak_quantitation", spec, result, digest)


@mcp.tool(annotations=READ, structured_output=True)
def measurement_get_analysis(analysis_id: str, ctx: Context) -> dict[str, Any]:
    """Retrieve a compact analysis record, method, diagnostics and provenance."""
    record = analysis_for(identity(ctx), analysis_id)
    result = record.pop("result")
    record.update({"status": "completed_with_warnings" if result.get("warnings") else "completed",
                   "qualification": "experimental", "summary": result.get("summary", {"peak_count": result.get("peak_count")}),
                   "diagnostics": result.get("diagnostics"), "warnings": result.get("warnings", []), "notes": result.get("notes", [])})
    return record


@mcp.tool(annotations=READ, structured_output=True)
def measurement_compare_sec(analysis_ids: Annotated[list[str], Field(min_length=2, max_length=8)], ctx: Context) -> dict[str, Any]:
    """Compare stored SEC moments to the first analysis; report method and detector differences without assigning pass/fail."""
    if len(set(analysis_ids)) != len(analysis_ids):
        raise ValueError("Provide distinct analysis IDs")
    owner = identity(ctx)
    records = [analysis_for(owner, analysis_id) for analysis_id in analysis_ids]
    if any(record["kind"] != "sec" for record in records):
        raise ValueError("All selected analyses must be SEC analyses")
    traces = [trace_for(dataset_for(owner, record["dataset_id"])[0], record["trace_id"]) for record in records]
    reference = records[0]["result"]["summary"]
    fields = ("mn_g_mol", "mw_g_mol", "mz_g_mol", "mp_g_mol", "dispersity")
    rows = []
    for record in records:
        summary = record["result"]["summary"]
        changes = {}
        for field in fields:
            value, baseline = summary.get(field), reference.get(field)
            changes[field] = {"value": value, "difference": None if value is None or baseline is None else value - baseline,
                              "percent_difference": None if value is None or not baseline else 100 * (value - baseline) / baseline}
        rows.append({"analysis_id": record["analysis_id"], "trace_id": record["trace_id"], "values": changes,
                     "warnings": record["result"].get("warnings", []),
                     "calibration_r_squared": (record["result"].get("diagnostics") or {}).get("r_squared")})
    method_matches = all(r["provenance"]["method_sha256"] == records[0]["provenance"]["method_sha256"] for r in records)
    fingerprint = records[0]["provenance"].get("installed_r_code_md5") or {}
    fingerprints_match = bool(fingerprint.get("measure") and fingerprint.get("measure.sec")) and all(
        r["provenance"].get("installed_r_code_md5") == fingerprint for r in records)
    detector_matches = all((t["detector"], t["channel"], t["axis_unit"], t["signal_unit"]) ==
                           (traces[0]["detector"], traces[0]["channel"], traces[0]["axis_unit"], traces[0]["signal_unit"]) for t in traces)
    return {"qualification": "experimental", "reference_analysis_id": analysis_ids[0], "results": rows,
            "comparability": {"same_method": method_matches, "same_backend_r_code_fingerprints": fingerprints_match,
                              "same_detector_channel_and_units": detector_matches},
            "notes": ["Comparability flags are checks, not proof of equivalent samples, methods, or instrument conditions.",
                      "Differences are relative to the first analysis and do not establish statistical significance."],
            "artifact": True}


@mcp.tool(annotations=READ, structured_output=True)
def measurement_read_analysis_data(analysis_id: str, kind: Literal["peaks", "processed_trace", "distribution"],
                                   ctx: Context, offset: int = 0, limit: Annotated[int, Field(ge=1, le=MAX_PAGE)] = 100) -> dict[str, Any]:
    """Read a bounded page of peaks, processed trace points, or SEC distribution points."""
    if offset < 0:
        raise ValueError("offset must be nonnegative")
    record = analysis_for(identity(ctx), analysis_id)
    result = record["result"]
    if kind not in result:
        raise ValueError("This analysis does not contain the requested data")
    data = result[kind]
    if isinstance(data, list):
        count, page = len(data), data[offset:offset + limit]
    elif isinstance(data, dict):
        lengths = {len(v) for v in data.values() if isinstance(v, list)}
        if len(lengths) != 1:
            raise ValueError("Stored analysis data have inconsistent lengths")
        count = next(iter(lengths))
        page = {k: v[offset:offset + limit] for k, v in data.items()}
    else:
        raise ValueError("Stored analysis data have an unsupported shape")
    return {"analysis_id": analysis_id, "kind": kind, "offset": offset, "count": count,
            "next_offset": offset + limit if offset + limit < count else None, "data": page}


app = mcp.streamable_http_app()

if __name__ == "__main__":
    mcp.run(transport="stdio")
