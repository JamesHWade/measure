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
import sqlite3
import subprocess
import uuid
from datetime import datetime, timezone
from pathlib import Path
from typing import Annotated, Any, Literal

from mcp.server.fastmcp import Context, FastMCP
from mcp.types import ToolAnnotations
from pydantic import BaseModel, ConfigDict, Field, model_validator

ROOT = Path(__file__).resolve().parent
MAX_TRACES = 32
MAX_POINTS = 50_000
MAX_DATASET_POINTS = 150_000
MAX_PAYLOAD_BYTES = 4_000_000
MAX_PAGE = 1000


def canonical(value: object) -> str:
    return json.dumps(value, sort_keys=True, separators=(",", ":"), ensure_ascii=False, allow_nan=False)


def sha256(value: object) -> str:
    return hashlib.sha256(canonical(value).encode("utf-8")).hexdigest()


def now() -> str:
    return datetime.now(timezone.utc).isoformat()


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


def db_path() -> Path:
    root = Path(os.environ.get("MEASURE_MCP_DATA_DIR", str(Path.home() / ".local" / "share" / "measure-mcp")))
    root.mkdir(mode=0o700, parents=True, exist_ok=True)
    return root / "measure.sqlite3"


def connect() -> sqlite3.Connection:
    con = sqlite3.connect(db_path(), timeout=30)
    con.execute("PRAGMA foreign_keys=ON")
    con.executescript("""
    CREATE TABLE IF NOT EXISTS datasets (
      id TEXT PRIMARY KEY, owner TEXT NOT NULL, digest TEXT NOT NULL,
      payload TEXT NOT NULL, created_at TEXT NOT NULL
    );
    CREATE TABLE IF NOT EXISTS analyses (
      id TEXT PRIMARY KEY, owner TEXT NOT NULL, dataset_id TEXT NOT NULL,
      trace_id TEXT NOT NULL, kind TEXT NOT NULL, method TEXT NOT NULL,
      result TEXT NOT NULL, provenance TEXT NOT NULL, created_at TEXT NOT NULL,
      FOREIGN KEY(dataset_id) REFERENCES datasets(id)
    );
    """)
    return con


def dataset_for(owner: str, dataset_id: str) -> tuple[dict, str]:
    with connect() as con:
        row = con.execute("SELECT payload, digest FROM datasets WHERE id=? AND owner=?", (dataset_id, owner)).fetchone()
    if row is None:
        raise ValueError("Dataset was not found or is not accessible")
    return json.loads(row[0]), row[1]


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
    provenance = {"input_sha256": digest, "method_sha256": sha256(method), "worker_sha256": worker_digest,
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


@mcp.tool(annotations=READ, structured_output=True)
def measurement_capabilities(ctx: Context) -> dict[str, Any]:
    """Report installed R backends and the experimental operations available."""
    identity(ctx)
    result = run_worker({"operation": "capabilities"})
    packages = result.get("packages", {})
    return {"service": "measure-separations", "qualification": "experimental", "backend": result,
            "techniques": {"generic_1d_trace": bool(packages.get("measure") and packages.get("recipes")),
                           "linear_response_quantitation": bool(packages.get("measure") and packages.get("recipes")),
                           "relative_sec": bool(result.get("sec_calibrated_curve_support") and packages.get("measure") and packages.get("recipes")),
                           "relative_sec_requires_corrected_development_revision": True},
            "limits": {"traces_per_dataset": MAX_TRACES, "points_per_trace": MAX_POINTS,
                       "points_per_dataset": MAX_DATASET_POINTS, "page_size": MAX_PAGE}}


@mcp.tool(annotations=WRITE, structured_output=True)
def measurement_register_dataset(dataset: Dataset, ctx: Context) -> dict[str, Any]:
    """Register bounded one-dimensional traces and their sample, injection, detector and unit metadata."""
    owner = identity(ctx)
    payload = dataset.model_dump(mode="json")
    serialized = canonical(payload)
    if len(serialized.encode("utf-8")) > MAX_PAYLOAD_BYTES:
        raise ValueError("Dataset exceeds the request-size limit")
    dataset_id, digest, created = str(uuid.uuid4()), sha256(payload), now()
    with connect() as con:
        con.execute("INSERT INTO datasets VALUES (?, ?, ?, ?, ?)", (dataset_id, owner, digest, serialized, created))
    return {"dataset_id": dataset_id, "input_sha256": digest, "created_at": created,
            "trace_count": len(dataset.traces), "point_count": sum(len(t.axis) for t in dataset.traces)}


@mcp.tool(annotations=WRITE, structured_output=True)
def measurement_register_csv(title: str, csv_text: str, axis_column: str, signal_column: str,
                             metadata: TraceMetadata, ctx: Context,
                             delimiter: Literal[",", "\t", ";"] = ",") -> dict[str, Any]:
    """Register one explicitly mapped numeric trace from bounded CSV/TSV text; metadata are the same as for a Trace, excluding axis and signal."""
    if len(csv_text.encode("utf-8")) > MAX_PAYLOAD_BYTES:
        raise ValueError("CSV exceeds the request-size limit")
    reader = csv.DictReader(io.StringIO(csv_text.lstrip("\ufeff"), newline=""), delimiter=delimiter)
    fields = reader.fieldnames or []
    if len(set(fields)) != len(fields) or axis_column == signal_column or axis_column not in fields or signal_column not in fields:
        raise ValueError("CSV needs unique headers and distinct existing axis and signal columns")
    axis, signal = [], []
    try:
        for row in reader:
            if len(axis) >= MAX_POINTS:
                raise ValueError("CSV exceeds the point limit")
            if None in row:
                raise ValueError("CSV contains a row with extra fields")
            axis.append(float(row[axis_column]))
            signal.append(float(row[signal_column]))
    except (TypeError, KeyError) as exc:
        raise ValueError("CSV rows must have numeric values in both selected columns") from exc
    trace = Trace.model_validate({**metadata.model_dump(), "axis": axis, "signal": signal})
    return measurement_register_dataset(Dataset(title=title, traces=[trace]), ctx)


@mcp.tool(annotations=READ, structured_output=True)
def measurement_inspect_dataset(dataset_id: str, ctx: Context) -> dict[str, Any]:
    """Inspect available traces, sample/injection identities, axes and units without returning raw arrays."""
    dataset, digest = dataset_for(identity(ctx), dataset_id)
    traces = []
    for trace in dataset["traces"]:
        summary = {k: v for k, v in trace.items() if k not in ("axis", "signal")}
        summary.update({"point_count": len(trace["axis"]), "axis_range": [trace["axis"][0], trace["axis"][-1]],
                        "signal_range": [min(trace["signal"]), max(trace["signal"])]})
        traces.append(summary)
    return {"dataset_id": dataset_id, "input_sha256": digest, "title": dataset["title"], "traces": traces}


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
        rows.append({"analysis_id": record["analysis_id"], "trace_id": record["trace_id"], "values": changes})
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
