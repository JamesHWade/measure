"""Experiment registry semantics, atomicity, ownership, concurrency and MCP contracts."""
import asyncio
from concurrent.futures import ThreadPoolExecutor
import json
import os
from pathlib import Path
import sqlite3
import subprocess
import sys

import pytest
from mcp import ClientSession, StdioServerParameters
from mcp.client.stdio import stdio_client
from pydantic import ValidationError

MCP_DIR = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(MCP_DIR))
import experiments as registry
import server
from storage import canonical, db_path, sha256


@pytest.fixture
def isolated_store(tmp_path, monkeypatch):
    monkeypatch.setenv("MEASURE_MCP_DATA_DIR", str(tmp_path))
    return tmp_path


def experiment(owner="a", key="design"):
    spec = registry.ExperimentSpec(title="Separation scouting", source="manual",
        proposal_reference="external-proposal-12",
        intended_conditions=[registry.Condition(name="flow", value=0.4, unit="mL/min"),
                             registry.Condition(name="column", value="C18"),
                             registry.Condition(name="flush", value=True)])
    return registry.create_experiment(owner, spec, key)


def attempt(experiment_id, owner="a", key="attempt", **changes):
    return registry.record_attempt(owner, experiment_id, registry.AttemptSpec(status="completed", **changes), key)


def measured_dataset(owner="a", **metadata):
    trace = {"trace_id": "uv", "sample_id": "sample-1", "injection_id": "injection-1", "channel": "uv",
             "separation": "LC", "detector": "UV", "axis_unit": "min", "signal_unit": "mAU",
             "axis": [0, 1, 2], "signal": [0, 2, 0]} | metadata
    return server.register_dataset(owner, server.Dataset(title="Scouting run", traces=[server.Trace(**trace)]))


def analysis(dataset, owner="a", **changes):
    result = {"summary": {"concentration": 4.25, "zero": 0, "flag": True, "label": "A"},
              "warnings": ["Outside calibration range"]} | changes
    return server.save_analysis(owner, dataset["dataset_id"], "uv", "peak_quantitation", {"unit": "mg/L"}, result, dataset["input_sha256"])


def observation(metric="concentration", **changes):
    return registry.ResultSpec.model_validate({"metric": metric, "status": "observed", "unit": "mg/L", "value": 0} | changes)


@pytest.mark.parametrize("model,payload", [
    (registry.Condition, {"name": "temperature", "value": 25}),
    (registry.Condition, {"name": "column", "value": "C18", "unit": "m"}),
    (registry.Condition, {"name": "flag", "value": True, "unit": "1"}),
    (registry.Condition, {"name": "flow", "value": float("inf"), "unit": "mL/min"}),
    (registry.Condition, {"name": "flow", "value": 10**400, "unit": "1"}),
    (registry.Condition, {"name": "column", "value": ["C18"]}),
    (registry.AttemptSpec, {"status": "failed"}),
    (registry.AttemptSpec, {"status": "unknown", "explanation": "  "}),
    (registry.AttemptSpec, {"status": "completed", "started_at": "2026-10-28T12:00:00"}),
    (registry.AttemptSpec, {"status": "completed", "started_at": "2026-10-28T12:00:00Z", "ended_at": "2026-10-28T11:00:00Z"}),
    (registry.AttemptSpec, {"status": "running", "ended_at": "2026-10-28T12:00:00Z"}),
    (registry.AttemptSpec, {"status": "completed", "replicate_index": 2}),
    (registry.AttemptSpec, {"status": "completed", "fidelity": True}),
    (registry.AttemptSpec, {"status": "completed", "fidelity": 1.1}),
    (registry.ResultSpec, {"metric": "A", "status": "observed", "unit": "1", "value": True}),
    (registry.ResultSpec, {"metric": "A", "status": "observed", "unit": "1", "value": "1"}),
    (registry.ResultSpec, {"metric": "A", "status": "observed", "unit": "1", "value": 10**400}),
    (registry.ResultSpec, {"metric": "A", "status": "observed", "unit": "1", "value": float("nan")}),
    (registry.ResultSpec, {"metric": "A", "status": "observed", "unit": " ", "value": 0}),
    (registry.ResultSpec, {"metric": "A", "status": "not_detected", "unit": "1", "value": 0, "explanation": "No signal"}),
    (registry.ResultSpec, {"metric": "A", "status": "below_limit", "unit": "1", "explanation": "Below quantitation"}),
    (registry.ResultSpec, {"metric": "A", "status": "censored", "unit": "1", "explanation": "Censored", "lower_bound": 2, "upper_bound": 1}),
    (registry.ResultSpec, {"metric": "A", "status": "observed", "unit": "1", "analysis_id": "x", "summary_field": "concentration", "value": 1}),
])
def test_invalid_scientific_records(model, payload):
    with pytest.raises(ValidationError):
        model.model_validate(payload)


def test_conditions_timestamps_and_duplicate_identity():
    values = [registry.Condition(name="numeric", value=2, unit="1"), registry.Condition(name="category", value="2"),
              registry.Condition(name="boolean", value=False)]
    assert [type(v.value) for v in values] == [int, str, bool]
    with pytest.raises(ValidationError):
        registry.ExperimentSpec(title="x", source="manual", intended_conditions=[values[0], values[0]])
    with pytest.raises(ValidationError):
        registry.AttemptSpec(status="completed", measurements=[{"dataset_id": "d", "trace_id": "t"}] * 2)
    historical = registry.AttemptSpec(status="completed", started_at="2026-10-28T08:00:00-04:00", ended_at="2026-10-28T12:05:00Z")
    assert historical.model_dump(mode="json")["started_at"] == "2026-10-28T12:00:00Z"


def test_attempt_revisions_results_and_explicit_corrections(isolated_store):
    exp = experiment()
    ds = measured_dataset()
    run = attempt(exp["experiment_id"], actual_conditions=[{"name": "flow", "value": 0.39, "unit": "mL/min"}],
                  measurements=[{"dataset_id": ds["dataset_id"], "trace_id": "uv"}],
                  replicate_group="g", replicate_kind="technical", replicate_index=1)
    assert run["spec"]["measurements"][0]["metadata"]["injection_id"] == "injection-1"
    assert registry.get_experiment("a", exp["experiment_id"])["spec"]["intended_conditions"][0]["value"] == 0.4
    stored_analysis = analysis(ds)
    specs = [observation(value=None, analysis_id=stored_analysis["analysis_id"], summary_field="concentration"),
             registry.ResultSpec(metric="B", status="not_detected", unit="mg/L", explanation="No peak in assigned window"),
             registry.ResultSpec(metric="C", status="below_limit", unit="mg/L", upper_bound=0.05, explanation="Below declared reporting limit"),
             registry.ResultSpec(metric="D", status="censored", unit="min", lower_bound=20, explanation="Not eluted by end of run"),
             registry.ResultSpec(metric="E", status="missing", unit="1", explanation="Detector not acquired"),
             registry.ResultSpec(metric="F", status="failed", unit="1", explanation="Processing failed"),
             registry.ResultSpec(metric="G", status="infeasible", unit="1", explanation="Method exceeds configured limit"),
             observation(metric="valid-zero", unit="1")]
    batch = registry.record_results("a", run["attempt_id"], 1, specs, "results")
    first = batch["results"][0]
    assert first["outcome"]["value"] == 4.25
    assert first["outcome"]["provenance"]["analysis_warnings"] == ["Outside calibration range"]
    assert first["outcome"]["provenance"]["unit_origin"] == "caller_declared"
    assert first["outcome"]["provenance"]["analysis_provenance"] == stored_analysis["provenance"]
    assert [v["outcome"]["value"] for v in batch["results"]][1:] == [None] * 6 + [0]
    correction = registry.AttemptSpec.model_validate({**run["spec"], "measurements": [{"dataset_id": ds["dataset_id"], "trace_id": "uv"}],
        "actual_conditions": [{"name": "flow", "value": 0.395, "unit": "mL/min"}]})
    revised = registry.revise_attempt("a", run["attempt_id"], 1, correction, "Correct instrument readback", "revise")
    assert revised["revision"] == 2
    assert registry.get_attempt("a", run["attempt_id"], 1)["spec"]["actual_conditions"][0]["value"] == 0.39
    assert registry.get_result("a", first["result_id"])["attempt_revision"] == 1
    replacement = observation(value=4.1, supersedes_result_id=first["result_id"], explanation="Independent corrected quantitation")
    latest = registry.record_results("a", run["attempt_id"], 2, [replacement], "correct-result")["results"][0]
    old = registry.get_result("a", first["result_id"])
    assert old["outcome"]["value"] == 4.25 and old["superseded_by_result_id"] == latest["result_id"]
    assert latest["attempt_revision"] == 2 and latest["outcome"]["value"] == 4.1
    page = registry.list_results("a", run["attempt_id"], limit=3)
    assert page["count"] == 8 and page["next_offset"] == 3 and len(page["items"]) == 3
    assert registry.list_results("a", run["attempt_id"], include_superseded=True)["count"] == 9
    assert registry.list_attempts("a", exp["experiment_id"])["items"][0]["revision"] == 2


def test_retries_conflicts_and_owner_isolation(isolated_store):
    exp = experiment()
    assert experiment() == exp
    other = experiment("b")
    assert other["experiment_id"] != exp["experiment_id"]
    assert registry.list_experiments("b")["count"] == 1
    with pytest.raises(ValueError, match="different registry request"):
        registry.create_experiment("a", registry.ExperimentSpec(title="Changed", source="manual"), "design")
    run = attempt(exp["experiment_id"])
    outcome = registry.record_results("a", run["attempt_id"], 1, [observation()], "outcome")
    assert registry.record_results("a", run["attempt_id"], 1, [observation()], "outcome") == outcome
    for fn, args in [(registry.get_experiment, (exp["experiment_id"],)), (registry.get_attempt, (run["attempt_id"],)),
                     (registry.get_result, (outcome["results"][0]["result_id"],)),
                     (registry.list_attempts, (exp["experiment_id"],)), (registry.list_results, (run["attempt_id"],))]:
        with pytest.raises(ValueError, match="not found or is not accessible"):
            fn("b", *args)
    with pytest.raises(ValueError, match="not found or is not accessible"):
        attempt(exp["experiment_id"], "b", "other-attempt")
    with pytest.raises(ValueError):
        registry.record_results("b", run["attempt_id"], 1, [observation()], "outcome")


def test_atomic_batch_and_analysis_evidence_rejection(isolated_store):
    exp = experiment()
    ds = measured_dataset()
    run = attempt(exp["experiment_id"], measurements=[{"dataset_id": ds["dataset_id"], "trace_id": "uv"}])
    wrong_ds = measured_dataset(injection_id="other-injection")
    wrong = analysis(wrong_ds)
    with pytest.raises(ValueError, match="linked to this attempt revision"):
        registry.record_results("a", run["attempt_id"], 1,
            [observation(metric="valid"), observation(value=None, analysis_id=wrong["analysis_id"], summary_field="concentration")], "atomic")
    assert registry.list_results("a", run["attempt_id"])["count"] == 0
    assert registry.record_results("a", run["attempt_id"], 1, [observation(metric="valid")], "atomic")["results"]
    valid = analysis(ds)
    for field in ("flag", "label", "does-not-exist"):
        with pytest.raises(ValueError, match="finite numeric value"):
            registry.record_results("a", run["attempt_id"], 1,
                [observation(value=None, analysis_id=valid["analysis_id"], summary_field=field)], "bad-" + field)
    chrom = analysis(ds, summary=None, peak_count=2)
    with pytest.raises(ValueError, match="finite numeric value"):
        registry.record_results("a", run["attempt_id"], 1,
            [observation(metric="bad-count", unit="1", value=None, analysis_id=chrom["analysis_id"], summary_field="peak_count")], "bad-count")
    numerical = {"peak_count": 2, "warnings": []}
    chrom = server.save_analysis("a", ds["dataset_id"], "uv", "chromatogram", {}, numerical, ds["input_sha256"])
    count = registry.record_results("a", run["attempt_id"], 1,
        [observation(metric="peak-count", unit="1", value=None, analysis_id=chrom["analysis_id"], summary_field="peak_count")], "peak-count")
    assert count["results"][0]["outcome"]["value"] == 2
    other_ds = measured_dataset("b")
    other_analysis = analysis(other_ds, "b")
    with pytest.raises(ValueError, match="not found or is not accessible"):
        registry.record_results("a", run["attempt_id"], 1,
            [observation(value=None, analysis_id=other_analysis["analysis_id"], summary_field="concentration")], "other-analysis")
    for dataset_id, trace_id in [(other_ds["dataset_id"], "uv"), (ds["dataset_id"], "absent")]:
        with pytest.raises(ValueError):
            attempt(exp["experiment_id"], key="bad-measurement", measurements=[{"dataset_id": dataset_id, "trace_id": trace_id}])
    assert registry.list_attempts("a", exp["experiment_id"])["count"] == 1


def test_concurrent_retries_and_optimistic_revisions(isolated_store):
    exp = experiment()
    with ThreadPoolExecutor(max_workers=6) as pool:
        runs = list(pool.map(lambda _: attempt(exp["experiment_id"], key="same-request"), range(6)))
    assert len({r["attempt_id"] for r in runs}) == 1
    run_id = runs[0]["attempt_id"]
    assert registry.list_attempts("a", exp["experiment_id"])["count"] == 1

    def revise(i):
        try:
            return registry.revise_attempt("a", run_id, 1, registry.AttemptSpec(status="completed", explanation=f"readback {i}"), f"Correction {i}", f"revision-{i}")
        except ValueError as exc:
            return str(exc)
    with ThreadPoolExecutor(max_workers=2) as pool:
        revisions = list(pool.map(revise, range(2)))
    assert sum(isinstance(v, dict) for v in revisions) == 1
    assert sum("revision conflict" in v for v in revisions if isinstance(v, str)) == 1
    assert registry.get_attempt("a", run_id)["revision"] == 2
    winner = next(i for i, v in enumerate(revisions) if isinstance(v, dict))
    registry.revise_attempt("a", run_id, 2, registry.AttemptSpec(status="completed"), "Later correction", "revision-3")
    assert revise(winner) == revisions[winner], "A retry returns its original receipt even after a later revision"
    with pytest.raises(ValueError, match="revision conflict"):
        registry.record_results("a", run_id, 2, [observation()], "stale-outcome")


def test_supersession_is_linear_and_atomic(isolated_store):
    exp = experiment()
    run = attempt(exp["experiment_id"])
    first = registry.record_results("a", run["attempt_id"], 1, [observation()], "first")["results"][0]
    with pytest.raises(ValueError, match="explicitly supersede"):
        registry.record_results("a", run["attempt_id"], 1, [observation()], "duplicate")
    def replace(i):
        spec = observation(value=i, supersedes_result_id=first["result_id"], explanation=f"Correction {i}")
        try:
            return registry.record_results("a", run["attempt_id"], 1, [spec], f"replace-{i}")
        except ValueError as exc:
            return str(exc)
    with ThreadPoolExecutor(max_workers=2) as pool:
        values = list(pool.map(replace, (1, 2)))
    assert sum(isinstance(v, dict) for v in values) == 1
    assert registry.list_results("a", run["attempt_id"])["count"] == 1
    assert registry.list_results("a", run["attempt_id"], True)["count"] == 2


def test_corrected_assignment_preserves_history_and_evaluator_origin(isolated_store):
    exp = experiment()
    run = attempt(exp["experiment_id"], evaluator_kind="simulator", evaluator_reference="synthetic-reference",
                  evaluator_revision="v1", fidelity=0.5, training_snapshot_reference="caller-snapshot-1")
    first = registry.record_results("a", run["attempt_id"], 1,
        [observation(analyte_reference="candidate A"), observation(metric="occupied", analyte_reference="candidate B")], "initial")["results"][0]
    changed = observation(metric="reassigned", analyte_reference="candidate B", value=2.0,
                          supersedes_result_id=first["result_id"], explanation="Corrected the caller's label and metric")
    second = registry.record_results("a", run["attempt_id"], 1, [changed], "reassign")["results"][0]
    assert second["outcome"]["provenance"]["evaluator"]["evaluator_kind"] == "simulator"
    assert second["outcome"]["provenance"]["evaluator"]["fidelity"] == 0.5
    assert registry.get_result("a", first["result_id"])["outcome"]["analyte_reference"] == "candidate A"
    assert registry.get_result("a", first["result_id"])["superseded_by_result_id"] == second["result_id"]
    with pytest.raises(ValueError, match="collide"):
        registry.record_results("a", run["attempt_id"], 1,
            [observation(metric="occupied", analyte_reference="candidate B", supersedes_result_id=second["result_id"], explanation="Conflicting merge")], "collision")
    another = attempt(exp["experiment_id"], key="another-attempt")
    with pytest.raises(ValueError, match="same attempt"):
        registry.record_results("a", another["attempt_id"], 1,
            [observation(supersedes_result_id=second["result_id"], explanation="Wrong attempt")], "wrong-attempt")
    assert registry.list_results("a", run["attempt_id"])["count"] == 2


def test_payload_pagination_limits_and_persistence_in_fresh_process(isolated_store):
    exp = experiment()
    run = attempt(exp["experiment_id"])
    for offset, limit in [(-1, 20), (0, 0), (0, 51)]:
        with pytest.raises(ValueError):
            registry.list_experiments("a", offset, limit)
    with pytest.raises(ValueError, match="byte limit"):
        registry.bounded("x" * registry.MAX_RECORD_BYTES)
    with pytest.raises(ValueError, match="idempotency"):
        attempt(exp["experiment_id"], key=" ")
    script = "import experiments as r,json,sys; print(json.dumps(r.get_attempt('a',sys.argv[1])))"
    env = dict(os.environ, PYTHONPATH=str(MCP_DIR))
    fresh = subprocess.run([sys.executable, "-c", script, run["attempt_id"]], env=env, text=True, capture_output=True, check=True)
    assert json.loads(fresh.stdout) == run


def test_existing_analysis_database_is_preserved(isolated_store):
    con = sqlite3.connect(db_path())
    con.execute("CREATE TABLE datasets (id TEXT PRIMARY KEY, owner TEXT NOT NULL, digest TEXT NOT NULL, payload TEXT NOT NULL, created_at TEXT NOT NULL)")
    payload = {"title": "Legacy", "traces": []}
    con.execute("INSERT INTO datasets VALUES (?, ?, ?, ?, ?)", ("legacy", "a", sha256(payload), canonical(payload), "2026-10-27T12:00:00Z"))
    con.commit()
    con.close()
    exp = experiment()
    assert server.dataset_for("a", "legacy") == (payload, sha256(payload))
    assert registry.get_experiment("a", exp["experiment_id"])["spec"]["title"] == "Separation scouting"


def test_registry_mcp_round_trip_without_r(tmp_path):
    params = StdioServerParameters(command=sys.executable, args=[str(MCP_DIR / "server.py")],
        env=dict(os.environ, MEASURE_MCP_DATA_DIR=str(tmp_path), MEASURE_MCP_IDENTITY="local", MEASURE_MCP_R="/nonexistent/R"))
    async def exercise():
        async with stdio_client(params) as (read, write):
            async with ClientSession(read, write) as client:
                await client.initialize()
                tools = {t.name: t for t in (await client.list_tools()).tools}
                assert tools["measurement_record_results"].annotations.idempotentHint
                assert not tools["measurement_record_results"].annotations.readOnlyHint
                assert tools["measurement_get_attempt"].annotations.readOnlyHint
                async def call(name, arguments):
                    response = await client.call_tool(name, arguments)
                    assert not response.isError, response.content
                    assert response.structuredContent is not None
                    return response.structuredContent
                caps = await call("measurement_registry_capabilities", {})
                assert not caps["bo_suggestions"] and not caps["instrument_actuation"]
                exp_args = {"spec": {"title": "MCP experiment", "source": "manual"}, "idempotency_key": "create"}
                exp = await call("measurement_create_experiment", exp_args)
                assert await call("measurement_create_experiment", exp_args) == exp
                run = await call("measurement_record_attempt", {"experiment_id": exp["experiment_id"], "spec": {"status": "completed"}, "idempotency_key": "attempt"})
                batch = await call("measurement_record_results", {"attempt_id": run["attempt_id"], "expected_attempt_revision": 1,
                    "results": [{"metric": "response", "status": "observed", "unit": "1", "value": 0}], "idempotency_key": "outcomes"})
                assert (await call("measurement_get_result", {"result_id": batch["results"][0]["result_id"]}))["outcome"]["value"] == 0
                assert (await call("measurement_list_experiments", {}))["count"] == 1
                invalid = await client.call_tool("measurement_record_results", {"attempt_id": run["attempt_id"], "expected_attempt_revision": 1,
                    "results": [{"metric": "response", "status": "failed", "unit": "1", "value": 0}], "idempotency_key": "invalid"})
                assert invalid.isError
    asyncio.run(exercise())
