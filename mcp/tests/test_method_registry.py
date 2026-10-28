"""Immutable method identity, retry safety and experiment linkage over MCP."""
import asyncio
from concurrent.futures import ThreadPoolExecutor
import json
import os
from pathlib import Path
import subprocess
import sys

import pytest
from mcp import ClientSession, StdioServerParameters
from mcp.client.stdio import stdio_client

MCP_DIR = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(MCP_DIR))
import experiments
import method_registry as registry
from methods import MethodConstraints, MethodDeclaration
from storage import canonical, sha256


def method_payload(flow=1000):
    return {"schema_version": "1.0", "time_unit": "s", "flow_unit": "uL/min", "solvents": ["A", "B"],
            "program": {"kind": "gradient_absolute", "points": [
                {"time": 0, "state": {"flow": flow, "solvent_fractions": {"A": 0.9, "B": 0.1}}},
                {"time": 120, "state": {"flow": flow, "solvent_fractions": {"A": 0.1, "B": 0.9}}}]}}


def register(owner="a", key="method", flow=1000, constraints=None):
    return registry.register_method(owner, "LC scouting", MethodDeclaration.model_validate(method_payload(flow)), constraints, key)


@pytest.fixture(autouse=True)
def isolated_store(tmp_path, monkeypatch):
    monkeypatch.setenv("MEASURE_MCP_DATA_DIR", str(tmp_path))


def test_retains_declaration_canonical_program_constraints_and_digests():
    record = register()
    assert record["evaluation"]["declaration"]["time_unit"] == "s"
    program = record["evaluation"]["canonical"]
    assert program["acquisition_duration_min"] == 2
    assert program["total_solvent_use_ml"] == {"A": pytest.approx(1), "B": pytest.approx(1)}
    assert record["canonical_program_sha256"] == sha256(program)
    expected = {"title": "LC scouting", "declaration": record["evaluation"]["declaration"], "constraints": record["constraints"]}
    assert record["input_sha256"] == sha256(expected)
    assert registry.get_method("a", record["method_id"]) == record
    assert register() == record
    assert registry.list_methods("a")["count"] == 1
    assert "evaluation" not in registry.list_methods("a")["items"][0]
    duration = method_payload()
    duration["program"] = {"kind": "gradient_duration", "initial_state": duration["program"]["points"][0]["state"],
                           "segments": [{"duration": 120, "end_state": duration["program"]["points"][1]["state"]}]}
    second = registry.register_method("a", "LC scouting", MethodDeclaration.model_validate(duration), None, "duration")
    assert second["canonical_program_sha256"] == record["canonical_program_sha256"]
    assert second["input_sha256"] != record["input_sha256"]


def test_method_identity_ownership_and_request_reuse():
    record = register()
    other = register("b")
    assert record["method_id"] != other["method_id"]
    assert registry.list_methods("b")["count"] == 1
    with pytest.raises(ValueError, match="not found or is not accessible"):
        registry.get_method("b", record["method_id"])
    with pytest.raises(ValueError, match="different registry request"):
        register(flow=2000)
    with pytest.raises(ValueError, match="different registry request"):
        experiments.create_experiment("a", experiments.ExperimentSpec(title="Other operation", source="manual"), "method")
    for offset, limit in [(-1, 1), (0, 51), (False, 20), (0, True)]:
        with pytest.raises(ValueError):
            registry.list_methods("a", offset, limit)
    for title in (" ", "x" * 201):
        with pytest.raises(ValueError, match="title"):
            registry.register_method("a", title, MethodDeclaration.model_validate(method_payload()), None, "bad-title")


def test_infeasible_registration_rolls_back_and_concurrent_retries_are_one_record():
    with pytest.raises(ValueError, match="violates declared constraints"):
        register(constraints=MethodConstraints(max_flow_ml_min=0.5))
    assert registry.list_methods("a")["count"] == 0
    with ThreadPoolExecutor(max_workers=6) as pool:
        records = list(pool.map(lambda _: register(), range(6)))
    assert len({r["method_id"] for r in records}) == 1
    assert registry.list_methods("a")["count"] == 1
    for i in range(3):
        register(key=f"more-{i}")
    page = registry.list_methods("a", limit=2)
    assert page["count"] == 4 and page["next_offset"] == 2
    last = registry.list_methods("a", offset=page["next_offset"], limit=2)
    assert last["next_offset"] is None
    assert set(r["method_id"] for r in page["items"]).isdisjoint(r["method_id"] for r in last["items"])


def test_intended_and_reported_methods_retain_separate_immutable_snapshots():
    intended = register()
    reported = register(key="readback", flow=1100)
    design = experiments.create_experiment("a", experiments.ExperimentSpec(title="Scouting", source="manual",
        executable_method_id=intended["method_id"]), "design")
    assert design["executable_method"]["input_sha256"] == intended["input_sha256"]
    run = experiments.record_attempt("a", design["experiment_id"], experiments.AttemptSpec(status="completed",
        executable_method_id=reported["method_id"]), "attempt")
    assert run["executable_method"]["input_sha256"] == reported["input_sha256"]
    assert experiments.get_experiment("a", design["experiment_id"]) == design
    corrected = experiments.revise_attempt("a", run["attempt_id"], 1, experiments.AttemptSpec(status="completed",
        executable_method_id=intended["method_id"]), "Corrected reported method", "correction")
    assert corrected["revision"] == 2
    assert corrected["executable_method"]["method_id"] == intended["method_id"]
    assert experiments.get_attempt("a", run["attempt_id"], 1) == run | {"latest_revision": 2}
    assert experiments.AttemptSpec.model_validate(corrected["spec"]).executable_method_id == intended["method_id"]
    assert experiments.ExperimentSpec.model_validate(design["spec"]).executable_method_id == intended["method_id"]
    env = dict(os.environ, PYTHONPATH=str(MCP_DIR))
    code = "import method_registry,json; print(json.dumps(method_registry.get_method('a', " + repr(intended["method_id"]) + ")))"
    restored = json.loads(subprocess.check_output([sys.executable, "-c", code], env=env, text=True))
    assert restored == intended


def test_foreign_or_missing_method_links_fail_atomically_and_old_records_still_read():
    foreign = register("b")
    for method_id in (foreign["method_id"], "missing"):
        with pytest.raises(ValueError, match="not found or is not accessible"):
            experiments.create_experiment("a", experiments.ExperimentSpec(title="Invalid link", source="manual",
                executable_method_id=method_id), "bad-design")
    assert experiments.list_experiments("a")["count"] == 0
    design = experiments.create_experiment("a", experiments.ExperimentSpec(title="No executable method", source="manual"), "design")
    assert design["executable_method"] is None
    with pytest.raises(ValueError, match="not found or is not accessible"):
        experiments.record_attempt("a", design["experiment_id"], experiments.AttemptSpec(status="planned",
            executable_method_id=foreign["method_id"]), "bad-attempt")
    assert experiments.list_attempts("a", design["experiment_id"])["count"] == 0
    from storage import registry_connection
    old_payload = {"title": "Prior version record", "source": "archive", "intended_conditions": []}
    with registry_connection(write=True) as con:
        con.execute("INSERT INTO experiments VALUES (?, ?, ?, ?, ?)",
                    ("old", "a", canonical(old_payload), sha256(old_payload), "2026-01-01T00:00:00Z"))
    old = experiments.get_experiment("a", "old")
    assert old["spec"] == old_payload and old["executable_method"] is None


def test_stdio_validates_registers_and_links_methods_without_r(tmp_path):
    params = StdioServerParameters(command=sys.executable, args=[str(MCP_DIR / "server.py")],
        env=dict(os.environ, MEASURE_MCP_IDENTITY="local", MEASURE_MCP_R="/missing/R"))

    def result(response):
        assert not response.isError, response.content
        return response.structuredContent

    async def exercise():
        async with stdio_client(params) as (read, write):
            async with ClientSession(read, write) as client:
                await client.initialize()
                tools = {tool.name: tool for tool in (await client.list_tools()).tools}
                assert tools["measurement_validate_method"].annotations.readOnlyHint is True
                assert tools["measurement_register_method"].annotations.idempotentHint is True
                assert tools["measurement_register_method"].annotations.readOnlyHint is False
                caps = result(await client.call_tool("measurement_method_capabilities", {}))
                assert caps["instrument_actuation"] is False and caps["hardware_qualification"] is False
                arguments = {"declaration": method_payload()}
                validation = result(await client.call_tool("measurement_validate_method", arguments))
                assert validation["feasible"] is True
                invalid = result(await client.call_tool("measurement_validate_method", arguments | {"constraints": {"max_flow_ml_min": 0.5}}))
                assert invalid["feasible"] is False and invalid["violations"]
                assert (await client.call_tool("measurement_register_method", arguments | {
                    "title": "Infeasible", "idempotency_key": "invalid", "constraints": {"max_flow_ml_min": 0.5}})).isError
                assert result(await client.call_tool("measurement_list_methods", {}))["count"] == 0
                request = arguments | {"title": "Scouting gradient", "idempotency_key": "method"}
                record = result(await client.call_tool("measurement_register_method", request))
                assert result(await client.call_tool("measurement_register_method", request)) == record
                assert result(await client.call_tool("measurement_get_method", {"method_id": record["method_id"]})) == record
                design = result(await client.call_tool("measurement_create_experiment", {
                    "spec": {"title": "Scouting", "source": "manual", "executable_method_id": record["method_id"]}, "idempotency_key": "design"}))
                run = result(await client.call_tool("measurement_record_attempt", {
                    "experiment_id": design["experiment_id"], "spec": {"status": "planned", "executable_method_id": record["method_id"]}, "idempotency_key": "attempt"}))
                assert run["executable_method"] == design["executable_method"]
                assert run["executable_method"]["input_sha256"] == record["input_sha256"]

    asyncio.run(exercise())


def test_retry_receipts_from_before_optional_method_fields_remain_valid():
    """Exercise persisted v1 requests that predate executable_method_id."""
    from storage import now, registry_connection
    design_spec = experiments.ExperimentSpec(title="Prior design", source="manual")
    attempt_spec = experiments.AttemptSpec(status="planned")
    old_design = design_spec.model_dump(mode="json")
    old_attempt = attempt_spec.model_dump(mode="json")
    old_design.pop("executable_method_id")
    old_attempt.pop("executable_method_id")
    cases = [
        ({"operation": "create_experiment", "spec": old_design}, "prior-design", {"experiment_id": "old-design"}),
        ({"operation": "record_attempt", "experiment_id": "old-design", "spec": old_attempt}, "prior-attempt", {"attempt_id": "old-attempt", "revision": 1}),
        ({"operation": "revise_attempt", "attempt_id": "old-attempt", "expected_revision": 1,
          "spec": old_attempt, "reason": "Corrected metadata"}, "prior-revision", {"attempt_id": "old-attempt", "revision": 2}),
    ]
    with registry_connection(write=True) as con:
        for request, key, response in cases:
            con.execute("INSERT INTO registry_receipts VALUES (?, ?, ?, ?, ?)", ("a", key, sha256(request), canonical(response), now()))
    assert experiments.create_experiment("a", design_spec, "prior-design") == cases[0][2]
    assert experiments.record_attempt("a", "old-design", attempt_spec, "prior-attempt") == cases[1][2]
    assert experiments.revise_attempt("a", "old-attempt", 1, attempt_spec, "Corrected metadata", "prior-revision") == cases[2][2]
    # Explicit null supplied by a new client has the same meaning as omission.
    explicit_null = experiments.ExperimentSpec.model_validate(old_design | {"executable_method_id": None})
    assert experiments.create_experiment("a", explicit_null, "prior-design") == cases[0][2]
