"""Contract and end-to-end tests for the experimental MCP service."""
import asyncio
import hashlib
import json
import math
import os
from pathlib import Path
import socket
import subprocess
import sys
import time
import uuid

import httpx
import pytest
from mcp import ClientSession, StdioServerParameters
from mcp.client.stdio import stdio_client
from mcp.client.streamable_http import streamable_http_client
from pydantic import ValidationError

MCP_DIR = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(MCP_DIR))
import server


def trace(**changes):
    data = {"trace_id": "t1", "sample_id": "s1", "injection_id": "i1", "channel": "ri",
            "separation": "SEC", "detector": "RI", "axis_unit": "min", "signal_unit": "a.u.",
            "axis": [1.0, 2.0, 3.0], "signal": [0.1, 1.0, 0.1]}
    data.update(changes)
    return data


def test_input_validation():
    for changes in ({"axis": [1, 2, 2]}, {"axis": [1, 2, 3], "signal": [1, 2, float("nan")]},
                    {"axis": [1, 2, 3], "signal": [1, 2]}):
        with pytest.raises(ValidationError):
            server.Trace.model_validate(trace(**changes))
    with pytest.raises(ValidationError):
        server.Dataset.model_validate({"title": "invalid", "traces": [trace(), trace(trace_id="t2", channel="uv", sample_id="s2")]})
    with pytest.raises(ValidationError):
        server.SECMethod.model_validate({"standards": [{"location": 1, "molecular_weight_g_mol": 100},
                                                        {"location": 2, "molecular_weight_g_mol": 100}],
                                         "standard_description": "invalid", "baseline_corrected": True,
                                         "signal_proportional_to_weight_concentration": True})
    with pytest.raises(ValidationError):
        server.PeakQuantitationMethod.model_validate({"analyte": "A", "concentration_unit": "mg/L",
            "standards": [{"concentration": x, "response": x} for x in (0, 1, 2)], "weighting": "1/x",
            "peak_assigned_to_analyte": True, "standards_use_same_area_basis": True})


def result(response):
    assert not response.isError, response.content
    assert response.structuredContent is not None
    return response.structuredContent


def test_stdio_end_to_end(tmp_path):
    r = os.environ.get("MEASURE_MCP_TEST_R", "R")
    env = dict(os.environ, MEASURE_MCP_R=r, MEASURE_MCP_DATA_DIR=str(tmp_path), MEASURE_MCP_IDENTITY="local")
    params = StdioServerParameters(command=sys.executable, args=[str(MCP_DIR / "server.py")], env=env)

    async def exercise():
        async with stdio_client(params) as (read, write):
            async with ClientSession(read, write) as client:
                await client.initialize()
                tools = {tool.name: tool for tool in (await client.list_tools()).tools}
                assert tools["measurement_register_dataset"].annotations.readOnlyHint is False
                assert tools["measurement_get_analysis"].annotations.readOnlyHint is True
                caps = result(await client.call_tool("measurement_capabilities", {}))
                assert caps["techniques"]["generic_1d_trace"]
                assert {"structured_traces", "delimited_wide", "delimited_long"}.issubset(caps["input_formats"])
                assert caps["techniques"]["relative_sec"], "install the corrected measure.sec revision for this test"
                x = [1 + i * 0.02 for i in range(101)]
                y = [math.exp(-0.5 * ((v - 2) / 0.16) ** 2) for v in x]
                raw = trace(axis=x, signal=y)
                ds = result(await client.call_tool("measurement_register_dataset", {"dataset": {"title": "Synthetic SEC", "traces": [raw]}}))
                inspect = result(await client.call_tool("measurement_inspect_dataset", {"dataset_id": ds["dataset_id"]}))
                assert "axis" not in inspect["traces"][0] and inspect["traces"][0]["point_count"] == 101
                args = {"dataset_id": ds["dataset_id"], "trace_id": "t1"}
                chrom = result(await client.call_tool("measurement_analyze_chromatogram", {**args, "method": {}}))
                assert chrom["artifact"] is True and chrom["summary"]["peak_count"] == 1
                assert chrom["provenance"]["input_sha256"] == ds["input_sha256"]
                peaks = result(await client.call_tool("measurement_read_analysis_data", {"analysis_id": chrom["analysis_id"], "kind": "peaks"}))
                assert peaks["data"][0]["location"] == pytest.approx(2)
                assert peaks["data"][0]["area"] == pytest.approx(math.sqrt(2 * math.pi) * 0.16, rel=0.001)
                quant_method = {"analyte": "assigned A", "concentration_unit": "mg/L",
                    "standards": [{"concentration": v, "response": 0.01 + 0.4 * v} for v in (0, 1, 2, 3)],
                    "peak_assigned_to_analyte": True, "standards_use_same_area_basis": True}
                quant = result(await client.call_tool("measurement_quantify_peak", {"analysis_id": chrom["analysis_id"],
                    "peak_id": peaks["data"][0]["peak_id"], "method": quant_method}))
                assert quant["summary"]["concentration"] == pytest.approx((peaks["data"][0]["area"] - 0.01) / 0.4)
                assert not quant["summary"]["outside_standard_range"]
                assert quant["diagnostics"]["residual_degrees_of_freedom"] == 2
                outside_method = quant_method | {"standards": [{"concentration": v, "response": v / 10} for v in (10, 20, 30)]}
                outside = result(await client.call_tool("measurement_quantify_peak", {"analysis_id": chrom["analysis_id"],
                    "peak_id": peaks["data"][0]["peak_id"], "method": outside_method}))
                assert outside["summary"]["outside_standard_range"] and outside["warnings"]
                page = result(await client.call_tool("measurement_read_analysis_data", {"analysis_id": chrom["analysis_id"], "kind": "processed_trace", "offset": 5, "limit": 3}))
                assert page["count"] == 101 and page["next_offset"] == 8 and len(page["data"]["axis"]) == 3
                standards = [{"location": v, "molecular_weight_g_mol": 10 ** (6 - 0.4*v + 0.03*v*v)} for v in [1, 1.5, 2, 2.5, 3]]
                method = {"standards": standards, "fit_type": "quadratic", "standard_description": "Synthetic standards",
                          "baseline_corrected": True, "signal_proportional_to_weight_concentration": True}
                sec = result(await client.call_tool("measurement_analyze_sec", {**args, "method": method}))
                weights = [y[i] * (0.01 if i in (0, 100) else 0.02) for i in range(101)]
                mw = [10 ** (6 - 0.4*v + 0.03*v*v) for v in x]
                expected_mw = sum(w*m for w, m in zip(weights, mw)) / sum(weights)
                expected_mn = sum(weights) / sum(w/m for w, m in zip(weights, mw))
                assert sec["summary"]["mw_g_mol"] == pytest.approx(expected_mw, rel=1e-8)
                assert sec["summary"]["mn_g_mol"] == pytest.approx(expected_mn, rel=1e-8)
                assert sec["diagnostics"]["residual_degrees_of_freedom"] == 2
                assert sec["provenance"]["installed_r_code_md5"]["measure.sec"]
                dist = result(await client.call_tool("measurement_read_analysis_data", {"analysis_id": sec["analysis_id"], "kind": "distribution", "limit": 200}))["data"]
                integral = sum((dist["differential"][i]+dist["differential"][i+1]) / 2 * (dist["log10_mw"][i+1]-dist["log10_mw"][i]) for i in range(100))
                assert integral == pytest.approx(1, rel=1e-5)
                assert dist["cumulative"][0] == pytest.approx(0) and dist["cumulative"][-1] == pytest.approx(1)
                retrieved = result(await client.call_tool("measurement_get_analysis", {"analysis_id": sec["analysis_id"]}))
                assert retrieved["method"] == method | {"extrapolation": "none", "integration_range": None}
                assert retrieved["diagnostics"]["r_squared"] == pytest.approx(1)
                second = result(await client.call_tool("measurement_analyze_sec", {**args, "method": method}))
                compare = result(await client.call_tool("measurement_compare_sec", {"analysis_ids": [sec["analysis_id"], second["analysis_id"]]}))
                assert compare["comparability"]["same_method"] and compare["comparability"]["same_detector_channel_and_units"]
                assert compare["results"][1]["values"]["mw_g_mol"]["difference"] == pytest.approx(0)
                empty_range = await client.call_tool("measurement_analyze_sec", {**args, "method": method | {"integration_range": [10, 11]}})
                assert empty_range.isError
                text = "time,response\n1,0.1\n2,1\n3,0.1\n"
                meta = {k:v for k,v in raw.items() if k not in ("axis", "signal")}
                csv_ds = result(await client.call_tool("measurement_register_csv", {"title":"CSV", "csv_text":text,
                                "axis_column":"time", "signal_column":"response", "metadata":meta}))
                assert csv_ds["point_count"] == 3
                assert csv_ds["source_sha256"] == hashlib.sha256(text.encode()).hexdigest()
                source = result(await client.call_tool("measurement_read_source", {"dataset_id": csv_ds["dataset_id"], "offset": 0, "limit": 5}))
                assert source["text"] == text[:5] and source["next_offset"] == 5
                csv_analysis = result(await client.call_tool("measurement_analyze_chromatogram", {"dataset_id": csv_ds["dataset_id"], "trace_id":"t1", "method":{}}))
                assert csv_analysis["provenance"]["source_sha256"] == csv_ds["source_sha256"]
                wide_text = "\ufefftime;ri;uv\n1;0.1;0.2\n2;1;2\n3;0.1;0.2\n"
                uv = meta | {"trace_id":"t2", "channel":"uv", "detector":"UV"}
                wide = result(await client.call_tool("measurement_register_wide_csv", {"title":"Two channels", "csv_text":wide_text,
                    "axis_column":"time", "delimiter":";", "channels":[{"column":"ri", "metadata":meta}, {"column":"uv", "metadata":uv}]}))
                assert wide["trace_count"] == 2 and wide["point_count"] == 6
                wide_inspect = result(await client.call_tool("measurement_inspect_dataset", {"dataset_id":wide["dataset_id"]}))
                assert [t["channel"] for t in wide_inspect["traces"]] == ["ri", "uv"]
                assert wide_inspect["source"]["mapping"]["delimiter"] == ";"
                wide_source = result(await client.call_tool("measurement_read_source", {"dataset_id":wide["dataset_id"]}))
                assert wide_source["text"] == wide_text and wide_source["source_sha256"] == hashlib.sha256(wide_text.encode()).hexdigest()
                long_text = ("sample,detector,time,response\nA,RI,1,0\nB,UV,1,0\nA,RI,2,1\n"
                             "B,UV,2.5,2\nA,RI,3,0\nB,UV,4,0\n")
                uv_other = uv | {"sample_id":"s2", "injection_id":"i2"}
                long_args = {"title":"Grouped runs", "csv_text":long_text, "axis_column":"time", "signal_column":"response",
                    "group_columns":["sample", "detector"], "groups":[{"values":["A","RI"], "metadata":meta},
                                                                       {"values":["B","UV"], "metadata":uv_other}]}
                long_ds = result(await client.call_tool("measurement_register_long_csv", long_args))
                long_info = result(await client.call_tool("measurement_inspect_dataset", {"dataset_id":long_ds["dataset_id"]}))
                assert long_ds["trace_count"] == 2 and long_info["traces"][1]["axis_range"] == [1, 4]
                assert long_info["source"]["mapping"]["group_columns"] == ["sample", "detector"]
                unmapped = await client.call_tool("measurement_register_long_csv", long_args | {"groups":long_args["groups"][:1]})
                assert unmapped.isError
                bad = await client.call_tool("measurement_register_csv", {"title":"CSV", "csv_text":"x,x\n1,2\n",
                                "axis_column":"x", "signal_column":"x", "metadata":meta})
                assert bad.isError
                missing = await client.call_tool("measurement_register_wide_csv", {"title":"missing", "csv_text":"time;ri;uv\n1;0;0\n2;1;\n3;0;0\n",
                    "axis_column":"time", "delimiter":";", "channels":[{"column":"ri", "metadata":meta}, {"column":"uv", "metadata":uv}]})
                assert missing.isError
    asyncio.run(exercise())


def test_http_user_isolation_and_missing_identity(tmp_path):
    sock = socket.socket()
    sock.bind(("127.0.0.1", 0))
    port = sock.getsockname()[1]
    sock.close()
    env = dict(os.environ, MEASURE_MCP_DATA_DIR=str(tmp_path), MEASURE_MCP_IDENTITY="connect",
               MEASURE_MCP_TRUST_CONNECT_HEADERS="1", PYTHONPATH=str(MCP_DIR))
    proc = subprocess.Popen([sys.executable, "-m", "uvicorn", "server:app", "--host", "127.0.0.1", "--port", str(port), "--log-level", "error"], env=env, stdout=subprocess.DEVNULL, stderr=subprocess.PIPE)
    url = f"http://127.0.0.1:{port}/mcp"

    async def exercise():
        for _ in range(100):
            try:
                with socket.create_connection(("127.0.0.1", port), timeout=.1):
                    break
            except OSError:
                if proc.poll() is not None:
                    raise RuntimeError(proc.stderr.read().decode())
                await asyncio.sleep(.1)
        else:
            raise RuntimeError("HTTP server did not start")
        user_a, user_b = str(uuid.uuid4()), str(uuid.uuid4())
        async def call_as(user, name, arguments):
            headers = {} if user is None else {"RStudio-Connect-Credentials": json.dumps({"user": user})}
            async with httpx.AsyncClient(headers=headers, timeout=20, trust_env=False) as http:
                async with streamable_http_client(url, http_client=http) as (read, write, _):
                    async with ClientSession(read, write) as client:
                        await client.initialize()
                        return await client.call_tool(name, arguments)
        registered = result(await call_as(user_a, "measurement_register_dataset", {"dataset":{"title":"private", "traces":[trace()]}}))
        assert not (await call_as(user_a, "measurement_inspect_dataset", {"dataset_id": registered["dataset_id"]})).isError
        assert (await call_as(user_b, "measurement_inspect_dataset", {"dataset_id": registered["dataset_id"]})).isError
        assert (await call_as(None, "measurement_inspect_dataset", {"dataset_id": registered["dataset_id"]})).isError
        meta = {k:v for k,v in trace().items() if k not in ("axis", "signal")}
        csv_ds = result(await call_as(user_a, "measurement_register_csv", {"title":"private CSV", "csv_text":"x,y\n1,1\n2,2\n3,1\n",
            "axis_column":"x", "signal_column":"y", "metadata":meta}))
        assert not (await call_as(user_a, "measurement_read_source", {"dataset_id":csv_ds["dataset_id"]})).isError
        assert (await call_as(user_b, "measurement_read_source", {"dataset_id":csv_ds["dataset_id"]})).isError
    try:
        asyncio.run(exercise())
    finally:
        proc.terminate()
        proc.wait(timeout=10)
