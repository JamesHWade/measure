# Experimental separations MCP service

This is a first end-to-end service for agents that work with one-dimensional analytical traces. It uses the installed R `measure` and `measure.sec` packages for the calculations, stores input and result records, and exposes bounded, structured MCP tools. It is a research prototype: validate its outputs against instrument-specific reference data before relying on them for decisions.

## What it does

- Registers numeric traces with separate sample, injection and detector-channel identities, units, batch, sample type and run order. Delimited text may contain a single signal, multiple detector columns sharing an axis, or row-oriented groups with independent axes. Map columns and each trace's metadata explicitly. Comma, tab and semicolon delimiters are supported; unmapped columns are ignored, while unmapped row groups or missing numeric values are rejected. The caller supplies the data; the server does not open caller-supplied file paths.
- Retains the original delimited text, its exact UTF-8 digest and the column mapping, alongside the normalized numeric dataset. The source text can be retrieved in bounded pages. Structured trace input is retained as its canonical dataset instead.
- Inspects trace metadata without returning full arrays; detects and integrates chromatogram peaks using configurable baseline, smoothing and peak parameters.
- Quantifies a caller-assigned peak with supplied linear response standards, reporting fit diagnostics and whether the estimate falls outside their concentration range. The caller must establish the peak identity and declare comparable response units and integration methods.
- Calculates relative SEC molecular-weight averages and distributions using supplied calibration standards, a specified calibration fit and a concentration-proportional, baseline-corrected signal. The result includes calibration fit diagnostics, warnings and assumptions.
- Reads result arrays in pages and compares stored SEC analyses to a reference, with method, backend-code, detector and unit comparability flags.
- Records a canonical input digest, method digest, R-worker digest, package versions and an MD5 fingerprint of each installed package's R code database. Optional operator-supplied source revisions are recorded separately. The code fingerprint identifies part of a build; it is not a complete software bill of materials or proof of scientific validation.
- Tracks immutable experiment designs, revisioned attempts with intended versus actual conditions, and immutable outcomes linked to exact attempt revisions. Numeric conditions carry explicit units, categorical and boolean conditions retain their types, and stored traces supply sample/injection metadata. Atomic result batches preserve observed zero, non-detection, below-limit/censored, missing, failed and infeasible outcomes as distinct states.
- Uses required per-owner idempotency keys for registry writes, optimistic attempt revisions and explicit result supersession. Analysis-derived outcome values are copied from an owned stored analysis summary, including its provenance and warnings; units and analyte labels remain caller-declared.

Supported axes are strictly increasing 1D numeric arrays. The generic peak and response-calibration tools work with LC, GC, CE, SEC or other scalar traces, but do not identify compounds. Peak quantitation does not apply internal-standard correction, dilution factors, detection limits, or an uncertainty interval. SEC currently requires time in `min` or volume in `mL`, and produces molecular weight relative to the supplied standards. No LC-MS, GC-MS, DAD spectra, vendor-file readers, detector synchronization, multi-angle light-scattering fit or automated compound identification is implemented here. CSV input is text passed in the tool call; a client upload/asset bridge remains to be built.

## Prerequisites

1. Python 3.10+ and the dependencies in `requirements.txt`.
2. For numerical analysis, R with `jsonlite`, `tibble`, `recipes`, and `measure` installed. Relative SEC additionally requires the corrected `measure.sec` development revision that includes `mw_col` and `mw_scale` in `step_sec_mw_averages()`. The released/default revision may have the same package version but lack these fixes; the worker checks for this capability and refuses to perform SEC when it is missing.
3. For numerical analysis, the `R` executable on `PATH`, or its absolute path in `MEASURE_MCP_R`. It must be able to load those packages in the service process's R library environment. Registry-only operations do not require R.

The integration tests in this branch ran with R 4.3.3, `measure` 0.0.1.9002, the corrected `measure.sec` 0.0.0.9000 checkout (commit `a3ff1fe965f53b6388a3ce535746ff40fdbda0c2`) and `recipes` 1.4.0.9000. Install the precise commits you have tested; package version strings alone do not identify an unreleased development checkout. Set `MEASURE_MCP_MEASURE_REVISION` and `MEASURE_MCP_SEC_REVISION` to record operator-supplied source revisions in analysis provenance.

For example, from checked-out package directories, using an already configured R library:

```sh
R CMD INSTALL ../measure
R CMD INSTALL ../measure.sec
python -m venv .venv
.venv/bin/pip install -r mcp/requirements.txt pytest
MEASURE_MCP_TEST_R="$(command -v R)" .venv/bin/pytest -q mcp/tests
```

## Local stdio use

Configure an MCP client with the absolute paths to the Python interpreter and this directory's `server.py`. For example:

```json
{
  "mcpServers": {
    "measure-separations": {
      "command": "/absolute/path/to/venv/bin/python",
      "args": ["/absolute/path/to/measure/mcp/server.py"],
      "env": {
        "MEASURE_MCP_R": "/absolute/path/to/R",
        "MEASURE_MCP_DATA_DIR": "/private/path/to/measure-mcp"
      }
    }
  }
}
```

`measurement_capabilities` reports available backend support. Register a dataset with `measurement_register_dataset`, `measurement_register_csv`, `measurement_register_wide_csv` or `measurement_register_long_csv`, inspect it, and then call `measurement_analyze_chromatogram` or `measurement_analyze_sec`. An explicitly identified chromatogram peak can be passed to `measurement_quantify_peak` with known response standards. Use `measurement_get_analysis`, `measurement_read_analysis_data` and `measurement_compare_sec` to retrieve results, or `measurement_read_source` for retained text. Each completed analysis is an immutable record; re-running a method creates another record.

Stdio defaults to one `local` principal. Data are stored in `measure.sqlite3` under `MEASURE_MCP_DATA_DIR` (default `~/.local/share/measure-mcp`). Protect this directory as analytical data. There is no deletion/retention UI, backup policy, multi-node storage or quota per user. Registry writes have idempotency keys; dataset registration and numerical analysis calls still create a new record on every call.

## Experiment and outcome registry

The registry is the first experiment-tracking slice of [issue #91](https://github.com/JamesHWade/measure/issues/91). It works without an R installation; use `measurement_registry_capabilities` to discover its contract and limits. It does not suggest experiments, operate instruments, validate a BO campaign, or consume outcomes into an optimizer.

An experiment is an immutable design containing intended conditions and a caller-declared origin. A design can have several attempts, including retries and replicates. Each attempt has an independently reported status, actual conditions, optional timezone-aware start/end timestamps, instrument/method references, replicate identifiers and dataset/trace links. The server resolves those links to owned measurements and snapshots their sample, injection, detector and unit metadata. It keeps planned versus actual conditions separate and does not invent missing timestamps or units.

Attempts distinguish `manual` (the default), `instrument`, `simulator` and `replay` evaluators. Optional evaluator/version, model-training snapshot reference and fidelity coordinate remain caller-declared; they are not verified model artifacts or measured accuracy. The exact evaluator declarations are carried into each outcome's provenance. A simulated value can therefore be numerically observed in the registry without being presented as a physical measurement.

For example, call `measurement_create_experiment` with:

```json
{
  "spec": {
    "title": "LC scouting",
    "source": "manual",
    "intended_conditions": [
      {"name": "flow", "value": 0.4, "unit": "mL/min"},
      {"name": "column", "value": "C18"},
      {"name": "flush", "value": true}
    ]
  },
  "idempotency_key": "design-001"
}
```

Pass its `experiment_id` to `measurement_record_attempt` with a `spec` such as:

```json
{
  "status": "completed",
  "actual_conditions": [{"name": "flow", "value": 0.395, "unit": "mL/min"}],
  "measurements": [{"dataset_id": "<registered dataset ID>", "trace_id": "uv"}],
  "replicate_group": "scouting-replicates",
  "replicate_kind": "technical",
  "replicate_index": 1
}
```

Supply a fresh `idempotency_key` for that operation too. Use `measurement_revise_attempt` to append an updated reported status or correction with `expected_revision`, a complete replacement `spec`, and `reason`. Old snapshots remain available through `measurement_get_attempt(revision=...)`. This is a record of reported lifecycle states, not an execution state machine: recording or correcting a status never starts, stops or repeats a physical experiment. A retry of a physical experiment should be a separate attempt. Correcting an immutable intended design currently requires recording a new experiment; design-supersession links are not implemented.

Record one or more outcomes with `measurement_record_results`, giving `attempt_id`, `expected_attempt_revision`, `results` and an idempotency key. Examples of `results` entries:

```json
[
  {"metric": "blank_response", "status": "observed", "value": 0, "unit": "mAU * min"},
  {"metric": "target_concentration", "status": "observed", "unit": "mg/L",
   "analysis_id": "<stored quantitation analysis ID>", "summary_field": "concentration"},
  {"metric": "impurity_concentration", "status": "below_limit", "upper_bound": 0.05,
   "unit": "mg/L", "explanation": "Below the declared reporting limit"}
]
```

An analysis-derived observed outcome omits `value`; the server copies a finite numeric field directly from the stored analysis summary. That analysis must belong to a dataset/trace linked to the selected attempt revision. The registry does not decide which metric or analyte the field represents, verify/convert the caller's unit, or establish a chemical identity. These declarations are visible alongside `value_origin`, `unit_origin`, the exact attempt digest, analysis provenance and any analysis warnings. Arbitrary nested result paths or executable transformations are not accepted.

An observed manual result requires a finite numeric value. Non-observed outcomes omit `value` and require an explanation; `below_limit` requires an upper bound and `censored` requires at least one bound. No outcome state is silently converted to zero. An optional analysis link on a non-observed outcome is supporting evidence for the caller's reported decision, not an automatic classification by the service.

There is one current result for each attempt/metric/analyte-label combination. To correct it, supply `supersedes_result_id` and an explanation in a new result. Corrections may change a metric or analyte label within the same attempt, provided they do not collide with another current result. The old value and assignment remain accessible; competing corrections to the same head are rejected. `measurement_list_results` defaults to current heads and can include the full history. Replicate measurements belong to distinct attempts or explicitly distinguished metric/analyte labels. Read a specific result with `measurement_get_result`, and find records through bounded `measurement_list_experiments` / `measurement_list_attempts` pages.

All registry writes atomically commit their records and retry receipt in the same local SQLite transaction. Reusing a key with the same normalized request returns the original response even after later revisions; reusing it for different arguments or a different registry operation fails. Keys are scoped to the authenticated principal. A stale expected revision fails without writing, and an invalid entry rolls back the entire result batch. These guarantees apply to this single-node database, not to arbitrary multi-instance deployments or physical instruments.

Limits are 64 conditions per set, 32 linked measurements per attempt, 50 outcomes per atomic batch, 64,000 serialized UTF-8 bytes per record/request/response batch, and 50 compact records per retrieval page. Individual records are bounded as well. New registry tables are added alongside existing datasets/analyses; prior records are retained. Durable hosting, backups, retention, migration policy, result-table CSV mapping, generic objective extraction and BO integration remain follow-on work.

Validation includes concurrent retries and competing revisions/corrections, ownership of linked data/analyses, atomic rollback, prior-database compatibility, fresh-process retrieval, local MCP without R, and simulated HTTP users. `.github/workflows/mcp-registry.yaml` runs the registry and HTTP contracts. The full R numerical integration remains an additional check using the prerequisites above; it is explicitly excluded from that Python-only job.

## HTTP and client integration status

The ASGI app `server:app` exposes stateless Streamable HTTP at `/mcp`. HTTP calls fail closed in local identity mode. The optional Connect-header mode requires both `MEASURE_MCP_IDENTITY=connect` and `MEASURE_MCP_TRUST_CONNECT_HEADERS=1`, and a single `RStudio-Connect-Credentials` header whose `user` is a GUID. The automated HTTP test simulates two injected GUIDs and checks that their records are isolated.

**This header is not authentication by itself.** Enable this mode only behind a verified Posit Connect deployment that authenticates each caller, strips any client-supplied identity headers, injects its own, and cannot be bypassed. Connect defaults to names, so an administrator must verify GUID delivery (`Authorization.ContentCredentialsUseGUID`) and the actual request shape for the deployment. Do not expose the ASGI process directly while trusting these headers. A local simulation does not verify the proxy, a client’s per-user token exchange, Connect deployment, TLS, durability, or multi-instance operation. See [Posit Connect's user metadata documentation](https://docs.posit.co/connect/user/fastapi/#user-meta-data) and [GUID configuration](https://docs.posit.co/connect/admin/appendix/advanced-user-group/#uniqueness-and-leverage-the-connect-server-api-using-posit-connect-guids).

Before a client integration is considered usable, test the real authenticated connection, implement the asset-to-trace bridge, choose durable shared storage and retention controls, and validate the resulting scientific workflows against real reference data. These tasks are tracked in the associated roadmap issues.
