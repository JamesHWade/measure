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

1. Python 3.10+ and the runtime dependencies in `requirements.txt`.
2. Numerical analysis requires R, `jsonlite`, `tibble`, `recipes`, and `measure`. Relative SEC also requires the corrected `measure.sec` implementation with calibrated-curve support. The worker checks for the required SEC capabilities and refuses the calculation when they are absent.
3. Set `MEASURE_MCP_R` to the `R` executable when it is not on `PATH`. The executable must see the same R library containing the required packages. Registry-only calls do not require R.

## Numerical service CI and reproducibility

`.github/workflows/mcp-numerical.yaml` runs the complete `python -m pytest -q mcp/tests` suite. The stdio end-to-end test starts the actual Python MCP server and its R worker, then exercises chromatography, calibration, and corrected SEC analysis. The CI job uses Python 3.12, the current R release, and a fresh R user library for each run. It installs `measure` from the checked-out Measure source and follows the active `fix/separations-sec-correctness` branch for `measure.sec` until issue #61 is merged.

Before testing, the job records the checked-out Measure commit, the SEC branch head seen at job start, and the actual `RemoteSha` of the installed `measure.sec`. The branch SHA at job start is evidence only; installation follows the branch name so CI exercises its current head. Every run prints both SEC revisions and runs the suite against the installed revision. A push to the SEC branch alone does not trigger a workflow in this repository; use `workflow_dispatch` to test that change immediately. The job prints the Python, R, MCP, pytest, recipes, measure, and measure.sec versions and checks that the installed Measure package matches this checkout, both numerical packages came from the isolated R user library, and the SEC calibration APIs required by the worker are present. The SEC package declares `Remotes: JamesHWade/measure`; CI installs the hard dependencies of this checkout first, verifies every hard dependency declared by the installed SEC package is loadable, and disables recursive dependency installation for SEC so it cannot replace the local Measure build.

The numerical expectations are small synthetic known-answer cases: a Gaussian peak area is compared with its analytic integral; a linear calibration is checked against its known line; and SEC moments are compared with direct calculations from a synthetic calibration curve and signal weights. These tests exercise Python-to-MCP-to-R integration and deterministic calculations. They do not reproduce a literature protocol, qualify an instrument, or validate against measured reference traces.

To reproduce the service suite locally with an isolated R user library:

```sh
python3.12 -m venv .venv
.venv/bin/python -m pip install -r mcp/requirements.txt 'pytest>=9,<10'
export R_LIBS_USER="$(mktemp -d)"
Rscript -e 'install.packages("remotes"); remotes::install_deps(".", dependencies = "hard", upgrade = TRUE)'
R CMD INSTALL --library="$R_LIBS_USER" .
Rscript -e 'remotes::install_github("JamesHWade/measure.sec@fix/separations-sec-correctness", dependencies = FALSE)'
export MEASURE_MCP_R="$(command -v R)"
export MEASURE_MCP_TEST_R="$MEASURE_MCP_R"
export MEASURE_MCP_MEASURE_REVISION="$(git rev-parse HEAD)"
export MEASURE_MCP_SEC_REVISION="$(Rscript -e 'cat(packageDescription("measure.sec")$RemoteSha)')"
.venv/bin/python -m pytest -q mcp/tests
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

## Executable chromatography methods

`measurement_method_capabilities` describes the independent method contract; it works without R. `measurement_validate_method` accepts a versioned `declaration` and optional `constraints`. It retains the declared units, normalizes a copy to minutes and mL/min, and returns `feasible` plus individual constraint violations with values, limits and units. Invalid structure or unsupported units produce an error. A well-formed method that exceeds a declared limit is returned as infeasible.

The contract covers LC isocratic programs, absolute-time gradients (`gradient_absolute`) and segment-duration gradients (`gradient_duration`). Gradients interpolate both flow and each solvent fraction linearly between endpoints. All named solvents must occur in every state and fractions must sum to one. Holds, increases and decreases are allowed. Absolute programs start exactly at analytical time zero; duration programs retain an initial state and positive durations. Equivalent absolute and duration programs produce the same canonical gradient.

This validation call describes a two-minute gradient in seconds and microlitres per minute:

```json
{
  "declaration": {
    "schema_version": "1.0",
    "time_unit": "s",
    "flow_unit": "uL/min",
    "solvents": ["A", "B"],
    "program": {
      "kind": "gradient_absolute",
      "points": [
        {"time": 0, "state": {"flow": 1000, "solvent_fractions": {"A": 0.9, "B": 0.1}}},
        {"time": 120, "state": {"flow": 1000, "solvent_fractions": {"A": 0.1, "B": 0.9}}}
      ]
    }
  },
  "constraints": {"max_flow_ml_min": 1.5, "max_acquisition_duration_min": 5}
}
```

Its canonical acquisition duration is 2 min, with 1 mL each of A and B. The same declaration can be registered with `measurement_register_method`, adding `title` and a fresh `idempotency_key`. Registration requires feasibility under the supplied limits. Retrieve it with `measurement_get_method`; list compact identities with `measurement_list_methods` (maximum 50 records per page). Methods are immutable: register a new method to change the program or limits. The registry retains the original declaration, normalized program, constraints, calculation results, creation time and two SHA-256 digests. `input_sha256` identifies the complete normalized registration request, including title and constraints; `canonical_program_sha256` identifies the normalized numerical schedule. Equipment declarations are retained in the input identity and are not part of the numerical-program digest.

Both `ExperimentSpec` and `AttemptSpec` accept `executable_method_id`. An experiment links its intended method; each attempt links its separately reported method. The registry verifies ownership and captures the method's immutable ID and digests in `executable_method` alongside the record. Revising an attempt preserves the earlier method link and digest. There is no automatic inheritance from intended to actual: callers must report what was attempted. Existing free-text `method_reference` values remain external references and are not treated as validated methods.

Optional constant `pre_acquisition` and `post_acquisition` phases use the declaration's time and flow units. They contribute to occupancy and total solvent use, while analytical points retain their own zero. Solvent use is the exact integral of flow multiplied by fraction within each linear segment; multiplying the mean flow by the mean fraction is generally incorrect when both vary. For example, a 2 min segment with flow 1 → 3 mL/min and A fraction 0.2 → 0.8 uses 2.2 mL of A and 1.8 mL of B. Constant overhead phases are added separately. Acquisition duration, occupancy and solvent use are reported as different quantities.

Constraint values carry canonical units in their field names. Supported limits include minimum segment duration, maximum acquisition and occupancy duration, flow, solvent-fraction ranges, absolute fraction slopes and declared categorical equipment compatibility. Constraints describe the caller's permitted method; they do not establish pressure feasibility, solvent chemical identity, column suitability or hardware qualification. Instrument-specific temperature, pressure prediction, nonlinear vendor curves, optimizer search-space parameterization and initial-design sampling remain future work. A registered method never starts an acquisition or establishes that an instrument followed the program. The complete stored record/request is limited to 64,000 UTF-8 bytes; the schema also bounds points, solvents and metadata.

The method schema permits up to 16 analytical points and 6 solvents; its bounded violation list has at most 256 entries. Fractions must sum to one within an absolute tolerance of `1e-9`. Limit comparisons allow floating-point differences within relative `1e-9` or absolute `1e-12` tolerance; input values and reported quantities are retained rather than rounded onto a coarser schedule. Missing or `unknown` equipment compatibility does not establish suitability, even when the numerical method is feasible under its declared limits.
