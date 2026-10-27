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

Supported axes are strictly increasing 1D numeric arrays. The generic peak and response-calibration tools work with LC, GC, CE, SEC or other scalar traces, but do not identify compounds. Peak quantitation does not apply internal-standard correction, dilution factors, detection limits, or an uncertainty interval. SEC currently requires time in `min` or volume in `mL`, and produces molecular weight relative to the supplied standards. No LC-MS, GC-MS, DAD spectra, vendor-file readers, detector synchronization, multi-angle light-scattering fit or automated compound identification is implemented here. CSV input is text passed in the tool call; a client upload/asset bridge remains to be built.

## Prerequisites

1. Python 3.10+ and the dependencies in `requirements.txt`.
2. R with `jsonlite`, `tibble`, `recipes`, and `measure` installed. Relative SEC additionally requires the corrected `measure.sec` development revision that includes `mw_col` and `mw_scale` in `step_sec_mw_averages()`. The released/default revision may have the same package version but lack these fixes; the worker checks for this capability and refuses to perform SEC when it is missing.
3. The `R` executable on `PATH`, or its absolute path in `MEASURE_MCP_R`. It must be able to load those packages in the service process's R library environment.

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

Stdio defaults to one `local` principal. Data are stored in `measure.sqlite3` under `MEASURE_MCP_DATA_DIR` (default `~/.local/share/measure-mcp`). Protect this directory as analytical data. There is no deletion/retention UI, backup policy, multi-node storage, quota per user, or request idempotency mechanism yet.

## HTTP and client integration status

The ASGI app `server:app` exposes stateless Streamable HTTP at `/mcp`. HTTP calls fail closed in local identity mode. The optional Connect-header mode requires both `MEASURE_MCP_IDENTITY=connect` and `MEASURE_MCP_TRUST_CONNECT_HEADERS=1`, and a single `RStudio-Connect-Credentials` header whose `user` is a GUID. The automated HTTP test simulates two injected GUIDs and checks that their records are isolated.

**This header is not authentication by itself.** Enable this mode only behind a verified Posit Connect deployment that authenticates each caller, strips any client-supplied identity headers, injects its own, and cannot be bypassed. Connect defaults to names, so an administrator must verify GUID delivery (`Authorization.ContentCredentialsUseGUID`) and the actual request shape for the deployment. Do not expose the ASGI process directly while trusting these headers. A local simulation does not verify the proxy, a client’s per-user token exchange, Connect deployment, TLS, durability, or multi-instance operation. See [Posit Connect's user metadata documentation](https://docs.posit.co/connect/user/fastapi/#user-meta-data) and [GUID configuration](https://docs.posit.co/connect/admin/appendix/advanced-user-group/#uniqueness-and-leverage-the-connect-server-api-using-posit-connect-guids).

Before a client integration is considered usable, test the real authenticated connection, implement the asset-to-trace bridge, choose durable shared storage and retention controls, and validate the resulting scientific workflows against real reference data. These tasks are tracked in the associated roadmap issues.
