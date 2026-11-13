# nD operations qualification

This directory contains a deterministic producer for the fixed synthetic
qualification of the existing nD unfold/fold, slice, and project APIs. It
checks the actual installed measure namespace. It does not use
devtools::load_all() or infer the installed package build from the checkout
that contains this tool.

## Inputs and run

The script locates fixtures.json and declaration.json beside itself or in
the adjacent qualification-input/ directory. The files must retain the
frozen bytes. Their SHA256 values are frozen in the producer and checked before
any fixture is evaluated. A manifest may be kept beside the input files.

Install the candidate into a fresh library, then run from a new output path.
For example:

~~~sh
R_LIBS=/tmp/measure-candidate-library \
  Rscript tools/nd-qualification/qualify.R /tmp/measure-nd-qualification
python3 tools/nd-qualification/verify.py /tmp/measure-nd-qualification
~~~

The output root must not exist. The producer creates it once and never replaces
an existing result directory. If qualification stops, it retains
qualification-progress.json and run-status.json with the latest completed
cases and failure stage. A successful run writes results.json,
runtime.json, and a completed run-status.json.

results.json reports the installed package path and version, R version,
separate tool checkout head, input hashes, every declared fold variant, slice,
projection and rejection case, and the positive paired-list selection. Array
fields remain JSON arrays for zero- and one-element results. Measurement
values encode ordinary numbers, NA, NaN, and infinities with separate
states. runtime.json records sessionInfo() and relevant dependency
versions.

The independent Python verifier checks coordinates, values, selections,
grouped summaries, and the fixed rejection set against its own oracle.
These deterministic fixtures qualify software behavior for the declared
operations. They do not qualify an analytical method, physical instrument,
acquisition model, or an nD interchange format.
