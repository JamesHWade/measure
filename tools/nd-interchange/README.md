# Installed nD interchange qualification

This tool checks the installed measure namespace against fixed, independently
specified numerical truth for the measure-nd-interchange/1 profile. It writes
and reads 15 objects in 30 forward CSV/TSV bundles, then verifies two
Python-authored reverse bundles, two physical row reorders, 49 corrupt bundles,
and two cross-format rejections. The fixed binary source contains 8,208 finite
binary64 values. The producer reads those values with R readBin; Python reads
the same committed bytes independently with struct.

The producer uses only exported constructors and readers from the installed
measure namespace, plus public vctrs::new_list_of() for the explicit prototype
fixtures. It never loads package source with load_all(). Before recording
results, it checks that the loaded namespace path resolves to
find.package("measure") and that the installed Meta/package.rds exists.
Installed package provenance is recorded separately from the tool checkout
HEAD. A copied tool directory without its own .git repository records a null
tool HEAD.

## Runtime setup

Choose the Rscript executable and R library containing the candidate
installation. Set TOOL to the absolute tools/nd-interchange directory and
OUTPUT to a new child path under an existing parent. Quote paths because they
may contain spaces:

~~~sh
R="/absolute/path/to/Rscript"
export R_LIBS="/absolute/path/to/candidate-library:$R_LIBS"
TOOL="/absolute/path/to/package/tools/nd-interchange"
OUTPUT="/absolute/path/to/existing-parent/new run"
~~~

To test copied paths with spaces, copy only tools/nd-interchange/ to a new
directory whose path contains a space. Fixture files resolve beside the copied
script. If the calculated package root has no .git directory and no measure
DESCRIPTION, the recorded tool checkout HEAD is null. The producer decodes the
~+~ space escaping Rscript uses in its --file= argument.

## Five-command qualification

Run these commands in order with a new output root:

~~~sh
"$R" "$TOOL/qualify.R" produce "$OUTPUT"
python3 "$TOOL/verify.py" "$OUTPUT"
python3 "$TOOL/mutate.py" "$OUTPUT"
"$R" "$TOOL/qualify.R" reject "$OUTPUT"
python3 "$TOOL/verify.py" "$OUTPUT" --rejections
~~~

The producer reserves a new output root and refuses existing paths, including
dangling symlinks. Its parent directory must already exist. Python verification
independently checks the emitted bundle bytes and R observations against the
frozen fixture inputs. The mutator writes only declared paths under the output
root. During the reject phase, R first reads all four positive controls and
checks the scalar native-record control, then requires actual errors for every
declared corrupt bundle and cross-format case. The final Python command checks
the R rejection receipt and all source and mutation hashes.

If a phase fails, keep its output directory: incomplete status and progress
receipts identify the failed stage and preserve evidence. Do not retry into an
existing root or rerun rejection over existing phase receipts; use a fresh
output root for another sequence. No failed attempt qualifies as a completed
run.

The output records actual read-back data tokens, storage types, list names,
prototype contents, dimension metadata, special numerical state tags and
finite binary64 words. Empty collections remain JSON arrays. The R tool does
not emit a separate numerical expected-value file; the independent Python
verifier derives expected values from the frozen fixture inputs and binary data.

This qualifies a synthetic, owned-data interchange profile between these R
and Python implementations. It does not implement or qualify mzML, NPZ,
vendor formats, acquisition metadata, physical instruments, scientific
methods, hosted-user workflows, or adversarial filesystem race resistance.
