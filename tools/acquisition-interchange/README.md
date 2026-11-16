# Acquisition interchange qualification

This directory contains a fixed synthetic qualification profile for the
`measure-acquisition/1` native interchange. It tests the package's installed R
writer, reader, and centroid trace API against an independently declared
Python oracle. The tool uses Python's standard library only and does not load
package source into the producer process.

The frozen inputs bind 16 acquisition objects, 32 CSV/TSV forward bundles,
10 valid TIC/XIC calls with 25 expected rows, 15 trace rejections, four
independent read controls, 46 malformed native bundles, and four cross-format
rejections. Fixture numeric vectors state their storage type and binary64
words explicitly. The 8,208-value finite corpus is committed big-endian data;
its bytes are authoritative.

## Five-phase run

Select the R executable and library containing the candidate installed
package. Use an existing output parent and a new child directory. Quote the
paths so the tool can run from a copied directory with spaces:

~~~sh
R="/absolute/path/to/Rscript"
export R_LIBS="/absolute/path/to/candidate-library:$R_LIBS"
TOOL="/absolute/path/to/tools/acquisition-interchange"
OUTPUT="/absolute/path/to/existing-parent/new acquisition run"

"$R" "$TOOL/qualify.R" produce "$OUTPUT"
python3 "$TOOL/verify.py" "$OUTPUT"
python3 "$TOOL/mutate.py" "$OUTPUT"
"$R" "$TOOL/qualify.R" reject "$OUTPUT"
python3 "$TOOL/verify.py" "$OUTPUT" --rejections
~~~

The producer reserves a new output directory, copies all five frozen inputs to
`frozen-inputs/`, and records runtime, progress, and read-back evidence. Python
checks the marker and compares each copied input byte-for-byte with the tool
inputs, verifies each emitted payload independently, derives the trace rows
from the raw points, and writes an exclusive forward receipt.
The mutator creates two Python-authored reverse bundles, two physical row-order
controls, and the fixed malformed bundles. The R reject phase must reject each
malformed and cross-format input and read all controls successfully. The final
Python command checks those outcomes and writes an exclusive rejection receipt.

If any phase fails, keep that output directory as evidence. Status and progress
receipts identify the last completed stage. Do not retry in that directory;
choose a new output path for another full run.

## Coverage boundary

The fixtures preserve row and point order, scalar-record tables, literal text
IDs, MS stream selectors, nullable and nonfinite raw numeric states, array
storage types and metadata presence, per-point masks and source-row references,
missing and empty scans, and different nD grids. TIC/XIC expectations include
inclusive boundaries, duplicate contributors, contributor indices, mask
precedence, missing-intensity propagation, and declared selector errors.

This is synthetic software qualification of the fixed native profile. It does
not qualify arbitrary external readers, vendor or open mass-spectrometry
formats, hosted-user workflows, analytical methods, instruments, or physical
measurement performance.
