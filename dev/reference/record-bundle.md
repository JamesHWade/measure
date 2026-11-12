# Read and write native measurement record bundles

A bundle is a new directory containing one fixed manifest.dcf and six
fixed UTF-8 CSV or TSV files. The manifest selects only documented v1
file names; it cannot provide paths. Text values remain machine data,
including values that spreadsheet software may interpret as formulas.
Hashes detect accidental changes and do not authenticate a source.

## Usage

``` r
write_measure_record(x, path, delim = c(",", "\t"))

read_measure_record(path, max_bytes = 100 * 1024^2)
```

## Arguments

- x:

  A validated measure_record.

- path:

  A new directory path to write, or a directory containing a complete
  bundle to read. Existing writer destinations are rejected.

- delim:

  A comma or tab field delimiter.

- max_bytes:

  A positive whole-number byte limit for all files read.

## Value

write_measure_record() returns the normalized destination path
invisibly. read_measure_record() returns a validated measure_record.

## Details

The reader checks the manifest, payload hashes, table schemas, row and
column counts, and point references before constructing a record. Its
byte limit applies to the actual total bytes read from all seven files.

## Bundle version 1

The six fixed payload names have the selected `.csv` or `.tsv`
extension. Their ordered headers are:

- `sources`: `source_id`, `source_name`, `source_sha256`.

- `samples`: `sample_id`.

- `injections`: `injection_id`, `sample_id`.

- `channels`: `channel_id`, `channel_name`.

- `curves`: `curve_id`, `injection_id`, `channel_id`, `source_id`,
  `axis_name`, `axis_unit`, `value_name`, `value_unit`.

- `points`: `curve_id`, `point_index`, `location`, `value`,
  `source_row`.

The metadata tables retain the corresponding schema in
[`measure_record()`](https://jameshwade.github.io/measure/dev/reference/measure_record.md).
The single DCF manifest declares `Format: measure-record`, `Version: 1`,
and `Delimiter: csv` or `Delimiter: tsv`. It also has `-Rows`,
`-Columns`, and `-SHA256` fields for each of the prefixes `Sources`,
`Samples`, `Injections`, `Channels`, `Curves`, and `Points`: for
example, `Points-Rows`. The reader accepts any order of this exact field
set.

Empty cells in nullable text fields mean missing; literal `NA` text
remains text. Numeric missing values are written blank, and the reader
also accepts `NA` in numeric fields. Positive contiguous `point_index`
values restore point order within each curve. Numeric fidelity is exact
double equality after positive-zero canonicalization; arbitrary R
attributes are not serialized.

The writer validates and stages files beside the destination, reserves a
new destination directory, copies payloads without overwriting, then
copies the manifest last. A failed copy may leave an incomplete
destination without a manifest; the reader rejects it. Existing
destinations are never replaced.

## Examples

``` r
record <- measure_record()
local({
  bundle <- tempfile("measure-record-")
  on.exit(unlink(bundle, recursive = TRUE), add = TRUE)
  write_measure_record(record, bundle)
  identical(record, read_measure_record(bundle))
})
#> [1] TRUE
```
