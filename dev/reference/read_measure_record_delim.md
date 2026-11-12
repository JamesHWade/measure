# Read one CSV or TSV file into a measurement record

`read_measure_record_delim()` reads a single local CSV or TSV long table
with strict UTF-8 and fixed column names, then delegates record
construction to
[`measure_record_from_long()`](https://jameshwade.github.io/measure/dev/reference/measure_record_from_long.md).
It reads the source bytes once, enforces `max_bytes` while reading, and
records the caller-declared source ID, the input basename, and a SHA-256
digest of those exact bytes. The digest helps detect accidental changes;
it does not authenticate the source or record the complete mapping and
transformation history.

## Usage

``` r
read_measure_record_delim(
  file,
  columns,
  source_id,
  delim = c(",", "\t"),
  allow_extra_columns = FALSE,
  max_bytes = 100 * 1024^2
)
```

## Arguments

- file:

  A local regular CSV or TSV file path.

- columns:

  A named character vector mapping canonical fields to exact input
  header names. See
  [`measure_record_from_long()`](https://jameshwade.github.io/measure/dev/reference/measure_record_from_long.md)
  for the required and optional canonical names.

- source_id:

  A caller-declared, nonblank character source identifier.

- delim:

  A comma or tab field delimiter. No delimiter inference is performed.

- allow_extra_columns:

  Set to `TRUE` to ignore columns not named in `columns`.

- max_bytes:

  A positive whole-number byte limit for the single source file.

## Value

A `measure_record` with one source row containing the input basename and
SHA-256 digest.

## Details

All fields are initially read as character. Identifiers such as `0007`,
`1e3`, `NA`, and formula-like strings remain literal text. Numeric
fields use a strict, locale-independent decimal grammar: locations must
be finite, and signal values may be finite or missing. Blank optional
metadata cells become unknown (`NA_character_`); literal `NA` remains
text. Mappings, units, and identities are never inferred. Curves may
have independent finite, monotone grids and are not sorted or
interpolated.

Unmapped columns are rejected unless `allow_extra_columns = TRUE`, which
explicitly opts into ignoring them. This reader covers one-dimensional
curves in CSV/TSV text; it does not parse nD data, DAD/MS vendor
structures, or vendor-specific formats.

## Examples

``` r
csv_file <- tempfile(fileext = ".csv")
writeLines(
  c(
    "sample_id,injection_id,channel_id,curve_id,location,value",
    "sample-01,injection-01,uv,curve-01,250,0.02",
    "sample-01,injection-01,uv,curve-01,251,0.04"
  ),
  csv_file,
  useBytes = TRUE
)
csv_columns <- c(
  sample_id = "sample_id",
  injection_id = "injection_id",
  channel_id = "channel_id",
  curve_id = "curve_id",
  location = "location",
  value = "value"
)
imported <- read_measure_record_delim(csv_file, csv_columns, "source-01")
imported
#> <measure_record>
#>   Samples: 1
#>   Injections: 1
#>   Channels: 1
#>   Curves: 1
#>   Sources: 1
unlink(csv_file)
```
