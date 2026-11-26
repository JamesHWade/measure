# Read an infrared JCAMP-DX spectrum

Import one continuous infrared spectrum into a
[`measure_record()`](https://jameshwade.github.io/measure/dev/reference/measure_record.md).
Supply the identities from your experiment. The file title is
descriptive metadata and may be shared by many acquisitions.

## Usage

``` r
measure_read_jcamp(
  path,
  sample_id,
  injection_id,
  channel_id,
  curve_id,
  source_id,
  max_bytes = 64 * 1024^2
)

measure_jcamp_headers(path, max_bytes = 64 * 1024^2)
```

## Arguments

- path:

  Path to a local JCAMP-DX text file.

- sample_id, injection_id, channel_id, curve_id, source_id:

  Single, nonblank character identifiers for the sample, injection,
  detector channel, curve, and original file, respectively.

- max_bytes:

  Maximum input size in bytes. Defaults to 64 MiB.

## Value

`measure_read_jcamp()` returns a validated `measure_record`. The source
table contains the input filename and SHA-256 digest of the bytes
parsed. The source ordinals count spectral points in acquisition order.

`measure_jcamp_headers()` returns a tibble of `label` and `value` in
source order. It retains original label spellings and repeat entries.
Comments, surrounding whitespace, and data rows are omitted. Multiline
declarations use a newline between lines. Reading headers does not
validate the numeric spectrum.

## Details

The reader supports a single infrared block with plain numeric (AFFN)
data, in either `XYPOINTS=(XY..XY)` or `XYDATA=(X++(Y..Y))` form.
Explicit pairs keep their individual coordinates. For a regular grid the
first coordinate, last coordinate, and point count define the axis.
Declared scale factors apply to the stored numbers. An ordinate marked
`?` becomes `NA_real_`. The record keeps the declared units and any
explicit `XLABEL` and `YLABEL` axis and response names. The maximum
spectrum length is one million points.

Use `measure_jcamp_headers()` to inspect declarations such as title,
origin, owner, acquisition settings, and instrument-specific fields.
Keep the original file for the complete acquisition metadata. Use
[`measure_write_jcamp()`](https://jameshwade.github.io/measure/dev/reference/measure_write_jcamp.md)
to exchange one curve and
[`write_measure_record()`](https://jameshwade.github.io/measure/dev/reference/record-bundle.md)
to save all five record tables.

## References

[IUPAC JCAMP-DX
specifications](https://iupac.org/what-we-do/digital-standards/jcamp-dx/)

## Examples

``` r
path <- system.file("extdata", "ir-film.jdx", package = "measure")
headers <- measure_jcamp_headers(path)
headers[headers$label %in% c("TITLE", "ORIGIN", "OWNER"), ]
#> # A tibble: 3 × 2
#>   label  value                  
#>   <chr>  <chr>                  
#> 1 TITLE  Simulated film spectrum
#> 2 ORIGIN measure examples       
#> 3 OWNER  measure contributors   
record <- measure_read_jcamp(path, "film", "scan-01", "ir", "film-ir", "file-01")
spectrum <- record$curves$.measures[[1]]
spectrum[which.max(spectrum$value), ]
#> <measure_tbl [1 x 2]>
#> # A tibble: 1 × 2
#>   location value
#>      <dbl> <dbl>
#> 1     1736 0.455
```
