# Create a bounded mass-spectrometry and array acquisition

An acquisition composes a validated
[`measure_record()`](https://jameshwade.github.io/measure/dev/reference/measure_record.md)
with declared streams, mass-spectrometry scans, and two-dimensional
arrays. Raw values and point order are retained without interpreting,
aligning, or serializing the measurements.

## Usage

``` r
measure_acquisition(record, streams = NULL, scans = NULL, arrays = NULL)

# S3 method for class 'measure_acquisition'
print(x, ...)

# S3 method for class 'measure_acquisition'
summary(object, ...)
```

## Arguments

- record:

  A `measure_record` describing samples, injections, channels, sources,
  and scalar curves.

- streams:

  A data frame with stream declarations, or `NULL` for an empty table.
  The first four columns are `stream_id`, `injection_id`, `channel_id`,
  and `kind`; remaining columns are nullable text metadata.

- scans:

  A data frame with scan declarations and raw one-dimensional spectra,
  or `NULL` for an empty table.

- arrays:

  A data frame with array declarations and raw two-dimensional
  measurements, or `NULL` for an empty table.

- x:

  A measurement acquisition to print.

- ...:

  Additional arguments; currently unused.

- object:

  A measurement acquisition to summarize.

## Value

A list with class `measure_acquisition` and components `record`,
`streams`, `scans`, and `arrays`.

## Details

All supplied tables use named columns. Unknown fields, duplicate fields,
extension attributes and malformed row sizes are rejected. The
constructor retains table order; it does not generate identifiers or
infer units. Omitted nullable metadata becomes typed missing values.

## Stream declarations

Each stream requires `stream_id`, `injection_id`, `channel_id`, and
`kind` (`"ms"` or `"nd"`). Injection and channel IDs refer to the
embedded record; stream IDs are unique within the acquisition. Optional
columns are `source_id`, `time_unit`, `mz_unit`, `value_name`, and
`value_unit`. A nonmissing source ID refers to the record's source
table. ND stream axis metadata belongs to its arrays, so its `time_unit`
and `mz_unit` must be missing. Units are declarations; no conversion is
performed.

## Scan declarations

Each scan requires `scan_id`, `stream_id`, `scan_index`, `scan_status`,
and `.measures`. Its stream must have kind `"ms"`. The explicit positive
`scan_index` is unique within a stream and may have gaps; gaps do not
create missing scans. `scan_status` is `"acquired"` or `"missing"`; a
declared missing scan has zero points and differs from an acquired-empty
scan.

Optional columns are `retention_time`, `polarity`, `ms_level`,
`spectrum_type`, `source_scan_id`, `.source_rows`, and `.saturated`.
Retention time becomes double, with finite or missing values. MS level
becomes a positive integer or missing. Polarity is `"positive"`,
`"negative"`, or missing; spectrum type is `"centroid"`, `"profile"`, or
missing. A source scan ID is opaque text, not a parsed-row ordinal.

`.measures` is an unnamed list of standard
[`new_measure_tbl()`](https://jameshwade.github.io/measure/dev/reference/new_measure_tbl.md)
payloads: location stores m/z and value stores intensity. Raw
integer/double storage, point order, duplicate or unsorted coordinates,
signed zero, nonfinite states and typed empty vectors are retained.
Storage validation does not establish suitability for a calculation.

## Array declarations and point metadata

Each array requires `array_id`, `stream_id`, and `.measures`; its stream
has kind `"nd"`. Payloads are an unnamed list of standard
two-dimensional
[`new_measure_nd_tbl()`](https://jameshwade.github.io/measure/dev/reference/new_measure_nd_tbl.md)
objects. Each array retains its own coordinates, dimension metadata and
numeric storage, including sparse or repeated grids.

Scans and arrays may provide `.source_rows` and `.saturated` as unnamed
lists aligned to their payload points. Source rows are positive integer
parsed-record ordinals or missing, with duplicates allowed; they are not
physical file lines. Saturation is logical: `TRUE` means declared
saturated, `FALSE` declared not saturated, and `NA` not assessed.
Omitted masks and row references are unknown, with typed empty vectors
for zero-point payloads. Missing signal values do not supply a
saturation assessment.

This container does not define a vendor parser, physical instrument
qualification, precursor annotation, method schema or processing graph.
It preserves the supported explicit declarations without authenticating
their source. The embedded scalar record keeps its independent contract.

## Examples

``` r
context <- measure_record(
  samples = tibble::tibble(sample_id = "sample-1"),
  injections = tibble::tibble(
    injection_id = "injection-1", sample_id = "sample-1"
  ),
  channels = tibble::tibble(channel_id = "ms")
)
acquisition <- measure_acquisition(
  context,
  streams = tibble::tibble(
    stream_id = "ms-1", injection_id = "injection-1",
    channel_id = "ms", kind = "ms", time_unit = "s", mz_unit = "m/z"
  ),
  scans = tibble::tibble(
    scan_id = "scan-1", stream_id = "ms-1", scan_index = 1L,
    scan_status = "acquired", retention_time = 1,
    polarity = "positive", ms_level = 1L, spectrum_type = "centroid",
    .measures = list(new_measure_tbl(c(100, 101), c(4, 8)))
  )
)
acquisition
#> <measure_acquisition>
#>   Samples: 1
#>   Injections: 1
#>   Channels: 1
#>   Sources: 0
#>   Curves: 0
#>   Streams: 1
#>   Scans: 1
#>   Arrays: 0
summary(acquisition)
#> <summary_measure_acquisition>
#>   samples: 1
#>   injections: 1
#>   channels: 1
#>   sources: 0
#>   curves: 0
#>   streams: 1
#>   scans: 1
#>   arrays: 0
```
