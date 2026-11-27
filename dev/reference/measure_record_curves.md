# Extract a provenance-linked curve table from a measurement record

`measure_record_curves()` joins the relational tables of a
[`measure_record()`](https://jameshwade.github.io/measure/dev/reference/measure_record.md)
into a recipe-ready tibble while retaining sample, injection, channel,
curve, source-digest, and parsed source-row provenance. Curves are
returned in their original order without sorting, resampling, or unit
conversion.

## Usage

``` r
measure_record_curves(record, channel_id = NULL)
```

## Arguments

- record:

  A
  [`measure_record()`](https://jameshwade.github.io/measure/dev/reference/measure_record.md)
  object.

- channel_id:

  Optional single character string restricting output to one declared
  channel in `record$channels`.

## Value

A tibble with one row per curve and columns `sample_id`, `injection_id`,
`curve_id`, `channel_id`, `channel_name`, `source_id`, `source_name`,
`source_sha256`, `axis_name`, `axis_unit`, `value_name`, `value_unit`,
`n_points`, `source_row_start`, `source_row_end`, `.measures`, and
`.source_rows`.

## See also

[`measure_record()`](https://jameshwade.github.io/measure/dev/reference/measure_record.md),
[`read_measure_record_delim()`](https://jameshwade.github.io/measure/dev/reference/read_measure_record_delim.md),
[`measure_quantify_batch()`](https://jameshwade.github.io/measure/dev/reference/measure_quantify_batch.md)

## Examples

``` r
curve <- new_measure_tbl(c(1, 2, 3), c(0.1, 0.5, 0.2))
rec <- measure_record(
  samples = tibble::tibble(sample_id = "S1"),
  injections = tibble::tibble(injection_id = "INJ-1", sample_id = "S1"),
  channels = tibble::tibble(channel_id = "uv254", channel_name = "UV 254 nm"),
  curves = tibble::tibble(
    curve_id = "C1",
    injection_id = "INJ-1",
    channel_id = "uv254",
    .measures = list(curve)
  )
)
measure_record_curves(rec)
#> # A tibble: 1 × 17
#>   sample_id injection_id curve_id channel_id channel_name source_id source_name
#>   <chr>     <chr>        <chr>    <chr>      <chr>        <chr>     <chr>      
#> 1 S1        INJ-1        C1       uv254      UV 254 nm    NA        NA         
#> # ℹ 10 more variables: source_sha256 <chr>, axis_name <chr>, axis_unit <chr>,
#> #   value_name <chr>, value_unit <chr>, n_points <int>, source_row_start <int>,
#> #   source_row_end <int>, .measures <meas>, .source_rows <list>
```
