# System Suitability Check

Performs system suitability tests on QC or reference samples to verify
instrument performance meets requirements.

## Usage

``` r
measure_system_suitability(
  data,
  metrics,
  sample_type_col = NULL,
  sst_type = "sst"
)
```

## Arguments

- data:

  A data frame containing system suitability data.

- metrics:

  Named list of columns and their acceptance criteria. Each element
  should be a list with `col`, `min`, and/or `max`.

- sample_type_col:

  Optional column identifying sample types.

- sst_type:

  Value in sample_type_col that identifies SST samples.

## Value

A `measure_sst` object containing:

- `results`: Pass/fail status for each metric

- `summary`: Overall pass/fail and summary statistics

- `details`: Individual sample results

## Details

System suitability testing (SST) verifies that the analytical system is
performing adequately before, during, or after a run. Common metrics
include:

- Peak resolution

- Retention time reproducibility

- Peak symmetry/tailing factor

- Signal-to-noise ratio

- Plate count

## See also

Other control-charts:
[`measure_control_chart()`](https://jameshwade.github.io/measure/dev/reference/measure_control_chart.md),
[`measure_control_limits()`](https://jameshwade.github.io/measure/dev/reference/measure_control_limits.md)

## Examples

``` r
# System suitability check
sst_data <- data.frame(
  sample_id = paste0("SST_", 1:5),
  resolution = c(2.1, 2.3, 2.2, 2.0, 2.1),
  tailing = c(1.1, 1.0, 1.2, 1.1, 1.0),
  plates = c(5200, 5100, 5300, 5000, 5150)
)

result <- measure_system_suitability(
  sst_data,
  metrics = list(
    resolution = list(col = "resolution", min = 2.0),
    tailing = list(col = "tailing", max = 1.5),
    plates = list(col = "plates", min = 5000)
  )
)
print(result)
#> measure_system_suitability
#> ──────────────────────────────────────────────────────────────────────────────── 
#> 
#> Samples evaluated: 5 
#> Metrics checked: 3 
#> Passed: 3 / 3 
#> 
#> Overall Status: PASS
#> 
#> Results:
#> # A tibble: 3 × 11
#>   metric     column        n   mean      sd    cv min_spec max_spec observed_min
#>   <chr>      <chr>     <int>  <dbl>   <dbl> <dbl>    <dbl>    <dbl>        <dbl>
#> 1 resolution resoluti…     5 2.14e0 1.14e-1  5.33        2     NA              2
#> 2 tailing    tailing       5 1.08e0 8.37e-2  7.75       NA      1.5            1
#> 3 plates     plates        5 5.15e3 1.12e+2  2.17     5000     NA           5000
#> # ℹ 2 more variables: observed_max <dbl>, pass <lgl>
```
