# Summarize measure data quality

Provides a comprehensive quality summary for measure data, including
axis information and validation results.

## Usage

``` r
measure_quality_summary(x, verbose = TRUE)
```

## Arguments

- x:

  A `measure_tbl`, `measure_list`, or data frame with measure column.

- verbose:

  Logical; if TRUE, prints summary to console. Default is TRUE.

## Value

Invisibly returns a list containing axis info and validation results.

## Examples

``` r
specs <- new_measure_list(list(
  new_measure_tbl(location = seq(1000, 2500, by = 2), value = rnorm(751)),
  new_measure_tbl(location = seq(1000, 2500, by = 2), value = rnorm(751))
))
measure_quality_summary(specs)
#> 
#> ── Measure Data Quality Summary ──
#> 
#> 
#> 
#> ── Overview 
#> • Samples: 2
#> • Points per sample: 751
#> • Axis range: 1000 to 2500
#> • Axis type: wavelength_nm
#> • Direction: increasing
#> • Regular spacing: Yes
#> • Spacing: 2
#> 
#> 
#> ── Validation 
#>   • ✔ monotonic: Axis is monotonic
#>   • ✔ duplicates: No duplicate locations
#>   • ✔ missing: No missing values
#>   • ✔ spacing: Spacing is regular
#> 
#> 
#> ── Consistency 
#> ✔ All samples have consistent axes
```
