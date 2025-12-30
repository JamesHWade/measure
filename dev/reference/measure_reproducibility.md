# Reproducibility (Between-Lab Precision)

Calculates reproducibility statistics for measurements performed at
different laboratories.

## Usage

``` r
measure_reproducibility(
  data,
  response_col,
  lab_col,
  group_col = NULL,
  conf_level = 0.95
)
```

## Arguments

- data:

  A data frame containing measurements from multiple labs.

- response_col:

  Name of the column containing the response values.

- lab_col:

  Name of the column identifying the laboratory.

- group_col:

  Optional grouping column (e.g., concentration level).

- conf_level:

  Confidence level for intervals. Default is 0.95.

## Value

A `measure_precision` object containing:

- Within-lab variance (repeatability)

- Between-lab variance

- Total reproducibility variance

- Corresponding CV estimates

## Details

Reproducibility represents the precision of a method when performed at
different laboratories. It includes both within-lab (repeatability) and
between-lab variance components.

## See also

[`measure_repeatability()`](https://jameshwade.github.io/measure/dev/reference/measure_repeatability.md),
[`measure_intermediate_precision()`](https://jameshwade.github.io/measure/dev/reference/measure_intermediate_precision.md)

Other precision:
[`measure_gage_rr()`](https://jameshwade.github.io/measure/dev/reference/measure_gage_rr.md),
[`measure_intermediate_precision()`](https://jameshwade.github.io/measure/dev/reference/measure_intermediate_precision.md),
[`measure_repeatability()`](https://jameshwade.github.io/measure/dev/reference/measure_repeatability.md)

## Examples

``` r
# Reproducibility across laboratories
set.seed(123)
data <- data.frame(
  lab_id = rep(c("Lab_A", "Lab_B", "Lab_C"), each = 10),
  concentration = rnorm(30, mean = 100, sd = 2) +
    rep(c(0, 3, -2), each = 10)  # Lab bias
)
measure_reproducibility(data, "concentration", lab_col = "lab_id")
#> measure_precision: reproducibility 
#> ──────────────────────────────────────────────────────────────────────────────── 
#> 
#> Variance Components:
#>   lab_id: 9.442 (71%)
#>   Residual: 3.805 (29%)
#> 
#> CV by component:
#>   lab_id: 3.1%
#>   Residual: 1.9%
```
