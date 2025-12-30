# Intermediate Precision (Between-Run Precision)

Calculates intermediate precision statistics for measurements performed
under varying conditions (different days, analysts, or instruments).

## Usage

``` r
measure_intermediate_precision(
  data,
  response_col,
  factors,
  group_col = NULL,
  conf_level = 0.95
)
```

## Arguments

- data:

  A data frame containing measurements with factor columns.

- response_col:

  Name of the column containing the response values.

- factors:

  Character vector of factor column names (e.g., `c("day", "analyst")`).

- group_col:

  Optional grouping column (e.g., concentration level).

- conf_level:

  Confidence level for intervals. Default is 0.95.

## Value

A `measure_precision` object containing variance components and
precision estimates:

- `component`: Name of the variance component

- `variance`: Estimated variance

- `percent_variance`: Percentage of total variance

- `sd`: Standard deviation (square root of variance)

- `cv`: Coefficient of variation (%) for that component

## Details

Intermediate precision quantifies the variability due to different
conditions within the same laboratory. This typically includes:

- Different days

- Different analysts

- Different equipment (of the same type)

The function uses a one-way or nested ANOVA approach to estimate
variance components. For more complex designs, consider using mixed
effects models with the `lme4` package.

## See also

[`measure_repeatability()`](https://jameshwade.github.io/measure/dev/reference/measure_repeatability.md),
[`measure_reproducibility()`](https://jameshwade.github.io/measure/dev/reference/measure_reproducibility.md)

Other precision:
[`measure_gage_rr()`](https://jameshwade.github.io/measure/dev/reference/measure_gage_rr.md),
[`measure_repeatability()`](https://jameshwade.github.io/measure/dev/reference/measure_repeatability.md),
[`measure_reproducibility()`](https://jameshwade.github.io/measure/dev/reference/measure_reproducibility.md)

## Examples

``` r
# Intermediate precision across days
set.seed(123)
data <- data.frame(
  day = rep(1:5, each = 6),
  concentration = rnorm(30, mean = 100, sd = 3) +
    rep(rnorm(5, 0, 2), each = 6)  # Day effect
)
measure_intermediate_precision(data, "concentration", factors = "day")
#> measure_precision: intermediate 
#> ──────────────────────────────────────────────────────────────────────────────── 
#> 
#> Variance Components:
#>   day: 0 (0%)
#>   Residual: 9.311 (100%)
#> 
#> CV by component:
#>   day: 0%
#>   Residual: 3%
```
