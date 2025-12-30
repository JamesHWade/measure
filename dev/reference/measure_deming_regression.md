# Deming Regression for Method Comparison

Performs Deming regression to compare two measurement methods when both
have measurement error. This is preferred over ordinary least squares
when both methods have non-negligible error.

## Usage

``` r
measure_deming_regression(
  data,
  method1_col,
  method2_col,
  error_ratio = NULL,
  method1_sd = NULL,
  method2_sd = NULL,
  bootstrap = FALSE,
  bootstrap_n = 1000,
  conf_level = 0.95
)
```

## Arguments

- data:

  A data frame containing paired measurements.

- method1_col:

  Name of column for method 1 (typically reference/comparator).

- method2_col:

  Name of column for method 2 (typically test method).

- error_ratio:

  Ratio of error variances (var_method2 / var_method1). Default is 1
  (equal variances). Can be estimated from replicate data.

- method1_sd:

  Optional known SD of method 1. Used to calculate error_ratio.

- method2_sd:

  Optional known SD of method 2. Used to calculate error_ratio.

- bootstrap:

  Use bootstrap for confidence intervals? Default is FALSE.

- bootstrap_n:

  Number of bootstrap samples. Default is 1000.

- conf_level:

  Confidence level for intervals. Default is 0.95.

## Value

A `measure_deming_regression` object containing:

- `coefficients`: Tibble with intercept and slope estimates and CIs

- `statistics`: List of diagnostic statistics (RMSE, R-squared)

- `data_summary`: Summary of input data

- `bootstrap`: Bootstrap results if requested

## Details

### Error Ratio

The error ratio (lambda) represents the ratio of error variances:
`lambda = var(method2) / var(method1)`

Common approaches:

- `lambda = 1`: Assume equal error variances

- Estimate from replicates: Use SDs from replicate measurements

- Estimate from calibration: Use known method precision data

### Interpretation

For equivalent methods:

- Slope should be close to 1 (proportional agreement)

- Intercept should be close to 0 (no constant bias)

If 95% CI for slope includes 1 and CI for intercept includes 0, methods
are considered equivalent.

### Implementation

If the `mcr` package is available, it is used for fitting. Otherwise, a
manual implementation is used with optional bootstrap CIs.

## See also

[`measure_bland_altman()`](https://jameshwade.github.io/measure/dev/reference/measure_bland_altman.md),
[`measure_passing_bablok()`](https://jameshwade.github.io/measure/dev/reference/measure_passing_bablok.md)

Other method-comparison:
[`measure_bland_altman()`](https://jameshwade.github.io/measure/dev/reference/measure_bland_altman.md),
[`measure_passing_bablok()`](https://jameshwade.github.io/measure/dev/reference/measure_passing_bablok.md),
[`measure_proficiency_score()`](https://jameshwade.github.io/measure/dev/reference/measure_proficiency_score.md)

## Examples

``` r
# Method comparison data
data <- data.frame(
  reference = c(5.2, 10.5, 15.8, 25.3, 50.1, 75.4, 100.2),
  new_method = c(5.1, 10.8, 16.2, 25.9, 49.8, 76.1, 101.3)
)

# Deming regression with bootstrap CIs
result <- measure_deming_regression(
  data,
  method1_col = "reference",
  method2_col = "new_method",
  bootstrap = TRUE,
  bootstrap_n = 500
)
#> Using default error ratio of 1. Provide `error_ratio` or SDs for more accurate
#> results.

print(result)
#> measure_deming_regression
#> ──────────────────────────────────────────────────────────────────────────────── 
#> 
#> Coefficients:
#> # A tibble: 2 × 4
#>   term      estimate ci_lower ci_upper
#>   <chr>        <dbl>    <dbl>    <dbl>
#> 1 intercept   0.0594   -0.359    0.447
#> 2 slope       1.01      0.988    1.03 
#> 
#> Statistics:
#>   n = 7 
#>   Error ratio = 1 
#>   RMSE = 0.3508 
#>   R² = 0.9999 
#> 
#> (Fitted using mcr package)
tidy(result)
#> # A tibble: 2 × 4
#>   term      estimate ci_lower ci_upper
#>   <chr>        <dbl>    <dbl>    <dbl>
#> 1 intercept   0.0594   -0.359    0.447
#> 2 slope       1.01      0.988    1.03 
```
