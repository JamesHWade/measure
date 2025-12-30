# Glance at Calibration Curve Summary

Extract one-row summary statistics from a calibration curve.

## Usage

``` r
# S3 method for class 'measure_calibration'
glance(x, ...)
```

## Arguments

- x:

  A
  [measure_calibration](https://jameshwade.github.io/measure/dev/reference/measure_calibration.md)
  object.

- ...:

  Additional arguments (unused).

## Value

A tibble with columns:

- `r_squared`: Coefficient of determination

- `adj_r_squared`: Adjusted R-squared

- `sigma`: Residual standard error

- `df`: Degrees of freedom

- `model_type`: Model type (linear/quadratic)

- `weights_type`: Weighting scheme

- `n_points`: Number of calibration points

- `n_outliers`: Number of flagged outliers

## Examples

``` r
data <- data.frame(
  nominal_conc = c(0, 10, 25, 50, 100),
  response = c(0.5, 15.2, 35.8, 72.1, 148.3)
)
cal <- measure_calibration_fit(data, response ~ nominal_conc)
glance(cal)
#> # A tibble: 1 × 8
#>   r_squared adj_r_squared sigma    df model_type weights_type n_points
#>       <dbl>         <dbl> <dbl> <int> <chr>      <chr>           <int>
#> 1     1.000         1.000  1.26     3 linear     none                5
#> # ℹ 1 more variable: n_outliers <int>
```
