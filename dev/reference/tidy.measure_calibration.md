# Tidy a Calibration Curve

Extract coefficients and statistics from a calibration curve in tidy
format.

## Usage

``` r
# S3 method for class 'measure_calibration'
tidy(x, ...)

# S3 method for class 'measure_calibration_verify'
tidy(x, ...)
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

- `term`: Coefficient name (intercept, slope, quadratic)

- `estimate`: Coefficient estimate

- `std_error`: Standard error

- `statistic`: t-statistic

- `p_value`: p-value

## Examples

``` r
data <- data.frame(
  nominal_conc = c(0, 10, 25, 50, 100),
  response = c(0.5, 15.2, 35.8, 72.1, 148.3)
)
cal <- measure_calibration_fit(data, response ~ nominal_conc)
tidy(cal)
#> # A tibble: 2 × 5
#>   term         estimate std_error statistic    p_value
#>   <chr>           <dbl>     <dbl>     <dbl>      <dbl>
#> 1 (Intercept)    -0.260    0.811     -0.320 0.770     
#> 2 nominal_conc    1.48     0.0158    93.6   0.00000269
```
