# Augment Calibration Data

Add fitted values and residuals to calibration data.

## Usage

``` r
# S3 method for class 'measure_calibration'
augment(x, ...)
```

## Arguments

- x:

  A
  [measure_calibration](https://jameshwade.github.io/measure/dev/reference/measure_calibration.md)
  object.

- ...:

  Additional arguments (unused).

## Value

A tibble with the original calibration data plus:

- `.fitted`: Fitted values

- `.resid`: Residuals

- `.std_resid`: Standardized residuals

- `.hat`: Leverage values

- `.cooksd`: Cook's distance
