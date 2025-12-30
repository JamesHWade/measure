# Extract Calibration Curve Data

S3 method to extract the underlying data from a calibration object in a
format suitable for ggplot2.

## Usage

``` r
# S3 method for class 'measure_calibration'
fortify(model, data = NULL, ...)
```

## Arguments

- model:

  A
  [measure_calibration](https://jameshwade.github.io/measure/dev/reference/measure_calibration.md)
  object.

- data:

  Ignored.

- ...:

  Additional arguments (unused).

## Value

A data frame with the calibration data and fitted values/residuals.
