# Test if Object is a Calibration Curve

Test if Object is a Calibration Curve

## Usage

``` r
is_measure_calibration(x)
```

## Arguments

- x:

  Object to test.

## Value

Logical: TRUE if `x` is a `measure_calibration` object.

## Examples

``` r
# After fitting a calibration curve
data <- data.frame(
  nominal_conc = c(0, 10, 25, 50, 100),
  response = c(0.5, 15.2, 35.8, 72.1, 148.3)
)
cal <- measure_calibration_fit(data, response ~ nominal_conc)
is_measure_calibration(cal)
#> [1] TRUE
```
