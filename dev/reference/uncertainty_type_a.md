# Create Type A Uncertainty from Repeated Measurements

Helper function to calculate Type A uncertainty from a vector of
repeated measurements.

## Usage

``` r
uncertainty_type_a(x, name = "Type A", sensitivity = 1)
```

## Arguments

- x:

  Numeric vector of repeated measurements.

- name:

  Name for this uncertainty component.

- sensitivity:

  Sensitivity coefficient (default 1).

## Value

An
[uncertainty_component](https://jameshwade.github.io/measure/dev/reference/uncertainty_component.md)
object.

## Examples

``` r
measurements <- c(10.1, 10.3, 9.9, 10.2, 10.0)
u_repeat <- uncertainty_type_a(measurements, "Repeatability")
```
