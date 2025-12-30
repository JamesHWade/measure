# Create Type B Uncertainty from Rectangular Distribution

Helper function to create a Type B uncertainty component from a
rectangular (uniform) distribution, common for specifications or
tolerances.

## Usage

``` r
uncertainty_type_b_rectangular(half_width, name = "Type B", sensitivity = 1)
```

## Arguments

- half_width:

  The half-width of the rectangular distribution (a). Standard
  uncertainty will be `a / sqrt(3)`.

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
# Temperature stability +/- 0.5 degrees
u_temp <- uncertainty_type_b_rectangular(0.5, name = "Temperature")
```
