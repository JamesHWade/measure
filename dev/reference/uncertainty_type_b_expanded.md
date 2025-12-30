# Create Type B Uncertainty from Expanded Uncertainty

Helper function to create a Type B uncertainty component from an
expanded uncertainty value (e.g., from a certificate).

## Usage

``` r
uncertainty_type_b_expanded(
  expanded_U,
  k = 2,
  name = "Type B",
  df = Inf,
  sensitivity = 1
)
```

## Arguments

- expanded_U:

  The expanded uncertainty value.

- k:

  Coverage factor used for the expanded uncertainty.

- name:

  Name for this uncertainty component.

- df:

  Degrees of freedom (default Inf).

- sensitivity:

  Sensitivity coefficient (default 1).

## Value

An
[uncertainty_component](https://jameshwade.github.io/measure/dev/reference/uncertainty_component.md)
object.

## Examples

``` r
# From a calibrator certificate: U = 0.05, k = 2
u_cal <- uncertainty_type_b_expanded(0.05, k = 2, name = "Calibrator")
```
