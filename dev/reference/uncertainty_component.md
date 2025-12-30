# Create an Uncertainty Component

Defines a single uncertainty component for use in an uncertainty budget.
This follows ISO GUM terminology with Type A (statistical) and Type B
(other means) uncertainty evaluation.

## Usage

``` r
uncertainty_component(
  name,
  value,
  type = c("A", "B"),
  sensitivity = 1,
  df = Inf,
  distribution = c("normal", "rectangular", "triangular", "u-shaped"),
  coverage_factor = 1
)
```

## Arguments

- name:

  Name/description of the uncertainty source.

- value:

  Standard uncertainty value (u).

- type:

  Type of evaluation:

  - `"A"`: Statistical evaluation (from repeated measurements)

  - `"B"`: Evaluated by other means (from specifications, certificates,
    etc.)

- sensitivity:

  Sensitivity coefficient (c). Default is 1. The contribution to
  combined uncertainty is `|c| * u`.

- df:

  Degrees of freedom for this component. Default is `Inf` (for Type B
  with no DOF information).

- distribution:

  Distribution assumed for Type B:

  - `"normal"`: Normal distribution (default for Type A)

  - `"rectangular"`: Uniform distribution (common for Type B)

  - `"triangular"`: Triangular distribution

  - `"u-shaped"`: U-shaped distribution

- coverage_factor:

  Coverage factor (k) used to derive this value from an expanded
  uncertainty. Default is 1 (value is already standard uncertainty).

## Value

An `uncertainty_component` object.

## Details

### Type A Evaluation

For Type A components, the standard uncertainty is typically the
standard error of the mean: `u = s / sqrt(n)`, with `df = n - 1`.

### Type B Evaluation

For Type B components from expanded uncertainties with coverage k:
`u = U / k`. For rectangular distributions: `u = a / sqrt(3)`.

## See also

[`measure_uncertainty_budget()`](https://jameshwade.github.io/measure/dev/reference/measure_uncertainty_budget.md)
for combining components,
[`measure_uncertainty()`](https://jameshwade.github.io/measure/dev/reference/measure_uncertainty.md)
for quick uncertainty calculation.

## Examples

``` r
# Type A: Repeatability from 10 measurements
u_repeat <- uncertainty_component(
  name = "Repeatability",
  value = 0.05,  # Standard error of mean
  type = "A",
  df = 9
)

# Type B: Calibrator uncertainty from certificate (k=2)
u_cal <- uncertainty_component(
  name = "Calibrator",
  value = 0.02 / 2,  # Divide expanded uncertainty by k
  type = "B",
  df = 50
)

# Type B: Temperature effect (rectangular distribution)
u_temp <- uncertainty_component(
  name = "Temperature",
  value = 0.1 / sqrt(3),  # Half-width / sqrt(3) for rectangular
  type = "B",
  distribution = "rectangular"
)
```
