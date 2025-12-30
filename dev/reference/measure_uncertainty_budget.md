# Create an Uncertainty Budget

Combines multiple uncertainty components into a complete uncertainty
budget following ISO GUM methodology. Calculates combined standard
uncertainty, effective degrees of freedom (Welch-Satterthwaite), and
expanded uncertainty.

## Usage

``` r
measure_uncertainty_budget(..., .list = NULL, k = 2, result_value = NULL)
```

## Arguments

- ...:

  [`uncertainty_component()`](https://jameshwade.github.io/measure/dev/reference/uncertainty_component.md)
  objects to include in the budget.

- .list:

  Optional list of uncertainty components.

- k:

  Coverage factor for expanded uncertainty. Default is 2 (approximately
  95% coverage for normal distribution).

- result_value:

  Optional. The measurement result value, used for calculating relative
  uncertainty.

## Value

A `measure_uncertainty_budget` object containing:

- `components`: List of input uncertainty components

- `combined_u`: Combined standard uncertainty

- `effective_df`: Effective degrees of freedom (Welch-Satterthwaite)

- `coverage_factor`: The k value used

- `expanded_U`: Expanded uncertainty (k \* combined_u)

- `result_value`: The measurement result (if provided)

- `relative_u`: Relative standard uncertainty (if result provided)

## Details

### Combined Standard Uncertainty

Calculated as the root sum of squares of contributions: \$\$u_c =
\sqrt{\sum_i (c_i \cdot u_i)^2}\$\$

### Welch-Satterthwaite Effective Degrees of Freedom

\$\$\nu\_{eff} = \frac{u_c^4}{\sum_i \frac{(c_i \cdot
u_i)^4}{\nu_i}}\$\$

This is used to determine the appropriate coverage factor for a given
confidence level.

### Expanded Uncertainty

\$\$U = k \cdot u_c\$\$

With k=2, this provides approximately 95% coverage.

## See also

[`uncertainty_component()`](https://jameshwade.github.io/measure/dev/reference/uncertainty_component.md)
for creating components,
[`tidy.measure_uncertainty_budget()`](https://jameshwade.github.io/measure/dev/reference/tidy.measure_uncertainty_budget.md)
for extracting results,
[`autoplot.measure_uncertainty_budget()`](https://jameshwade.github.io/measure/dev/reference/autoplot.measure_uncertainty_budget.md)
for visualization.

## Examples

``` r
# Create components
u_repeat <- uncertainty_component("Repeatability", 0.05, type = "A", df = 9)
u_cal <- uncertainty_component("Calibrator", 0.02, type = "B", df = 50)
u_temp <- uncertainty_component("Temperature", 0.03, type = "B")

# Create budget
budget <- measure_uncertainty_budget(u_repeat, u_cal, u_temp, k = 2)
print(budget)
#> <measure_uncertainty_budget>
#>   Components: 3 (1 Type A, 2 Type B)
#>   Combined u: 0.06164
#>   Effective df: 21
#>   Coverage k: 2
#>   Expanded U: 0.1233

# With result value for relative uncertainty
budget <- measure_uncertainty_budget(
  u_repeat, u_cal, u_temp,
  result_value = 10.5
)
```
