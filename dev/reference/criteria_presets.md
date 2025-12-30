# Preset Acceptance Criteria

Factory functions that return commonly-used criteria sets for analytical
validation workflows.

## Usage

``` r
criteria_bioanalytical(
  cv_qc = 15,
  cv_calibration = 20,
  r_squared = 0.99,
  recovery_range = c(80, 120),
  accuracy_bias = 15
)

criteria_ich_q2(
  cv_repeatability = 2,
  cv_intermediate = 5,
  recovery_range = c(98, 102),
  r_squared = 0.999
)
```

## Arguments

- cv_qc:

  Maximum allowable CV for QC samples (default 15%, bioanalytical).

- cv_calibration:

  Maximum allowable CV for calibration replicates (default 20%).

- r_squared:

  Minimum R-squared for calibration curve.

- recovery_range:

  Acceptable recovery range as c(lower, upper).

- accuracy_bias:

  Maximum allowable bias (default 15%).

- cv_repeatability:

  Maximum allowable CV for repeatability (default 2%, ICH Q2).

- cv_intermediate:

  Maximum allowable CV for intermediate precision (default 5%, ICH Q2).

## Value

A
[measure_criteria](https://jameshwade.github.io/measure/dev/reference/measure_criteria.md)
object.

## Examples

``` r
# Default bioanalytical criteria
criteria_bioanalytical()
#> <measure_criteria> with 5 criteria
#>   • QC CV <= 15%
#>   • Calibration CV <= 20%
#>   • R² >= 0.99
#>   • Recovery 80-120%
#>   • Bias within +/-15%

# Custom thresholds
criteria_bioanalytical(cv_qc = 20, r_squared = 0.98)
#> <measure_criteria> with 5 criteria
#>   • QC CV <= 20%
#>   • Calibration CV <= 20%
#>   • R² >= 0.98
#>   • Recovery 80-120%
#>   • Bias within +/-15%
```
