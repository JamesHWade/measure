# Create an Acceptance Criterion

Defines a single acceptance criterion for analytical validation.
Criteria are used with
[`measure_assess()`](https://jameshwade.github.io/measure/dev/reference/measure_assess.md)
to produce pass/fail decisions.

## Usage

``` r
criterion(
  name,
  operator = c("<", "<=", ">", ">=", "==", "!=", "between", "outside"),
  threshold,
  description = NULL,
  priority = c("major", "critical", "minor")
)
```

## Arguments

- name:

  Character string naming this criterion (e.g., "cv_qc", "r_squared").

- operator:

  Comparison operator: `"<"`, `"<="`, `">"`, `">="`, `"=="`, `"!="`,
  `"between"`, or `"outside"`.

- threshold:

  Numeric threshold value. For `"between"` and `"outside"`, provide a
  length-2 vector `c(lower, upper)`.

- description:

  Optional human-readable description of the criterion.

- priority:

  Optional priority level: `"critical"`, `"major"`, `"minor"`. Affects
  how failures are reported.

## Value

A `measure_criterion` object.

## See also

[`measure_criteria()`](https://jameshwade.github.io/measure/dev/reference/measure_criteria.md)
for combining multiple criteria,
[`measure_assess()`](https://jameshwade.github.io/measure/dev/reference/measure_assess.md)
for evaluating criteria.

## Examples

``` r
# QC coefficient of variation must be < 15%
criterion("cv_qc", "<", 15, description = "QC CV < 15%")
#> <measure_criterion>
#>   cv_qc < 15
#>   Priority: major
#>   Description: QC CV < 15%

# R-squared must be >= 0.99
criterion("r_squared", ">=", 0.99)
#> <measure_criterion>
#>   r_squared >= 0.99
#>   Priority: major
#>   Description: r_squared >= 0.99

# Recovery must be between 80% and 120%
criterion("recovery", "between", c(80, 120), priority = "critical")
#> <measure_criterion>
#>   recovery between  80120 [80, 120]
#>   Priority: critical
#>   Description: recovery in [80, 120]
```
