# Assess Data Against Acceptance Criteria

Evaluates a set of values against acceptance criteria and returns a
detailed assessment table with pass/fail status.

## Usage

``` r
measure_assess(data, criteria, action = c("return", "warn", "error"))
```

## Arguments

- data:

  A named list or data frame containing the values to assess. Names must
  match criterion names.

- criteria:

  A
  [`measure_criteria()`](https://jameshwade.github.io/measure/dev/reference/measure_criteria.md)
  object defining the acceptance criteria.

- action:

  What to do on failure: `"return"` (default) returns the assessment
  table, `"warn"` issues a warning for failures, `"error"` stops on any
  critical failures.

## Value

A tibble with class `measure_assessment` containing:

- `criterion`: Name of the criterion

- `value`: The observed value

- `threshold`: The threshold value(s)

- `operator`: The comparison operator

- `pass`: Logical indicating pass/fail

- `priority`: Priority level of the criterion

- `description`: Human-readable description

## See also

[`measure_criteria()`](https://jameshwade.github.io/measure/dev/reference/measure_criteria.md)
for creating criteria,
[`criterion()`](https://jameshwade.github.io/measure/dev/reference/criterion.md)
for individual criteria.

## Examples

``` r
# Define criteria
crit <- measure_criteria(
  cv_qc = list("<", 15),
  r_squared = list(">=", 0.99),
  recovery = list("between", c(80, 120))
)

# Assess results
results <- list(cv_qc = 12.5, r_squared = 0.995, recovery = 98.2)
measure_assess(results, crit)
#> <measure_assessment> [PASS]
#>   3 passed, 0 failed
#> 
#>   ✓ cv_qc: 12.5 (< 15)
#>   ✓ r_squared: 0.995 (>= 0.99)
#>   ✓ recovery: 98.2 (between [80, 120])

# Assess with some failures
results_bad <- list(cv_qc = 18.3, r_squared = 0.985, recovery = 75)
measure_assess(results_bad, crit)
#> <measure_assessment> [FAIL]
#>   0 passed, 3 failed
#> 
#>   ✗ cv_qc: 18.3 (< 15)
#>   ✗ r_squared: 0.985 (>= 0.99)
#>   ✗ recovery: 75 (between [80, 120])
```
