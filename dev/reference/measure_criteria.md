# Create a Set of Acceptance Criteria

Combines multiple
[`criterion()`](https://jameshwade.github.io/measure/dev/reference/criterion.md)
objects into a criteria set for use with
[`measure_assess()`](https://jameshwade.github.io/measure/dev/reference/measure_assess.md).

## Usage

``` r
measure_criteria(..., .list = NULL)
```

## Arguments

- ...:

  [`criterion()`](https://jameshwade.github.io/measure/dev/reference/criterion.md)
  objects or named arguments that will be converted to criteria. Named
  arguments use the format `name = list(operator, threshold)` or
  `name = threshold` (assumes `"<="`).

- .list:

  Optional list of criterion objects.

## Value

A `measure_criteria` object (list of `measure_criterion` objects).

## See also

[`criterion()`](https://jameshwade.github.io/measure/dev/reference/criterion.md)
for creating individual criteria,
[`measure_assess()`](https://jameshwade.github.io/measure/dev/reference/measure_assess.md)
for evaluating criteria.

## Examples

``` r
# Using criterion() objects
measure_criteria(
  criterion("cv_qc", "<", 15),
  criterion("r_squared", ">=", 0.99),
  criterion("recovery", "between", c(80, 120))
)
#> <measure_criteria> with 3 criteria
#>   • cv_qc < 15
#>   • r_squared >= 0.99
#>   • recovery in [80, 120]

# Using shorthand notation
measure_criteria(
  cv_qc = list("<", 15),
  r_squared = list(">=", 0.99),
  bias = list("between", c(-10, 10))
)
#> <measure_criteria> with 3 criteria
#>   • cv_qc < 15
#>   • r_squared >= 0.99
#>   • bias in [-10, 10]

# Simple threshold (assumes "<=")
measure_criteria(
  cv = 15,       # cv <= 15
  rsd = 20       # rsd <= 20
)
#> <measure_criteria> with 2 criteria
#>   • cv <= 15
#>   • rsd <= 20
```
