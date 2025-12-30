# Extract Failed Criteria

Returns only the criteria that failed assessment.

## Usage

``` r
get_failures(assessment)
```

## Arguments

- assessment:

  A `measure_assessment` object from
  [`measure_assess()`](https://jameshwade.github.io/measure/dev/reference/measure_assess.md).

## Value

A filtered `measure_assessment` tibble containing only failures.

## Examples

``` r
crit <- measure_criteria(cv = 15, rsd = 20)
results <- list(cv = 18, rsd = 25)  # Both fail
assessment <- measure_assess(results, crit)
get_failures(assessment)
#> <measure_assessment> [FAIL]
#>   0 passed, 2 failed
#> 
#>   ✗ cv: 18 (<= 15)
#>   ✗ rsd: 25 (<= 20)
```
