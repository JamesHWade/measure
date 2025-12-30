# Check if All Criteria Pass

A convenience function to check if all criteria in an assessment passed.

## Usage

``` r
all_pass(assessment, na_pass = FALSE)
```

## Arguments

- assessment:

  A `measure_assessment` object from
  [`measure_assess()`](https://jameshwade.github.io/measure/dev/reference/measure_assess.md).

- na_pass:

  Logical. Should NA results count as pass? Default is FALSE.

## Value

Logical: TRUE if all criteria passed, FALSE otherwise.

## Examples

``` r
crit <- measure_criteria(cv = 15, rsd = 20)
results <- list(cv = 10, rsd = 15)
assessment <- measure_assess(results, crit)
all_pass(assessment)
#> [1] TRUE
```
