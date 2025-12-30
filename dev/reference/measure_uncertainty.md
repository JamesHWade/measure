# Quick Uncertainty Calculation

A convenience function that returns just the key uncertainty values
without the full budget object.

## Usage

``` r
measure_uncertainty(..., .list = NULL, k = 2)
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

## Value

A named list with:

- `combined_u`: Combined standard uncertainty

- `expanded_U`: Expanded uncertainty

- `effective_df`: Effective degrees of freedom

- `coverage_factor`: Coverage factor used

## Examples

``` r
u1 <- uncertainty_component("A", 0.05, type = "A", df = 9)
u2 <- uncertainty_component("B", 0.03, type = "B")

measure_uncertainty(u1, u2)
#> $combined_u
#> [1] 0.05830952
#> 
#> $expanded_U
#> [1] 0.116619
#> 
#> $effective_df
#> [1] 16.6464
#> 
#> $coverage_factor
#> [1] 2
#> 
```
