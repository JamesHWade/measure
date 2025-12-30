# Tidy an Uncertainty Budget

Extract uncertainty budget information in tidy format.

## Usage

``` r
# S3 method for class 'measure_uncertainty_budget'
tidy(x, type = c("components", "summary"), ...)
```

## Arguments

- x:

  A
  [measure_uncertainty_budget](https://jameshwade.github.io/measure/dev/reference/measure_uncertainty_budget.md)
  object.

- type:

  What to return:

  - `"components"` (default): Table of individual components

  - `"summary"`: Single row summary of the budget

- ...:

  Additional arguments (unused).

## Value

A tibble with budget information.

## Examples

``` r
u1 <- uncertainty_component("Repeatability", 0.05, type = "A", df = 9)
u2 <- uncertainty_component("Calibrator", 0.02, type = "B")
budget <- measure_uncertainty_budget(u1, u2)

tidy(budget)
#> # A tibble: 2 × 8
#>   name          type      u sensitivity contribution variance_contribution
#>   <chr>         <chr> <dbl>       <dbl>        <dbl>                 <dbl>
#> 1 Repeatability A      0.05           1         0.05                0.0025
#> 2 Calibrator    B      0.02           1         0.02                0.0004
#> # ℹ 2 more variables: percent_contribution <dbl>, df <dbl>
tidy(budget, type = "summary")
#> # A tibble: 1 × 7
#>   combined_u effective_df coverage_factor expanded_U result_value relative_u
#>        <dbl>        <dbl>           <dbl>      <dbl>        <dbl>      <dbl>
#> 1     0.0539         12.1               2      0.108           NA         NA
#> # ℹ 1 more variable: relative_U <dbl>
```
