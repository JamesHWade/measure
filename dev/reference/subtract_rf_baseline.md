# Subtract baseline using robust fitting method

A standalone function for robust fitting baseline subtraction using
local regression with iterative reweighting. For use within a recipe
workflow, see
[`step_measure_baseline_rf()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_rf.md).

## Usage

``` r
subtract_rf_baseline(data, yvar, span = 2/3, maxit = c(5, 5))
```

## Arguments

- data:

  A dataframe containing the variable for baseline subtraction

- yvar:

  The name of the column for baseline subtraction

- span:

  Controls the amount of smoothing based on the fraction of data to use
  in computing each fitted value, defaults to `2/3`.

- maxit:

  The number of iterations to use the robust fit, defaults to `c(5, 5)`
  where the first value specifies iterations for asymmetric weighting
  function and the second value for symmetric weighting function.

## Value

A dataframe matching column in data plus `raw` and `baseline` columns

## See also

[`step_measure_baseline_rf()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_rf.md)
for the recipe step version.

## Examples

``` r
library(dplyr)
meats_long |>
  group_by(id) |>
  subtract_rf_baseline(yvar = transmittance)
#> # A tibble: 21,500 × 8
#> # Groups:   id [215]
#>       id water   fat protein channel transmittance   raw baseline
#>    <int> <dbl> <dbl>   <dbl>   <int>         <dbl> <dbl>    <dbl>
#>  1     1  60.5  22.5    16.7       1       0.0668   2.62     2.55
#>  2     1  60.5  22.5    16.7       2       0.0598   2.62     2.56
#>  3     1  60.5  22.5    16.7       3       0.0527   2.62     2.57
#>  4     1  60.5  22.5    16.7       4       0.0458   2.62     2.57
#>  5     1  60.5  22.5    16.7       5       0.0389   2.62     2.58
#>  6     1  60.5  22.5    16.7       6       0.0321   2.62     2.59
#>  7     1  60.5  22.5    16.7       7       0.0256   2.62     2.60
#>  8     1  60.5  22.5    16.7       8       0.0193   2.62     2.60
#>  9     1  60.5  22.5    16.7       9       0.0133   2.63     2.61
#> 10     1  60.5  22.5    16.7      10       0.00753  2.63     2.62
#> # ℹ 21,490 more rows
```
