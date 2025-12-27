# Subtract baseline using robust fitting method

Subtract baseline using robust fitting method

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

## Examples

``` r
meats_long %>% subtract_rf_baseline(yvar = transmittance)
#> # A tibble: 21,500 × 8
#>       id water   fat protein channel transmittance   raw baseline
#>    <int> <dbl> <dbl>   <dbl>   <int>         <dbl> <dbl>    <dbl>
#>  1     1  60.5  22.5    16.7       1        -0.532  2.62     3.15
#>  2     1  60.5  22.5    16.7       2        -0.532  2.62     3.15
#>  3     1  60.5  22.5    16.7       3        -0.531  2.62     3.15
#>  4     1  60.5  22.5    16.7       4        -0.531  2.62     3.15
#>  5     1  60.5  22.5    16.7       5        -0.530  2.62     3.15
#>  6     1  60.5  22.5    16.7       6        -0.529  2.62     3.15
#>  7     1  60.5  22.5    16.7       7        -0.528  2.62     3.15
#>  8     1  60.5  22.5    16.7       8        -0.527  2.62     3.15
#>  9     1  60.5  22.5    16.7       9        -0.525  2.63     3.15
#> 10     1  60.5  22.5    16.7      10        -0.523  2.63     3.15
#> # ℹ 21,490 more rows
```
