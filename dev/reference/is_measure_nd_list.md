# Test if object is an n-dimensional measure list

Test if object is an n-dimensional measure list

## Usage

``` r
is_measure_nd_list(x)
```

## Arguments

- x:

  Object to test.

## Value

Logical indicating if `x` inherits from `measure_nd_list`.

## Examples

``` r
# Create and test a measure_nd_list
meas1 <- new_measure_nd_tbl(
  location_1 = 1:5,
  location_2 = rep(1, 5),
  value = rnorm(5)
)
ml <- new_measure_nd_list(list(meas1))
is_measure_nd_list(ml)  # TRUE
#> [1] TRUE
```
