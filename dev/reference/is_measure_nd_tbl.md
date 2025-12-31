# Test if object is an n-dimensional measure tibble

Test if object is an n-dimensional measure tibble

## Usage

``` r
is_measure_nd_tbl(x)
```

## Arguments

- x:

  Object to test.

## Value

Logical indicating if `x` inherits from `measure_nd_tbl`.

## Examples

``` r
# Create a 2D measure tibble
mt <- new_measure_nd_tbl(
  location_1 = 1:10,
  location_2 = rep(1:2, each = 5),
  value = rnorm(10)
)
is_measure_nd_tbl(mt)  # TRUE
#> [1] TRUE

# Regular tibbles are not measure_nd_tbl
is_measure_nd_tbl(tibble::tibble(x = 1:5))  # FALSE
#> [1] FALSE
```
