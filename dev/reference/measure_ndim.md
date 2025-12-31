# Get the number of dimensions of a measurement

Returns the dimensionality of a measurement object. For 1D measurements
(`measure_tbl`), returns 1. For n-dimensional measurements
(`measure_nd_tbl`), returns the number of location dimensions.

## Usage

``` r
measure_ndim(x)
```

## Arguments

- x:

  A `measure_tbl`, `measure_nd_tbl`, `measure_list`, or
  `measure_nd_list` object.

## Value

Integer indicating the number of dimensions.

## Examples

``` r
# 1D measurement
m1d <- new_measure_tbl(location = 1:10, value = rnorm(10))
measure_ndim(m1d)  # 1
#> [1] 1

# 2D measurement
m2d <- new_measure_nd_tbl(
  location_1 = rep(1:5, each = 3),
  location_2 = rep(1:3, times = 5),
  value = rnorm(15)
)
measure_ndim(m2d)  # 2
#> [1] 2
```
