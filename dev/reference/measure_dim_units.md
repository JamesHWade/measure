# Get dimension units of an n-dimensional measurement

Returns the units for each dimension (e.g., "nm", "min").

## Usage

``` r
measure_dim_units(x)
```

## Arguments

- x:

  A `measure_nd_tbl` or `measure_nd_list` object.

## Value

Character vector of dimension units, or `NULL` if not set.

## Examples

``` r
m2d <- new_measure_nd_tbl(
  location_1 = 1:10,
  location_2 = rep(1:2, each = 5),
  value = rnorm(10),
  dim_units = c("min", "nm")
)
measure_dim_units(m2d)
#> [1] "min" "nm" 
```
