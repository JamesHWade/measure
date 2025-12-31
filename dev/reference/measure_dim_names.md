# Get dimension names of an n-dimensional measurement

Returns the semantic names for each dimension (e.g., "wavelength",
"retention_time").

## Usage

``` r
measure_dim_names(x)
```

## Arguments

- x:

  A `measure_nd_tbl` or `measure_nd_list` object.

## Value

Character vector of dimension names, or `NULL` if not set.

## Examples

``` r
m2d <- new_measure_nd_tbl(
  location_1 = 1:10,
  location_2 = rep(1:2, each = 5),
  value = rnorm(10),
  dim_names = c("retention_time", "wavelength")
)
measure_dim_names(m2d)
#> [1] "retention_time" "wavelength"    
```
