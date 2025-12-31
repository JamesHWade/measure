# Create a new n-dimensional measure list

Constructor for creating a collection of n-dimensional measurements
suitable for use as a list column in a data frame. Each element should
be a `measure_nd_tbl` or tibble with `location_*` and `value` columns.

## Usage

``` r
new_measure_nd_list(x = list())
```

## Arguments

- x:

  A list of `measure_nd_tbl` objects or tibbles with `location_*` and
  `value` columns.

## Value

A list with class `measure_nd_list`.

## See also

[`new_measure_nd_tbl()`](https://jameshwade.github.io/measure/dev/reference/new_measure_nd_tbl.md)
for creating individual nD measurements,
[`is_measure_nd_list()`](https://jameshwade.github.io/measure/dev/reference/is_measure_nd_list.md)
for checking object class.

## Examples

``` r
# Create individual 2D measurements
meas1 <- new_measure_nd_tbl(
  location_1 = rep(1:5, each = 3),
  location_2 = rep(1:3, times = 5),
  value = rnorm(15)
)
meas2 <- new_measure_nd_tbl(
  location_1 = rep(1:5, each = 3),
  location_2 = rep(1:3, times = 5),
  value = rnorm(15)
)

# Combine into a measure_nd_list
meas_list <- new_measure_nd_list(list(meas1, meas2))
meas_list
#> A measure_nd_list with 2 measurements (2D)
#> Sizes: 15, 15 (15-15 points)
```
