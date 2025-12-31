# Check if an n-dimensional measurement has a regular grid

A regular grid means all combinations of unique coordinate values exist
exactly once (i.e., it forms a complete rectangular grid).

## Usage

``` r
measure_is_regular(x)
```

## Arguments

- x:

  A `measure_nd_tbl` object.

## Value

Logical indicating if the measurement has a regular grid.

## Examples

``` r
# Regular grid (all combinations present)
regular <- new_measure_nd_tbl(
  location_1 = rep(1:3, each = 2),
  location_2 = rep(1:2, times = 3),
  value = rnorm(6)
)
measure_is_regular(regular)  # TRUE
#> [1] TRUE

# Irregular grid (missing combinations)
irregular <- new_measure_nd_tbl(
  location_1 = c(1, 1, 2, 3),
  location_2 = c(1, 2, 1, 2),
  value = rnorm(4)
)
measure_is_regular(irregular)  # FALSE
#> [1] FALSE
```
