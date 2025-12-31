# Get grid information for an n-dimensional measurement

Returns detailed information about the coordinate grid, including unique
values per dimension, grid shape, and regularity status.

## Usage

``` r
measure_grid_info(x)
```

## Arguments

- x:

  A `measure_nd_tbl` object.

## Value

A list with components:

- `ndim`: Number of dimensions

- `dim_names`: Semantic dimension names (if set)

- `dim_units`: Dimension units (if set)

- `unique_values`: List of unique coordinate values per dimension

- `shape`: Integer vector of unique value counts per dimension

- `n_points`: Total number of data points

- `is_regular`: Whether the grid is regular

- `has_na`: Whether any values are NA

## Examples

``` r
m2d <- new_measure_nd_tbl(
  location_1 = rep(seq(0, 10, by = 2), each = 4),
  location_2 = rep(c(254, 280, 320, 350), times = 6),
  value = rnorm(24),
  dim_names = c("time", "wavelength"),
  dim_units = c("min", "nm")
)
measure_grid_info(m2d)
#> $ndim
#> [1] 2
#> 
#> $dim_names
#> [1] "time"       "wavelength"
#> 
#> $dim_units
#> [1] "min" "nm" 
#> 
#> $unique_values
#> $unique_values$dim_1
#> [1]  0  2  4  6  8 10
#> 
#> $unique_values$dim_2
#> [1] 254 280 320 350
#> 
#> 
#> $shape
#> dim_1 dim_2 
#>     6     4 
#> 
#> $n_points
#> [1] 24
#> 
#> $is_regular
#> [1] TRUE
#> 
#> $has_na
#> [1] FALSE
#> 
```
