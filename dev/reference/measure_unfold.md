# Unfold n-dimensional measurement to 1D

Converts an n-dimensional measurement to a 1D vector by flattening
according to a specified dimension order. Stores metadata needed to
reconstruct the original nD structure via
[`measure_fold()`](https://jameshwade.github.io/measure/dev/reference/measure_fold.md).

## Usage

``` r
measure_unfold(x, order = NULL)
```

## Arguments

- x:

  A `measure_nd_tbl` or `measure_nd_list` object.

- order:

  Integer vector specifying the order of dimensions for unfolding.
  Default is `NULL`, which uses the natural order (1, 2, ..., n). The
  first dimension varies fastest.

## Value

A `measure_tbl` or `measure_list` with an attribute `"fold_info"`
containing the metadata needed to reconstruct the nD structure.

## Details

Unfolding is useful for:

- Applying 1D modeling techniques (PCA, PLS) to nD data

- Exporting to formats that expect 1D vectors

- Visualization as a single trace

The fold metadata includes:

- `ndim`: Original number of dimensions

- `dim_names`, `dim_units`: Original dimension metadata

- `coordinates`: The original coordinate values for each dimension

- `order`: The unfolding order used

## See also

[`measure_fold()`](https://jameshwade.github.io/measure/dev/reference/measure_fold.md)
to reconstruct the nD structure

## Examples

``` r
# Create a 2D measurement (3 x 4 grid)
m2d <- new_measure_nd_tbl(
  location_1 = rep(1:3, each = 4),
  location_2 = rep(1:4, times = 3),
  value = 1:12,
  dim_names = c("time", "wavelength")
)

# Unfold to 1D
m1d <- measure_unfold(m2d)
m1d
#> <measure_tbl [12 x 2]>
#> # A tibble: 12 × 2
#>    location value
#>       <int> <int>
#>  1        1     1
#>  2        2     5
#>  3        3     9
#>  4        4     2
#>  5        5     6
#>  6        6    10
#>  7        7     3
#>  8        8     7
#>  9        9    11
#> 10       10     4
#> 11       11     8
#> 12       12    12

# Reconstruct
m2d_restored <- measure_fold(m1d)
```
