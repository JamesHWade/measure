# Fold 1D measurement back to n-dimensional

Reconstructs an n-dimensional measurement from a 1D vector that was
created by
[`measure_unfold()`](https://jameshwade.github.io/measure/dev/reference/measure_unfold.md).
Requires the fold metadata attribute.

## Usage

``` r
measure_fold(x)
```

## Arguments

- x:

  A `measure_tbl` or `measure_list` with `"fold_info"` attribute.

## Value

A `measure_nd_tbl` or `measure_nd_list` with the original dimensional
structure restored.

## See also

[`measure_unfold()`](https://jameshwade.github.io/measure/dev/reference/measure_unfold.md)
to create foldable 1D data

## Examples

``` r
# Create, unfold, then fold back
m2d <- new_measure_nd_tbl(
  location_1 = rep(1:3, each = 4),
  location_2 = rep(1:4, times = 3),
  value = 1:12
)

m1d <- measure_unfold(m2d)
m2d_restored <- measure_fold(m1d)

# Values are preserved
all.equal(m2d$value, m2d_restored$value)
#> [1] "Mean relative difference: 0.4923077"
```
