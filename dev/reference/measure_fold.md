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

## Details

Each 1D payload must retain its own `fold_info` mapping. When any member
of a collection has this metadata, every member must have its own valid
mapping. New list unfolds never attach the first member's grid to the
collection. For legacy objects with no member mappings, a list-level
descriptor is accepted only as a caller's explicit shared-grid
declaration.

The 1D `location` column contains point indices, not physical
coordinates. It must contain each integer from 1 to the saved point
count exactly once. Reordering rows is supported: values are matched
back by point index. Signal values may be edited, but deleting points,
duplicating indices, or losing part of the mapping is an error. Folding
restores canonical unfold order, rather than the original acquisition
row order. To edit a collection while retaining its mappings, transform
the extracted members with
[`lapply()`](https://rdrr.io/r/base/lapply.html) and rebuild with
[`new_measure_list()`](https://jameshwade.github.io/measure/dev/reference/new_measure_list.md).
List-column assignment or other operations that remove attributes can
lose a mapping.

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
