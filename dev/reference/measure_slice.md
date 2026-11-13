# Extract slices from n-dimensional measurement

Fixes one or more dimensions at specific coordinate values or ranges,
returning a lower-dimensional result.

## Usage

``` r
measure_slice(x, ..., drop = TRUE)
```

## Arguments

- x:

  A `measure_nd_tbl` or `measure_nd_list` object.

- ...:

  Named arguments specifying slice conditions. Names should be dimension
  numbers (e.g., `dim_1 = 5`) or dimension names if set (e.g.,
  `time = 5`). Values can be:

  - A single value: exact match

  - A numeric vector: match any of these values

  - A function: applied to coordinates, should return logical

- drop:

  One non-missing logical value. If `TRUE` (default), dimensions with a
  single observed coordinate are dropped from the result. If `FALSE`,
  all original dimensions are retained.

## Value

For one measurement, a numeric vector when all dimensions are dropped, a
`measure_tbl` when one dimension remains, or a `measure_nd_tbl` when two
or more dimensions remain. For a list input, homogeneous results are
returned as a numeric vector, `measure_list`, or `measure_nd_list`,
respectively.

## Details

Coordinate selections use exact numeric membership. A numeric selection
can include `NA` to select rows whose stored coordinate is unknown. A
predicate must return one non-missing logical value for every input row.
Empty slices warn and remain n-dimensional with the original axes
because no axis has a single observed coordinate. Slicing retains the
input point order. If list members would have different result
dimensionalities or retained axis schemas, an error recommends
`drop = FALSE` or mapping over members separately. The dropping rule
counts a stored `NA` coordinate as one distinct state; it does not infer
a physical coordinate for it. Repeated observations at one coordinate
remain a numeric vector for a single measurement. A list cannot flatten
such multi-value results into one scalar per member.

## Examples

``` r
# Create a 3D measurement (2 x 3 x 4)
m3d <- new_measure_nd_tbl(
  location_1 = rep(1:2, each = 12),
  location_2 = rep(rep(1:3, each = 4), 2),
  location_3 = rep(1:4, 6),
  value = 1:24,
  dim_names = c("sample", "time", "wavelength")
)

# Extract slice at sample = 1
slice_2d <- measure_slice(m3d, dim_1 = 1)
measure_ndim(slice_2d)  # 2D
#> [1] 2

# Extract at specific time points
slice_subset <- measure_slice(m3d, dim_2 = c(1, 3))

# Use dimension names
slice_wl <- measure_slice(m3d, wavelength = 2)
```
