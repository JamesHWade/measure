# Project n-dimensional measurement by aggregating across dimensions

Reduces dimensionality by applying an aggregation function across one or
more dimensions.

## Usage

``` r
measure_project(x, along, fn = mean, na_rm = TRUE, ...)
```

## Arguments

- x:

  A `measure_nd_tbl` or `measure_nd_list` object.

- along:

  A nonempty vector of unique whole-number dimension indices or known
  semantic dimension names to aggregate across.

- fn:

  Function that returns one plain numeric scalar for each group. Default
  is `mean`.

- na_rm:

  One non-missing logical value. Remove `NA` values before aggregation?
  Default `TRUE`.

- ...:

  Additional arguments passed to `fn`.

## Value

A numeric scalar when all dimensions are projected, a `measure_tbl` when
one dimension remains, or a `measure_nd_tbl` when two or more dimensions
remain. For a list input, homogeneous results are returned as a numeric
vector, `measure_list`, or `measure_nd_list`, respectively.

## Details

Projection groups rows by the retained coordinate columns. `dplyr` sorts
these groups in ascending coordinate order (with missing coordinates
last), so the result order describes coordinate groups rather than
acquisition timing. With `na_rm = TRUE`, `NA` values are removed before
`fn` is called. An all-missing group therefore calls `fn(numeric(0))`;
for example, `mean` returns `NaN` and `sum` returns `0`. An empty input
with retained axes yields an empty measurement; projecting all axes
applies `fn` to `numeric(0)`. List members must retain the same axis
positions, names, and units or projection errors and recommends mapping
over members separately.

## Examples

``` r
# Create 2D measurement (time x wavelength)
m2d <- new_measure_nd_tbl(
  location_1 = rep(1:5, each = 3),
  location_2 = rep(c(254, 280, 320), times = 5),
  value = rnorm(15, mean = 100),
  dim_names = c("time", "wavelength")
)

# Project across wavelength (average spectrum at each time)
time_trace <- measure_project(m2d, along = 2)

# Project across time (average time profile at each wavelength)
wavelength_profile <- measure_project(m2d, along = 1)

# Use sum instead of mean
total <- measure_project(m2d, along = 2, fn = sum)
```
