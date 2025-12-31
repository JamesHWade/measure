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

  Integer or character specifying which dimension(s) to aggregate
  across. Can use dimension numbers or names.

- fn:

  Aggregation function. Default is `mean`.

- na_rm:

  Logical. Remove NA values before aggregation? Default `TRUE`.

- ...:

  Additional arguments passed to `fn`.

## Value

A `measure_tbl`, `measure_nd_tbl`, `measure_list`, or `measure_nd_list`
with reduced dimensionality.

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
