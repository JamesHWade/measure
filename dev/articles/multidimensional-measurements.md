# Working with multidimensional measurements

``` r

library(measure)
library(tibble)
```

## Paired DAD data on different grids

An LC-DAD result can be represented with retention time and wavelength
as two location dimensions. These small measurements deliberately use
different time and wavelength grids and have different point counts.
Their shared dimension names describe the axes; the coordinates remain
specific to each measurement.

``` r

dad_a <- new_measure_nd_tbl(
  location_1 = c(1.75, 0, 3, 0.5, 0, 3, 0.5, 1.75),
  location_2 = c(254, 210, 254, 210, 254, 210, 254, 210),
  value = c(1.25, 0.125, 0.375, 0.625, 0.25, 0.5, 0.875, NA_real_),
  dim_names = c("retention_time", "wavelength"),
  dim_units = c("min", "nm")
)

dad_b <- new_measure_nd_tbl(
  location_1 = c(1, 0.125, 4, 2.5, 0.125, 1),
  location_2 = c(280, 235, 280, 235, 280, 235),
  value = c(12.5, 10, 14.5, 11.25, 10.25, 11.5),
  dim_names = c("retention_time", "wavelength"),
  dim_units = c("min", "nm")
)

dad_pair <- new_measure_nd_list(list(dad_a, dad_b))
c(nrow(dad_pair[[1]]), nrow(dad_pair[[2]]))
#> [1] 8 6
```

The pair can share an nD list while preserving each member’s grid and
metadata. The list does not assert that the two measurements have
identical coordinates.

## Unfold and restore each member

Unfolding creates one 1D payload per nD member and retains that member’s
fold metadata. Folding uses the saved coordinate mapping. The result is
in canonical coordinate order; it does not restore the acquisition row
order from the shuffled input.

``` r

unfolded_pair <- measure_unfold(dad_pair, order = c(1, 2))
restored_pair <- measure_fold(unfolded_pair)

canonical_cells <- function(x) {
  index <- order(x$location_1, x$location_2)
  data.frame(
    location_1 = x$location_1[index],
    location_2 = x$location_2[index],
    value = x$value[index]
  )
}

restored_exactly <- vapply(seq_along(dad_pair), function(i) {
  identical(
    canonical_cells(dad_pair[[i]]),
    canonical_cells(restored_pair[[i]])
  )
}, logical(1))
restored_exactly
#> [1] TRUE TRUE
stopifnot(all(restored_exactly))
```

For heterogeneous grids, keep each element’s own unfold metadata
attached. Do not copy one member’s fold metadata across the list. A
missing member mapping or invalid point index is an error, because the
coordinates could no longer be tied to the edited 1D values safely.

To edit an unfolded collection, edit its extracted members and rebuild
the list with
[`new_measure_list()`](https://jameshwade.github.io/measure/dev/reference/new_measure_list.md).
Assignment through a list-column prototype can remove non-prototype
attributes, including the fold mapping. The next block makes a
deliberate signal edit and verifies the saved point association.

``` r

edited_members <- lapply(unfolded_pair, function(member) {
  member$value <- 2 * member$value
  member
})
edited_pair <- measure_fold(new_measure_list(edited_members))
stopifnot(all(vapply(seq_along(edited_pair), function(i) {
  identical(edited_pair[[i]]$value, 2 * restored_pair[[i]]$value)
}, logical(1))))
```

## Slice and project without changing the measurement question

Use one named dimension selector at a time, or unique dimension numbers,
to make the chosen axis clear. A slice preserves the selected point
order. A predicate must return one complete logical decision per point.

``` r

trace_254 <- measure_slice(
  dad_a,
  wavelength = 254,
  drop = TRUE
)
trace_254
#> <measure_tbl [4 x 2]>
#> # A tibble: 4 × 2
#>   location value
#>      <dbl> <dbl>
#> 1     1.75 1.25 
#> 2     3    0.375
#> 3     0    0.25 
#> 4     0.5  0.875

time_window <- measure_slice(
  dad_a,
  retention_time = function(time) time >= 0.25 & time <= 2,
  drop = FALSE
)
time_window
#> <measure_nd_tbl [4 x 3] retention_time x wavelength>
#> <measure_tbl [4 x 3]>
#> # A tibble: 4 × 3
#>   location_1 location_2  value
#>        <dbl>      <dbl>  <dbl>
#> 1       1.75        254  1.25 
#> 2       0.5         210  0.625
#> 3       0.5         254  0.875
#> 4       1.75        210 NA
```

Projection groups points by the dimensions it keeps and applies the
requested summary to observed values in each group. The next table
compares two summaries across the wavelength points recorded at each
retention time.

``` r

total_by_time <- measure_project(
  dad_a,
  along = 2L,
  fn = sum,
  na_rm = TRUE
)
mean_by_time <- measure_project(
  dad_a,
  along = 2L,
  fn = mean,
  na_rm = TRUE
)

stopifnot(
  identical(total_by_time$location, c(0, 0.5, 1.75, 3)),
  identical(total_by_time$value, c(0.375, 1.5, 1.25, 0.875)),
  identical(mean_by_time$value, c(0.1875, 0.75, 1.25, 0.4375))
)

tibble(
  retention_time = total_by_time$location,
  sum_of_observed_values = total_by_time$value,
  mean_of_observed_values = mean_by_time$value
)
#> # A tibble: 4 × 3
#>   retention_time sum_of_observed_values mean_of_observed_values
#>            <dbl>                  <dbl>                   <dbl>
#> 1           0                     0.375                   0.188
#> 2           0.5                   1.5                     0.75 
#> 3           1.75                  1.25                    1.25 
#> 4           3                     0.875                   0.438
```

These are pointwise summaries. A sum does not integrate absorbance over
wavelength: it does not account for wavelength spacing or interpolate
missing bands. A mean and a sum answer different questions, and either
can be affected by how many wavelengths were measured. Choose a domain
method before comparing measurements whose grids differ.

## Make a scalar record from an extracted trace

The current measure_record schema stores one-dimensional curves. Here
the caller selects the 254 nm trace, orders its retention-time points
for the record’s monotonic-axis requirement, and supplies the sample,
injection, channel, axis, and signal labels explicitly.

``` r

trace_order <- order(trace_254$location)
record_trace <- new_measure_tbl(
  location = trace_254$location[trace_order],
  value = trace_254$value[trace_order]
)

record <- measure_record(
  samples = tibble(sample_id = "sample-001"),
  injections = tibble(
    injection_id = "injection-001",
    sample_id = "sample-001"
  ),
  channels = tibble(
    channel_id = "dad-254",
    channel_name = "DAD trace selected at 254 nm"
  ),
  curves = tibble(
    curve_id = "curve-001",
    injection_id = "injection-001",
    channel_id = "dad-254",
    axis_name = "retention time",
    axis_unit = "min",
    value_name = "absorbance",
    value_unit = "AU",
    .measures = list(record_trace)
  )
)

validate_measure_record(record)
record$curves$.measures[[1]]
#> <measure_tbl [4 x 2]>
#> # A tibble: 4 × 2
#>   location value
#>      <dbl> <dbl>
#> 1     0    0.25 
#> 2     0.5  0.875
#> 3     1.75 1.25 
#> 4     3    0.375
```

The record contains this extracted 1D trace; it does not contain dad_a
or dad_pair. A version 1 native record bundle likewise saves the
extracted curve payload. Keep the original nD measurement separately
when it is needed. The bundle does not establish raw-row lineage, an nD
interchange format, or instrument provenance.

``` r

bundle <- tempfile("measure-dad-record-")
write_measure_record(record, bundle)
saved_record <- read_measure_record(bundle)
stopifnot(identical(record, saved_record))
nrow(saved_record$curves$.measures[[1]])
#> [1] 4
unlink(bundle, recursive = TRUE)
```
