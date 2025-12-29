# Check axis consistency across samples

Validates that all samples in a `measure_list` have consistent axes
(same locations). This is important for matrix operations that assume
aligned data.

## Usage

``` r
check_axis_consistency(
  x,
  tolerance = 1e-10,
  action = c("error", "warn", "message")
)
```

## Arguments

- x:

  A `measure_list` or data frame with measure column.

- tolerance:

  Numeric tolerance for location comparison. Default is 1e-10.

- action:

  What to do when validation fails: `"error"` (default), `"warn"`, or
  `"message"`.

## Value

Invisibly returns a list with:

- `consistent`: Logical indicating if axes are consistent

- `reference_locations`: The reference locations (from first sample)

- `inconsistent_samples`: Indices of samples with different axes

- `max_deviation`: Maximum deviation from reference locations

## Examples

``` r
# Consistent axes
specs <- new_measure_list(list(
  new_measure_tbl(location = 1:10, value = rnorm(10)),
  new_measure_tbl(location = 1:10, value = rnorm(10))
))
check_axis_consistency(specs)

# Inconsistent axes
specs_bad <- new_measure_list(list(
  new_measure_tbl(location = 1:10, value = rnorm(10)),
  new_measure_tbl(location = 1:11, value = rnorm(11))
))
try(check_axis_consistency(specs_bad))
#> Error in check_axis_consistency(specs_bad) : 
#>   1 of 2 samples have inconsistent axes
#> ℹ Use `step_measure_resample()` or `step_measure_interpolate()` to align axes.
```
