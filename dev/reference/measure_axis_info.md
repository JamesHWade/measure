# Get axis information from measure data

Extracts metadata about the axis (location dimension) of measure data,
including range, spacing, direction, and inferred axis type.

## Usage

``` r
measure_axis_info(x, sample = 1L)
```

## Arguments

- x:

  A `measure_tbl`, `measure_list`, or data frame with measure column.

- sample:

  Integer index of sample to analyze (for `measure_list`). Default is 1.

## Value

A list with:

- `min`, `max`: Range of location values

- `n_points`: Number of data points

- `spacing`: Median absolute spacing between points

- `direction`: "increasing", "decreasing", or "mixed"

- `regular`: Logical indicating if spacing is regular (within tolerance)

- `axis_type`: Inferred axis type (see
  [`infer_axis_type()`](https://jameshwade.github.io/measure/dev/reference/infer_axis_type.md))

## Examples

``` r
# NIR spectrum
spec <- new_measure_tbl(
  location = seq(1000, 2500, by = 2),
  value = rnorm(751)
)
measure_axis_info(spec)
#> $min
#> [1] 1000
#> 
#> $max
#> [1] 2500
#> 
#> $n_points
#> [1] 751
#> 
#> $spacing
#> [1] 2
#> 
#> $direction
#> [1] "increasing"
#> 
#> $regular
#> [1] TRUE
#> 
#> $axis_type
#> [1] "wavelength_nm"
#> 

# Chromatogram
chrom <- new_measure_tbl(
  location = seq(0, 30, by = 0.01),
  value = rnorm(3001)
)
measure_axis_info(chrom)
#> $min
#> [1] 0
#> 
#> $max
#> [1] 30
#> 
#> $n_points
#> [1] 3001
#> 
#> $spacing
#> [1] 0.01
#> 
#> $direction
#> [1] "increasing"
#> 
#> $regular
#> [1] TRUE
#> 
#> $axis_type
#> [1] "retention_time"
#> 
```
