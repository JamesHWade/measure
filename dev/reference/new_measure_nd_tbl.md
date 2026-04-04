# Create a new n-dimensional measure tibble

Constructor for creating a single n-dimensional measurement object
containing location coordinates (e.g., wavelength, retention time) and
values.

## Usage

``` r
new_measure_nd_tbl(..., value = double(), dim_names = NULL, dim_units = NULL)
```

## Arguments

- ...:

  Named location vectors. Names should follow the pattern `location_1`,
  `location_2`, etc. Each must be a numeric vector of the same length.

- value:

  Numeric vector of measurement values (e.g., absorbance, intensity,
  signal). Must have the same length as location vectors.

- dim_names:

  Optional character vector of semantic dimension names (e.g.,
  `c("wavelength", "retention_time")`).

- dim_units:

  Optional character vector of dimension units (e.g., `c("nm", "min")`).

## Value

A tibble with class `measure_nd_tbl` containing `location_1`,
`location_2`, ..., `location_n`, and `value` columns. Attributes include
`ndim`, `dim_names`, `dim_units`, and `dim_order`.

## See also

[`new_measure_nd_list()`](https://jameshwade.github.io/measure/dev/reference/new_measure_nd_list.md)
for creating collections of nD measurements,
[`is_measure_nd_tbl()`](https://jameshwade.github.io/measure/dev/reference/is_measure_nd_tbl.md)
for checking object class,
[`measure_ndim()`](https://jameshwade.github.io/measure/dev/reference/measure_ndim.md)
for getting dimensionality.

## Examples

``` r
# Create a 2D measurement (e.g., LC-UV: retention time x wavelength)
meas_2d <- new_measure_nd_tbl(
  location_1 = rep(seq(0, 10, length.out = 5), each = 3),
  location_2 = rep(c(254, 280, 320), times = 5),
  value = rnorm(15),
  dim_names = c("retention_time", "wavelength"),
  dim_units = c("min", "nm")
)
meas_2d
#> <measure_nd_tbl [15 x 3] retention_time x wavelength>
#> <measure_tbl [15 x 3]>
#> # A tibble: 15 × 3
#>    location_1 location_2   value
#>         <dbl>      <dbl>   <dbl>
#>  1        0          254  2.19  
#>  2        0          280  1.53  
#>  3        0          320 -0.236 
#>  4        2.5        254 -1.03  
#>  5        2.5        280 -0.710 
#>  6        2.5        320  0.257 
#>  7        5          254 -0.247 
#>  8        5          280 -0.348 
#>  9        5          320 -0.952 
#> 10        7.5        254 -0.0450
#> 11        7.5        280 -0.785 
#> 12        7.5        320 -1.67  
#> 13       10          254 -0.380 
#> 14       10          280  0.919 
#> 15       10          320 -0.575 
```
