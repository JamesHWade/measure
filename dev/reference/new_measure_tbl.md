# Create a new measure tibble

Constructor for creating a single measurement object containing location
(e.g., wavelength, retention time) and value pairs.

## Usage

``` r
new_measure_tbl(location = double(), value = double())
```

## Arguments

- location:

  Numeric vector of measurement locations (e.g., wavelengths,
  wavenumbers, retention times).

- value:

  Numeric vector of measurement values (e.g., absorbance, intensity,
  signal).

## Value

A tibble with class `measure_tbl` containing `location` and `value`
columns.

## See also

[`new_measure_list()`](https://jameshwade.github.io/measure/dev/reference/new_measure_list.md)
for creating collections of measurements,
[`is_measure_tbl()`](https://jameshwade.github.io/measure/dev/reference/is_measure_tbl.md)
for checking object class.

## Examples

``` r
# Create a simple spectrum
spec <- new_measure_tbl(
  location = seq(1000, 1100, by = 10),
  value = sin(seq(1000, 1100, by = 10) / 50)
)
spec
#> <measure_tbl [11 x 2]>
#> # A tibble: 11 × 2
#>    location    value
#>       <dbl>    <dbl>
#>  1     1000  0.913  
#>  2     1010  0.976  
#>  3     1020  1.000  
#>  4     1030  0.984  
#>  5     1040  0.929  
#>  6     1050  0.837  
#>  7     1060  0.711  
#>  8     1070  0.557  
#>  9     1080  0.381  
#> 10     1090  0.190  
#> 11     1100 -0.00885
```
