# Common column naming patterns for analytical data

Named list of regex patterns for detecting measurement column types.
Used by
[`measure_identify_columns()`](https://jameshwade.github.io/measure/dev/reference/measure_identify_columns.md)
for auto-detection. Users can extend or modify these patterns and pass
them to detection functions.

## Usage

``` r
measure_column_patterns
```

## Format

Named list with regex patterns:

- wavenumber:

  `wn_` prefix for IR wavenumber (cm^-1)

- wavelength:

  `nm_` prefix for wavelength (nm)

- retention_time:

  `rt_` prefix for chromatography retention time

- mz:

  `mz_` prefix for mass-to-charge ratio (MS)

- ppm:

  `ppm_` prefix for NMR chemical shift

- channel:

  `ch_` prefix for numbered channels

- generic:

  `x_` prefix for generic/unknown axis

## Examples

``` r
# View default patterns
measure_column_patterns
#> $wavenumber
#> [1] "^wn_"
#> 
#> $wavelength
#> [1] "^nm_"
#> 
#> $retention_time
#> [1] "^rt_"
#> 
#> $mz
#> [1] "^mz_"
#> 
#> $ppm
#> [1] "^ppm_"
#> 
#> $channel
#> [1] "^ch_"
#> 
#> $generic
#> [1] "^x_"
#> 

# Create custom patterns
my_patterns <- c(measure_column_patterns, list(custom = "^my_prefix_"))
```
