# Get validation section data

Get validation section data

## Usage

``` r
get_validation_section(report, section)
```

## Arguments

- report:

  A `measure_validation_report` object.

- section:

  Section name to retrieve.

## Value

The section data, or NULL if not found.

## Examples

``` r
report <- measure_validation_report(title = "Test Report")
get_validation_section(report, "calibration")  # NULL
#> NULL
```
