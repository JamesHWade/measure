# Check if validation report has a section

Check if validation report has a section

## Usage

``` r
has_validation_section(report, section)
```

## Arguments

- report:

  A `measure_validation_report` object.

- section:

  Section name to check.

## Value

Logical indicating if section exists and has data.

## Examples

``` r
report <- measure_validation_report(title = "Test Report")
has_validation_section(report, "calibration")  # FALSE
#> [1] FALSE
```
