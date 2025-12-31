# Add or update a validation section

Add or update a validation section

## Usage

``` r
add_validation_section(report, section, data)
```

## Arguments

- report:

  A `measure_validation_report` object.

- section:

  Section name.

- data:

  Section data to add.

## Value

Updated `measure_validation_report` object.

## Examples

``` r
report <- measure_validation_report(title = "Test Report")
# Add custom section
report <- add_validation_section(
  report,
  "custom_study",
  list(results = data.frame(x = 1:3, y = 4:6))
)
```
