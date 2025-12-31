# Tidy a Validation Report

Extracts key parameters and statistics from all validation sections into
a tidy tibble format suitable for further analysis or reporting.

## Usage

``` r
# S3 method for class 'measure_validation_report'
tidy(x, ...)
```

## Arguments

- x:

  A `measure_validation_report` object.

- ...:

  Additional arguments (currently ignored).

## Value

A tibble with columns:

- `section`: Section name

- `parameter`: Parameter name

- `value`: Parameter value

- `unit`: Unit of measurement (if available)

- `status`: Pass/fail status (if available)

Returns an empty tibble if no sections contain tidy-able data.

## Examples

``` r
# Create sample data
blank_data <- data.frame(
  response = rnorm(10, mean = 50, sd = 15),
  sample_type = "blank"
)
lod_result <- measure_lod(blank_data, response_col = "response")

report <- measure_validation_report(
  title = "Test Report",
  lod_loq = lod_result
)
tidy(report)
#> # A tibble: 1 × 6
#>   section limit_type value method       k uncertainty
#>   <chr>   <chr>      <dbl> <chr>    <dbl>       <dbl>
#> 1 LOD/LOQ LOD         81.3 blank_sd     3        10.2
```
