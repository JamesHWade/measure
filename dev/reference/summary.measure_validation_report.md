# Summarize a Validation Report

Creates a summary table of all validation sections in the report,
showing section status, result counts, and notes.

## Usage

``` r
# S3 method for class 'measure_validation_report'
summary(object, ...)
```

## Arguments

- object:

  A `measure_validation_report` object.

- ...:

  Additional arguments (currently ignored).

## Value

A tibble with columns:

- `section`: Section name

- `status`: Pass/fail/info status

- `n_results`: Number of results in section

- `notes`: Additional notes

Returns `NULL` invisibly if the report has no validation sections.

## Examples

``` r
# Create a report with some sections
report <- measure_validation_report(
  title = "Test Report",
  specificity = "No interference observed"
)
summary(report)
#> 
#> ── Validation Report Summary ───────────────────────────────────────────────────
#> Method: Not specified
#> Date: 2026-01-01
#> 
#> # A tibble: 1 × 4
#>   section                 status n_results notes
#>   <chr>                   <chr>      <int> <chr>
#> 1 Specificity/Selectivity info          NA ""   
#> 
#> ✔ All sections meet acceptance criteria
```
