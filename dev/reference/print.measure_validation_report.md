# Print a Validation Report

Displays a formatted summary of a validation report object, including
metadata, section status, conclusions, and provenance information.

## Usage

``` r
# S3 method for class 'measure_validation_report'
print(x, ...)
```

## Arguments

- x:

  A `measure_validation_report` object.

- ...:

  Additional arguments (currently ignored).

## Value

Invisibly returns the input object.

## Examples

``` r
report <- measure_validation_report(
  title = "Test Report",
  method_name = "HPLC Assay",
  analyst = "J. Smith"
)
print(report)
#> 
#> ── Validation Report ───────────────────────────────────────────────────────────
#> Title: Test Report
#> Method: HPLC Assay
#> Analyst: J. Smith
#> Date: 2026-02-07
#> 
#> 
#> ── Provenance ──
#> 
#> Generated: 2026-02-07 01:54:44.245177
#> R version: 4.5.2
#> measure version: 0.0.1.9002
#> 
#> ℹ Use `render_validation_report()` to generate document
```
