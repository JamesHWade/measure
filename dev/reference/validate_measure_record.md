# Validate a measurement record

Rechecks the full table schema, key and foreign-key constraints, curve
payloads, and source-row references. This is useful after editing a
record's mutable tibble components.

## Usage

``` r
validate_measure_record(x)
```

## Arguments

- x:

  An object created by
  [`measure_record()`](https://jameshwade.github.io/measure/dev/reference/measure_record.md).

## Value

`x`, invisibly, if valid. Invalid records raise a cli error.

## Examples

``` r
record <- measure_record()
validate_measure_record(record)
```
