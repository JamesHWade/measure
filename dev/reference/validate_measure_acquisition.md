# Validate a measurement acquisition

Rechecks the canonical tables, embedded record, payloads, masks, and
foreign-key relationships after edits to the mutable object.

## Usage

``` r
validate_measure_acquisition(x)
```

## Arguments

- x:

  An object created by
  [`measure_acquisition()`](https://jameshwade.github.io/measure/dev/reference/measure_acquisition.md).

## Value

`x`, invisibly, when valid. Invalid acquisitions raise an error.
