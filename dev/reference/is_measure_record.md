# Test whether an object is a measurement record

Test whether an object is a measurement record

## Usage

``` r
is_measure_record(x)
```

## Arguments

- x:

  Object to test.

## Value

`TRUE` if `x` inherits from `measure_record`; otherwise `FALSE`. This is
a class check and does not validate mutable table contents.

## Examples

``` r
is_measure_record(measure_record())
#> [1] TRUE
is_measure_record(list())
#> [1] FALSE
```
