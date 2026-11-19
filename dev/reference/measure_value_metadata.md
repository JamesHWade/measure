# Get measurement value metadata

Read the declaration for the values in a measurement column. The
declaration describes the one homogeneous `measure_list` column, not the
individual observations inside each `measure_tbl` member.

## Usage

``` r
measure_value_metadata(x)
```

## Arguments

- x:

  A `measure_list` column.

## Value

`NULL` when `x` is not a `measure_list` or has no declaration; otherwise
a named list with `quantity` and `unit` elements. `unit` may be `NULL`
when it was not declared. A malformed declaration errors rather than
being treated as valid metadata.

## Examples

``` r
measures <- new_measure_list(list(
  new_measure_tbl(1:3, c(1, 2, 3)),
  new_measure_tbl(1:3, c(2, 3, 4))
))
measure_value_metadata(measures)
#> NULL
measures <- set_measure_value_metadata(
  measures,
  quantity = "absorbance",
  unit = "AU"
)
measure_value_metadata(measures)
#> $quantity
#> [1] "absorbance"
#> 
#> $unit
#> [1] "AU"
#> 
```
