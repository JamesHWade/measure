# Set measurement value metadata

Declare the quantity and optional unit for one homogeneous 1D
`measure_list` column. The declaration is stored on the column itself.
Row slicing, filtering, and homogeneous binding preserve it; a
value-changing function that rebuilds a plain list must explicitly call
this function on the new result, or leave the result untagged when its
quantity or unit is unknown.

## Usage

``` r
set_measure_value_metadata(x, quantity, unit = NULL)
```

## Arguments

- x:

  An ordinary 1D `measure_list` column.

- quantity:

  A non-empty, length-one character string naming the value quantity
  (for example, `"absorbance"` or `"intensity"`).

- unit:

  `NULL` or a non-empty, length-one character string declaring the value
  unit (for example, `"AU"`).

## Value

`x` with a `measure_value_metadata` declaration attached to the column.

## Examples

``` r
measures <- new_measure_list(list(
  new_measure_tbl(1:3, c(1, 2, 3)),
  new_measure_tbl(1:3, c(2, 3, 4))
))
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

# A transformed quantity gets a new explicit declaration.
transformed <- new_measure_list(lapply(measures, function(measure) {
  measure$value <- measure$value * 1000
  measure
}))
transformed <- set_measure_value_metadata(
  transformed,
  quantity = "signal",
  unit = "mAU"
)
```
