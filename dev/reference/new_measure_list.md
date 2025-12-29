# Create a new measure list

Constructor for creating a collection of measurements suitable for use
as a list column in a data frame. Each element should be a `measure_tbl`
or tibble with `location` and `value` columns.

## Usage

``` r
new_measure_list(x = list())
```

## Arguments

- x:

  A list of `measure_tbl` objects or tibbles with `location` and `value`
  columns.

## Value

A list with class `measure_list`.

## See also

[`new_measure_tbl()`](https://jameshwade.github.io/measure/dev/reference/new_measure_tbl.md)
for creating individual measurements,
[`is_measure_list()`](https://jameshwade.github.io/measure/dev/reference/is_measure_list.md)
for checking object class.

## Examples

``` r
# Create individual spectra
spec1 <- new_measure_tbl(location = 1:10, value = rnorm(10))
spec2 <- new_measure_tbl(location = 1:10, value = rnorm(10))

# Combine into a measure_list
specs <- new_measure_list(list(spec1, spec2))
specs
#> A measure_list with 2 measurements
#> Sizes: 10, 10 (10-10 points)
```
