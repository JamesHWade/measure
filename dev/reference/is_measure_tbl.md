# Test if object is a measure tibble

Test if object is a measure tibble

## Usage

``` r
is_measure_tbl(x)
```

## Arguments

- x:

  Object to test.

## Value

Logical indicating if `x` inherits from `measure_tbl`.

## Examples

``` r
# Create a measure tibble
mt <- measure:::new_measure_tbl(location = 1:5, value = rnorm(5))
is_measure_tbl(mt)
#> [1] TRUE

# Regular tibbles are not measure tibbles
is_measure_tbl(tibble::tibble(location = 1:5, value = rnorm(5)))
#> [1] FALSE
```
