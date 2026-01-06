# Check if a Peak Algorithm Exists

Checks whether a peak detection algorithm is registered.

## Usage

``` r
has_peak_algorithm(name)
```

## Arguments

- name:

  Algorithm name.

## Value

Logical `TRUE` if algorithm exists, `FALSE` otherwise.

## See also

[`peak_algorithms()`](https://jameshwade.github.io/measure/dev/reference/peak_algorithms.md)

## Examples

``` r
has_peak_algorithm("prominence")  # TRUE
#> [1] TRUE
has_peak_algorithm("nonexistent") # FALSE
#> [1] FALSE
```
