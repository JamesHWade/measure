# Parameter for measure steps

`window_side()` and `differentiation_order()` are used with
Savitzky-Golay processing.

## Usage

``` r
window_side(range = c(1L, 5L), trans = NULL)

differentiation_order(range = c(0L, 4L), trans = NULL)
```

## Arguments

- range:

  A two-element vector holding the *defaults* for the smallest and
  largest possible values, respectively. If a transformation is
  specified, these values should be in the *transformed units*.

- trans:

  A `trans` object from the `scales` package, such as
  [`scales::transform_log10()`](https://scales.r-lib.org/reference/transform_log.html)
  or
  [`scales::transform_reciprocal()`](https://scales.r-lib.org/reference/transform_reciprocal.html).
  If not provided, the default is used which matches the units used in
  `range`. If no transformation, `NULL`.

## Value

A function with classes `"quant_param"` and `"param"`.

## Details

This parameter is often used to correct for zero-count data in tables or
proportions.

## Examples

``` r
window_side()
#> Window Size (one side) (quantitative)
#> Range: [1, 5]
differentiation_order()
#> Differentiation Order (quantitative)
#> Range: [0, 4]
```
