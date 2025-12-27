# Parameters for peak normalization

`peak_location_min()` and `peak_location_max()` define the bounds for
the reference region in peak normalization. These should be specified in
the same units as the location values in your measurement data.

## Usage

``` r
peak_location_min(range = c(0, 100), trans = NULL)

peak_location_max(range = c(0, 100), trans = NULL)
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

## Examples

``` r
peak_location_min()
#> Peak Region Lower Bound (quantitative)
#> Range: [0, 100]
peak_location_max()
#> Peak Region Upper Bound (quantitative)
#> Range: [0, 100]
```
