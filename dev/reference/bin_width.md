# Parameters for feature engineering and scatter correction

`bin_width()` controls the width of bins in spectral binning.
`emsc_degree()` controls the polynomial degree for EMSC correction.
`osc_n_components()` controls the number of orthogonal components in
OSC.

## Usage

``` r
bin_width(range = c(1, 20), trans = NULL)

emsc_degree(range = c(0L, 4L), trans = NULL)

osc_n_components(range = c(1L, 10L), trans = NULL)
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
bin_width()
#> Bin Width (quantitative)
#> Range: [1, 20]
emsc_degree()
#> EMSC Polynomial Degree (quantitative)
#> Range: [0, 4]
osc_n_components()
#> Number of OSC Components (quantitative)
#> Range: [1, 10]
```
