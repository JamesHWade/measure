# Parameters for baseline correction steps

`baseline_lambda()` controls the smoothness penalty in ALS baseline
correction. `baseline_asymmetry()` controls the asymmetry parameter in
ALS. `baseline_degree()` controls the polynomial degree for baseline
fitting.

## Usage

``` r
baseline_lambda(range = c(2, 9), trans = scales::transform_log10())

baseline_asymmetry(range = c(0.001, 0.1), trans = NULL)

baseline_degree(range = c(1L, 6L), trans = NULL)

baseline_half_window(range = c(5L, 100L), trans = NULL)

baseline_span(range = c(0.1, 0.9), trans = NULL)
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
baseline_lambda()
#> Baseline Smoothness (lambda) (quantitative)
#> Transformer: log-10 [1e-100, Inf]
#> Range (transformed scale): [2, 9]
baseline_asymmetry()
#> Baseline Asymmetry (p) (quantitative)
#> Range: [0.001, 0.1]
baseline_degree()
#> Baseline Polynomial Degree (quantitative)
#> Range: [1, 6]
baseline_span()
#> LOESS Span (quantitative)
#> Range: [0.1, 0.9]
```
