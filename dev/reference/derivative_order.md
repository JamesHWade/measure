# Parameters for derivative steps

`derivative_order()` controls the order of differentiation in
[`step_measure_derivative()`](https://jameshwade.github.io/measure/dev/reference/step_measure_derivative.md)
(1 = first derivative, 2 = second derivative). `derivative_gap()` and
`derivative_segment()` control the gap derivative (Norris-Williams)
parameters in
[`step_measure_derivative_gap()`](https://jameshwade.github.io/measure/dev/reference/step_measure_derivative_gap.md).

## Usage

``` r
derivative_order(range = c(1L, 2L), trans = NULL)

derivative_gap(range = c(1L, 10L), trans = NULL)

derivative_segment(range = c(1L, 5L), trans = NULL)
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
derivative_order()
#> Derivative Order (quantitative)
#> Range: [1, 2]
derivative_gap()
#> Derivative Gap (quantitative)
#> Range: [1, 10]
derivative_segment()
#> Derivative Segment (quantitative)
#> Range: [1, 5]
```
