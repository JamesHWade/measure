# Parameters for quality control steps

`outlier_threshold()` controls the threshold for outlier detection (in
standard deviation or Mahalanobis distance units).

## Usage

``` r
outlier_threshold(range = c(2, 5), trans = NULL)
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
outlier_threshold()
#> Outlier Threshold (quantitative)
#> Range: [2, 5]
```
