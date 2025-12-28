# Parameters for smoothing steps

`smooth_window()` controls the window size for moving average and median
smoothing. `smooth_sigma()` controls the standard deviation for Gaussian
smoothing. `fourier_cutoff()` controls the frequency cutoff for Fourier
filtering.

## Usage

``` r
smooth_window(range = c(3L, 21L), trans = NULL)

smooth_sigma(range = c(0.5, 5), trans = NULL)

fourier_cutoff(range = c(0.01, 0.5), trans = NULL)

despike_threshold(range = c(2, 10), trans = NULL)
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
smooth_window()
#> Smoothing Window Size (quantitative)
#> Range: [3, 21]
smooth_sigma()
#> Gaussian Sigma (quantitative)
#> Range: [0.5, 5]
fourier_cutoff()
#> Fourier Cutoff Frequency (quantitative)
#> Range: [0.01, 0.5]
```
