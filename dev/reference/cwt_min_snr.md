# CWT peak detection tuning parameters

`cwt_min_snr()` controls the minimum signal-to-noise ratio of a
continuous wavelet transform ridge. `cwt_min_ridge_length()` and
`cwt_gap_thresh()` control ridge linking, while `cwt_noise_window()` and
`cwt_noise_quantile()` control the local noise estimate.

## Usage

``` r
cwt_min_snr(range = c(0, 20), trans = NULL)

cwt_min_ridge_length(range = c(1L, 7L), trans = NULL)

cwt_gap_thresh(range = c(0L, 6L), trans = NULL)

cwt_noise_window(range = c(3L, 101L), trans = NULL)

cwt_noise_quantile(range = c(0.5, 0.99), trans = NULL)
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

A dials parameter object with classes `"quant_param"` and `"param"`.

## Details

These parameters apply to scalar entries in `algorithm_params` for
`step_measure_peaks_detect(algorithm = "cwt")`. Keep the vector of
wavelet widths (`scales`) fixed. When extracted from a recipe, the
ridge-length and gap ranges use the number of widths in that vector.

## Examples

``` r
cwt_min_snr()
#> CWT Minimum Ridge Signal-to-noise Ratio (quantitative)
#> Range: [0, 20]
cwt_min_ridge_length()
#> CWT Minimum Ridge Length (quantitative)
#> Range: [1, 7]
cwt_gap_thresh()
#> CWT Ridge Gap Threshold (quantitative)
#> Range: [0, 6]
cwt_noise_window()
#> CWT Noise Window (quantitative)
#> Range: [3, 101]
cwt_noise_quantile()
#> CWT Noise Quantile (quantitative)
#> Range: (0.5, 0.99)
```
