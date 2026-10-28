# Peak detection and deconvolution tuning parameters

`peak_min_height()` and `peak_min_prominence()` set signal-height
thresholds. `peak_min_distance()` is in x-axis units, and
`peak_snr_threshold()` is a threshold in median absolute deviation
units. `peak_fit_r_squared()` sets a deconvolution fit-quality
threshold, while `peak_fit_iterations()` sets the optimizer's iteration
limit.

## Usage

``` r
peak_min_height(range = c(0, 1), trans = NULL)

peak_min_distance(range = c(0, 10), trans = NULL)

peak_min_prominence(range = c(0, 1), trans = NULL)

peak_snr_threshold(range = c(0, 10), trans = NULL)

peak_fit_r_squared(range = c(0.8, 1), trans = NULL)

peak_fit_iterations(range = c(100L, 1000L), trans = NULL)
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

The default search ranges are starting points only. Height and
prominence ranges depend on signal units and scale; distance ranges
depend on the measurement axis. Set `range` to values appropriate for
the data being analyzed.

## Examples

``` r
peak_min_height()
#> Minimum Peak Height (quantitative)
#> Range: [0, 1]
peak_min_distance()
#> Minimum Peak Distance (quantitative)
#> Range: [0, 10]
peak_min_prominence()
#> Minimum Peak Prominence (quantitative)
#> Range: [0, 1]
peak_snr_threshold()
#> Peak Signal-to-noise Threshold (quantitative)
#> Range: [0, 10]
peak_fit_r_squared()
#> Minimum Peak Fit R-squared (quantitative)
#> Range: [0.8, 1]
peak_fit_iterations()
#> Peak Fit Maximum Iterations (quantitative)
#> Range: [100, 1000]
```
