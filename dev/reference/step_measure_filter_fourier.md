# Fourier Low-Pass Filtering

`step_measure_filter_fourier()` creates a *specification* of a recipe
step that applies Fourier-domain low-pass filtering to remove
high-frequency noise.

## Usage

``` r
step_measure_filter_fourier(
  recipe,
  measures = NULL,
  cutoff = 0.1,
  type = c("lowpass", "highpass"),
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_filter_fourier")
)
```

## Arguments

- recipe:

  A recipe object.

- measures:

  An optional character vector of measure column names.

- cutoff:

  The cutoff frequency as a fraction of the Nyquist frequency (0 to
  0.5). Default is 0.1. Frequencies above this are attenuated. Tunable
  via
  [`fourier_cutoff()`](https://jameshwade.github.io/measure/dev/reference/smooth_window.md).

- type:

  Type of filter: `"lowpass"` (default) keeps low frequencies,
  `"highpass"` keeps high frequencies.

- role:

  Not used.

- trained:

  Logical indicating if the step has been trained.

- skip:

  Logical. Should the step be skipped when baking?

- id:

  Unique step identifier.

## Value

An updated recipe with the new step added.

## Details

Fourier filtering transforms the spectrum to the frequency domain using
FFT, applies a frequency mask, and transforms back. This is effective
for:

- Removing periodic noise

- Smoothing with precise frequency control

- Removing high-frequency detector noise

The cutoff is specified as a fraction of the Nyquist frequency. A cutoff
of 0.1 keeps only the lowest 10% of frequencies.

## See also

Other measure-smoothing:
[`step_measure_despike()`](https://jameshwade.github.io/measure/dev/reference/step_measure_despike.md),
[`step_measure_savitzky_golay()`](https://jameshwade.github.io/measure/dev/reference/step_measure_savitzky_golay.md),
[`step_measure_smooth_gaussian()`](https://jameshwade.github.io/measure/dev/reference/step_measure_smooth_gaussian.md),
[`step_measure_smooth_ma()`](https://jameshwade.github.io/measure/dev/reference/step_measure_smooth_ma.md),
[`step_measure_smooth_median()`](https://jameshwade.github.io/measure/dev/reference/step_measure_smooth_median.md),
[`step_measure_smooth_wavelet()`](https://jameshwade.github.io/measure/dev/reference/step_measure_smooth_wavelet.md)

## Examples

``` r
library(recipes)

rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
  update_role(id, new_role = "id") |>
  step_measure_input_long(transmittance, location = vars(channel)) |>
  step_measure_filter_fourier(cutoff = 0.1) |>
  prep()

bake(rec, new_data = NULL)
#> # A tibble: 215 × 5
#>       id water   fat protein .measures
#>    <int> <dbl> <dbl>   <dbl>    <meas>
#>  1     1  60.5  22.5    16.7 [100 × 2]
#>  2     2  46    40.1    13.5 [100 × 2]
#>  3     3  71     8.4    20.5 [100 × 2]
#>  4     4  72.8   5.9    20.7 [100 × 2]
#>  5     5  58.3  25.5    15.5 [100 × 2]
#>  6     6  44    42.7    13.7 [100 × 2]
#>  7     7  44    42.7    13.7 [100 × 2]
#>  8     8  69.3  10.6    19.3 [100 × 2]
#>  9     9  61.4  19.9    17.7 [100 × 2]
#> 10    10  61.4  19.9    17.7 [100 × 2]
#> # ℹ 205 more rows
```
