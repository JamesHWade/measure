# Wavelet Denoising

`step_measure_smooth_wavelet()` creates a *specification* of a recipe
step that applies wavelet-based denoising to measurement data. This
method is particularly effective for signals with localized features
like peaks.

## Usage

``` r
step_measure_smooth_wavelet(
  recipe,
  measures = NULL,
  wavelet = "DaubExPhase",
  filter_number = 4L,
  threshold_type = c("soft", "hard"),
  threshold_policy = c("universal", "sure", "cv"),
  levels = NULL,
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_smooth_wavelet")
)
```

## Arguments

- recipe:

  A recipe object.

- measures:

  An optional character vector of measure column names.

- wavelet:

  The wavelet family to use. Default is `"DaubExPhase"`. Options include
  `"DaubExPhase"`, `"DaubLeAsworthy"`, `"Lawton"`.

- filter_number:

  The filter number within the wavelet family. Default is 4. Higher
  numbers give smoother wavelets.

- threshold_type:

  Type of thresholding: `"soft"` (default) or `"hard"`. Soft
  thresholding shrinks coefficients toward zero; hard thresholding sets
  small coefficients exactly to zero.

- threshold_policy:

  How to determine the threshold:

  - `"universal"` (default): Uses universal threshold sqrt(2\*log(n))

  - `"sure"`: Stein's Unbiased Risk Estimate

  - `"cv"`: Cross-validation

- levels:

  Number of decomposition levels. Default is `NULL` (auto).

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

Wavelet denoising works by:

1.  Decomposing the signal into wavelet coefficients

2.  Thresholding small coefficients (presumed to be noise)

3.  Reconstructing the signal from remaining coefficients

This approach is powerful because:

- It adapts to local signal characteristics

- It preserves sharp features like peaks

- It can separate noise from signal at multiple scales

Requires the `wavethresh` package to be installed.

## Note

Wavelet transforms require signal lengths that are powers of 2. Signals
are automatically padded to the next power of 2 and trimmed after
processing.

## See also

Other measure-smoothing:
[`step_measure_despike()`](https://jameshwade.github.io/measure/dev/reference/step_measure_despike.md),
[`step_measure_filter_fourier()`](https://jameshwade.github.io/measure/dev/reference/step_measure_filter_fourier.md),
[`step_measure_savitzky_golay()`](https://jameshwade.github.io/measure/dev/reference/step_measure_savitzky_golay.md),
[`step_measure_smooth_gaussian()`](https://jameshwade.github.io/measure/dev/reference/step_measure_smooth_gaussian.md),
[`step_measure_smooth_ma()`](https://jameshwade.github.io/measure/dev/reference/step_measure_smooth_ma.md),
[`step_measure_smooth_median()`](https://jameshwade.github.io/measure/dev/reference/step_measure_smooth_median.md)

## Examples

``` r
library(recipes)

rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
  update_role(id, new_role = "id") |>
  step_measure_input_long(transmittance, location = vars(channel)) |>
  step_measure_smooth_wavelet() |>
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
