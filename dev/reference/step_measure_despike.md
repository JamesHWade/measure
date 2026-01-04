# Remove Spikes and Outliers from Measurements

`step_measure_despike()` creates a *specification* of a recipe step that
detects and removes spikes (sudden, brief outliers) from measurement
data. Spikes are common artifacts in spectroscopy (cosmic rays in Raman,
detector glitches) and chromatography (electrical noise).

## Usage

``` r
step_measure_despike(
  recipe,
  measures = NULL,
  window = 5L,
  threshold = 5,
  method = c("interpolate", "median", "mean"),
  max_width = 3L,
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_despike")
)
```

## Arguments

- recipe:

  A recipe object.

- measures:

  An optional character vector of measure column names.

- window:

  The window size for local statistics. Must be an odd integer of at
  least 3. Default is 5. Tunable via
  [`smooth_window()`](https://jameshwade.github.io/measure/dev/reference/smooth_window.md).

- threshold:

  The threshold multiplier for spike detection. Points deviating more
  than `threshold * MAD` from the local median are flagged. Default
  is 5. Tunable via
  [`despike_threshold()`](https://jameshwade.github.io/measure/dev/reference/smooth_window.md).

- method:

  How to replace detected spikes. One of `"interpolate"` (default,
  linear interpolation from neighbors), `"median"` (replace with local
  median), or `"mean"` (replace with local mean).

- max_width:

  Maximum width (in points) of a spike. Consecutive outliers wider than
  this are not considered spikes. Default is 3.

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

Spike detection uses a robust local statistic approach:

1.  For each point, calculate the local median and MAD (Median Absolute
    Deviation) within a sliding window

2.  Flag points where `|value - local_median| > threshold * MAD`

3.  Group consecutive flagged points into spike regions

4.  If a spike region is narrower than `max_width`, replace with the
    specified method

MAD is scaled by 1.4826 to be consistent with standard deviation for
normally distributed data.

This approach is robust because:

- Median and MAD are not affected by the spikes themselves

- The threshold adapts to local noise levels

- The max_width parameter prevents removing genuine peaks

## See also

Other measure-smoothing:
[`step_measure_filter_fourier()`](https://jameshwade.github.io/measure/dev/reference/step_measure_filter_fourier.md),
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
  step_measure_despike(threshold = 5) |>
  prep()

bake(rec, new_data = NULL)
#> # A tibble: 215 × 6
#>       id water   fat protein .measures channel    
#>    <int> <dbl> <dbl>   <dbl>    <meas> <list>     
#>  1     1  60.5  22.5    16.7 [100 × 2] <int [100]>
#>  2     2  46    40.1    13.5 [100 × 2] <int [100]>
#>  3     3  71     8.4    20.5 [100 × 2] <int [100]>
#>  4     4  72.8   5.9    20.7 [100 × 2] <int [100]>
#>  5     5  58.3  25.5    15.5 [100 × 2] <int [100]>
#>  6     6  44    42.7    13.7 [100 × 2] <int [100]>
#>  7     7  44    42.7    13.7 [100 × 2] <int [100]>
#>  8     8  69.3  10.6    19.3 [100 × 2] <int [100]>
#>  9     9  61.4  19.9    17.7 [100 × 2] <int [100]>
#> 10    10  61.4  19.9    17.7 [100 × 2] <int [100]>
#> # ℹ 205 more rows
```
