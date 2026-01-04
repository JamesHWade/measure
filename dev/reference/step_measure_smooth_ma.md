# Moving Average Smoothing

`step_measure_smooth_ma()` creates a *specification* of a recipe step
that applies moving average smoothing to measurement data. This is a
simple and fast method for reducing high-frequency noise.

## Usage

``` r
step_measure_smooth_ma(
  recipe,
  measures = NULL,
  window = 5L,
  edge_method = c("reflect", "constant", "NA"),
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_smooth_ma")
)
```

## Arguments

- recipe:

  A recipe object.

- measures:

  An optional character vector of measure column names.

- window:

  The window size for the moving average. Must be an odd integer of at
  least 3. Default is 5. Larger values produce more smoothing. Tunable
  via
  [`smooth_window()`](https://jameshwade.github.io/measure/dev/reference/smooth_window.md).

- edge_method:

  How to handle edges where the full window doesn't fit. One of
  `"reflect"` (default, reflects values at boundaries), `"constant"`
  (pads with edge values), or `"NA"` (returns NA for edge values).

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

Moving average smoothing replaces each point with the mean of its
neighbors within a sliding window. This is equivalent to convolution
with a uniform kernel.

For a window size of `w`, the smoothed value at position `i` is: \$\$y_i
= \frac{1}{w} \sum\_{j=-k}^{k} x\_{i+j}\$\$

where `k = (w-1)/2` is the half-window size.

## See also

Other measure-smoothing:
[`step_measure_despike()`](https://jameshwade.github.io/measure/dev/reference/step_measure_despike.md),
[`step_measure_filter_fourier()`](https://jameshwade.github.io/measure/dev/reference/step_measure_filter_fourier.md),
[`step_measure_savitzky_golay()`](https://jameshwade.github.io/measure/dev/reference/step_measure_savitzky_golay.md),
[`step_measure_smooth_gaussian()`](https://jameshwade.github.io/measure/dev/reference/step_measure_smooth_gaussian.md),
[`step_measure_smooth_median()`](https://jameshwade.github.io/measure/dev/reference/step_measure_smooth_median.md),
[`step_measure_smooth_wavelet()`](https://jameshwade.github.io/measure/dev/reference/step_measure_smooth_wavelet.md)

## Examples

``` r
library(recipes)

rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
  update_role(id, new_role = "id") |>
  step_measure_input_long(transmittance, location = vars(channel)) |>
  step_measure_smooth_ma(window = 5) |>
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
