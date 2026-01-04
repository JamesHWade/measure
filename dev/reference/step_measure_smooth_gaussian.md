# Gaussian Kernel Smoothing

`step_measure_smooth_gaussian()` creates a *specification* of a recipe
step that applies Gaussian kernel smoothing. This produces smooth
results while preserving the general shape of peaks.

## Usage

``` r
step_measure_smooth_gaussian(
  recipe,
  measures = NULL,
  sigma = 1,
  window = NULL,
  edge_method = c("reflect", "constant", "NA"),
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_smooth_gaussian")
)
```

## Arguments

- recipe:

  A recipe object.

- measures:

  An optional character vector of measure column names.

- sigma:

  The standard deviation of the Gaussian kernel. Default is 1. Larger
  values produce more smoothing. Tunable via
  [`smooth_sigma()`](https://jameshwade.github.io/measure/dev/reference/smooth_window.md).

- window:

  The window size. If `NULL` (default), automatically set to
  `ceiling(6 * sigma) | 1` (6 sigma rule, ensuring odd).

- edge_method:

  How to handle edges. One of `"reflect"` (default), `"constant"`, or
  `"NA"`.

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

Gaussian smoothing convolves the spectrum with a Gaussian kernel:
\$\$G(x) = \exp(-x^2 / 2\sigma^2)\$\$

The kernel is normalized to sum to 1. This provides smooth,
natural-looking results that preserve peak shapes better than moving
average.

## See also

Other measure-smoothing:
[`step_measure_despike()`](https://jameshwade.github.io/measure/dev/reference/step_measure_despike.md),
[`step_measure_filter_fourier()`](https://jameshwade.github.io/measure/dev/reference/step_measure_filter_fourier.md),
[`step_measure_savitzky_golay()`](https://jameshwade.github.io/measure/dev/reference/step_measure_savitzky_golay.md),
[`step_measure_smooth_ma()`](https://jameshwade.github.io/measure/dev/reference/step_measure_smooth_ma.md),
[`step_measure_smooth_median()`](https://jameshwade.github.io/measure/dev/reference/step_measure_smooth_median.md),
[`step_measure_smooth_wavelet()`](https://jameshwade.github.io/measure/dev/reference/step_measure_smooth_wavelet.md)

## Examples

``` r
library(recipes)

rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
  update_role(id, new_role = "id") |>
  step_measure_input_long(transmittance, location = vars(channel)) |>
  step_measure_smooth_gaussian(sigma = 2) |>
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
