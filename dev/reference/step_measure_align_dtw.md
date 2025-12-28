# Dynamic Time Warping Alignment

`step_measure_align_dtw()` creates a *specification* of a recipe step
that aligns spectra using Dynamic Time Warping (DTW). This method can
handle non-linear distortions in the x-axis.

## Usage

``` r
step_measure_align_dtw(
  recipe,
  measures = NULL,
  reference = c("mean", "median", "first"),
  window_type = c("none", "sakoechiba", "slantedband"),
  window_size = 10L,
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_align_dtw")
)
```

## Arguments

- recipe:

  A recipe object.

- measures:

  An optional character vector of measure column names.

- reference:

  How to determine the reference:

  - `"mean"` (default): Use the mean spectrum from training

  - `"median"`: Use the median spectrum from training

  - `"first"`: Use the first sample

- window_type:

  Windowing constraint for DTW. One of `"none"` (default),
  `"sakoechiba"`, or `"slantedband"`.

- window_size:

  Window size for constrained DTW. Default is 10.

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

DTW finds the optimal non-linear alignment between two sequences by
minimizing a distance measure while allowing warping of the time/x-axis.

This is useful for:

- Chromatographic peak alignment

- Correcting non-linear retention time shifts

- Aligning spectra with complex distortions

Requires the `dtw` package to be installed.

## See also

Other measure-align:
[`step_measure_align_cow()`](https://jameshwade.github.io/measure/dev/reference/step_measure_align_cow.md),
[`step_measure_align_ptw()`](https://jameshwade.github.io/measure/dev/reference/step_measure_align_ptw.md),
[`step_measure_align_reference()`](https://jameshwade.github.io/measure/dev/reference/step_measure_align_reference.md),
[`step_measure_align_shift()`](https://jameshwade.github.io/measure/dev/reference/step_measure_align_shift.md)

## Examples

``` r
library(recipes)

rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
  update_role(id, new_role = "id") |>
  step_measure_input_long(transmittance, location = vars(channel)) |>
  step_measure_align_dtw() |>
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
