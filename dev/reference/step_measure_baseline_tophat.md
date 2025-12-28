# Top-Hat Morphological Baseline Correction

`step_measure_baseline_tophat()` creates a *specification* of a recipe
step that applies top-hat morphological baseline correction.

## Usage

``` r
step_measure_baseline_tophat(
  recipe,
  measures = NULL,
  half_window = 50L,
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_baseline_tophat")
)
```

## Arguments

- recipe:

  A recipe object.

- measures:

  An optional character vector of measure column names.

- half_window:

  Half-window size for the structuring element. Default is 50.

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

The top-hat transform is a morphological operation that extracts bright
features (peaks) from a dark background. It is computed as the
difference between the original signal and its morphological opening.

This is effective for chromatography with sharp, well-defined peaks on a
smooth baseline.

## See also

Other measure-baseline:
[`step_measure_baseline_airpls()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_airpls.md),
[`step_measure_baseline_als()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_als.md),
[`step_measure_baseline_arpls()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_arpls.md),
[`step_measure_baseline_auto()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_auto.md),
[`step_measure_baseline_custom()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_custom.md),
[`step_measure_baseline_gpc()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_gpc.md),
[`step_measure_baseline_minima()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_minima.md),
[`step_measure_baseline_morph()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_morph.md),
[`step_measure_baseline_poly()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_poly.md),
[`step_measure_baseline_py()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_py.md),
[`step_measure_baseline_rf()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_rf.md),
[`step_measure_baseline_rolling()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_rolling.md),
[`step_measure_baseline_snip()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_snip.md),
[`step_measure_detrend()`](https://jameshwade.github.io/measure/dev/reference/step_measure_detrend.md)

## Examples

``` r
library(recipes)

rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
  update_role(id, new_role = "id") |>
  step_measure_input_long(transmittance, location = vars(channel)) |>
  step_measure_baseline_tophat(half_window = 30) |>
  prep()
```
