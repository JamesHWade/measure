# Automatic Baseline Correction Method Selection

`step_measure_baseline_auto()` creates a *specification* of a recipe
step that automatically selects and applies the best baseline correction
method based on signal characteristics.

## Usage

``` r
step_measure_baseline_auto(
  recipe,
  measures = NULL,
  methods = c("rolling", "airpls", "snip", "tophat", "minima"),
  role = NA,
  trained = FALSE,
  selected_method = NULL,
  skip = FALSE,
  id = recipes::rand_id("measure_baseline_auto")
)
```

## Arguments

- recipe:

  A recipe object.

- measures:

  An optional character vector of measure column names.

- methods:

  Character vector of methods to consider. Default includes all
  available methods.

- role:

  Not used.

- trained:

  Logical indicating if the step has been trained.

- selected_method:

  The method selected during training (internal).

- skip:

  Logical. Should the step be skipped when baking?

- id:

  Unique step identifier.

## Value

An updated recipe with the new step added.

## Details

This step analyzes the signal characteristics (noise level, baseline
curvature, peak density) during training and selects an appropriate
baseline correction method. The selected method is then applied
consistently during baking.

Method selection heuristics:

- High noise, smooth baseline: rolling ball

- Complex curvature: airPLS or arPLS

- Sharp peaks: SNIP or top-hat

- Simple baseline: polynomial or minima

## See also

Other measure-baseline:
[`step_measure_baseline_airpls()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_airpls.md),
[`step_measure_baseline_als()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_als.md),
[`step_measure_baseline_arpls()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_arpls.md),
[`step_measure_baseline_custom()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_custom.md),
[`step_measure_baseline_gpc()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_gpc.md),
[`step_measure_baseline_minima()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_minima.md),
[`step_measure_baseline_morph()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_morph.md),
[`step_measure_baseline_poly()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_poly.md),
[`step_measure_baseline_py()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_py.md),
[`step_measure_baseline_rf()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_rf.md),
[`step_measure_baseline_rolling()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_rolling.md),
[`step_measure_baseline_snip()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_snip.md),
[`step_measure_baseline_tophat()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_tophat.md),
[`step_measure_detrend()`](https://jameshwade.github.io/measure/dev/reference/step_measure_detrend.md)

## Examples

``` r
library(recipes)

rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
  update_role(id, new_role = "id") |>
  step_measure_input_long(transmittance, location = vars(channel)) |>
  step_measure_baseline_auto() |>
  prep()
```
