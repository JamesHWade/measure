# Apply Y-Axis Calibration (Response Factor)

`step_measure_calibrate_y()` creates a *specification* of a recipe step
that applies a response factor or calibration function to y-axis (value)
values.

## Usage

``` r
step_measure_calibrate_y(
  recipe,
  response_factor = 1,
  calibration = NULL,
  measures = NULL,
  role = NA,
  trained = FALSE,
  cal_fn = NULL,
  skip = FALSE,
  id = recipes::rand_id("measure_calibrate_y")
)
```

## Arguments

- recipe:

  A recipe object. The step will be added to the sequence of operations
  for this recipe.

- response_factor:

  A numeric value to multiply all values by. Default is `1.0` (no
  change). This is a simple scalar calibration.

- calibration:

  An optional calibration function that takes value(s) and returns
  calibrated value(s). If provided, this takes precedence over
  `response_factor`.

- measures:

  An optional character vector of measure column names to process. If
  `NULL` (the default), all measure columns will be processed.

- role:

  Not used by this step since no new variables are created.

- trained:

  A logical to indicate if the step has been trained.

- cal_fn:

  The calibration function to apply (built during prep).

- skip:

  A logical. Should the step be skipped when baking?

- id:

  A character string that is unique to this step.

## Value

An updated version of `recipe` with the new step added.

## Details

Y-axis calibration is used to convert raw signal intensities to
quantitative values. Common examples include:

- **Chromatography**: Apply detector response factors

- **Spectroscopy**: Apply molar absorptivity corrections

- **Mass spectrometry**: Apply ionization efficiency corrections

**Simple mode**: Use `response_factor` to multiply all values by a
constant.

**Complex mode**: Use `calibration` to provide a function for non-linear
calibration curves (e.g., from fitting standards).

**No selectors should be supplied to this step function**. The data
should be in the internal format produced by
[`step_measure_input_wide()`](https://jameshwade.github.io/measure/dev/reference/step_measure_input_wide.md)
or
[`step_measure_input_long()`](https://jameshwade.github.io/measure/dev/reference/step_measure_input_long.md).

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/reference/tidy.recipe.html)
this step, a tibble with columns `terms`, `response_factor`,
`has_calibration`, and `id` is returned.

## See also

[`step_measure_calibrate_x()`](https://jameshwade.github.io/measure/dev/reference/step_measure_calibrate_x.md)
for x-axis calibration

Other measure-preprocessing:
[`step_measure_calibrate_x()`](https://jameshwade.github.io/measure/dev/reference/step_measure_calibrate_x.md),
[`step_measure_map()`](https://jameshwade.github.io/measure/dev/reference/step_measure_map.md),
[`step_measure_msc()`](https://jameshwade.github.io/measure/dev/reference/step_measure_msc.md),
[`step_measure_normalize_istd()`](https://jameshwade.github.io/measure/dev/reference/step_measure_normalize_istd.md),
[`step_measure_ratio_reference()`](https://jameshwade.github.io/measure/dev/reference/step_measure_ratio_reference.md),
[`step_measure_snv()`](https://jameshwade.github.io/measure/dev/reference/step_measure_snv.md),
[`step_measure_subtract_blank()`](https://jameshwade.github.io/measure/dev/reference/step_measure_subtract_blank.md),
[`step_measure_subtract_reference()`](https://jameshwade.github.io/measure/dev/reference/step_measure_subtract_reference.md)

## Examples

``` r
library(recipes)

# Simple response factor
rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
  update_role(id, new_role = "id") |>
  step_measure_input_long(transmittance, location = vars(channel)) |>
  step_measure_calibrate_y(response_factor = 2.5)

# With calibration function (e.g., log transform)
rec2 <- recipe(water + fat + protein ~ ., data = meats_long) |>
  update_role(id, new_role = "id") |>
  step_measure_input_long(transmittance, location = vars(channel)) |>
  step_measure_calibrate_y(calibration = function(x) log10(x + 0.001))
```
