# Kubelka-Munk Transformation

`step_measure_kubelka_munk()` creates a *specification* of a recipe step
that applies the Kubelka-Munk transformation for diffuse reflectance
data.

## Usage

``` r
step_measure_kubelka_munk(
  recipe,
  measures = NULL,
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_kubelka_munk")
)
```

## Arguments

- recipe:

  A recipe object. The step will be added to the sequence of operations
  for this recipe.

- measures:

  An optional character vector of measure column names to process. If
  `NULL` (the default), all measure columns will be processed.

- role:

  Not used by this step since no new variables are created.

- trained:

  A logical to indicate if the step has been trained.

- skip:

  A logical. Should the step be skipped when baking?

- id:

  A character string that is unique to this step.

## Value

An updated version of `recipe` with the new step added.

## Details

The Kubelka-Munk transformation is used for diffuse reflectance
spectroscopy to convert reflectance to a quantity proportional to
concentration:

\$\$f(R) = \frac{(1-R)^2}{2R}\$\$

where \\R\\ is the reflectance (0 to 1).

**Important**: Reflectance values should be in the range (0, 1). Values
at the boundaries will produce extreme values or `Inf`.

This transformation is commonly used in:

- NIR diffuse reflectance spectroscopy

- Analysis of powders and solid samples

- When Beer-Lambert law doesn't apply directly

The measurement locations are preserved unchanged.

## See also

Other measure-preprocessing:
[`step_measure_absorbance()`](https://jameshwade.github.io/measure/dev/reference/step_measure_absorbance.md),
[`step_measure_calibrate_x()`](https://jameshwade.github.io/measure/dev/reference/step_measure_calibrate_x.md),
[`step_measure_calibrate_y()`](https://jameshwade.github.io/measure/dev/reference/step_measure_calibrate_y.md),
[`step_measure_derivative()`](https://jameshwade.github.io/measure/dev/reference/step_measure_derivative.md),
[`step_measure_derivative_gap()`](https://jameshwade.github.io/measure/dev/reference/step_measure_derivative_gap.md),
[`step_measure_emsc()`](https://jameshwade.github.io/measure/dev/reference/step_measure_emsc.md),
[`step_measure_log()`](https://jameshwade.github.io/measure/dev/reference/step_measure_log.md),
[`step_measure_map()`](https://jameshwade.github.io/measure/dev/reference/step_measure_map.md),
[`step_measure_msc()`](https://jameshwade.github.io/measure/dev/reference/step_measure_msc.md),
[`step_measure_normalize_istd()`](https://jameshwade.github.io/measure/dev/reference/step_measure_normalize_istd.md),
[`step_measure_osc()`](https://jameshwade.github.io/measure/dev/reference/step_measure_osc.md),
[`step_measure_ratio_reference()`](https://jameshwade.github.io/measure/dev/reference/step_measure_ratio_reference.md),
[`step_measure_snv()`](https://jameshwade.github.io/measure/dev/reference/step_measure_snv.md),
[`step_measure_subtract_blank()`](https://jameshwade.github.io/measure/dev/reference/step_measure_subtract_blank.md),
[`step_measure_subtract_reference()`](https://jameshwade.github.io/measure/dev/reference/step_measure_subtract_reference.md),
[`step_measure_transmittance()`](https://jameshwade.github.io/measure/dev/reference/step_measure_transmittance.md)

## Examples

``` r
library(recipes)

# Assuming reflectance data in (0, 1) range
# Note: meats_long has transmittance, this is illustrative
rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
  update_role(id, new_role = "id") |>
  step_measure_input_long(transmittance, location = vars(channel)) |>
  step_measure_kubelka_munk()
```
