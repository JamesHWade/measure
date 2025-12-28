# Subtract or Divide by Reference Spectrum

`step_measure_subtract_reference()` creates a *specification* of a
recipe step that subtracts or divides each spectrum by an external
reference. This is a simpler version of
[`step_measure_subtract_blank()`](https://jameshwade.github.io/measure/dev/reference/step_measure_subtract_blank.md)
that always uses an externally provided reference.

## Usage

``` r
step_measure_subtract_reference(
  recipe,
  reference,
  method = "subtract",
  measures = NULL,
  role = NA,
  trained = FALSE,
  learned_ref = NULL,
  skip = FALSE,
  id = recipes::rand_id("measure_subtract_reference")
)
```

## Arguments

- recipe:

  A recipe object. The step will be added to the sequence of operations
  for this recipe.

- reference:

  A required external reference spectrum. Can be:

  - A `measure_tbl` object with `location` and `value` columns

  - A numeric vector (must match the number of locations in data)

  - A data.frame with `location` and `value` columns (will be
    interpolated)

- method:

  The correction method to apply:

  - `"subtract"` (default): Subtract the blank from each spectrum

  - `"divide"`: Divide each spectrum by the blank

- measures:

  An optional character vector of measure column names to process. If
  `NULL` (the default), all measure columns (columns with class
  `measure_list`) will be processed.

- role:

  Not used by this step since no new variables are created.

- trained:

  A logical to indicate if the quantities for preprocessing have been
  estimated.

- learned_ref:

  A named list containing the validated reference values for each
  measure column. This is `NULL` until the step is trained.

- skip:

  A logical. Should the step be skipped when the recipe is baked?

- id:

  A character string that is unique to this step to identify it.

## Value

An updated version of `recipe` with the new step added.

## Details

This step applies a simple reference correction to each spectrum:

- `method = "subtract"`: `result = sample - reference`

- `method = "divide"`: `result = sample / reference`

Unlike
[`step_measure_subtract_blank()`](https://jameshwade.github.io/measure/dev/reference/step_measure_subtract_blank.md),
this step always requires an externally provided reference and does not
support learning from training data.

## See also

[`step_measure_subtract_blank()`](https://jameshwade.github.io/measure/dev/reference/step_measure_subtract_blank.md)
for blank correction with learning

Other measure-preprocessing:
[`step_measure_absorbance()`](https://jameshwade.github.io/measure/dev/reference/step_measure_absorbance.md),
[`step_measure_calibrate_x()`](https://jameshwade.github.io/measure/dev/reference/step_measure_calibrate_x.md),
[`step_measure_calibrate_y()`](https://jameshwade.github.io/measure/dev/reference/step_measure_calibrate_y.md),
[`step_measure_derivative()`](https://jameshwade.github.io/measure/dev/reference/step_measure_derivative.md),
[`step_measure_derivative_gap()`](https://jameshwade.github.io/measure/dev/reference/step_measure_derivative_gap.md),
[`step_measure_kubelka_munk()`](https://jameshwade.github.io/measure/dev/reference/step_measure_kubelka_munk.md),
[`step_measure_log()`](https://jameshwade.github.io/measure/dev/reference/step_measure_log.md),
[`step_measure_map()`](https://jameshwade.github.io/measure/dev/reference/step_measure_map.md),
[`step_measure_msc()`](https://jameshwade.github.io/measure/dev/reference/step_measure_msc.md),
[`step_measure_normalize_istd()`](https://jameshwade.github.io/measure/dev/reference/step_measure_normalize_istd.md),
[`step_measure_ratio_reference()`](https://jameshwade.github.io/measure/dev/reference/step_measure_ratio_reference.md),
[`step_measure_snv()`](https://jameshwade.github.io/measure/dev/reference/step_measure_snv.md),
[`step_measure_subtract_blank()`](https://jameshwade.github.io/measure/dev/reference/step_measure_subtract_blank.md),
[`step_measure_transmittance()`](https://jameshwade.github.io/measure/dev/reference/step_measure_transmittance.md)

## Examples

``` r
library(recipes)

# Create a reference spectrum
ref_spectrum <- rep(1.0, 100)

rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
  update_role(id, new_role = "id") |>
  step_measure_input_long(transmittance, location = vars(channel)) |>
  step_measure_subtract_reference(reference = ref_spectrum, method = "divide")
```
