# Compute Ratio to Reference Spectrum

`step_measure_ratio_reference()` creates a *specification* of a recipe
step that computes the ratio of each spectrum to a reference, optionally
with blank subtraction.

## Usage

``` r
step_measure_ratio_reference(
  recipe,
  reference,
  blank = NULL,
  measures = NULL,
  role = NA,
  trained = FALSE,
  learned_ref = NULL,
  learned_blank = NULL,
  skip = FALSE,
  id = recipes::rand_id("measure_ratio_reference")
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

- blank:

  An optional blank spectrum to subtract from both sample and reference
  before computing the ratio. Same format options as `reference`.

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

- learned_blank:

  A named list containing the learned blank values for each measure
  column. This is `NULL` until the step is trained.

- skip:

  A logical. Should the step be skipped when the recipe is baked?

- id:

  A character string that is unique to this step to identify it.

## Value

An updated version of `recipe` with the new step added.

## Details

This step computes a ratio relative to a reference spectrum:

- Without blank: `result = sample / reference`

- With blank: `result = (sample - blank) / (reference - blank)`

This is useful for computing relative measurements, such as absorbance
from transmittance when you have both sample and reference scans.

## See also

[`step_measure_subtract_blank()`](https://jameshwade.github.io/measure/dev/reference/step_measure_subtract_blank.md)
for simple blank subtraction

Other measure-preprocessing:
[`step_measure_calibrate_x()`](https://jameshwade.github.io/measure/dev/reference/step_measure_calibrate_x.md),
[`step_measure_calibrate_y()`](https://jameshwade.github.io/measure/dev/reference/step_measure_calibrate_y.md),
[`step_measure_map()`](https://jameshwade.github.io/measure/dev/reference/step_measure_map.md),
[`step_measure_msc()`](https://jameshwade.github.io/measure/dev/reference/step_measure_msc.md),
[`step_measure_normalize_istd()`](https://jameshwade.github.io/measure/dev/reference/step_measure_normalize_istd.md),
[`step_measure_snv()`](https://jameshwade.github.io/measure/dev/reference/step_measure_snv.md),
[`step_measure_subtract_blank()`](https://jameshwade.github.io/measure/dev/reference/step_measure_subtract_blank.md),
[`step_measure_subtract_reference()`](https://jameshwade.github.io/measure/dev/reference/step_measure_subtract_reference.md)

## Examples

``` r
library(recipes)

# Create reference and blank spectra
ref_spectrum <- rep(1.0, 100)
blank_spectrum <- rep(0.05, 100)

rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
  update_role(id, new_role = "id") |>
  step_measure_input_long(transmittance, location = vars(channel)) |>
  step_measure_ratio_reference(
    reference = ref_spectrum,
    blank = blank_spectrum
  )
```
