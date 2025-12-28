# Align to Reference Spectrum

`step_measure_align_reference()` creates a *specification* of a recipe
step that aligns spectra to a user-provided reference spectrum using
cross-correlation.

## Usage

``` r
step_measure_align_reference(
  recipe,
  measures = NULL,
  ref_spectrum,
  max_shift = 10L,
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_align_reference")
)
```

## Arguments

- recipe:

  A recipe object.

- measures:

  An optional character vector of measure column names.

- ref_spectrum:

  A numeric vector containing the reference spectrum. Must have the same
  length as the measurement spectra.

- max_shift:

  Maximum shift (in points) to consider. Default is 10.

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

Similar to
[`step_measure_align_shift()`](https://jameshwade.github.io/measure/dev/reference/step_measure_align_shift.md),
but uses an externally provided reference spectrum instead of computing
one from training data. This is useful when you have a known standard or
calibration spectrum.

## See also

Other measure-align:
[`step_measure_align_cow()`](https://jameshwade.github.io/measure/dev/reference/step_measure_align_cow.md),
[`step_measure_align_dtw()`](https://jameshwade.github.io/measure/dev/reference/step_measure_align_dtw.md),
[`step_measure_align_ptw()`](https://jameshwade.github.io/measure/dev/reference/step_measure_align_ptw.md),
[`step_measure_align_shift()`](https://jameshwade.github.io/measure/dev/reference/step_measure_align_shift.md)

## Examples

``` r
library(recipes)

# Create a reference spectrum (in practice, this would be from calibration)
ref <- rep(1, 100)  # placeholder

# Note: This example would need matching spectrum lengths to work
if (FALSE) { # \dontrun{
rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
  update_role(id, new_role = "id") |>
  step_measure_input_long(transmittance, location = vars(channel)) |>
  step_measure_align_reference(ref_spectrum = ref) |>
  prep()
} # }
```
