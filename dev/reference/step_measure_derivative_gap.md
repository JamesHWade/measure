# Gap (Norris-Williams) Derivatives

`step_measure_derivative_gap()` creates a *specification* of a recipe
step that computes gap derivatives using the Norris-Williams method.

## Usage

``` r
step_measure_derivative_gap(
  recipe,
  gap = 2L,
  segment = 1L,
  measures = NULL,
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_derivative_gap")
)
```

## Arguments

- recipe:

  A recipe object. The step will be added to the sequence of operations
  for this recipe.

- gap:

  The gap size (number of points to skip on each side). Default is `2`.
  The derivative at point i is computed from points i-gap and i+gap.

- segment:

  The segment size for averaging. Default is `1` (no averaging). When
  greater than 1, multiple points are averaged on each side before
  computing the difference.

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

Gap derivatives compute the difference between points separated by a
gap:

\$\$\frac{dy}{dx} \approx \frac{y\_{i+g} - y\_{i-g}}{x\_{i+g} -
x\_{i-g}}\$\$

where \\g\\ is the gap size.

When `segment > 1`, the Norris-Williams method is used, which averages
`segment` points on each side before computing the difference.

The spectrum length is reduced by `2 * gap` points.

Gap derivatives are often used in NIR chemometrics as an alternative to
Savitzky-Golay derivatives when less smoothing is desired.

## See also

[`step_measure_derivative()`](https://jameshwade.github.io/measure/dev/reference/step_measure_derivative.md)
for simple finite differences,
[`step_measure_savitzky_golay()`](https://jameshwade.github.io/measure/dev/reference/step_measure_savitzky_golay.md)
for smoothed derivatives

Other measure-preprocessing:
[`step_measure_absorbance()`](https://jameshwade.github.io/measure/dev/reference/step_measure_absorbance.md),
[`step_measure_calibrate_x()`](https://jameshwade.github.io/measure/dev/reference/step_measure_calibrate_x.md),
[`step_measure_calibrate_y()`](https://jameshwade.github.io/measure/dev/reference/step_measure_calibrate_y.md),
[`step_measure_derivative()`](https://jameshwade.github.io/measure/dev/reference/step_measure_derivative.md),
[`step_measure_kubelka_munk()`](https://jameshwade.github.io/measure/dev/reference/step_measure_kubelka_munk.md),
[`step_measure_log()`](https://jameshwade.github.io/measure/dev/reference/step_measure_log.md),
[`step_measure_map()`](https://jameshwade.github.io/measure/dev/reference/step_measure_map.md),
[`step_measure_msc()`](https://jameshwade.github.io/measure/dev/reference/step_measure_msc.md),
[`step_measure_normalize_istd()`](https://jameshwade.github.io/measure/dev/reference/step_measure_normalize_istd.md),
[`step_measure_ratio_reference()`](https://jameshwade.github.io/measure/dev/reference/step_measure_ratio_reference.md),
[`step_measure_snv()`](https://jameshwade.github.io/measure/dev/reference/step_measure_snv.md),
[`step_measure_subtract_blank()`](https://jameshwade.github.io/measure/dev/reference/step_measure_subtract_blank.md),
[`step_measure_subtract_reference()`](https://jameshwade.github.io/measure/dev/reference/step_measure_subtract_reference.md),
[`step_measure_transmittance()`](https://jameshwade.github.io/measure/dev/reference/step_measure_transmittance.md)

## Examples

``` r
library(recipes)

# Gap derivative with gap=2
rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
  update_role(id, new_role = "id") |>
  step_measure_input_long(transmittance, location = vars(channel)) |>
  step_measure_derivative_gap(gap = 2) |>
  prep()

# Norris-Williams with gap=3, segment=2
rec2 <- recipe(water + fat + protein ~ ., data = meats_long) |>
  update_role(id, new_role = "id") |>
  step_measure_input_long(transmittance, location = vars(channel)) |>
  step_measure_derivative_gap(gap = 3, segment = 2) |>
  prep()
```
