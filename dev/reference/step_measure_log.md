# Log Transformation

`step_measure_log()` creates a *specification* of a recipe step that
applies a logarithmic transformation to measurement values.

## Usage

``` r
step_measure_log(
  recipe,
  base = exp(1),
  offset = 0,
  measures = NULL,
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_log")
)
```

## Arguments

- recipe:

  A recipe object. The step will be added to the sequence of operations
  for this recipe.

- base:

  The base of the logarithm. Default is `exp(1)` (natural log). Use `10`
  for log10 transformation.

- offset:

  A numeric offset added to values before taking the log. Default is
  `0`. Use a small positive value (e.g., `1` for `log1p`) to handle zero
  or near-zero values.

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

This step applies the transformation:

\$\$y' = \log_b(y + \text{offset})\$\$

where \\b\\ is the base.

Log transformation is commonly used for:

- Variance stabilization

- Normalizing skewed distributions

- Converting multiplicative relationships to additive

**Warning**: Non-positive values (after offset) will produce `-Inf` or
`NaN`.

The measurement locations are preserved unchanged.

## See also

Other measure-preprocessing:
[`step_measure_absorbance()`](https://jameshwade.github.io/measure/dev/reference/step_measure_absorbance.md),
[`step_measure_calibrate_x()`](https://jameshwade.github.io/measure/dev/reference/step_measure_calibrate_x.md),
[`step_measure_calibrate_y()`](https://jameshwade.github.io/measure/dev/reference/step_measure_calibrate_y.md),
[`step_measure_derivative()`](https://jameshwade.github.io/measure/dev/reference/step_measure_derivative.md),
[`step_measure_derivative_gap()`](https://jameshwade.github.io/measure/dev/reference/step_measure_derivative_gap.md),
[`step_measure_emsc()`](https://jameshwade.github.io/measure/dev/reference/step_measure_emsc.md),
[`step_measure_kubelka_munk()`](https://jameshwade.github.io/measure/dev/reference/step_measure_kubelka_munk.md),
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

# Natural log transformation
rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
  update_role(id, new_role = "id") |>
  step_measure_input_long(transmittance, location = vars(channel)) |>
  step_measure_log(offset = 1) |>
  prep()

# Log10 transformation
rec2 <- recipe(water + fat + protein ~ ., data = meats_long) |>
  update_role(id, new_role = "id") |>
  step_measure_input_long(transmittance, location = vars(channel)) |>
  step_measure_log(base = 10) |>
  prep()
```
