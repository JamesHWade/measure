# Subtract Blank Measurement

`step_measure_subtract_blank()` creates a *specification* of a recipe
step that subtracts or divides by a blank measurement. The blank can be
provided externally or learned from training data.

## Usage

``` r
step_measure_subtract_blank(
  recipe,
  blank = NULL,
  blank_col = NULL,
  blank_value = NULL,
  method = "subtract",
  measures = NULL,
  role = NA,
  trained = FALSE,
  learned_blank = NULL,
  skip = FALSE,
  id = recipes::rand_id("measure_subtract_blank")
)
```

## Arguments

- recipe:

  A recipe object. The step will be added to the sequence of operations
  for this recipe.

- blank:

  An optional external blank to use. Can be:

  - A `measure_tbl` object with `location` and `value` columns

  - A numeric vector (must match the number of locations in data)

  - A data.frame with `location` and `value` columns (will be
    interpolated) If `NULL`, the blank is learned from training data
    using `blank_col` and `blank_value`.

- blank_col:

  An optional column name (unquoted) that identifies sample types. Used
  with `blank_value` to identify blank samples in training data.

- blank_value:

  The value in `blank_col` that identifies blank samples. When the step
  is prepped, the mean of all blank samples is computed and stored for
  use during baking.

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

- learned_blank:

  A named list containing the learned blank values for each measure
  column. This is `NULL` until the step is trained.

- skip:

  A logical. Should the step be skipped when the recipe is baked?

- id:

  A character string that is unique to this step to identify it.

## Value

An updated version of `recipe` with the new step added to the sequence
of any existing operations.

## Details

Blank subtraction is a fundamental preprocessing step in analytical
chemistry. It removes background signal that is present in all
measurements but is not related to the analyte of interest.

**Two modes of operation:**

1.  **External blank**: You provide a blank spectrum directly via the
    `blank` argument. This is useful when you have a known reference
    blank.

2.  **Learned blank**: You specify which samples are blanks in your
    training data using `blank_col` and `blank_value`. During
    [`prep()`](https://recipes.tidymodels.org/reference/prep.html), the
    mean of all blank samples is computed and stored. This approach is
    useful for batch-specific blank correction.

**Common use cases:**

- UV-Vis: Remove solvent absorbance

- IR: Remove atmospheric CO2/H2O interference

- Fluorescence: Remove buffer background and Raman scatter

- Chromatography: Remove ghost peaks and solvent artifacts

**No selectors should be supplied to this step function**. The data
should be in the internal format produced by
[`step_measure_input_wide()`](https://jameshwade.github.io/measure/dev/reference/step_measure_input_wide.md)
or
[`step_measure_input_long()`](https://jameshwade.github.io/measure/dev/reference/step_measure_input_long.md).

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/reference/tidy.recipe.html)
this step, a tibble with columns `terms`, `method`, `blank_source`, and
`id` is returned.

## See also

[`step_measure_subtract_reference()`](https://jameshwade.github.io/measure/dev/reference/step_measure_subtract_reference.md)
for simpler external reference

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
[`step_measure_subtract_reference()`](https://jameshwade.github.io/measure/dev/reference/step_measure_subtract_reference.md),
[`step_measure_transmittance()`](https://jameshwade.github.io/measure/dev/reference/step_measure_transmittance.md)

## Examples

``` r
library(recipes)

# Example with external blank (numeric vector)
blank_spectrum <- rep(0.1, 100)

rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
  update_role(id, new_role = "id") |>
  step_measure_input_long(transmittance, location = vars(channel)) |>
  step_measure_subtract_blank(blank = blank_spectrum)

# Example learning blank from training data
# (assuming sample_type column with "blank" values)
# rec <- recipe(outcome ~ ., data = my_data) |>
#   step_measure_input_long(...) |>
#   step_measure_subtract_blank(blank_col = sample_type, blank_value = "blank")
```
