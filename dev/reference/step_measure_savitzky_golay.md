# Savitzky-Golay Pre-Processing

`step_measure_savitzky_golay` creates a *specification* of a recipe step
that smooths and filters the measurement sequence.

## Usage

``` r
step_measure_savitzky_golay(
  recipe,
  measures = NULL,
  role = NA,
  trained = FALSE,
  degree = 3,
  window_side = 11,
  differentiation_order = 0,
  skip = FALSE,
  id = rand_id("measure_savitzky_golay")
)
```

## Arguments

- recipe:

  A recipe object. The step will be added to the sequence of operations
  for this recipe.

- measures:

  An optional character vector of measure column names to process. If
  `NULL` (the default), all measure columns (columns with class
  `measure_list`) will be processed. Use this to limit processing to
  specific measure columns when working with multiple measurement types.

- role:

  Not used by this step since no new variables are created.

- trained:

  A logical to indicate if the quantities for preprocessing have been
  estimated.

- degree:

  An integer for the polynomial degree to use for smoothing.

- window_side:

  An integer for how many units there are on each side of the window.
  This means that `window_side = 1` has a total window width of 3 (e.g.,
  width is `2 * window_side + 1`).

- differentiation_order:

  An integer for the degree of filtering (zero indicates no
  differentiation).

- skip:

  A logical. Should the step be skipped when the recipe is baked by
  [`recipes::bake()`](https://recipes.tidymodels.org/reference/bake.html)?
  While all operations are baked when
  [`recipes::prep()`](https://recipes.tidymodels.org/reference/prep.html)
  is run, some operations may not be able to be conducted on new data
  (e.g. processing the outcome variable(s)). Care should be taken when
  using `skip = TRUE` as it may affect the computations for subsequent
  operations

- id:

  A character string that is unique to this step to identify it.

## Value

An updated version of `recipe` with the new step added to the sequence
of any existing operations.

## Details

This method can both smooth out random noise and reduce
between-predictor correlation. It fits a polynomial to a window of
measurements and this results in fewer measurements than the input.
Measurements are assumed to be equally spaced.

The polynomial degree should be less than the window size. Also, window
size must be greater than polynomial degree. If either case is true, the
original argument values are increased to satisfy these conditions (with
a warning).

**No selectors should be supplied to this step function**. The data
should be in a special internal format produced by
[`step_measure_input_wide()`](https://jameshwade.github.io/measure/dev/reference/step_measure_input_wide.md)
or
[`step_measure_input_long()`](https://jameshwade.github.io/measure/dev/reference/step_measure_input_long.md).

The measurement locations are reset to integer indices starting at one.

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/reference/tidy.recipe.html)
this step, a tibble with columns is returned.

## Examples

``` r
if (rlang::is_installed("prospectr")) {
  rec <-
    recipe(water + fat + protein ~ ., data = meats_long) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    step_measure_savitzky_golay(
      differentiation_order = 1,
      degree = 3,
      window_side = 5
    ) %>%
    prep()
}
```
