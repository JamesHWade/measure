# Apply a Custom Function to Measurements

`step_measure_map()` creates a *specification* of a recipe step that
applies a custom function to each sample's measurements. Use this when
the built-in preprocessing steps (SNV, MSC, Savitzky-Golay) don't cover
your needs.

## Usage

``` r
step_measure_map(
  recipe,
  fn,
  ...,
  measures = NULL,
  verbosity = 1L,
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_map")
)
```

## Arguments

- recipe:

  A recipe object. The step will be added to the sequence of operations
  for this recipe.

- fn:

  A function to apply to each sample's measurement tibble. The

  function should accept a tibble with `location` and `value` columns
  and return a tibble with the same structure. Can also be a formula
  (e.g., `~ { .x$value <- log1p(.x$value); .x }`) which will be
  converted via
  [`rlang::as_function()`](https://rlang.r-lib.org/reference/as_function.html).

- ...:

  Additional arguments passed to `fn` during baking.

- measures:

  An optional character vector of measure column names to

  process. If `NULL` (the default), all measure columns will be
  processed.

- verbosity:

  An integer controlling output verbosity:

  - `0`: Silent - suppress all messages and output from `fn`

  - `1`: Normal (default) - show output from `fn`

- role:

  Not used by this step since no new variables are created.

- trained:

  A logical to indicate if the step has been trained.

- skip:

  A logical. Should the step be skipped when the recipe is baked?

- id:

  A character string that is unique to this step.

## Value

An updated version of `recipe` with the new step added.

## Details

This step is the "escape hatch" for custom sample-wise transformations
that aren't covered by the built-in steps. It integrates fully with the
recipes framework, meaning your custom transformation will be:

- Applied consistently during
  [`prep()`](https://recipes.tidymodels.org/reference/prep.html) and
  [`bake()`](https://recipes.tidymodels.org/reference/bake.html)

- Included when bundling recipes into workflows

- Reproducible across sessions

### Function Requirements

The function `fn` must:

- Accept a tibble with `location` and `value` columns

- Return a tibble with `location` and `value` columns

- Not change the number of rows (measurements must remain aligned)

### When to Use This Step

Use `step_measure_map()` for domain-specific transformations not covered
by the built-in steps:

- Custom baseline correction algorithms

- Specialized normalization methods

- Instrument-specific corrections

- Experimental preprocessing techniques

For common operations, prefer the built-in steps:

- Scatter correction →
  [`step_measure_snv()`](https://jameshwade.github.io/measure/dev/reference/step_measure_snv.md)
  or
  [`step_measure_msc()`](https://jameshwade.github.io/measure/dev/reference/step_measure_msc.md)

- Smoothing/derivatives →
  [`step_measure_savitzky_golay()`](https://jameshwade.github.io/measure/dev/reference/step_measure_savitzky_golay.md)

### Prototyping with measure_map()

When developing a custom transformation, you may find it helpful to
prototype using
[`measure_map()`](https://jameshwade.github.io/measure/dev/reference/measure_map.md)
on baked data before wrapping it in a step. Once your function works
correctly, use \`step_measure\_

for production pipelines.

## See also

- [`step_measure_snv()`](https://jameshwade.github.io/measure/dev/reference/step_measure_snv.md),
  [`step_measure_msc()`](https://jameshwade.github.io/measure/dev/reference/step_measure_msc.md),
  [`step_measure_savitzky_golay()`](https://jameshwade.github.io/measure/dev/reference/step_measure_savitzky_golay.md)
  for built-in preprocessing steps

- [`measure_map()`](https://jameshwade.github.io/measure/dev/reference/measure_map.md)
  for prototyping custom transformations

Other measure-preprocessing:
[`step_measure_absorbance()`](https://jameshwade.github.io/measure/dev/reference/step_measure_absorbance.md),
[`step_measure_calibrate_x()`](https://jameshwade.github.io/measure/dev/reference/step_measure_calibrate_x.md),
[`step_measure_calibrate_y()`](https://jameshwade.github.io/measure/dev/reference/step_measure_calibrate_y.md),
[`step_measure_derivative()`](https://jameshwade.github.io/measure/dev/reference/step_measure_derivative.md),
[`step_measure_derivative_gap()`](https://jameshwade.github.io/measure/dev/reference/step_measure_derivative_gap.md),
[`step_measure_kubelka_munk()`](https://jameshwade.github.io/measure/dev/reference/step_measure_kubelka_munk.md),
[`step_measure_log()`](https://jameshwade.github.io/measure/dev/reference/step_measure_log.md),
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

# Example 1: Custom log transformation
log_transform <- function(x) {
  x$value <- log1p(x$value)
  x
}

rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
  update_role(id, new_role = "id") |>
  step_measure_input_long(transmittance, location = vars(channel)) |>
  step_measure_map(log_transform) |>
  step_measure_snv() |>
  prep()

bake(rec, new_data = NULL)
#> # A tibble: 215 × 5
#>       id water   fat protein .measures
#>    <int> <dbl> <dbl>   <dbl>    <meas>
#>  1     1  60.5  22.5    16.7 [100 × 2]
#>  2     2  46    40.1    13.5 [100 × 2]
#>  3     3  71     8.4    20.5 [100 × 2]
#>  4     4  72.8   5.9    20.7 [100 × 2]
#>  5     5  58.3  25.5    15.5 [100 × 2]
#>  6     6  44    42.7    13.7 [100 × 2]
#>  7     7  44    42.7    13.7 [100 × 2]
#>  8     8  69.3  10.6    19.3 [100 × 2]
#>  9     9  61.4  19.9    17.7 [100 × 2]
#> 10    10  61.4  19.9    17.7 [100 × 2]
#> # ℹ 205 more rows

# Example 2: Using formula syntax for inline transformations
rec2 <- recipe(water + fat + protein ~ ., data = meats_long) |>
  update_role(id, new_role = "id") |>
  step_measure_input_long(transmittance, location = vars(channel)) |>
  step_measure_map(~ {
    # Subtract minimum to remove offset
    .x$value <- .x$value - min(.x$value)
    .x
  }) |>
  prep()

# Example 3: Using external package functions
# (e.g., custom baseline from a spectroscopy package)
if (FALSE) { # \dontrun{
rec3 <- recipe(water + fat + protein ~ ., data = meats_long) |>
  update_role(id, new_role = "id") |>
  step_measure_input_long(transmittance, location = vars(channel)) |>
  step_measure_map(my_baseline_correction, method = "als") |>
  step_measure_output_wide()
} # }
```
