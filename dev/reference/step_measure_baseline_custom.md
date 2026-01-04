# Custom Baseline Correction with User-Provided Function

`step_measure_baseline_custom()` creates a *specification* of a recipe
step that applies a user-provided function for baseline correction. This
allows for flexible, custom baseline estimation algorithms.

## Usage

``` r
step_measure_baseline_custom(
  recipe,
  .fn,
  ...,
  subtract = TRUE,
  measures = NULL,
  tunable = NULL,
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_baseline_custom")
)
```

## Arguments

- recipe:

  A recipe object. The step will be added to the sequence of operations
  for this recipe.

- .fn:

  A function or formula for baseline estimation. The function should
  accept a `measure_tbl` (tibble with `location` and `value` columns)
  and return a numeric vector of baseline values with the same length as
  the input. Formulas are converted to functions via
  [`rlang::as_function()`](https://rlang.r-lib.org/reference/as_function.html),
  where `.x` represents the `measure_tbl`.

- ...:

  Additional arguments passed to `.fn`. These are captured as quosures
  and evaluated at bake time.

- subtract:

  If `TRUE` (default), the baseline is subtracted from the signal. If
  `FALSE`, the baseline values replace the original values (useful for
  extracting baselines).

- measures:

  An optional character vector of measure column names to process. If
  `NULL` (the default), all measure columns (columns with class
  `measure_list`) will be processed.

- tunable:

  An optional named list specifying which arguments in `...` are
  tunable. Each element should be a list with `pkg`, `fun`, and
  optionally `range`. See Details.

- role:

  Not used by this step since no new variables are created.

- trained:

  A logical to indicate if the quantities for preprocessing have been
  estimated.

- skip:

  A logical. Should the step be skipped when the recipe is baked?

- id:

  A character string that is unique to this step to identify it.

## Value

An updated version of `recipe` with the new step added to the sequence
of any existing operations.

## Details

This step allows you to use any baseline estimation algorithm by
providing a custom function. The function receives a `measure_tbl`
object (a tibble with `location` and `value` columns) and should return
a numeric vector of the estimated baseline values.

### Function Contract

Your function should:

- Accept a `measure_tbl` as its first argument

- Return a numeric vector of the same length as `nrow(measure_tbl)`

- Handle NA values appropriately

### Formula Interface

You can use a formula instead of a function. The formula is converted to
a function where `.x` represents the `measure_tbl`:

    # These are equivalent:
    step_measure_baseline_custom(.fn = function(x) mean(x$value))
    step_measure_baseline_custom(.fn = ~ mean(.x$value))

### Tunability

To make parameters tunable with `dials`, provide a `tunable` argument:

    step_measure_baseline_custom(
      .fn = ~ stats::loess(.x$value ~ .x$location, span = span)$fitted,
      span = 0.5,
      tunable = list(
        span = list(pkg = "dials", fun = "degree", range = c(0.1, 0.9))
      )
    )

**No selectors should be supplied to this step function**. The data
should be in the internal format produced by
[`step_measure_input_wide()`](https://jameshwade.github.io/measure/dev/reference/step_measure_input_wide.md)
or
[`step_measure_input_long()`](https://jameshwade.github.io/measure/dev/reference/step_measure_input_long.md).

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/reference/tidy.recipe.html)
this step, a tibble with columns `terms`, `subtract`, and `id` is
returned.

## See also

[`step_measure_baseline_als()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_als.md),
[`step_measure_baseline_poly()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_poly.md)
for built-in baseline correction methods.

Other measure-baseline:
[`step_measure_baseline_airpls()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_airpls.md),
[`step_measure_baseline_als()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_als.md),
[`step_measure_baseline_arpls()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_arpls.md),
[`step_measure_baseline_auto()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_auto.md),
[`step_measure_baseline_gpc()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_gpc.md),
[`step_measure_baseline_minima()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_minima.md),
[`step_measure_baseline_morph()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_morph.md),
[`step_measure_baseline_poly()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_poly.md),
[`step_measure_baseline_py()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_py.md),
[`step_measure_baseline_rf()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_rf.md),
[`step_measure_baseline_rolling()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_rolling.md),
[`step_measure_baseline_snip()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_snip.md),
[`step_measure_baseline_tophat()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_tophat.md),
[`step_measure_detrend()`](https://jameshwade.github.io/measure/dev/reference/step_measure_detrend.md)

## Examples

``` r
library(recipes)

# Simple polynomial baseline using a function
poly_baseline <- function(x) {
  fit <- lm(x$value ~ poly(x$location, 2))
  predict(fit)
}

rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
  update_role(id, new_role = "id") |>
  step_measure_input_long(transmittance, location = vars(channel)) |>
  step_measure_baseline_custom(.fn = poly_baseline) |>
  prep()

bake(rec, new_data = NULL)
#> # A tibble: 215 × 6
#>       id water   fat protein .measures channel    
#>    <int> <dbl> <dbl>   <dbl>    <meas> <list>     
#>  1     1  60.5  22.5    16.7 [100 × 2] <int [100]>
#>  2     2  46    40.1    13.5 [100 × 2] <int [100]>
#>  3     3  71     8.4    20.5 [100 × 2] <int [100]>
#>  4     4  72.8   5.9    20.7 [100 × 2] <int [100]>
#>  5     5  58.3  25.5    15.5 [100 × 2] <int [100]>
#>  6     6  44    42.7    13.7 [100 × 2] <int [100]>
#>  7     7  44    42.7    13.7 [100 × 2] <int [100]>
#>  8     8  69.3  10.6    19.3 [100 × 2] <int [100]>
#>  9     9  61.4  19.9    17.7 [100 × 2] <int [100]>
#> 10    10  61.4  19.9    17.7 [100 × 2] <int [100]>
#> # ℹ 205 more rows

# Using formula interface with additional parameters
rec2 <- recipe(water + fat + protein ~ ., data = meats_long) |>
  update_role(id, new_role = "id") |>
  step_measure_input_long(transmittance, location = vars(channel)) |>
  step_measure_baseline_custom(
    .fn = ~ stats::loess(.x$value ~ .x$location, span = span)$fitted,
    span = 0.5
  ) |>
  prep()
```
