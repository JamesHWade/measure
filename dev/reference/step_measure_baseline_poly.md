# Polynomial Baseline Correction

`step_measure_baseline_poly()` creates a *specification* of a recipe
step that applies polynomial baseline correction to measurement data.
The method fits a polynomial to the spectrum, optionally with iterative
peak exclusion.

## Usage

``` r
step_measure_baseline_poly(
  recipe,
  measures = NULL,
  degree = 2L,
  max_iter = 0L,
  threshold = 1.5,
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_baseline_poly")
)
```

## Arguments

- recipe:

  A recipe object. The step will be added to the sequence of operations
  for this recipe.

- measures:

  An optional character vector of measure column names to process. If
  `NULL` (the default), all measure columns (columns with class
  `measure_list`) will be processed.

- degree:

  Polynomial degree for baseline fitting. Default is `2` (quadratic).
  Higher degrees fit more complex baselines but risk overfitting.
  Tunable via
  [`baseline_degree()`](https://jameshwade.github.io/measure/dev/reference/baseline_lambda.md).

- max_iter:

  Maximum number of iterations for peak exclusion. Default is `0` (no
  iteration, fit polynomial to all points). Set to a positive integer to
  iteratively exclude points above the fitted baseline.

- threshold:

  Number of standard deviations above baseline for a point to be
  excluded in iterative fitting. Default is `1.5`. Only used when
  `max_iter > 0`.

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

Polynomial baseline correction fits a polynomial function to the
spectrum and subtracts it. This is effective for removing smooth, curved
baselines caused by instrumental drift, scattering, or other slowly
varying effects.

When `max_iter > 0`, the algorithm uses iterative peak exclusion:

1.  Fit polynomial to all points

2.  Calculate residuals (spectrum - baseline)

3.  Exclude points where residual \> threshold \* SD(residuals)

4.  Refit polynomial to remaining points

5.  Repeat until convergence or max_iter reached

This iterative approach prevents peaks from pulling up the baseline
estimate.

**Degree selection:**

- `degree = 1`: Linear baseline (for simple drift)

- `degree = 2`: Quadratic (most common, handles gentle curvature)

- `degree = 3-5`: Higher-order (for complex baselines, use cautiously)

**No selectors should be supplied to this step function**. The data
should be in the internal format produced by
[`step_measure_input_wide()`](https://jameshwade.github.io/measure/dev/reference/step_measure_input_wide.md)
or
[`step_measure_input_long()`](https://jameshwade.github.io/measure/dev/reference/step_measure_input_long.md).

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/reference/tidy.recipe.html)
this step, a tibble with columns `terms`, `degree`, and `id` is
returned.

## See also

Other measure-baseline:
[`step_measure_baseline_airpls()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_airpls.md),
[`step_measure_baseline_als()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_als.md),
[`step_measure_baseline_arpls()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_arpls.md),
[`step_measure_baseline_aspls()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_aspls.md),
[`step_measure_baseline_auto()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_auto.md),
[`step_measure_baseline_custom()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_custom.md),
[`step_measure_baseline_fastchrom()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_fastchrom.md),
[`step_measure_baseline_gpc()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_gpc.md),
[`step_measure_baseline_iarpls()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_iarpls.md),
[`step_measure_baseline_minima()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_minima.md),
[`step_measure_baseline_morph()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_morph.md),
[`step_measure_baseline_morphological()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_morphological.md),
[`step_measure_baseline_py()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_py.md),
[`step_measure_baseline_rf()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_rf.md),
[`step_measure_baseline_rolling()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_rolling.md),
[`step_measure_baseline_snip()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_snip.md),
[`step_measure_baseline_tophat()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_tophat.md),
[`step_measure_detrend()`](https://jameshwade.github.io/measure/dev/reference/step_measure_detrend.md)

## Examples

``` r
library(recipes)

# Simple polynomial baseline (no iteration)
rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
  update_role(id, new_role = "id") |>
  step_measure_input_long(transmittance, location = vars(channel)) |>
  step_measure_baseline_poly(degree = 2) |>
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

# With iterative peak exclusion
rec2 <- recipe(water + fat + protein ~ ., data = meats_long) |>
  update_role(id, new_role = "id") |>
  step_measure_input_long(transmittance, location = vars(channel)) |>
  step_measure_baseline_poly(degree = 3, max_iter = 5, threshold = 2) |>
  prep()
```
