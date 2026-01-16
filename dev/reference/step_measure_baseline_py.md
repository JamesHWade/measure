# Python-Based Baseline Correction via pybaselines

`step_measure_baseline_py()` creates a *specification* of a recipe step
that applies baseline correction using the Python pybaselines library,
which provides 50+ baseline correction algorithms.

## Usage

``` r
step_measure_baseline_py(
  recipe,
  method = "asls",
  ...,
  subtract = TRUE,
  measures = NULL,
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_baseline_py")
)
```

## Arguments

- recipe:

  A recipe object. The step will be added to the sequence of operations
  for this recipe.

- method:

  The pybaselines method to use. Common methods include:

  - Whittaker methods: `"asls"`, `"iasls"`, `"airpls"`, `"arpls"`,
    `"drpls"`, `"psalsa"`

  - Polynomial methods: `"poly"`, `"modpoly"`, `"imodpoly"`, `"loess"`,
    `"quant_reg"`

  - Morphological: `"mor"`, `"imor"`, `"rolling_ball"`, `"tophat"`

  - Spline: `"pspline_asls"`, `"pspline_airpls"`, `"mixture_model"`

  - Smooth: `"snip"`, `"swima"`, `"noise_median"`

  - Classification: `"dietrich"`, `"golotvin"`, `"fastchrom"`

  - See pybaselines documentation for the full list.

- ...:

  Additional arguments passed to the pybaselines method. Common
  parameters include:

  - `lam`: Smoothness parameter for Whittaker methods (default varies by
    method)

  - `p`: Asymmetry parameter for ALS methods (default ~0.01)

  - `poly_order`: Polynomial degree for polynomial methods

  - `half_window`: Window size for morphological methods

  - `max_half_window`: Maximum window for SNIP method

- subtract:

  If `TRUE` (default), the baseline is subtracted from the signal. If
  `FALSE`, the baseline values replace the original values (useful for
  extracting baselines).

- measures:

  An optional character vector of measure column names to process. If
  `NULL` (the default), all measure columns (columns with class
  `measure_list`) will be processed.

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

This step provides access to the comprehensive pybaselines Python
library, which implements over 50 baseline correction algorithms across
several categories:

### Whittaker Methods

Based on penalized least squares with asymmetric weights:

- `asls`: Asymmetric Least Squares (good general-purpose method)

- `iasls`: Improved ALS with automatic smoothness selection

- `airpls`: Adaptive iteratively reweighted penalized least squares

- `arpls`: Asymmetrically reweighted penalized least squares

- `psalsa`: Peaked Signal's Asymmetric Least Squares Algorithm

### Polynomial Methods

Fit polynomials to baseline regions:

- `poly`: Simple polynomial fitting

- `modpoly`: Modified polynomial (iterative)

- `imodpoly`: Improved modified polynomial

- `loess`: Local regression (LOESS)

### Morphological Methods

Based on mathematical morphology:

- `mor`: Morphological opening

- `imor`: Improved morphological

- `rolling_ball`: Rolling ball algorithm

- `tophat`: Top-hat transform

### Requirements

This step requires the `reticulate` package and Python with pybaselines
installed. Install pybaselines with:

    reticulate::py_require("pybaselines")

**No selectors should be supplied to this step function**. The data
should be in the internal format produced by
[`step_measure_input_wide()`](https://jameshwade.github.io/measure/dev/reference/step_measure_input_wide.md)
or
[`step_measure_input_long()`](https://jameshwade.github.io/measure/dev/reference/step_measure_input_long.md).

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/reference/tidy.recipe.html)
this step, a tibble with columns `terms`, `method`, `subtract`, and `id`
is returned.

## See also

[`step_measure_baseline_als()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_als.md),
[`step_measure_baseline_custom()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_custom.md)
for R-based alternatives.

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
[`step_measure_baseline_poly()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_poly.md),
[`step_measure_baseline_rf()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_rf.md),
[`step_measure_baseline_rolling()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_rolling.md),
[`step_measure_baseline_snip()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_snip.md),
[`step_measure_baseline_tophat()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_tophat.md),
[`step_measure_detrend()`](https://jameshwade.github.io/measure/dev/reference/step_measure_detrend.md)

## Examples

``` r
if (FALSE) { # measure:::.pybaselines_available()
library(recipes)

# \donttest{
# Asymmetric Least Squares baseline correction
rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
  update_role(id, new_role = "id") |>
  step_measure_input_long(transmittance, location = vars(channel)) |>
  step_measure_baseline_py(method = "asls", lam = 1e6, p = 0.01) |>
  prep()

bake(rec, new_data = NULL)

# Using SNIP algorithm
rec2 <- recipe(water + fat + protein ~ ., data = meats_long) |>
  update_role(id, new_role = "id") |>
  step_measure_input_long(transmittance, location = vars(channel)) |>
  step_measure_baseline_py(method = "snip", max_half_window = 40) |>
  prep()
# }
}
```
