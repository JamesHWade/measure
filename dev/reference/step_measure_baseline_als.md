# Asymmetric Least Squares (ALS) Baseline Correction

`step_measure_baseline_als()` creates a *specification* of a recipe step
that applies Asymmetric Least Squares baseline correction to measurement
data. ALS iteratively fits a smooth baseline giving less weight to
points above the baseline (peaks).

## Usage

``` r
step_measure_baseline_als(
  recipe,
  measures = NULL,
  lambda = 1e+06,
  p = 0.01,
  max_iter = 20L,
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_baseline_als")
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

- lambda:

  Smoothness parameter (2nd derivative constraint). Higher values
  produce smoother baselines. Default is `1e6`. Typical range is 1e3 to
  1e9. Tunable via
  [`baseline_lambda()`](https://jameshwade.github.io/measure/dev/reference/baseline_lambda.md).

- p:

  Asymmetry parameter controlling weight for positive residuals. Values
  near 0 (e.g., 0.001-0.05) work well for spectra with peaks above
  baseline. Default is `0.01`. Tunable via
  [`baseline_asymmetry()`](https://jameshwade.github.io/measure/dev/reference/baseline_lambda.md).

- max_iter:

  Maximum number of iterations. Default is `20`.

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

Asymmetric Least Squares (ALS) baseline correction uses a Whittaker
smoother with asymmetric weights to fit a baseline that follows the
lower envelope of the spectrum. The algorithm iteratively:

1

. Fits a smooth baseline using penalized least squares 2. Calculates
residuals (spectrum - baseline) 3. Assigns weights: `p` for positive
residuals (peaks), `1-p` for negative 4. Repeats until convergence or
max iterations

The smoothness is controlled by `lambda`, which penalizes the second
derivative of the baseline. Larger `lambda` produces smoother baselines.

ALS is particularly effective for:

- NIR/IR spectroscopy with broad baseline drift

- Raman spectroscopy with fluorescence background

- UV-Vis spectroscopy with scattering effects

**No selectors should be supplied to this step function**. The data
should be in the internal format produced by
[`step_measure_input_wide()`](https://jameshwade.github.io/measure/dev/reference/step_measure_input_wide.md)
or
[`step_measure_input_long()`](https://jameshwade.github.io/measure/dev/reference/step_measure_input_long.md).

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/reference/tidy.recipe.html)
this step, a tibble with columns `terms`, `lambda`, `p`, and `id` is
returned.

## Tuning

This step has parameters that can be tuned:

- `lambda`: Use
  [`baseline_lambda()`](https://jameshwade.github.io/measure/dev/reference/baseline_lambda.md)
  (log10 scale recommended)

- `p`: Use
  [`baseline_asymmetry()`](https://jameshwade.github.io/measure/dev/reference/baseline_lambda.md)

## References

Eilers, P.H.C. and Boelens, H.F.M. (2005). Baseline Correction with
Asymmetric Least Squares Smoothing. Leiden University Medical Centre
report.

## See also

Other measure-baseline:
[`step_measure_baseline_airpls()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_airpls.md),
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
[`step_measure_baseline_py()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_py.md),
[`step_measure_baseline_rf()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_rf.md),
[`step_measure_baseline_rolling()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_rolling.md),
[`step_measure_baseline_snip()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_snip.md),
[`step_measure_baseline_tophat()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_tophat.md),
[`step_measure_detrend()`](https://jameshwade.github.io/measure/dev/reference/step_measure_detrend.md)

## Examples

``` r
library(recipes)

# \donttest{
rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
  update_role(id, new_role = "id") |>
  step_measure_input_long(transmittance, location = vars(channel)) |>
  step_measure_baseline_als(lambda = 1e6, p = 0.01) |>
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
# }
```
