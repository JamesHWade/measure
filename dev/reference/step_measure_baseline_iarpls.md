# Improved arPLS Baseline Correction (Two-Stage)

`step_measure_baseline_iarpls()` creates a *specification* of a recipe
step that applies Improved arPLS baseline correction using a two-stage
approach.

## Usage

``` r
step_measure_baseline_iarpls(
  recipe,
  measures = NULL,
  lambda = 1e+06,
  lambda_1 = 10000,
  max_iter = 10L,
  tol = 1e-05,
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_baseline_iarpls")
)
```

## Arguments

- recipe:

  A recipe object.

- measures:

  An optional character vector of measure column names.

- lambda:

  Final smoothness parameter. Default is 1e6.

- lambda_1:

  First stage (coarse) smoothness parameter. Default is 1e4.

- max_iter:

  Maximum number of iterations. Default is 10.

- tol:

  Convergence tolerance. Default is 1e-5.

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

iarpls uses a two-stage approach:

1.  First stage with smaller lambda for coarse baseline estimation

2.  Second stage with larger lambda for refined baseline using weights
    derived from the first stage

This approach often provides better results than single-stage arPLS for
signals with complex baseline patterns.

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
  step_measure_baseline_iarpls(lambda = 1e6, lambda_1 = 1e4) |>
  prep()
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 5.95e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 6.37e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.0419 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.0508 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 4.37e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 2.7e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 6.03e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.00106 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.000711 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 4.87e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 2.6e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.045 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.0835 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.0761 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.0733 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.0817 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.0227 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 1.86e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.00403 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.125 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.0109 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 1.29e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.0167 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.00196 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.00109 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 1.77e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 1.77e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.0356 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 6.96e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.000425 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.000251 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.000175 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.000102 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.000136 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 5.77e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.000353 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.000145 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 1.8e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 1.46e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 6.94e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.00171 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.00613 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.00382 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.034 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.0134 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 2.6e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.212 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.000339 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.0761 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.0727 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.0733 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.194 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 1.4e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.0138 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.0255 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.016 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.016 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.0223 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 2.15e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 1.44e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 1.55e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.000568 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.0135 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 1.22e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.024 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.0255 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 4.45e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.000983 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.00543 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.00916 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.0334 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.0391 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.0158 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 2.14e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.0369 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.0136 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 2.86e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.0237 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 1.02e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 2.79e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 2.66e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.00785 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 2.81e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 3.12e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 1.96e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 2.09e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.0163 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.00237 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.108 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 1.03e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.0204 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.0467 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 1.38e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 5.43e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.0772 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 4.1e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 2.91e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.00375 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 4.02e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.000136 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 3.39e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 1.01e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 3.67e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 3.38e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 4.72e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 5.77e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.000353 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.000173 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 3.61e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 3.24e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 7.22e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.000173 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 1.8e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 4.63e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 3.7e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 4.61e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 5.5e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.00264 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 8.61e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 3.88e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.00814 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.000159 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.0828 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 4.45e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 1.48e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.00196 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 1.71e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.000157 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.000163 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.000145 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.000203 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.025 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.000399 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.108 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.0834 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.09 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.0917 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 2.15e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.185 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.018 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.0181 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.0384 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 4.24e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.00277 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.0236 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 8.34e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 1.19e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.00167 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 5.82e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 5.62e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 3.43e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.000108 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.000206 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 7.74e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 4.58e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.000212 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.000378 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 5.81e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 1.47e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.000399 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.176 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 2.18e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.157 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 2.86e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 1.02e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 5.35e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.000139 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 3.21e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.000124 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.000284 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.0326 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.045 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 4.41e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.0817 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.00768 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.0509 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.00449 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.118 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 1.67e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.0402 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.0138 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.000259 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.0192 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 3.07e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.000144 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 1.71e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 4.09e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.0235 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 4.46e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 9.36e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 7.31e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.000135 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 4.71e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 2.67e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 2.4e-05 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.00345 (tol: 1e-05)
#> Warning: iarpls did not converge after 10 iterations.
#> ℹ Final change: 0.000284 (tol: 1e-05)

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
