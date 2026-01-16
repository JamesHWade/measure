# Adaptive Iteratively Reweighted Penalized Least Squares Baseline

`step_measure_baseline_airpls()` creates a *specification* of a recipe
step that applies airPLS baseline correction. This method automatically
adjusts weights based on the difference between the signal and fitted
baseline.

## Usage

``` r
step_measure_baseline_airpls(
  recipe,
  measures = NULL,
  lambda = 1e+05,
  max_iter = 50L,
  tol = 0.001,
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_baseline_airpls")
)
```

## Arguments

- recipe:

  A recipe object.

- measures:

  An optional character vector of measure column names.

- lambda:

  Smoothness parameter. Higher values produce smoother baselines.
  Default is 1e5. Tunable via
  [`baseline_lambda()`](https://jameshwade.github.io/measure/dev/reference/baseline_lambda.md).

- max_iter:

  Maximum number of iterations. Default is 50.

- tol:

  Convergence tolerance for weight changes. Default is 1e-3.

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

airPLS (Adaptive Iteratively Reweighted Penalized Least Squares) is an
improvement over standard ALS that automatically adapts the asymmetry
parameter based on the residuals. Key features:

- No need to manually set asymmetry parameter

- Good for signals with varying baseline curvature

- Robust to different peak heights

## References

Zhang, Z.M., Chen, S., & Liang, Y.Z. (2010). Baseline correction using
adaptive iteratively reweighted penalized least squares. Analyst, 135,
1138-1146.

## See also

Other measure-baseline:
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
  step_measure_baseline_airpls(lambda = 1e5) |>
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
