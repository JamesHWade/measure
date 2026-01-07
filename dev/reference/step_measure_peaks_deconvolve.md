# Deconvolve Overlapping Peaks

`step_measure_peaks_deconvolve()` creates a *specification* of a recipe
step that resolves overlapping peaks using curve fitting. This step
requires peaks to have been detected first using
[`step_measure_peaks_detect()`](https://jameshwade.github.io/measure/dev/reference/step_measure_peaks_detect.md).

## Usage

``` r
step_measure_peaks_deconvolve(
  recipe,
  model = "gaussian",
  optimizer = "auto",
  max_iter = 500L,
  tol = 1e-06,
  n_starts = 5L,
  constrain_positions = TRUE,
  quality_threshold = 0.8,
  store_components = FALSE,
  smart_init = TRUE,
  peaks_col = ".peaks",
  measures_col = ".measures",
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_peaks_deconvolve")
)
```

## Arguments

- recipe:

  A recipe object.

- model:

  Peak model to use. Either a character string naming a registered model
  (`"gaussian"`, `"emg"`, `"bigaussian"`, `"lorentzian"`), or a
  `peak_model` object created directly. Use
  [`peak_models()`](https://jameshwade.github.io/measure/dev/reference/peak_models.md)
  to see all registered models. Default is `"gaussian"`.

- optimizer:

  Optimization method: `"auto"` (default), `"lbfgsb"`, `"multistart"`,
  or `"nelder_mead"`. Auto-selection chooses based on problem complexity
  and signal-to-noise ratio.

- max_iter:

  Maximum iterations for optimization. Default is 500.

- tol:

  Convergence tolerance. Default is 1e-6.

- n_starts:

  Number of random starts for `optimizer = "multistart"`. Default is 5.

- constrain_positions:

  Logical. If `TRUE`, enforce that peak centers maintain their relative
  ordering. Default is `TRUE`.

- quality_threshold:

  Minimum R-squared to accept fit. Fits below this threshold trigger a
  warning. Default is 0.8.

- store_components:

  Logical. If `TRUE`, store individual fitted peak curves in the output.
  Default is `FALSE`.

- smart_init:

  Logical. If `TRUE`, use smart initialization based on peak properties.
  Default is `TRUE`.

- peaks_col:

  Name of the peaks column. Default is ".peaks".

- measures_col:

  Name of the measures column. Default is ".measures".

- role:

  Not used.

- trained:

  Logical indicating if the step has been trained.

- skip:

  Logical. Should the step be skipped when baking?

- id:

  Unique step identifier.

## Value

An updated recipe with the new step added. The `.peaks` column will be
updated with deconvolved peak parameters, fitted areas, and quality
metrics.

## Details

Peak deconvolution fits mathematical models to overlapping peaks to
determine their individual contributions. This is essential for
quantitative analysis when peaks are not baseline-resolved.

**Peak Models:**

Built-in models (use
[`peak_models()`](https://jameshwade.github.io/measure/dev/reference/peak_models.md)
to see all):

- `"gaussian"`: Symmetric Gaussian (3 params: height, center, width)

- `"emg"`: Exponentially Modified Gaussian (4 params, handles tailing)

- `"bigaussian"`: Bi-Gaussian (4 params, flexible asymmetry)

- `"lorentzian"`: Lorentzian/Cauchy peak (3 params, heavier tails)

Technique packs may register additional models.

**Optimizers:**

- `"auto"`: Selects based on problem complexity and SNR

- `"lbfgsb"`: L-BFGS-B (fast, local optimization)

- `"multistart"`: Multiple L-BFGS-B runs from perturbed starts (robust)

- `"nelder_mead"`: Derivative-free Nelder-Mead simplex

**Quality Assessment:**

Each fit is assessed for quality. The `.peaks` tibble gains columns:

- `fit_r_squared`: R-squared of the overall fit

- `fit_quality`: Quality grade (A/B/C/D/F)

- `purity`: How much of signal at peak max comes from this peak

## See also

[`optimize_deconvolution()`](https://jameshwade.github.io/measure/dev/reference/optimize_deconvolution.md),
[`assess_deconv_quality()`](https://jameshwade.github.io/measure/dev/reference/assess_deconv_quality.md),
[`peak_models()`](https://jameshwade.github.io/measure/dev/reference/peak_models.md),
[`gaussian_peak_model()`](https://jameshwade.github.io/measure/dev/reference/gaussian_peak_model.md)

Other peak-operations:
[`step_measure_peaks_detect()`](https://jameshwade.github.io/measure/dev/reference/step_measure_peaks_detect.md),
[`step_measure_peaks_filter()`](https://jameshwade.github.io/measure/dev/reference/step_measure_peaks_filter.md),
[`step_measure_peaks_integrate()`](https://jameshwade.github.io/measure/dev/reference/step_measure_peaks_integrate.md),
[`step_measure_peaks_to_table()`](https://jameshwade.github.io/measure/dev/reference/step_measure_peaks_to_table.md)

## Examples

``` r
library(recipes)

# Create synthetic data with overlapping peaks
set.seed(42)
x <- seq(0, 20, by = 0.1)
y <- 1.5 * exp(-0.5 * ((x - 8) / 1)^2) +
  0.8 * exp(-0.5 * ((x - 12) / 1.5)^2) +
  rnorm(length(x), sd = 0.02)
df <- data.frame(id = "sample1", location = x, value = y)

# \donttest{
# Deconvolve overlapping peaks
rec <- recipe(~., data = df) |>
  update_role(id, new_role = "id") |>
  step_measure_input_long(value, location = vars(location)) |>
  step_measure_peaks_detect(min_height = 0.5, min_prominence = 0.3) |>
  step_measure_peaks_deconvolve(model = "gaussian") |>
  prep()

result <- bake(rec, new_data = NULL)
# Check fitted peaks
result$.peaks[[1]]
#> # A tibble: 2 × 10
#>   peak_id location height left_base right_base  area fit_r_squared fit_quality
#>     <int>    <dbl>  <dbl>     <dbl>      <dbl> <dbl>         <dbl> <chr>      
#> 1       1     8.00  1.50       6.49       9.50  3.77         0.998 A          
#> 2       2    12.0   0.792      9.74      14.3   2.99         0.998 A          
#> # ℹ 2 more variables: purity <dbl>, overlap_degree <dbl>
# }
```
