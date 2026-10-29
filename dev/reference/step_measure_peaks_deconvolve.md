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

  Maximum iterations for optimization. Default is 500. Tune this with
  [`peak_fit_iterations()`](https://jameshwade.github.io/measure/dev/reference/peak_min_height.md).

- tol:

  Convergence tolerance. Default is 1e-6.

- n_starts:

  Number of random starts for `optimizer = "multistart"`. Default is 5.

- constrain_positions:

  Logical. If `TRUE`, enforce that peak centers maintain their relative
  ordering. Default is `TRUE`.

- quality_threshold:

  Minimum R-squared to accept fit. Fits below this threshold trigger a
  warning. Default is 0.8. Tune this with
  [`peak_fit_r_squared()`](https://jameshwade.github.io/measure/dev/reference/peak_min_height.md).

- store_components:

  Logical. If `TRUE`, also store sampled fitted peak curves for
  diagnostics. Fitted models and parameters are always retained for
  subsequent component integration. Default is `FALSE`.

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

**Reported Peak Properties:**

For EMG fits, `location` and `height` report the apex of the fitted
component within the measured axis range. They use the continuous fitted
curve, including on irregular axes, and differ from the precursor
Gaussian parameters returned by
[`optimize_deconvolution()`](https://jameshwade.github.io/measure/dev/reference/optimize_deconvolution.md).

**Quality Assessment:**

Each fit is assessed for quality. The `.peaks` tibble gains columns:

- `fit_r_squared`: R-squared of the overall fit

- `fit_quality`: Quality grade (A/B/C/D/F)

- `purity`: How much of signal at peak max comes from this peak

`max_iter` and `quality_threshold` are tunable. The R-squared range is
bounded by 0 and 1; choose iteration ranges that fit the model and
runtime budget for your data. Before finalization,
[`tidy()`](https://generics.r-lib.org/reference/tidy.html) retains
unresolved tuning calls in list columns.

The columns `fit_model` and `fit_params` retain each fitted component.
The `detected_left_base` and `detected_right_base` columns retain
original detected minima. The `area_basis` column identifies
fitted-component areas.

`left_base` and `right_base` define a finite integration interval within
the measured axis. The interval retains detected minima where possible
and expands as needed to contain the reported fitted apex. Fitted areas
integrate each component over this interval without adding a physical
baseline.

## See also

[`optimize_deconvolution()`](https://jameshwade.github.io/measure/dev/reference/optimize_deconvolution.md),
[`assess_deconv_quality()`](https://jameshwade.github.io/measure/dev/reference/assess_deconv_quality.md),
[`peak_models()`](https://jameshwade.github.io/measure/dev/reference/peak_models.md),
[`gaussian_peak_model()`](https://jameshwade.github.io/measure/dev/reference/gaussian_peak_model.md)

Other peak-operations:
[`step_measure_peaks_detect()`](https://jameshwade.github.io/measure/dev/reference/step_measure_peaks_detect.md),
[`step_measure_peaks_filter()`](https://jameshwade.github.io/measure/dev/reference/step_measure_peaks_filter.md),
[`step_measure_peaks_integrate()`](https://jameshwade.github.io/measure/dev/reference/step_measure_peaks_integrate.md),
[`step_measure_peaks_properties()`](https://jameshwade.github.io/measure/dev/reference/step_measure_peaks_properties.md),
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
#> # A tibble: 2 × 15
#>   peak_id location height left_base right_base  area detected_left_base
#>     <int>    <dbl>  <dbl>     <dbl>      <dbl> <dbl>              <dbl>
#> 1       1     8.00  1.50        1.7         20  3.77                1.7
#> 2       2    12.0   0.792      10.2         20  2.64               10.2
#> # ℹ 8 more variables: detected_right_base <dbl>, fit_model <list>,
#> #   fit_params <list>, area_basis <chr>, fit_r_squared <dbl>,
#> #   fit_quality <chr>, purity <dbl>, overlap_degree <dbl>
# }
```
