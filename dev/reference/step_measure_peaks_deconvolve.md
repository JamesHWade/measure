# Deconvolve Overlapping Peaks

`step_measure_peaks_deconvolve()` creates a *specification* of a recipe
step that resolves overlapping peaks using curve fitting. This step
requires peaks to have been detected first using
[`step_measure_peaks_detect()`](https://jameshwade.github.io/measure/dev/reference/step_measure_peaks_detect.md).

## Usage

``` r
step_measure_peaks_deconvolve(
  recipe,
  model = c("gaussian", "emg", "bigaussian"),
  max_iter = 100L,
  tol = 1e-06,
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

  Peak model to use: "gaussian" (symmetric), "emg" (exponentially
  modified Gaussian for tailing peaks), or "bigaussian" (asymmetric).
  Default is "gaussian".

- max_iter:

  Maximum iterations for optimization. Default is 100.

- tol:

  Convergence tolerance. Default is 1e-6.

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
updated with deconvolved peak parameters and fitted areas.

## Details

Peak deconvolution fits mathematical models to overlapping peaks to
determine their individual contributions. This is essential for
quantitative analysis when peaks are not baseline-resolved.

**Peak Models:**

- `gaussian`: Symmetric Gaussian peak (3 params: height, center, width)

- `emg`: Exponentially Modified Gaussian (4 params, handles tailing)

- `bigaussian`: Bi-Gaussian (5 params, flexible asymmetry)

The optimization uses initial estimates from detected peak positions and
refines them to minimize the residual sum of squares.

## See also

Other measure-chromatography:
[`step_measure_mw_averages()`](https://jameshwade.github.io/measure/dev/reference/step_measure_mw_averages.md),
[`step_measure_mw_distribution()`](https://jameshwade.github.io/measure/dev/reference/step_measure_mw_distribution.md),
[`step_measure_mw_fractions()`](https://jameshwade.github.io/measure/dev/reference/step_measure_mw_fractions.md)

## Examples

``` r
library(recipes)

# Deconvolve overlapping peaks
# rec <- recipe(~., data = chromatogram_data) |>
#   step_measure_input_long(signal, location = vars(time)) |>
#   step_measure_peaks_detect(method = "derivative") |>
#   step_measure_peaks_deconvolve(model = "gaussian") |>
#   prep()
```
