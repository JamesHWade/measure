# Integrate Peak Areas

`step_measure_peaks_integrate()` creates a *specification* of a recipe
step that calculates the area under each detected peak.

## Usage

``` r
step_measure_peaks_integrate(
  recipe,
  method = c("trapezoid", "simpson"),
  baseline = c("local", "none", "global"),
  measures = NULL,
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_peaks_integrate")
)
```

## Arguments

- recipe:

  A recipe object.

- method:

  Integration method. One of `"trapezoid"` (default) or `"simpson"`.

- baseline:

  Baseline handling. One of `"local"` (linear interpolation between peak
  bases), `"none"` (integrate to zero), or `"global"` (use minimum value
  as baseline).

- measures:

  Optional character vector of measure column names.

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

This step calculates the area under each peak detected by
[`step_measure_peaks_detect()`](https://jameshwade.github.io/measure/dev/reference/step_measure_peaks_detect.md).
The areas are stored in the `area` column of the `.peaks` tibble.

**Integration methods:**

- `"trapezoid"`: Trapezoidal rule integration. Fast and accurate for
  well-resolved peaks.

- `"simpson"`: Simpson's rule integration. More accurate for smooth
  curves but requires odd number of points.

**Baseline handling:**

- `"local"`: Subtracts a linear baseline connecting the left and right
  peak bases before integration.

- `"none"`: Integrates directly to y=0.

- `"global"`: Subtracts the minimum value in the peak region.

## See also

Other peak-operations:
[`step_measure_peaks_deconvolve()`](https://jameshwade.github.io/measure/dev/reference/step_measure_peaks_deconvolve.md),
[`step_measure_peaks_detect()`](https://jameshwade.github.io/measure/dev/reference/step_measure_peaks_detect.md),
[`step_measure_peaks_filter()`](https://jameshwade.github.io/measure/dev/reference/step_measure_peaks_filter.md),
[`step_measure_peaks_to_table()`](https://jameshwade.github.io/measure/dev/reference/step_measure_peaks_to_table.md)

## Examples

``` r
library(recipes)

rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
  update_role(id, new_role = "id") |>
  step_measure_input_long(transmittance, location = vars(channel)) |>
  step_measure_peaks_detect(min_height = 0.5) |>
  step_measure_peaks_integrate() |>
  prep()

result <- bake(rec, new_data = NULL)
```
