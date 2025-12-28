# Filter Peaks by Criteria

`step_measure_peaks_filter()` creates a *specification* of a recipe step
that filters detected peaks based on various criteria.

## Usage

``` r
step_measure_peaks_filter(
  recipe,
  min_height = NULL,
  min_area = NULL,
  min_area_pct = NULL,
  min_prominence = NULL,
  max_peaks = NULL,
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_peaks_filter")
)
```

## Arguments

- recipe:

  A recipe object.

- min_height:

  Minimum peak height. Peaks below this are removed.

- min_area:

  Minimum peak area. Requires prior integration.

- min_area_pct:

  Minimum area as percentage of total. Peaks with area less than this
  percentage of total peak area are removed.

- min_prominence:

  Minimum peak prominence.

- max_peaks:

  Maximum number of peaks to keep (keeps largest by area or height).

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

This step removes peaks that don't meet specified criteria. Multiple
criteria can be combined - peaks must pass ALL specified filters.

## See also

Other peak-operations:
[`step_measure_peaks_detect()`](https://jameshwade.github.io/measure/dev/reference/step_measure_peaks_detect.md),
[`step_measure_peaks_integrate()`](https://jameshwade.github.io/measure/dev/reference/step_measure_peaks_integrate.md),
[`step_measure_peaks_to_table()`](https://jameshwade.github.io/measure/dev/reference/step_measure_peaks_to_table.md)

## Examples

``` r
library(recipes)

rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
  update_role(id, new_role = "id") |>
  step_measure_input_long(transmittance, location = vars(channel)) |>
  step_measure_peaks_detect(min_height = 0.3) |>
  step_measure_peaks_integrate() |>
  step_measure_peaks_filter(min_area_pct = 1) |>
  prep()

result <- bake(rec, new_data = NULL)
```
