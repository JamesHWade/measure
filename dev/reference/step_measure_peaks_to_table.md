# Convert Peaks to Tidy Table

`step_measure_peaks_to_table()` creates a *specification* of a recipe
step that converts the peaks list-column to a wide format with one
column per peak property.

## Usage

``` r
step_measure_peaks_to_table(
  recipe,
  prefix = "peak_",
  properties = c("location", "height", "area"),
  max_peaks = 10,
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_peaks_to_table")
)
```

## Arguments

- recipe:

  A recipe object.

- prefix:

  Prefix for generated column names. Default is `"peak_"`.

- properties:

  Which peak properties to include. Default includes location, height,
  and area for each peak.

- max_peaks:

  Maximum number of peaks to include in output. If a sample has more
  peaks, only the first `max_peaks` are included.

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

This step converts peak data to a wide format suitable for modeling. For
each peak, it creates columns like `peak_1_location`, `peak_1_height`,
`peak_1_area`, etc.

The `.peaks` and `.measures` columns are removed after conversion.

## See also

Other peak-operations:
[`step_measure_peaks_deconvolve()`](https://jameshwade.github.io/measure/dev/reference/step_measure_peaks_deconvolve.md),
[`step_measure_peaks_detect()`](https://jameshwade.github.io/measure/dev/reference/step_measure_peaks_detect.md),
[`step_measure_peaks_filter()`](https://jameshwade.github.io/measure/dev/reference/step_measure_peaks_filter.md),
[`step_measure_peaks_integrate()`](https://jameshwade.github.io/measure/dev/reference/step_measure_peaks_integrate.md)

## Examples

``` r
library(recipes)

rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
  update_role(id, new_role = "id") |>
  step_measure_input_long(transmittance, location = vars(channel)) |>
  step_measure_peaks_detect(min_height = 0.5) |>
  step_measure_peaks_integrate() |>
  step_measure_peaks_to_table(max_peaks = 5) |>
  prep()

result <- bake(rec, new_data = NULL)
```
