# Calculate Peak Properties

`step_measure_peaks_properties()` creates a *specification* of a recipe
step that calculates derived peak metrics from the measured signal and
stores them in the `.peaks` tibble.

## Usage

``` r
step_measure_peaks_properties(
  recipe,
  properties = c("prominence", "fwhm"),
  measures = NULL,
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_peaks_properties")
)
```

## Arguments

- recipe:

  A recipe object.

- properties:

  Character vector of peak properties to calculate. Supported values are
  `"prominence"` and `"fwhm"`.

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

This step calculates additional peak metrics from the observed signal
for each detected peak:

- `"prominence"`: Peak height above the higher of the left and right
  base intensities.

- `"fwhm"`: Full width at half maximum, estimated with linear
  interpolation after subtracting a local linear baseline between the
  left and right bases.

The calculated properties are added as new columns in the `.peaks`
tibble and can be exported later with
[`step_measure_peaks_to_table()`](https://jameshwade.github.io/measure/dev/reference/step_measure_peaks_to_table.md).

## See also

Other peak-operations:
[`step_measure_peaks_deconvolve()`](https://jameshwade.github.io/measure/dev/reference/step_measure_peaks_deconvolve.md),
[`step_measure_peaks_detect()`](https://jameshwade.github.io/measure/dev/reference/step_measure_peaks_detect.md),
[`step_measure_peaks_filter()`](https://jameshwade.github.io/measure/dev/reference/step_measure_peaks_filter.md),
[`step_measure_peaks_integrate()`](https://jameshwade.github.io/measure/dev/reference/step_measure_peaks_integrate.md),
[`step_measure_peaks_to_table()`](https://jameshwade.github.io/measure/dev/reference/step_measure_peaks_to_table.md)

## Examples

``` r
library(recipes)

rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
  update_role(id, new_role = "id") |>
  step_measure_input_long(transmittance, location = vars(channel)) |>
  step_measure_peaks_detect(min_height = 0.5) |>
  step_measure_peaks_properties(c("prominence", "fwhm")) |>
  prep()

result <- bake(rec, new_data = NULL)
result$.peaks[[1]]
#> # A tibble: 1 × 8
#>   peak_id location height left_base right_base  area prominence  fwhm
#>     <int>    <int>  <dbl>     <int>      <int> <dbl>      <dbl> <dbl>
#> 1       1       65   3.39         1        100    NA      0.576  35.8
```
