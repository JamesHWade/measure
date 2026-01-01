# Generate Molecular Weight Distribution Curve

**\[superseded\]**

`step_measure_mw_distribution()` creates a *specification* of a recipe
step that generates molecular weight distribution curves from SEC/GPC
data.

**This step has been superseded by
`measure.sec::step_sec_mw_distribution()`.** For new code, we recommend
using the `measure.sec` package which provides more complete SEC/GPC
analysis functionality.

## Usage

``` r
step_measure_mw_distribution(
  recipe,
  measures = NULL,
  type = c("differential", "cumulative", "both"),
  calibration = NULL,
  n_points = 100L,
  mw_range = NULL,
  normalize = TRUE,
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_mw_distribution")
)
```

## Arguments

- recipe:

  A recipe object.

- measures:

  An optional character vector of measure column names.

- type:

  Type of distribution to generate:

  - `"differential"` (default): dW/d(log M) differential distribution

  - `"cumulative"`: Cumulative weight fraction distribution

  - `"both"`: Generate both distributions

- calibration:

  Calibration method for converting x-axis to log(MW). See
  [`step_measure_mw_averages()`](https://jameshwade.github.io/measure/dev/reference/step_measure_mw_averages.md)
  for details.

- n_points:

  Number of points in the output distribution. Default is 100. If
  `NULL`, uses the original data resolution.

- mw_range:

  Optional numeric vector `c(min, max)` specifying the MW range for the
  output distribution. If `NULL`, uses the range from data.

- normalize:

  Logical. Should the differential distribution be normalized to
  integrate to 1? Default is `TRUE`.

- role:

  Role for generated columns. Default is `"predictor"`.

- trained:

  Logical indicating if the step has been trained.

- skip:

  Logical. Should the step be skipped when baking?

- id:

  Unique step identifier.

## Value

An updated recipe with the new step added.

## Details

This step transforms the raw chromatogram into standard MW distribution
representations:

**Differential Distribution (dW/d(log M)):** The weight fraction per
unit log(MW). This representation is preferred because the area under
the curve represents the weight fraction in that MW range.

**Cumulative Distribution:** The cumulative weight fraction from low to
high MW. Values range from 0 to 1.

The output replaces the `.measures` column with the distribution data,
where `location` contains log10(MW) values and `value` contains the
distribution values.

## See also

Other measure-chromatography:
[`step_measure_mw_averages()`](https://jameshwade.github.io/measure/dev/reference/step_measure_mw_averages.md),
[`step_measure_mw_fractions()`](https://jameshwade.github.io/measure/dev/reference/step_measure_mw_fractions.md),
[`step_measure_peaks_deconvolve()`](https://jameshwade.github.io/measure/dev/reference/step_measure_peaks_deconvolve.md)

## Examples

``` r
library(recipes)

# Generate differential MW distribution
# rec <- recipe(~., data = gpc_data) |>
#   step_measure_input_wide(starts_with("signal_")) |>
#   step_measure_baseline_als() |>
#   step_measure_mw_distribution(type = "differential") |>
#   prep()
```
