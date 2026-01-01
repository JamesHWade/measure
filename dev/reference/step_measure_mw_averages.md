# Calculate Molecular Weight Averages for SEC/GPC

**\[superseded\]**

`step_measure_mw_averages()` creates a *specification* of a recipe step
that calculates molecular weight averages from size exclusion
chromatography data.

**This step has been superseded by
`measure.sec::step_sec_mw_averages()`.** For new code, we recommend
using the `measure.sec` package which provides more complete SEC/GPC
analysis functionality.

## Usage

``` r
step_measure_mw_averages(
  recipe,
  measures = NULL,
  calibration = NULL,
  integration_range = NULL,
  output_cols = c("mn", "mw", "mz", "mp", "dispersity"),
  prefix = "mw_",
  role = "predictor",
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_mw_averages")
)
```

## Arguments

- recipe:

  A recipe object.

- measures:

  An optional character vector of measure column names.

- calibration:

  Calibration method for converting x-axis to log(MW). Can be:

  - `NULL` (default): Assumes x-axis is already log10(MW)

  - A numeric vector of length 2: Linear calibration
    `c(slope, intercept)` where `log10(MW) = slope * x + intercept`

  - `"auto"`: Estimate from data range (assumes typical polymer range)

- integration_range:

  Optional numeric vector `c(min, max)` specifying the x-axis range for
  integration. If `NULL`, uses full range.

- output_cols:

  Character vector of metrics to calculate. Default includes all:
  `c("mn", "mw", "mz", "mp", "dispersity")`.

- prefix:

  Prefix for output column names. Default is `"mw_"`.

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

This step calculates standard molecular weight averages from SEC/GPC
data:

|        |                    |                                   |
|--------|--------------------|-----------------------------------|
| Metric | Formula            | Description                       |
| Mn     | Σwᵢ / Σ(wᵢ/Mᵢ)     | Number-average molecular weight   |
| Mw     | Σ(wᵢMᵢ) / Σwᵢ      | Weight-average molecular weight   |
| Mz     | Σ(wᵢMᵢ²) / Σ(wᵢMᵢ) | Z-average molecular weight        |
| Mp     | M at peak maximum  | Peak molecular weight             |
| Đ      | Mw/Mn              | Dispersity (polydispersity index) |

The detector signal is assumed to be proportional to weight
concentration. For RI detection, this is typically valid. For UV
detection, response factors may need to be applied first using
[`step_measure_calibrate_y()`](https://jameshwade.github.io/measure/dev/reference/step_measure_calibrate_y.md).

**Prerequisites:**

- Data should be baseline corrected

- X-axis should represent retention time/volume or log(MW)

- Integration limits should exclude solvent peaks

## See also

Other measure-chromatography:
[`step_measure_mw_distribution()`](https://jameshwade.github.io/measure/dev/reference/step_measure_mw_distribution.md),
[`step_measure_mw_fractions()`](https://jameshwade.github.io/measure/dev/reference/step_measure_mw_fractions.md),
[`step_measure_peaks_deconvolve()`](https://jameshwade.github.io/measure/dev/reference/step_measure_peaks_deconvolve.md)

## Examples

``` r
library(recipes)

# Assuming x-axis is already calibrated to log10(MW)
# rec <- recipe(~., data = gpc_data) |>
#   step_measure_input_wide(starts_with("signal_")) |>
#   step_measure_baseline_als() |>
#   step_measure_mw_averages() |>
#   prep()
```
