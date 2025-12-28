# Apply X-Axis Calibration

`step_measure_calibrate_x()` creates a *specification* of a recipe step
that transforms the x-axis (location) values using a calibration
function or calibration data.

## Usage

``` r
step_measure_calibrate_x(
  recipe,
  calibration,
  from = "x",
  to = "y",
  method = "spline",
  extrapolate = FALSE,
  measures = NULL,
  role = NA,
  trained = FALSE,
  cal_fn = NULL,
  skip = FALSE,
  id = recipes::rand_id("measure_calibrate_x")
)
```

## Arguments

- recipe:

  A recipe object. The step will be added to the sequence of operations
  for this recipe.

- calibration:

  The calibration to apply. Can be:

  - A data.frame with columns specified by `from` and `to`

  - A function that takes location values and returns calibrated values

- from:

  Column name in calibration data.frame containing original x values.
  Default is `"x"`.

- to:

  Column name in calibration data.frame containing calibrated values.
  Default is `"y"`.

- method:

  Interpolation method when using calibration data.frame:

  - `"linear"`: Linear interpolation

  - `"spline"` (default): Cubic spline interpolation

- extrapolate:

  Logical. If `TRUE`, allow extrapolation outside the calibration range.
  If `FALSE` (default), values outside the range will return `NA` for
  linear interpolation or use spline extrapolation.

- measures:

  An optional character vector of measure column names to process. If
  `NULL` (the default), all measure columns will be processed.

- role:

  Not used by this step since no new variables are created.

- trained:

  A logical to indicate if the step has been trained.

- cal_fn:

  The calibration function created during training.

- skip:

  A logical. Should the step be skipped when baking?

- id:

  A character string that is unique to this step.

## Value

An updated version of `recipe` with the new step added.

## Details

X-axis calibration is commonly used to convert raw measurement units to
physically meaningful values. Common examples include:

- **GPC/SEC**: Convert retention time to molecular weight (via log MW)

- **Mass spectrometry**: Apply m/z calibration corrections

- **Spectroscopy**: Convert pixel or channel numbers to
  wavelength/wavenumber

The calibration can be provided as either:

1.  **Calibration data**: A data.frame with known x→y mappings. The step
    will build an interpolation function during
    [`prep()`](https://recipes.tidymodels.org/reference/prep.html).

2.  **Calibration function**: A function that directly transforms x
    values.

**Warning**: This step modifies the `location` column. Subsequent steps
will see the calibrated values. Make sure your calibration is
appropriate for your data range.

**No selectors should be supplied to this step function**. The data
should be in the internal format produced by
[`step_measure_input_wide()`](https://jameshwade.github.io/measure/dev/reference/step_measure_input_wide.md)
or
[`step_measure_input_long()`](https://jameshwade.github.io/measure/dev/reference/step_measure_input_long.md).

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/reference/tidy.recipe.html)
this step, a tibble with columns `terms`, `method`, `extrapolate`, and
`id` is returned.

## See also

[`step_measure_calibrate_y()`](https://jameshwade.github.io/measure/dev/reference/step_measure_calibrate_y.md)
for y-axis calibration

Other measure-preprocessing:
[`step_measure_calibrate_y()`](https://jameshwade.github.io/measure/dev/reference/step_measure_calibrate_y.md),
[`step_measure_map()`](https://jameshwade.github.io/measure/dev/reference/step_measure_map.md),
[`step_measure_msc()`](https://jameshwade.github.io/measure/dev/reference/step_measure_msc.md),
[`step_measure_normalize_istd()`](https://jameshwade.github.io/measure/dev/reference/step_measure_normalize_istd.md),
[`step_measure_ratio_reference()`](https://jameshwade.github.io/measure/dev/reference/step_measure_ratio_reference.md),
[`step_measure_snv()`](https://jameshwade.github.io/measure/dev/reference/step_measure_snv.md),
[`step_measure_subtract_blank()`](https://jameshwade.github.io/measure/dev/reference/step_measure_subtract_blank.md),
[`step_measure_subtract_reference()`](https://jameshwade.github.io/measure/dev/reference/step_measure_subtract_reference.md)

## Examples

``` r
library(recipes)

# Example: GPC molecular weight calibration
# Calibration standards: retention_time -> log(MW)
gpc_cal <- data.frame(
  retention_time = c(10, 12, 14, 16, 18),
  log_mw = c(6.5, 5.8, 5.0, 4.2, 3.5)
)

# Note: meats_long doesn't have retention time, this is illustrative
rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
  update_role(id, new_role = "id") |>
  step_measure_input_long(transmittance, location = vars(channel)) |>
  step_measure_calibrate_x(
    calibration = function(x) log10(x + 1),  # Example transformation
    method = "spline"
  )

# With calibration data
# rec <- recipe(...) |>
#   step_measure_calibrate_x(
#     calibration = gpc_cal,
#     from = "retention_time",
#     to = "log_mw",
#     method = "spline"
#   )
```
