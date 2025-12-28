# Interpolate Gaps in Measurement Data

`step_measure_interpolate()` creates a *specification* of a recipe step
that fills gaps or missing values in measurement data using
interpolation.

## Usage

``` r
step_measure_interpolate(
  recipe,
  ranges,
  method = c("linear", "spline"),
  measures = NULL,
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_interpolate")
)
```

## Arguments

- recipe:

  A recipe object.

- ranges:

  A list of numeric vectors specifying ranges to interpolate. Each
  element should be a vector of length 2: `c(min, max)`.

- method:

  Interpolation method: "linear" or "spline". Default is "linear".

- measures:

  An optional character vector of measure column names.

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

This step is useful for:

- Filling gaps left by excluded regions that need restoration

- Handling missing or invalid data points

- Smoothing over detector saturation regions

The interpolation uses data points immediately outside the specified
ranges to estimate values within the ranges.

## Examples

``` r
library(recipes)

# Interpolate over a problematic region
rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
  update_role(id, new_role = "id") |>
  step_measure_input_long(transmittance, location = vars(channel)) |>
  step_measure_interpolate(ranges = list(c(40, 50)), method = "spline") |>
  prep()
```
