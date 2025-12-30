# QC-Based Drift Correction Using LOESS

`step_measure_drift_qc_loess()` creates a *specification* of a recipe
step that corrects for signal drift across run order using QC (or
reference) samples. This implements the QC-RLSC (robust LOESS signal
correction) method.

## Usage

``` r
step_measure_drift_qc_loess(
  recipe,
  ...,
  run_order_col = "run_order",
  sample_type_col = "sample_type",
  qc_type = "qc",
  apply_to = c("all", "unknown"),
  span = 0.75,
  degree = 2,
  robust = TRUE,
  min_qc = 5,
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_drift_qc_loess")
)
```

## Arguments

- recipe:

  A recipe object.

- ...:

  One or more selector functions to choose feature columns. For
  feature-level data, select the numeric response columns. For
  curve-level data with `.measures`, leave empty to apply to all
  locations.

- run_order_col:

  Name of the column containing run order (injection sequence). Must be
  numeric/integer.

- sample_type_col:

  Name of the column containing sample type.

- qc_type:

  Value(s) in `sample_type_col` that identify QC samples to use for
  drift modeling. Default is `"qc"`.

- apply_to:

  Which samples to apply correction to:

  - `"all"` (default): Correct all samples

  - `"unknown"`: Only correct unknown samples

- span:

  LOESS span parameter controlling smoothness. Default is 0.75. Smaller
  values = more flexible fit.

- degree:

  Polynomial degree for LOESS (1 or 2). Default is 2.

- robust:

  Logical. Use robust LOESS fitting? Default is TRUE.

- min_qc:

  Minimum number of QC samples required. Default is 5.

- role:

  Not used by this step.

- trained:

  Logical indicating if the step has been trained.

- skip:

  Logical. Should the step be skipped when baking?

- id:

  Unique step identifier.

## Value

An updated recipe with the new step added.

## Details

### How It Works

1.  During
    [`prep()`](https://recipes.tidymodels.org/reference/prep.html): A
    LOESS model is fit to QC sample responses vs run order for each
    feature/location.

2.  During
    [`bake()`](https://recipes.tidymodels.org/reference/bake.html):
    Correction factors are calculated as:
    `correction = median(QC_responses) / predicted_value`

    Each sample's response is multiplied by the correction factor at its
    run order position.

### Data Levels

This step supports both:

- **Feature-level data**: Applies correction to each selected numeric
  column

- **Curve-level data**: Applies correction to each location in the
  measure_list

### Diagnostics

The trained step stores drift model information accessible via
[`tidy()`](https://generics.r-lib.org/reference/tidy.html):

- LOESS model parameters

- QC response trends

- Correction factors applied

## See also

[`measure_detect_drift()`](https://jameshwade.github.io/measure/dev/reference/measure_detect_drift.md)
for drift detection before correction.

Other drift-correction:
[`step_measure_drift_linear()`](https://jameshwade.github.io/measure/dev/reference/step_measure_drift_linear.md),
[`step_measure_drift_spline()`](https://jameshwade.github.io/measure/dev/reference/step_measure_drift_spline.md),
[`step_measure_qc_bracket()`](https://jameshwade.github.io/measure/dev/reference/step_measure_qc_bracket.md)

## Examples

``` r
library(recipes)

# Feature-level data with drift
data <- data.frame(
  sample_id = paste0("S", 1:20),
  sample_type = rep(c("qc", "unknown", "unknown", "unknown", "qc"), 4),
  run_order = 1:20,
  feature1 = 100 + (1:20) * 0.5 + rnorm(20, sd = 2),  # Upward drift
  feature2 = 50 - (1:20) * 0.3 + rnorm(20, sd = 1)    # Downward drift
)

rec <- recipe(~ ., data = data) |>
  update_role(sample_id, new_role = "id") |>
  step_measure_drift_qc_loess(feature1, feature2) |>
  prep()

corrected <- bake(rec, new_data = NULL)
```
