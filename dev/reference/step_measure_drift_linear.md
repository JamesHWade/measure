# Linear Drift Correction

`step_measure_drift_linear()` creates a *specification* of a recipe step
that corrects for linear signal drift across run order using QC or
reference samples. This is a simpler alternative to LOESS when drift is
approximately linear.

## Usage

``` r
step_measure_drift_linear(
  recipe,
  ...,
  run_order_col = "run_order",
  sample_type_col = "sample_type",
  qc_type = "qc",
  apply_to = c("all", "unknown"),
  min_qc = 3,
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_drift_linear")
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
    linear regression is fit to QC sample responses vs run order for
    each feature.

2.  During
    [`bake()`](https://recipes.tidymodels.org/reference/bake.html):
    Correction factors are calculated as:
    `correction = median(QC_responses) / predicted_value`

    Each sample's response is multiplied by the correction factor.

### When to Use

Use linear drift correction when:

- Drift is approximately linear over the run

- You have fewer QC samples (requires at least 3)

- You want a more conservative correction

For non-linear drift patterns, use
[`step_measure_drift_qc_loess()`](https://jameshwade.github.io/measure/dev/reference/step_measure_drift_qc_loess.md)
or
[`step_measure_drift_spline()`](https://jameshwade.github.io/measure/dev/reference/step_measure_drift_spline.md).

## See also

[`step_measure_drift_qc_loess()`](https://jameshwade.github.io/measure/dev/reference/step_measure_drift_qc_loess.md)
for LOESS-based correction,
[`step_measure_drift_spline()`](https://jameshwade.github.io/measure/dev/reference/step_measure_drift_spline.md)
for spline-based correction.

Other drift-correction:
[`step_measure_drift_qc_loess()`](https://jameshwade.github.io/measure/dev/reference/step_measure_drift_qc_loess.md),
[`step_measure_drift_spline()`](https://jameshwade.github.io/measure/dev/reference/step_measure_drift_spline.md),
[`step_measure_qc_bracket()`](https://jameshwade.github.io/measure/dev/reference/step_measure_qc_bracket.md)

## Examples

``` r
library(recipes)

# Data with linear drift
data <- data.frame(
  sample_id = paste0("S", 1:20),
  sample_type = rep(c("qc", "unknown", "unknown", "unknown", "qc"), 4),
  run_order = 1:20,
  feature1 = 100 + (1:20) * 0.5 + rnorm(20, sd = 2)
)

rec <- recipe(~ ., data = data) |>
  update_role(sample_id, new_role = "id") |>
  step_measure_drift_linear(feature1) |>
  prep()

corrected <- bake(rec, new_data = NULL)
```
