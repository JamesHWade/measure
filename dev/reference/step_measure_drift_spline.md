# Spline-Based Drift Correction

`step_measure_drift_spline()` creates a *specification* of a recipe step
that corrects for signal drift using smoothing splines fit to QC
samples. This offers more flexibility than linear correction while being
more stable than LOESS for sparse QC data.

## Usage

``` r
step_measure_drift_spline(
  recipe,
  ...,
  run_order_col = "run_order",
  sample_type_col = "sample_type",
  qc_type = "qc",
  apply_to = c("all", "unknown"),
  df = NULL,
  spar = NULL,
  min_qc = 4,
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_drift_spline")
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

- df:

  Degrees of freedom for the smoothing spline. Default is NULL, which
  uses cross-validation to select optimal df. Lower values = smoother.

- spar:

  Smoothing parameter (alternative to df). If NULL (default),
  cross-validation is used.

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

Uses
[`stats::smooth.spline()`](https://rdrr.io/r/stats/smooth.spline.html)
to fit a flexible curve through QC responses. The spline automatically
adapts to the data complexity when `df` is not specified.

### Comparison with Other Methods

|        |                        |                |
|--------|------------------------|----------------|
| Method | Best For               | Min QC Samples |
| Linear | Simple linear drift    | 3              |
| Spline | Moderate non-linearity | 4+             |
| LOESS  | Complex patterns       | 5+             |

## See also

[`step_measure_drift_linear()`](https://jameshwade.github.io/measure/dev/reference/step_measure_drift_linear.md)
for linear correction,
[`step_measure_drift_qc_loess()`](https://jameshwade.github.io/measure/dev/reference/step_measure_drift_qc_loess.md)
for LOESS-based correction.

Other drift-correction:
[`step_measure_drift_linear()`](https://jameshwade.github.io/measure/dev/reference/step_measure_drift_linear.md),
[`step_measure_drift_qc_loess()`](https://jameshwade.github.io/measure/dev/reference/step_measure_drift_qc_loess.md),
[`step_measure_qc_bracket()`](https://jameshwade.github.io/measure/dev/reference/step_measure_qc_bracket.md)

## Examples

``` r
library(recipes)

# Data with non-linear drift
set.seed(123)
data <- data.frame(
  sample_id = paste0("S", 1:30),
  sample_type = rep(c("qc", "unknown", "unknown", "unknown", "unknown", "qc"), 5),
  run_order = 1:30,
  feature1 = 100 + sin((1:30) / 5) * 10 + rnorm(30, sd = 2)
)

rec <- recipe(~ ., data = data) |>
  update_role(sample_id, new_role = "id") |>
  step_measure_drift_spline(feature1) |>
  prep()

corrected <- bake(rec, new_data = NULL)
```
