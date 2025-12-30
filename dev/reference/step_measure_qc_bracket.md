# QC Bracketing Interpolation

`step_measure_qc_bracket()` creates a *specification* of a recipe step
that corrects for drift using linear interpolation between bracketing QC
or reference samples. This is a simple, intuitive method where each
sample is corrected based on the two nearest QC samples.

## Usage

``` r
step_measure_qc_bracket(
  recipe,
  ...,
  run_order_col = "run_order",
  sample_type_col = "sample_type",
  qc_type = "qc",
  apply_to = c("all", "unknown"),
  extrapolate = TRUE,
  min_qc = 2,
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_qc_bracket")
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

- extrapolate:

  Logical. Should correction be extrapolated for samples before the
  first or after the last QC? Default is TRUE. If FALSE, those samples
  use the nearest QC's correction factor.

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

For each sample at run order `t`:

1.  Find the nearest QC samples before (`t1`) and after (`t2`)

2.  Calculate correction factors at `t1` and `t2` (target / observed)

3.  Linearly interpolate the correction factor for `t`

4.  Apply the interpolated correction

This method is commonly used in clinical and bioanalytical laboratories
where QC samples are injected at regular intervals throughout the run.

### When to Use

- Regular QC injection intervals

- Short analytical runs

- When you want simple, transparent corrections

- Regulatory environments where interpretability is important

## See also

Other drift-correction:
[`step_measure_drift_linear()`](https://jameshwade.github.io/measure/dev/reference/step_measure_drift_linear.md),
[`step_measure_drift_qc_loess()`](https://jameshwade.github.io/measure/dev/reference/step_measure_drift_qc_loess.md),
[`step_measure_drift_spline()`](https://jameshwade.github.io/measure/dev/reference/step_measure_drift_spline.md)

## Examples

``` r
library(recipes)

# Data with QC samples at regular intervals
data <- data.frame(
  sample_id = paste0("S", 1:15),
  sample_type = c("qc", rep("unknown", 4), "qc", rep("unknown", 4), "qc",
                  rep("unknown", 3), "qc"),
  run_order = 1:15,
  feature1 = c(100, 101, 103, 105, 107, 105, 107, 109, 111, 113,
               110, 112, 114, 116, 115)  # Drift pattern
)

rec <- recipe(~ ., data = data) |>
  update_role(sample_id, new_role = "id") |>
  step_measure_qc_bracket(feature1) |>
  prep()

corrected <- bake(rec, new_data = NULL)
```
