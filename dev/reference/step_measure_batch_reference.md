# Reference-Based Batch Correction

`step_measure_batch_reference()` creates a *specification* of a recipe
step that corrects for batch effects using reference samples. This is a
simpler alternative to ComBat-style correction that doesn't require
heavy dependencies.

## Usage

``` r
step_measure_batch_reference(
  recipe,
  ...,
  batch_col = "batch_id",
  sample_type_col = "sample_type",
  reference_type = "reference",
  method = c("median_ratio", "mean_ratio", "median_center", "mean_center"),
  target_batch = NULL,
  min_ref = 2,
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_batch_reference")
)
```

## Arguments

- recipe:

  A recipe object.

- ...:

  One or more selector functions to choose feature columns.

- batch_col:

  Name of the column containing batch identifiers.

- sample_type_col:

  Name of the column containing sample type.

- reference_type:

  Value(s) in `sample_type_col` that identify reference samples to use
  for batch correction. Default is `"reference"`.

- method:

  Correction method:

  - `"median_ratio"` (default): Scale by ratio of reference medians

  - `"mean_ratio"`: Scale by ratio of reference means

  - `"median_center"`: Center batches to common median

  - `"mean_center"`: Center batches to common mean

- target_batch:

  Which batch to use as reference. Default is the first batch
  (alphabetically). Can also be `"global"` to use global reference
  median/mean.

- min_ref:

  Minimum number of reference samples per batch. Default is 2.

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

### Correction Methods

**Median/Mean Ratio**: Multiplies all samples in a batch by:
`target_reference / batch_reference`

This preserves relative differences within batches while aligning batch
centers.

**Median/Mean Center**: Subtracts the difference between batch reference
and target reference. This is appropriate for log-transformed data.

### Reference Samples

Reference samples should be identical samples run in each batch (e.g.,
pooled QC, reference material). The step will error if any batch lacks
sufficient reference samples.

## See also

[`step_measure_drift_qc_loess()`](https://jameshwade.github.io/measure/dev/reference/step_measure_drift_qc_loess.md)
for within-batch drift correction.

## Examples

``` r
library(recipes)

# Data with batch effects
data <- data.frame(
  sample_id = paste0("S", 1:20),
  sample_type = rep(c("reference", "unknown", "unknown", "unknown", "reference"), 4),
  batch_id = rep(c("B1", "B1", "B2", "B2"), 5),
  feature1 = c(rep(100, 10), rep(120, 10)) + rnorm(20, sd = 5),  # Batch effect
  feature2 = c(rep(50, 10), rep(45, 10)) + rnorm(20, sd = 2)
)

rec <- recipe(~ ., data = data) |>
  update_role(sample_id, new_role = "id") |>
  step_measure_batch_reference(feature1, feature2, batch_col = "batch_id") |>
  prep()

corrected <- bake(rec, new_data = NULL)
```
