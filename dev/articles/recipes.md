# Integrating with tidymodels

``` r
library(measure)
library(tidymodels)
library(modeldata)

tidymodels_prefer()
set.seed(42)
```

## Introduction

One of measure’s key design goals is seamless integration with the
tidymodels ecosystem. This vignette shows how to use measure
preprocessing within complete modeling workflows, including:

- Bundling preprocessing with models using workflows
- Cross-validation with measure recipes
- Hyperparameter tuning for preprocessing steps
- Comparing preprocessing strategies

## Setup: The meats dataset

We’ll use the NIR spectroscopy dataset from modeldata to predict water
content in meat samples. We’ll focus on just predicting `water` to keep
the example simple.

``` r
data(meats)

# Keep only water as outcome and spectral columns
meats_water <- meats |>
  select(water, starts_with("x_"))

# Create train/test split
split <- initial_split(meats_water, prop = 0.75)
train <- training(split)
test <- testing(split)

cat("Training samples:", nrow(train), "\n")
#> Training samples: 161
cat("Test samples:", nrow(test), "\n")
#> Test samples: 54
```

## Basic workflow

The simplest way to use measure with tidymodels is through a workflow
that bundles preprocessing and modeling:

``` r
# Define preprocessing recipe
rec <- recipe(water ~ ., data = train) |>
  step_measure_input_wide(starts_with("x_")) |>
  step_measure_savitzky_golay(window_side = 5, differentiation_order = 1) |>
  step_measure_snv() |>
  step_measure_output_wide()

# Define model - simple linear regression
# (For real applications, use regularization like glmnet or PLS)
lm_spec <- linear_reg() |>
  set_engine("lm")

# Create workflow
wf <- workflow() |>
  add_recipe(rec) |>
  add_model(lm_spec)

wf
#> ══ Workflow ════════════════════════════════════════════════════════════════════
#> Preprocessor: Recipe
#> Model: linear_reg()
#> 
#> ── Preprocessor ────────────────────────────────────────────────────────────────
#> 4 Recipe Steps
#> 
#> • step_measure_input_wide()
#> • step_measure_savitzky_golay()
#> • step_measure_snv()
#> • step_measure_output_wide()
#> 
#> ── Model ───────────────────────────────────────────────────────────────────────
#> Linear Regression Model Specification (regression)
#> 
#> Computational engine: lm
```

### Fit and evaluate

``` r
# Fit the workflow
wf_fit <- fit(wf, data = train)

# Predict on test data
predictions <- predict(wf_fit, test) |>
  bind_cols(test |> select(water))

# Evaluate
metrics(predictions, truth = water, estimate = .pred)
```

## Cross-validation

Cross-validation is straightforward with workflows:

``` r
# Create folds
folds <- vfold_cv(train, v = 5)

# Fit resamples
cv_results <- fit_resamples(wf, resamples = folds)

# Collect metrics
collect_metrics(cv_results)
```

## Tuning preprocessing parameters

Several measure steps have tunable parameters:

- [`step_measure_savitzky_golay()`](https://jameshwade.github.io/measure/dev/reference/step_measure_savitzky_golay.md):
  `window_side`, `differentiation_order`, `degree`
- [`step_measure_normalize_peak()`](https://jameshwade.github.io/measure/dev/reference/step_measure_normalize_peak.md):
  `location_min`, `location_max`
- Baseline correction steps: `lambda`, `p`, `degree`, etc.

Let’s find the optimal Savitzky-Golay parameters:

``` r
# Create a tunable recipe
rec_tune <- recipe(water ~ ., data = train) |>
  step_measure_input_wide(starts_with("x_")) |>
  step_measure_savitzky_golay(
    window_side = tune(),
    differentiation_order = tune()
  ) |>
  step_measure_snv() |>
  step_measure_output_wide()

# Create tunable workflow
wf_tune <- workflow() |>
  add_recipe(rec_tune) |>
  add_model(lm_spec)
```

### Define the parameter grid

``` r
# Create a grid
grid <- grid_regular(
  window_side(range = c(3L, 11L)),
  differentiation_order(range = c(0L, 2L)),
  levels = c(5, 3)
)

grid
```

### Run the tuning

``` r
tune_results <- tune_grid(
  wf_tune,
  resamples = folds,
  grid = grid
)

# Show best results
show_best(tune_results, metric = "rmse", n = 5)
```

### Visualize tuning results

``` r
autoplot(tune_results)
```

### Finalize the workflow

``` r
# Select best parameters
best_params <- select_best(tune_results, metric = "rmse")
best_params

# Finalize workflow
final_wf <- finalize_workflow(wf_tune, best_params)

# Fit on full training data and evaluate on test
final_fit <- last_fit(final_wf, split)

collect_metrics(final_fit)
```

## Comparing preprocessing strategies

A common workflow is comparing different preprocessing approaches.
Here’s how to set up a fair comparison:

``` r
# Strategy 1: SNV only
rec_snv <- recipe(water ~ ., data = train) |>
  step_measure_input_wide(starts_with("x_")) |>
  step_measure_snv() |>
  step_measure_output_wide()

# Strategy 2: First derivative + SNV
rec_d1_snv <- recipe(water ~ ., data = train) |>
  step_measure_input_wide(starts_with("x_")) |>
  step_measure_savitzky_golay(window_side = 7, differentiation_order = 1) |>
  step_measure_snv() |>
  step_measure_output_wide()

# Strategy 3: MSC
rec_msc <- recipe(water ~ ., data = train) |>
  step_measure_input_wide(starts_with("x_")) |>
  step_measure_msc() |>
  step_measure_output_wide()

# Strategy 4: Second derivative only
rec_d2 <- recipe(water ~ ., data = train) |>
  step_measure_input_wide(starts_with("x_")) |>
  step_measure_savitzky_golay(window_side = 7, differentiation_order = 2) |>
  step_measure_output_wide()

# Strategy 5: SNV + centering (good for PLS)
rec_snv_center <- recipe(water ~ ., data = train) |>
  step_measure_input_wide(starts_with("x_")) |>
  step_measure_snv() |>
  step_measure_center() |>
  step_measure_output_wide()

# Strategy 6: Sum normalization + auto-scaling
rec_norm_scale <- recipe(water ~ ., data = train) |>
  step_measure_input_wide(starts_with("x_")) |>
  step_measure_normalize_sum() |>
  step_measure_scale_auto() |>
  step_measure_output_wide()

# Create workflow set
wf_set <- workflow_set(
  preproc = list(
    snv = rec_snv,
    d1_snv = rec_d1_snv,
    msc = rec_msc,
    d2 = rec_d2,
    snv_center = rec_snv_center,
    norm_scale = rec_norm_scale
  ),
  models = list(lm = lm_spec)
)

wf_set
```

### Evaluate all strategies

``` r
# Fit all workflows with cross-validation
comparison <- workflow_map(
  wf_set,
  fn = "fit_resamples",
  resamples = folds
)

# Rank by performance
rank_results(comparison, rank_metric = "rmse")
```

### Visualize comparison

``` r
autoplot(comparison) +
  labs(title = "Preprocessing Strategy Comparison")
```

## Working with workflows and new data

Once you’ve selected your final workflow, here’s how to use it for
predictions on new data:

``` r
# Use the finalized workflow from tuning
final_trained <- fit(final_wf, train)

# Predict on new data
new_predictions <- predict(final_trained, test)

# Or use augment for predictions with original data
augment(final_trained, test) |>
  select(water, .pred) |>
  head()
```

## Tips for spectral modeling

### High-dimensional data

Spectral data is typically high-dimensional (many features, fewer
samples). Consider:

1.  **Regularization**: Use ridge or elastic net regression
    (`linear_reg(penalty = tune(), mixture = tune())`)
2.  **PLS regression**: Use `pls()` from parsnip with the mixOmics
    engine
3.  **Feature selection**: Consider variable importance after initial
    modeling

### Preprocessing for PCA/PLS

For multivariate methods like PCA and PLS, centering is essential:

``` r
rec_for_pls <- recipe(water ~ ., data = train) |>
  step_measure_input_wide(starts_with("x_")) |>
  step_measure_snv() |>
  step_measure_center() |>  # Essential for PCA/PLS
  step_measure_output_wide()
```

Use
[`step_measure_scale_auto()`](https://jameshwade.github.io/measure/dev/reference/step_measure_scale_auto.md)
if you want to give equal weight to all wavelengths, or
[`step_measure_scale_pareto()`](https://jameshwade.github.io/measure/dev/reference/step_measure_scale_pareto.md)
for a compromise that preserves some magnitude information.

### Memory considerations

For very large spectral datasets:

- The internal `.measures` format is memory-efficient
- Consider processing in batches if memory is limited
- Use
  [`step_measure_output_wide()`](https://jameshwade.github.io/measure/dev/reference/step_measure_output_wide.md)
  only when needed for modeling

### Reproducibility

Always set a seed before cross-validation or tuning:

``` r
set.seed(123)
folds <- vfold_cv(train, v = 10)
```

## Summary

measure integrates naturally with tidymodels:

- Use
  [`workflow()`](https://workflows.tidymodels.org/reference/workflow.html)
  to bundle preprocessing and modeling
- Cross-validate with
  [`fit_resamples()`](https://tune.tidymodels.org/reference/fit_resamples.html)
  or `vfold_cv()`
- Tune preprocessing parameters (Savitzky-Golay, peak normalization,
  baseline) with
  [`tune_grid()`](https://tune.tidymodels.org/reference/tune_grid.html)
- Compare strategies with `workflow_set()`
- Variable-wise scaling steps
  ([`step_measure_center()`](https://jameshwade.github.io/measure/dev/reference/step_measure_center.md),
  `step_measure_scale_*()`) learn from training data and apply
  consistently to new data

The recipes paradigm means your preprocessing is applied consistently to
training data, cross-validation folds, and new predictions - eliminating
a common source of data leakage in chemometric modeling.
