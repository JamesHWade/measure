# Combine Multiple Channels

`step_measure_channel_combine()` creates a *specification* of a recipe
step that combines multiple measurement channels into a single
representation.

## Usage

``` r
step_measure_channel_combine(
  recipe,
  ...,
  strategy = c("stack", "concat", "weighted_sum", "mean"),
  weights = NULL,
  output_col = ".measures",
  remove_original = TRUE,
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_channel_combine")
)
```

## Arguments

- recipe:

  A recipe object.

- ...:

  One or more selector functions to choose measure columns. If empty,
  all measure columns are used.

- strategy:

  How to combine channels:

  - `"stack"`: Stack channels into an nD measurement with channel as a
    dimension

  - `"concat"`: Concatenate channels into a single 1D measurement

  - `"weighted_sum"`: Compute weighted sum across channels

  - `"mean"`: Average across channels (equal weights)

- weights:

  For `strategy = "weighted_sum"`, a numeric vector of weights. Must
  have same length as number of channels. Default is equal weights.

- output_col:

  Name of the output measure column. Default is `".measures"`.

- remove_original:

  Logical. Should original channel columns be removed? Default is
  `TRUE`.

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

After aligning multiple channels to a common grid with
[`step_measure_channel_align()`](https://jameshwade.github.io/measure/dev/reference/step_measure_channel_align.md),
this step combines them for downstream analysis. The choice of strategy
depends on the analysis goal:

### Strategies

- **stack**: Creates an n-dimensional measurement where channel becomes
  a dimension. Useful for multi-way analysis (PARAFAC, Tucker).

- **concat**: Concatenates all channels end-to-end into a single long
  vector. Useful for PLS or other models that expect 1D input.

- **weighted_sum**: Computes a weighted combination of channel values at
  each location. Useful when channels should be fused into a single
  signal.

- **mean**: Simple average across channels (special case of
  weighted_sum).

## Note

Channels must be aligned to the same grid before combining. Use
[`step_measure_channel_align()`](https://jameshwade.github.io/measure/dev/reference/step_measure_channel_align.md)
first if grids differ.

## See also

Other measure-channel:
[`step_measure_channel_align()`](https://jameshwade.github.io/measure/dev/reference/step_measure_channel_align.md),
[`step_measure_channel_ratio()`](https://jameshwade.github.io/measure/dev/reference/step_measure_channel_ratio.md)

## Examples

``` r
library(recipes)
library(tibble)

# Create sample multi-channel data (already aligned)
df <- tibble(
  id = rep(1:3, each = 10),
  time = rep(seq(0, 9, by = 1), 3),
  uv = rnorm(30, 100, 10),
  ri = rnorm(30, 50, 5),
  concentration = rep(c(10, 25, 50), each = 10)
)

# Ingest and combine with stacking
rec <- recipe(concentration ~ ., data = df) |>
  update_role(id, new_role = "id") |>
  step_measure_input_long(uv, location = vars(time)) |>
  step_measure_input_long(ri, location = vars(time)) |>
  step_measure_channel_combine(strategy = "stack")
```
