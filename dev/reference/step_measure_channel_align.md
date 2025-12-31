# Align Multiple Channels to a Common Grid

`step_measure_channel_align()` creates a *specification* of a recipe
step that aligns multiple measurement channels to a common location
grid.

## Usage

``` r
step_measure_channel_align(
  recipe,
  ...,
  method = c("union", "intersection", "reference"),
  reference = 1L,
  interpolation = c("linear", "spline", "constant"),
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_channel_align")
)
```

## Arguments

- recipe:

  A recipe object.

- ...:

  One or more selector functions to choose measure columns. If empty,
  all measure columns are used.

- method:

  How to determine the common grid:

  - `"union"` (default): Use all unique locations from all channels

  - `"intersection"`: Use only locations present in all channels

  - `"reference"`: Use the grid from the reference channel

- reference:

  For `method = "reference"`, which channel to use as reference. Can be
  a column name (character) or column index (integer). Default is 1
  (first channel).

- interpolation:

  Interpolation method for missing values:

  - `"linear"` (default): Linear interpolation

  - `"spline"`: Cubic spline interpolation

  - `"constant"`: Nearest neighbor (constant)

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

Multi-channel analytical instruments (e.g., LC-DAD, SEC with multiple
detectors) often produce measurements at slightly different location
grids for each channel. This step aligns all channels to a common grid,
enabling:

- Direct comparison between channels

- Channel combination or ratio calculations

- Modeling with consistent feature dimensions

### Grid Methods

- **Union**: Creates a grid containing all unique locations from all
  channels. Values are interpolated where channels don't have data.

- **Intersection**: Uses only locations where all channels have data. No
  interpolation needed but may lose data at edges.

- **Reference**: Uses one channel's grid as the target. Other channels
  are interpolated to match.

## See also

Other measure-channel:
[`step_measure_channel_combine()`](https://jameshwade.github.io/measure/dev/reference/step_measure_channel_combine.md),
[`step_measure_channel_ratio()`](https://jameshwade.github.io/measure/dev/reference/step_measure_channel_ratio.md)

## Examples

``` r
library(recipes)
library(tibble)

# Create sample multi-channel data
df <- tibble(
  id = rep(1:3, each = 10),
  time_uv = rep(seq(0, 9, by = 1), 3),
  absorbance_uv = rnorm(30, 100, 10),
  time_ri = rep(seq(0.5, 9.5, by = 1), 3),
  absorbance_ri = rnorm(30, 50, 5),
  concentration = rep(c(10, 25, 50), each = 10)
)

# Ingest as separate channels, then align
rec <- recipe(concentration ~ ., data = df) |>
  update_role(id, new_role = "id") |>
  step_measure_input_long(absorbance_uv, location = vars(time_uv)) |>
  step_measure_input_long(absorbance_ri, location = vars(time_ri)) |>
  step_measure_channel_align(method = "union")
```
