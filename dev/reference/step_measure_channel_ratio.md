# Compute Ratios Between Channels

`step_measure_channel_ratio()` creates a *specification* of a recipe
step that computes ratios between pairs of measurement channels.

## Usage

``` r
step_measure_channel_ratio(
  recipe,
  numerator,
  denominator,
  output_prefix = "ratio_",
  epsilon = 1e-10,
  log_transform = FALSE,
  remove_original = FALSE,
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_channel_ratio")
)
```

## Arguments

- recipe:

  A recipe object.

- numerator:

  Column name(s) for the numerator channel(s).

- denominator:

  Column name(s) for the denominator channel(s). Must have same length
  as `numerator` (paired ratios).

- output_prefix:

  Prefix for output column names. Default is `"ratio_"`.

- epsilon:

  Small value added to denominator to avoid division by zero. Default is
  `1e-10`.

- log_transform:

  Logical. Should the ratio be log-transformed? Default is `FALSE`.

- remove_original:

  Logical. Should original channel columns be removed? Default is
  `FALSE`.

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

Channel ratios are useful in analytical chemistry for:

- **Normalization**: UV/RI ratios normalize for concentration variations

- **Identification**: Characteristic ratios help identify compounds

- **Quality control**: Ratio stability indicates system performance

### Output Columns

For each numerator/denominator pair, creates a new measure column named
`{output_prefix}{numerator}_{denominator}` (e.g., `"ratio_uv_ri"`).

### Log Transform

When `log_transform = TRUE`, computes `log(numerator / denominator)`
which can be useful for:

- Normalizing skewed distributions

- Converting multiplicative relationships to additive

- Working with absorbance ratios

## Note

Channels must be aligned to the same grid before computing ratios. Use
[`step_measure_channel_align()`](https://jameshwade.github.io/measure/dev/reference/step_measure_channel_align.md)
first if grids differ.

## See also

Other measure-channel:
[`step_measure_channel_align()`](https://jameshwade.github.io/measure/dev/reference/step_measure_channel_align.md),
[`step_measure_channel_combine()`](https://jameshwade.github.io/measure/dev/reference/step_measure_channel_combine.md)

## Examples

``` r
library(recipes)
library(tibble)

# Create sample multi-channel data
df <- tibble(
  id = rep(1:3, each = 10),
  time = rep(seq(0, 9, by = 1), 3),
  uv = rnorm(30, 100, 10),
  ri = rnorm(30, 50, 5),
  concentration = rep(c(10, 25, 50), each = 10)
)

# Compute UV/RI ratio
rec <- recipe(concentration ~ ., data = df) |>
  update_role(id, new_role = "id") |>
  step_measure_input_long(uv, location = vars(time)) |>
  step_measure_input_long(ri, location = vars(time)) |>
  step_measure_channel_ratio(numerator = "uv", denominator = "ri")
```
