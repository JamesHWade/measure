# Add Random Noise to Measurements

`step_measure_augment_noise()` creates a *specification* of a recipe
step that adds controlled random noise to spectral data for data
augmentation.

## Usage

``` r
step_measure_augment_noise(
  recipe,
  sd = 0.01,
  distribution = c("gaussian", "uniform"),
  relative = TRUE,
  measures = NULL,
  role = NA,
  trained = FALSE,
  skip = TRUE,
  id = recipes::rand_id("measure_augment_noise")
)
```

## Arguments

- recipe:

  A recipe object.

- sd:

  Standard deviation of noise. If `relative = TRUE` (default), this is
  relative to the signal range (0.01 = 1% of range). If
  `relative = FALSE`, this is the absolute noise level.

- distribution:

  Noise distribution: `"gaussian"` (default) or `"uniform"`.

- relative:

  Logical. If `TRUE` (default), `sd` is relative to signal range.

- measures:

  An optional character vector of measure column names.

- role:

  Not used.

- trained:

  Logical indicating if the step has been trained.

- skip:

  Logical. Should the step be skipped when baking? Default is `TRUE`,
  meaning augmentation only applies during training.

- id:

  Unique step identifier.

## Value

An updated recipe with the new step added.

## Details

Data augmentation adds variability to training data to improve model
robustness. Adding noise simulates measurement uncertainty and helps
models generalize better.

**Default behavior (`skip = TRUE`):** The augmentation is only applied
during [`prep()`](https://recipes.tidymodels.org/reference/prep.html) on
training data. When
[`bake()`](https://recipes.tidymodels.org/reference/bake.html) is called
on new data, the step is skipped.

**Reproducibility:** The noise is deterministic based on the row
content, so the same input always produces the same augmented output
within a session.

## See also

Other measure-augmentation:
[`step_measure_augment_scale()`](https://jameshwade.github.io/measure/dev/reference/step_measure_augment_scale.md),
[`step_measure_augment_shift()`](https://jameshwade.github.io/measure/dev/reference/step_measure_augment_shift.md)

## Examples

``` r
library(recipes)

rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
  update_role(id, new_role = "id") |>
  step_measure_input_long(transmittance, location = vars(channel)) |>
  step_measure_augment_noise(sd = 0.02) |>
  prep()

# Noise only applied to training data
bake(rec, new_data = NULL)
#> # A tibble: 215 × 5
#>       id water   fat protein .measures
#>    <int> <dbl> <dbl>   <dbl>    <meas>
#>  1     1  60.5  22.5    16.7 [100 × 2]
#>  2     2  46    40.1    13.5 [100 × 2]
#>  3     3  71     8.4    20.5 [100 × 2]
#>  4     4  72.8   5.9    20.7 [100 × 2]
#>  5     5  58.3  25.5    15.5 [100 × 2]
#>  6     6  44    42.7    13.7 [100 × 2]
#>  7     7  44    42.7    13.7 [100 × 2]
#>  8     8  69.3  10.6    19.3 [100 × 2]
#>  9     9  61.4  19.9    17.7 [100 × 2]
#> 10    10  61.4  19.9    17.7 [100 × 2]
#> # ℹ 205 more rows
```
