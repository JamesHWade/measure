# Random Intensity Scaling

`step_measure_augment_scale()` creates a *specification* of a recipe
step that applies random intensity scaling for scale invariance
training.

## Usage

``` r
step_measure_augment_scale(
  recipe,
  range = c(0.9, 1.1),
  measures = NULL,
  role = NA,
  trained = FALSE,
  skip = TRUE,
  id = recipes::rand_id("measure_augment_scale")
)
```

## Arguments

- recipe:

  A recipe object.

- range:

  A numeric vector of length 2 specifying the range of scaling factors.
  Default is `c(0.9, 1.1)`, meaning 90%-110% of original.

- measures:

  An optional character vector of measure column names.

- role:

  Not used.

- trained:

  Logical indicating if the step has been trained.

- skip:

  Logical. Should the step be skipped when baking? Default is `TRUE`.

- id:

  Unique step identifier.

## Value

An updated recipe with the new step added.

## Details

This step multiplies spectrum values by a random scaling factor sampled
uniformly from the specified range. This helps models become robust to
variations in signal intensity.

**Common use cases:**

- Simulating concentration variations

- Compensating for detector sensitivity differences

- Making models robust to sample preparation variability

**Default behavior (`skip = TRUE`):** The scaling is only applied during
training. When predicting on new data, the step is skipped.

## See also

Other measure-augmentation:
[`step_measure_augment_noise()`](https://jameshwade.github.io/measure/dev/reference/step_measure_augment_noise.md),
[`step_measure_augment_shift()`](https://jameshwade.github.io/measure/dev/reference/step_measure_augment_shift.md)

## Examples

``` r
library(recipes)

rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
  update_role(id, new_role = "id") |>
  step_measure_input_long(transmittance, location = vars(channel)) |>
  step_measure_augment_scale(range = c(0.8, 1.2)) |>
  prep()

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
