# Trim Measurements to Specified Range

`step_measure_trim()` creates a *specification* of a recipe step that
keeps only the measurement points within the specified x-axis range(s).

## Usage

``` r
step_measure_trim(
  recipe,
  range,
  measures = NULL,
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_trim")
)
```

## Arguments

- recipe:

  A recipe object. The step will be added to the sequence of operations
  for this recipe.

- range:

  A numeric vector of length 2 specifying the range to keep as
  `c(min, max)`. Points with location \>= min and \<= max are retained.

- measures:

  An optional character vector of measure column names to process. If
  `NULL` (the default), all measure columns will be processed.

- role:

  Not used by this step since no new variables are created.

- trained:

  A logical to indicate if the step has been trained.

- skip:

  A logical. Should the step be skipped when baking?

- id:

  A character string that is unique to this step.

## Value

An updated version of `recipe` with the new step added.

## Details

This step filters measurements to keep only points within the specified
range. This is useful for:

- Defining integration windows (e.g., keep only 8-18 mL elution range)

- Removing noisy regions at start/end of measurement

- Focusing analysis on a region of interest

Points with location values outside the range are removed. The order of
remaining points is preserved.

## See also

[`step_measure_exclude()`](https://jameshwade.github.io/measure/dev/reference/step_measure_exclude.md)
for removing specific ranges,
[`step_measure_resample()`](https://jameshwade.github.io/measure/dev/reference/step_measure_resample.md)
for interpolating to a new grid

Other region-operations:
[`step_measure_exclude()`](https://jameshwade.github.io/measure/dev/reference/step_measure_exclude.md),
[`step_measure_resample()`](https://jameshwade.github.io/measure/dev/reference/step_measure_resample.md)

## Examples

``` r
library(recipes)

# Keep only a specific wavelength range
rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
  update_role(id, new_role = "id") |>
  step_measure_input_long(transmittance, location = vars(channel)) |>
  step_measure_trim(range = c(10, 90)) |>
  prep()

bake(rec, new_data = NULL)
#> # A tibble: 215 × 5
#>       id water   fat protein .measures
#>    <int> <dbl> <dbl>   <dbl>    <meas>
#>  1     1  60.5  22.5    16.7  [81 × 2]
#>  2     2  46    40.1    13.5  [81 × 2]
#>  3     3  71     8.4    20.5  [81 × 2]
#>  4     4  72.8   5.9    20.7  [81 × 2]
#>  5     5  58.3  25.5    15.5  [81 × 2]
#>  6     6  44    42.7    13.7  [81 × 2]
#>  7     7  44    42.7    13.7  [81 × 2]
#>  8     8  69.3  10.6    19.3  [81 × 2]
#>  9     9  61.4  19.9    17.7  [81 × 2]
#> 10    10  61.4  19.9    17.7  [81 × 2]
#> # ℹ 205 more rows
```
