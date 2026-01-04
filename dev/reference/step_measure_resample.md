# Resample Measurements to New Grid

`step_measure_resample()` creates a *specification* of a recipe step
that interpolates measurements to a new regular x-axis grid.

## Usage

``` r
step_measure_resample(
  recipe,
  n = NULL,
  spacing = NULL,
  range = NULL,
  method = c("linear", "spline"),
  measures = NULL,
  role = NA,
  trained = FALSE,
  new_locations = NULL,
  skip = FALSE,
  id = recipes::rand_id("measure_resample")
)
```

## Arguments

- recipe:

  A recipe object. The step will be added to the sequence of operations
  for this recipe.

- n:

  A positive integer specifying the number of points in the new grid.
  Mutually exclusive with `spacing`.

- spacing:

  A positive numeric value specifying the spacing between points in the
  new grid. Mutually exclusive with `n`.

- range:

  Optional numeric vector of length 2 specifying the range for the new
  grid as `c(min, max)`. If `NULL` (default), uses the range of the
  existing measurements.

- method:

  The interpolation method. One of:

  - `"linear"`: Linear interpolation (default)

  - `"spline"`: Cubic spline interpolation (smoother)

- measures:

  An optional character vector of measure column names to process. If
  `NULL` (the default), all measure columns will be processed.

- role:

  Not used by this step since no new variables are created.

- trained:

  A logical to indicate if the step has been trained.

- new_locations:

  The computed new grid locations (after training).

- skip:

  A logical. Should the step be skipped when baking?

- id:

  A character string that is unique to this step.

## Value

An updated version of `recipe` with the new step added.

## Details

This step interpolates measurements to a new regular grid of x-axis
values. This is useful for:

- Aligning data from different instruments with different sampling rates

- Reducing data density for faster processing

- Ensuring uniform spacing for methods that require it

- Matching measurements to a reference grid

The new grid is determined during
[`prep()`](https://recipes.tidymodels.org/reference/prep.html) based on
the training data. If `range` is not specified, the grid spans from the
minimum to maximum location values in the training data.

**Interpolation methods:**

- `"linear"`: Fast and simple, may introduce slight distortion at peaks

- `"spline"`: Smoother interpolation that preserves peak shape better

## See also

[`step_measure_trim()`](https://jameshwade.github.io/measure/dev/reference/step_measure_trim.md)
for keeping specific ranges,
[`step_measure_exclude()`](https://jameshwade.github.io/measure/dev/reference/step_measure_exclude.md)
for removing specific ranges

Other region-operations:
[`step_measure_exclude()`](https://jameshwade.github.io/measure/dev/reference/step_measure_exclude.md),
[`step_measure_trim()`](https://jameshwade.github.io/measure/dev/reference/step_measure_trim.md)

## Examples

``` r
library(recipes)

# Resample to 50 evenly spaced points
rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
  update_role(id, new_role = "id") |>
  step_measure_input_long(transmittance, location = vars(channel)) |>
  step_measure_resample(n = 50) |>
  prep()

bake(rec, new_data = NULL)
#> # A tibble: 215 × 6
#>       id water   fat protein .measures channel    
#>    <int> <dbl> <dbl>   <dbl>    <meas> <list>     
#>  1     1  60.5  22.5    16.7  [50 × 2] <int [100]>
#>  2     2  46    40.1    13.5  [50 × 2] <int [100]>
#>  3     3  71     8.4    20.5  [50 × 2] <int [100]>
#>  4     4  72.8   5.9    20.7  [50 × 2] <int [100]>
#>  5     5  58.3  25.5    15.5  [50 × 2] <int [100]>
#>  6     6  44    42.7    13.7  [50 × 2] <int [100]>
#>  7     7  44    42.7    13.7  [50 × 2] <int [100]>
#>  8     8  69.3  10.6    19.3  [50 × 2] <int [100]>
#>  9     9  61.4  19.9    17.7  [50 × 2] <int [100]>
#> 10    10  61.4  19.9    17.7  [50 × 2] <int [100]>
#> # ℹ 205 more rows

# Resample with specific spacing
rec2 <- recipe(water + fat + protein ~ ., data = meats_long) |>
  update_role(id, new_role = "id") |>
  step_measure_input_long(transmittance, location = vars(channel)) |>
  step_measure_resample(spacing = 2, method = "spline") |>
  prep()

bake(rec2, new_data = NULL)
#> # A tibble: 215 × 6
#>       id water   fat protein .measures channel    
#>    <int> <dbl> <dbl>   <dbl>    <meas> <list>     
#>  1     1  60.5  22.5    16.7  [51 × 2] <int [100]>
#>  2     2  46    40.1    13.5  [51 × 2] <int [100]>
#>  3     3  71     8.4    20.5  [51 × 2] <int [100]>
#>  4     4  72.8   5.9    20.7  [51 × 2] <int [100]>
#>  5     5  58.3  25.5    15.5  [51 × 2] <int [100]>
#>  6     6  44    42.7    13.7  [51 × 2] <int [100]>
#>  7     7  44    42.7    13.7  [51 × 2] <int [100]>
#>  8     8  69.3  10.6    19.3  [51 × 2] <int [100]>
#>  9     9  61.4  19.9    17.7  [51 × 2] <int [100]>
#> 10    10  61.4  19.9    17.7  [51 × 2] <int [100]>
#> # ℹ 205 more rows
```
