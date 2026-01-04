# Exclude Measurement Ranges

`step_measure_exclude()` creates a *specification* of a recipe step that
removes measurement points within the specified x-axis range(s).

## Usage

``` r
step_measure_exclude(
  recipe,
  ranges,
  measures = NULL,
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_exclude")
)
```

## Arguments

- recipe:

  A recipe object. The step will be added to the sequence of operations
  for this recipe.

- ranges:

  A list of numeric vectors, each of length 2 specifying ranges to
  exclude as `c(min, max)`. Points with location \>= min and \<= max in
  any range are removed.

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

This step removes measurements falling within specified ranges. This is
useful for:

- Removing solvent peaks in chromatography

- Excluding system peaks or artifacts

- Removing detector saturation regions

- Removing known interference regions in spectroscopy

Multiple ranges can be excluded by providing a list of ranges. Points
falling within any of the specified ranges are removed.

## See also

[`step_measure_trim()`](https://jameshwade.github.io/measure/dev/reference/step_measure_trim.md)
for keeping specific ranges,
[`step_measure_resample()`](https://jameshwade.github.io/measure/dev/reference/step_measure_resample.md)
for interpolating to a new grid

Other region-operations:
[`step_measure_resample()`](https://jameshwade.github.io/measure/dev/reference/step_measure_resample.md),
[`step_measure_trim()`](https://jameshwade.github.io/measure/dev/reference/step_measure_trim.md)

## Examples

``` r
library(recipes)

# Exclude specific regions (e.g., solvent peaks)
rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
  update_role(id, new_role = "id") |>
  step_measure_input_long(transmittance, location = vars(channel)) |>
  step_measure_exclude(ranges = list(c(1, 5), c(95, 100))) |>
  prep()

bake(rec, new_data = NULL)
#> # A tibble: 215 × 6
#>       id water   fat protein .measures channel    
#>    <int> <dbl> <dbl>   <dbl>    <meas> <list>     
#>  1     1  60.5  22.5    16.7  [89 × 2] <int [100]>
#>  2     2  46    40.1    13.5  [89 × 2] <int [100]>
#>  3     3  71     8.4    20.5  [89 × 2] <int [100]>
#>  4     4  72.8   5.9    20.7  [89 × 2] <int [100]>
#>  5     5  58.3  25.5    15.5  [89 × 2] <int [100]>
#>  6     6  44    42.7    13.7  [89 × 2] <int [100]>
#>  7     7  44    42.7    13.7  [89 × 2] <int [100]>
#>  8     8  69.3  10.6    19.3  [89 × 2] <int [100]>
#>  9     9  61.4  19.9    17.7  [89 × 2] <int [100]>
#> 10    10  61.4  19.9    17.7  [89 × 2] <int [100]>
#> # ℹ 205 more rows
```
