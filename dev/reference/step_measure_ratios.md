# Calculate Region Ratios

`step_measure_ratios()` creates a *specification* of a recipe step that
calculates ratios between integrated regions.

## Usage

``` r
step_measure_ratios(
  recipe,
  numerator,
  denominator,
  name = NULL,
  method = c("trapezoid", "simpson"),
  measures = NULL,
  prefix = "ratio_",
  role = "predictor",
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_ratios")
)
```

## Arguments

- recipe:

  A recipe object.

- numerator:

  A numeric vector of length 2 specifying the numerator region.

- denominator:

  A numeric vector of length 2 specifying the denominator region.

- name:

  Output column name. If `NULL`, auto-generated from prefix.

- method:

  Integration method: `"trapezoid"` (default) or `"simpson"`.

- measures:

  An optional character vector of measure column names.

- prefix:

  Prefix for output column name if `name` is NULL. Default is
  `"ratio_"`.

- role:

  Role for generated column. Default is `"predictor"`.

- trained:

  Logical indicating if the step has been trained.

- skip:

  Logical. Should the step be skipped when baking?

- id:

  Unique step identifier.

## Value

An updated recipe with the new step added.

## Details

This step calculates the ratio of integrated areas between two regions:
`ratio = integral(numerator) / integral(denominator)`

This is useful for calculating peak ratios in spectroscopy, or relative
concentrations in chromatography.

If the denominator integral is zero or NA, the ratio will be NA.

## See also

Other measure-features:
[`step_measure_bin()`](https://jameshwade.github.io/measure/dev/reference/step_measure_bin.md),
[`step_measure_integrals()`](https://jameshwade.github.io/measure/dev/reference/step_measure_integrals.md),
[`step_measure_moments()`](https://jameshwade.github.io/measure/dev/reference/step_measure_moments.md)

## Examples

``` r
library(recipes)

rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
  update_role(id, new_role = "id") |>
  step_measure_input_long(transmittance, location = vars(channel)) |>
  step_measure_ratios(
    numerator = c(1, 30),
    denominator = c(70, 100),
    name = "low_high_ratio"
  ) |>
  prep()

bake(rec, new_data = NULL)
#> # A tibble: 215 × 7
#>       id water   fat protein .measures channel     low_high_ratio
#>    <int> <dbl> <dbl>   <dbl>    <meas> <list>               <dbl>
#>  1     1  60.5  22.5    16.7 [100 × 2] <int [100]>          0.824
#>  2     2  46    40.1    13.5 [100 × 2] <int [100]>          0.839
#>  3     3  71     8.4    20.5 [100 × 2] <int [100]>          0.896
#>  4     4  72.8   5.9    20.7 [100 × 2] <int [100]>          0.882
#>  5     5  58.3  25.5    15.5 [100 × 2] <int [100]>          0.800
#>  6     6  44    42.7    13.7 [100 × 2] <int [100]>          0.829
#>  7     7  44    42.7    13.7 [100 × 2] <int [100]>          0.841
#>  8     8  69.3  10.6    19.3 [100 × 2] <int [100]>          0.821
#>  9     9  61.4  19.9    17.7 [100 × 2] <int [100]>          0.824
#> 10    10  61.4  19.9    17.7 [100 × 2] <int [100]>          0.828
#> # ℹ 205 more rows
```
