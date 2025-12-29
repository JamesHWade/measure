# Calculate Statistical Moments

`step_measure_moments()` creates a *specification* of a recipe step that
calculates statistical moments from spectra.

## Usage

``` r
step_measure_moments(
  recipe,
  moments = c("mean", "sd", "skewness", "kurtosis"),
  weighted = FALSE,
  measures = NULL,
  prefix = "moment_",
  role = "predictor",
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_moments")
)
```

## Arguments

- recipe:

  A recipe object.

- moments:

  Character vector specifying which moments to calculate. Options:
  `"mean"`, `"sd"`, `"skewness"`, `"kurtosis"`, `"entropy"`. Default is
  `c("mean", "sd", "skewness", "kurtosis")`.

- weighted:

  Logical. If `TRUE`, moments are weighted by location values. Default
  is `FALSE`.

- measures:

  An optional character vector of measure column names.

- prefix:

  Prefix for output column names. Default is `"moment_"`.

- role:

  Role for generated columns. Default is `"predictor"`.

- trained:

  Logical indicating if the step has been trained.

- skip:

  Logical. Should the step be skipped when baking?

- id:

  Unique step identifier.

## Value

An updated recipe with the new step added.

## Details

This step calculates statistical moments that summarize the distribution
of values in each spectrum:

|          |                                            |
|----------|--------------------------------------------|
| Moment   | Description                                |
| mean     | Mean value of the spectrum                 |
| sd       | Standard deviation of values               |
| skewness | Asymmetry of the distribution              |
| kurtosis | "Tailedness" of the distribution           |
| entropy  | Shannon entropy (requires positive values) |

When `weighted = TRUE`, the location (x-axis) values are used as
weights, which can be useful for calculating center of mass or weighted
statistics.

## See also

Other measure-features:
[`step_measure_bin()`](https://jameshwade.github.io/measure/dev/reference/step_measure_bin.md),
[`step_measure_integrals()`](https://jameshwade.github.io/measure/dev/reference/step_measure_integrals.md),
[`step_measure_ratios()`](https://jameshwade.github.io/measure/dev/reference/step_measure_ratios.md)

## Examples

``` r
library(recipes)

rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
  update_role(id, new_role = "id") |>
  step_measure_input_long(transmittance, location = vars(channel)) |>
  step_measure_moments(moments = c("mean", "sd", "skewness")) |>
  prep()

bake(rec, new_data = NULL)
#> # A tibble: 215 × 8
#>       id water   fat protein .measures moment_mean moment_sd moment_skewness
#>    <int> <dbl> <dbl>   <dbl>    <meas>       <dbl>     <dbl>           <dbl>
#>  1     1  60.5  22.5    16.7 [100 × 2]        2.97     0.270          0.222 
#>  2     2  46    40.1    13.5 [100 × 2]        3.24     0.234         -0.311 
#>  3     3  71     8.4    20.5 [100 × 2]        2.82     0.206          0.536 
#>  4     4  72.8   5.9    20.7 [100 × 2]        3.09     0.238          0.540 
#>  5     5  58.3  25.5    15.5 [100 × 2]        3.25     0.326          0.102 
#>  6     6  44    42.7    13.7 [100 × 2]        3.48     0.262         -0.387 
#>  7     7  44    42.7    13.7 [100 × 2]        3.44     0.249         -0.405 
#>  8     8  69.3  10.6    19.3 [100 × 2]        2.87     0.280          0.338 
#>  9     9  61.4  19.9    17.7 [100 × 2]        3.79     0.329          0.0588
#> 10    10  61.4  19.9    17.7 [100 × 2]        3.94     0.345          0.0582
#> # ℹ 205 more rows
```
