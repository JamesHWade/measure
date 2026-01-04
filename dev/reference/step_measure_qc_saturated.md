# Detect Saturated Measurements

`step_measure_qc_saturated()` creates a *specification* of a recipe step
that detects saturated (clipped) regions in measurements and adds
metadata columns indicating saturation status.

## Usage

``` r
step_measure_qc_saturated(
  recipe,
  measures = NULL,
  upper_limit = NULL,
  lower_limit = NULL,
  tolerance = 0.01,
  new_col_flag = ".saturated",
  new_col_pct = ".sat_pct",
  role = "predictor",
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_qc_saturated")
)
```

## Arguments

- recipe:

  A recipe object.

- measures:

  An optional character vector of measure column names.

- upper_limit:

  Upper saturation threshold. Default is `NULL` (auto-detect).

- lower_limit:

  Lower saturation threshold. Default is `NULL` (auto-detect).

- tolerance:

  How close to the limit counts as saturated. Default is 0.01.

- new_col_flag:

  Name of column for saturation flag. Default is `".saturated"`.

- new_col_pct:

  Name of column for saturation percentage. Default is `".sat_pct"`.

- role:

  Role for new columns. Default is `"predictor"`.

- trained:

  Logical indicating if the step has been trained.

- skip:

  Logical. Should the step be skipped when baking?

- id:

  Unique step identifier.

## Value

An updated recipe with the new step added.

## Details

Saturation occurs when detector response reaches its maximum (or
minimum) capacity. Saturated data points lose quantitative information
and may need special handling.

If limits are not specified, they are auto-detected as values appearing
as flat regions at extreme values (using
[`min()`](https://rdrr.io/r/base/Extremes.html) and
[`max()`](https://rdrr.io/r/base/Extremes.html)).

Two new columns are added:

- `.saturated`: Logical, TRUE if any saturation detected

- `.sat_pct`: Percentage of points that are saturated

## See also

Other measure-qc:
[`step_measure_impute()`](https://jameshwade.github.io/measure/dev/reference/step_measure_impute.md),
[`step_measure_qc_outlier()`](https://jameshwade.github.io/measure/dev/reference/step_measure_qc_outlier.md),
[`step_measure_qc_snr()`](https://jameshwade.github.io/measure/dev/reference/step_measure_qc_snr.md)

## Examples

``` r
library(recipes)

rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
  update_role(id, new_role = "id") |>
  step_measure_input_long(transmittance, location = vars(channel)) |>
  step_measure_qc_saturated() |>
  prep()

bake(rec, new_data = NULL)
#> # A tibble: 215 × 8
#>       id water   fat protein .measures channel     .saturated .sat_pct
#>    <int> <dbl> <dbl>   <dbl>    <meas> <list>      <lgl>         <dbl>
#>  1     1  60.5  22.5    16.7 [100 × 2] <int [100]> FALSE             0
#>  2     2  46    40.1    13.5 [100 × 2] <int [100]> FALSE             0
#>  3     3  71     8.4    20.5 [100 × 2] <int [100]> FALSE             0
#>  4     4  72.8   5.9    20.7 [100 × 2] <int [100]> FALSE             0
#>  5     5  58.3  25.5    15.5 [100 × 2] <int [100]> FALSE             0
#>  6     6  44    42.7    13.7 [100 × 2] <int [100]> FALSE             0
#>  7     7  44    42.7    13.7 [100 × 2] <int [100]> FALSE             0
#>  8     8  69.3  10.6    19.3 [100 × 2] <int [100]> FALSE             0
#>  9     9  61.4  19.9    17.7 [100 × 2] <int [100]> FALSE             0
#> 10    10  61.4  19.9    17.7 [100 × 2] <int [100]> FALSE             0
#> # ℹ 205 more rows
```
