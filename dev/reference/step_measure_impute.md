# Impute Missing Values in Measurements

`step_measure_impute()` creates a *specification* of a recipe step that
imputes (fills in) missing values (NA) in measurement data using
interpolation or other methods.

## Usage

``` r
step_measure_impute(
  recipe,
  measures = NULL,
  method = c("linear", "spline", "constant", "mean"),
  max_gap = Inf,
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_impute")
)
```

## Arguments

- recipe:

  A recipe object.

- measures:

  An optional character vector of measure column names.

- method:

  Imputation method:

  - `"linear"` (default): Linear interpolation

  - `"spline"`: Cubic spline interpolation

  - `"constant"`: Nearest non-NA value

  - `"mean"`: Global mean of non-NA values

- max_gap:

  Maximum gap size to impute. Gaps larger than this are left as NA.
  Default is `Inf` (impute all).

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

Missing values can occur due to:

- Removed spikes (after despiking with replacement set to NA)

- Excluded regions

- Instrument gaps or dropouts

Linear and spline interpolation use the
[`stats::approx()`](https://rdrr.io/r/stats/approxfun.html) and
[`stats::spline()`](https://rdrr.io/r/stats/splinefun.html) functions
respectively. They are most appropriate when gaps are small relative to
spectral features.

## See also

Other measure-qc:
[`step_measure_qc_outlier()`](https://jameshwade.github.io/measure/dev/reference/step_measure_qc_outlier.md),
[`step_measure_qc_saturated()`](https://jameshwade.github.io/measure/dev/reference/step_measure_qc_saturated.md),
[`step_measure_qc_snr()`](https://jameshwade.github.io/measure/dev/reference/step_measure_qc_snr.md)

## Examples

``` r
library(recipes)

rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
  update_role(id, new_role = "id") |>
  step_measure_input_long(transmittance, location = vars(channel)) |>
  step_measure_impute(method = "linear") |>
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
