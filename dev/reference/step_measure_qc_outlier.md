# Detect Outlier Samples

`step_measure_qc_outlier()` creates a *specification* of a recipe step
that detects outlier samples using Mahalanobis distance or PCA-based
methods. A new column is added indicating outlier status.

## Usage

``` r
step_measure_qc_outlier(
  recipe,
  measures = NULL,
  method = c("mahalanobis", "pca"),
  threshold = 3,
  n_components = NULL,
  new_col = ".outlier",
  new_col_score = ".outlier_score",
  role = "predictor",
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_qc_outlier")
)
```

## Arguments

- recipe:

  A recipe object.

- measures:

  An optional character vector of measure column names.

- method:

  Detection method:

  - `"mahalanobis"` (default): Mahalanobis distance with robust
    covariance

  - `"pca"`: PCA score-based outliers (Hotelling's T^2)

- threshold:

  Threshold for outlier detection in standard deviation units. Default
  is 3. Tunable via
  [`outlier_threshold()`](https://jameshwade.github.io/measure/dev/reference/outlier_threshold.md).

- n_components:

  For PCA method, number of components to use. Default is `NULL`
  (auto-select based on variance explained).

- new_col:

  Name of the new outlier flag column. Default is `".outlier"`.

- new_col_score:

  Name of the outlier score column. Default is `".outlier_score"`.

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

Outlier samples can arise from measurement errors, sample preparation
issues, or genuine unusual samples. This step helps identify them.

**Mahalanobis method**: Computes the multivariate distance from each
sample to the center of the distribution, accounting for correlations.
Uses robust estimation of center and covariance via median and MAD.

**PCA method**: Projects data onto principal components and computes
Hotelling's T^2 statistic. Samples with extreme scores are flagged.

Two columns are added:

- `.outlier`: Logical flag

- `.outlier_score`: Numeric score (higher = more extreme)

## See also

Other measure-qc:
[`step_measure_impute()`](https://jameshwade.github.io/measure/dev/reference/step_measure_impute.md),
[`step_measure_qc_saturated()`](https://jameshwade.github.io/measure/dev/reference/step_measure_qc_saturated.md),
[`step_measure_qc_snr()`](https://jameshwade.github.io/measure/dev/reference/step_measure_qc_snr.md)

## Examples

``` r
library(recipes)

rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
  update_role(id, new_role = "id") |>
  step_measure_input_long(transmittance, location = vars(channel)) |>
  step_measure_qc_outlier(threshold = 3) |>
  prep()

bake(rec, new_data = NULL)
#> # A tibble: 215 × 7
#>       id water   fat protein .measures .outlier .outlier_score
#>    <int> <dbl> <dbl>   <dbl>    <meas> <lgl>             <dbl>
#>  1     1  60.5  22.5    16.7 [100 × 2] FALSE             0.305
#>  2     2  46    40.1    13.5 [100 × 2] FALSE             0.323
#>  3     3  71     8.4    20.5 [100 × 2] FALSE             0.626
#>  4     4  72.8   5.9    20.7 [100 × 2] FALSE             0.133
#>  5     5  58.3  25.5    15.5 [100 × 2] FALSE             0.310
#>  6     6  44    42.7    13.7 [100 × 2] FALSE             0.793
#>  7     7  44    42.7    13.7 [100 × 2] FALSE             0.731
#>  8     8  69.3  10.6    19.3 [100 × 2] FALSE             0.522
#>  9     9  61.4  19.9    17.7 [100 × 2] FALSE             1.44 
#> 10    10  61.4  19.9    17.7 [100 × 2] FALSE             1.78 
#> # ℹ 205 more rows
```
