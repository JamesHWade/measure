# Calculate Signal-to-Noise Ratio

`step_measure_qc_snr()` creates a *specification* of a recipe step that
calculates the signal-to-noise ratio (SNR) for each measurement and adds
it as a new column. This is useful for quality control and filtering.

## Usage

``` r
step_measure_qc_snr(
  recipe,
  measures = NULL,
  new_col = ".snr",
  signal_method = c("max", "range", "rms"),
  noise_method = c("diff", "mad", "residual"),
  role = "predictor",
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_qc_snr")
)
```

## Arguments

- recipe:

  A recipe object.

- measures:

  An optional character vector of measure column names.

- new_col:

  Name of the new column to store SNR values. Default is `".snr"`.

- signal_method:

  How to estimate signal:

  - `"max"` (default): Maximum absolute value

  - `"range"`: Peak-to-peak range

  - `"rms"`: Root mean square

- noise_method:

  How to estimate noise:

  - `"diff"` (default): RMS of first differences (estimates high-freq
    noise)

  - `"mad"`: Median absolute deviation of values

  - `"residual"`: Residuals from smoothed fit

- role:

  Role for the new column. Default is `"predictor"`.

- trained:

  Logical indicating if the step has been trained.

- skip:

  Logical. Should the step be skipped when baking?

- id:

  Unique step identifier.

## Value

An updated recipe with the new step added.

## Details

SNR is calculated as `signal / noise`, where signal and noise are
estimated using the specified methods. Higher values indicate cleaner
data.

The `"diff"` noise method is particularly useful because it estimates
high-frequency noise without being affected by broad spectral features:
\$\$noise = \sqrt{\frac{1}{2(n-1)} \sum\_{i=2}^{n} (x_i -
x\_{i-1})^2}\$\$

## See also

Other measure-qc:
[`step_measure_impute()`](https://jameshwade.github.io/measure/dev/reference/step_measure_impute.md),
[`step_measure_qc_outlier()`](https://jameshwade.github.io/measure/dev/reference/step_measure_qc_outlier.md),
[`step_measure_qc_saturated()`](https://jameshwade.github.io/measure/dev/reference/step_measure_qc_saturated.md)

## Examples

``` r
library(recipes)

rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
  update_role(id, new_role = "id") |>
  step_measure_input_long(transmittance, location = vars(channel)) |>
  step_measure_qc_snr() |>
  prep()

bake(rec, new_data = NULL)
#> # A tibble: 215 × 7
#>       id water   fat protein .measures channel      .snr
#>    <int> <dbl> <dbl>   <dbl>    <meas> <list>      <dbl>
#>  1     1  60.5  22.5    16.7 [100 × 2] <int [100]>  277.
#>  2     2  46    40.1    13.5 [100 × 2] <int [100]>  370.
#>  3     3  71     8.4    20.5 [100 × 2] <int [100]>  291.
#>  4     4  72.8   5.9    20.7 [100 × 2] <int [100]>  278.
#>  5     5  58.3  25.5    15.5 [100 × 2] <int [100]>  271.
#>  6     6  44    42.7    13.7 [100 × 2] <int [100]>  374.
#>  7     7  44    42.7    13.7 [100 × 2] <int [100]>  374.
#>  8     8  69.3  10.6    19.3 [100 × 2] <int [100]>  255.
#>  9     9  61.4  19.9    17.7 [100 × 2] <int [100]>  310.
#> 10    10  61.4  19.9    17.7 [100 × 2] <int [100]>  303.
#> # ℹ 205 more rows
```
