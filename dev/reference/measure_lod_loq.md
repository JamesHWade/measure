# Calculate LOD and LOQ Together

Convenience function to calculate both LOD and LOQ using the same
method.

## Usage

``` r
measure_lod_loq(
  data,
  response_col,
  method = c("blank_sd", "calibration", "sn", "precision"),
  conc_col = "nominal_conc",
  sample_type_col = "sample_type",
  calibration = NULL,
  k_lod = NULL,
  k_loq = 10,
  ...
)
```

## Arguments

- data:

  A data frame containing the measurement data.

- response_col:

  Name of the response column.

- method:

  Method for LOD calculation:

  - `"blank_sd"`: 3 \* SD of blank samples (requires
    `sample_type == "blank"`)

  - `"calibration"`: 3.3 \* sigma / slope from calibration curve

  - `"sn"`: Signal-to-noise ratio method (requires `sn_col` or noise
    estimate)

  - `"precision"`: Based on acceptable precision at low concentrations

- conc_col:

  Name of concentration column (for calibration method).

- sample_type_col:

  Name of sample type column. Default is `"sample_type"`.

- calibration:

  Optional
  [measure_calibration](https://jameshwade.github.io/measure/dev/reference/measure_calibration.md)
  object for calibration method.

- k_lod:

  Multiplier for LOD (default 3 or 3.3 for calibration).

- k_loq:

  Multiplier for LOQ (default 10).

- ...:

  Additional arguments passed to method-specific calculations.

## Value

A list with components `lod` and `loq`, each being the respective limit
object.

## See also

[`measure_lod()`](https://jameshwade.github.io/measure/dev/reference/measure_lod.md),
[`measure_loq()`](https://jameshwade.github.io/measure/dev/reference/measure_loq.md).

## Examples

``` r
data <- data.frame(
  sample_type = c(rep("blank", 10), rep("standard", 5)),
  response = c(rnorm(10, mean = 0.5, sd = 0.1),
               c(5, 15, 35, 70, 150)),
  nominal_conc = c(rep(0, 10), c(10, 25, 50, 100, 200))
)

limits <- measure_lod_loq(data, "response", method = "blank_sd")
limits$lod
#> <measure_lod>
#>   Value: 0.752
#>   Method: blank_sd
#>   k: 3
#>   Uncertainty: 0.06627
#>   Parameters:
#>     blank_mean: 0.5424
#>     blank_sd: 0.06986
#>     n_blanks: 10
limits$loq
#> <measure_loq>
#>   Value: 1.241
#>   Method: blank_sd
#>   k: 10
#>   Uncertainty: 0.2209
#>   Parameters:
#>     blank_mean: 0.5424
#>     blank_sd: 0.06986
#>     n_blanks: 10
```
