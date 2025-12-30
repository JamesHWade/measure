# Calculate Limit of Quantitation (LOQ)

Calculates the limit of quantitation using one of several accepted
methods. The method used is explicitly documented in the output.

## Usage

``` r
measure_loq(
  data,
  response_col,
  method = c("blank_sd", "calibration", "sn", "precision"),
  conc_col = "nominal_conc",
  sample_type_col = "sample_type",
  calibration = NULL,
  k = 10,
  sn_col = NULL,
  noise = NULL,
  sn_threshold = 10,
  precision_cv = 20,
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

- k:

  Multiplier for SD. Default is 10 for LOQ.

- sn_col:

  Column containing S/N ratios (for `"sn"` method).

- noise:

  Noise estimate for S/N calculation (alternative to `sn_col`).

- sn_threshold:

  S/N threshold for LOQ (default 10).

- precision_cv:

  Maximum allowable CV for LOQ (default 20%).

- ...:

  Additional arguments passed to method-specific calculations.

## Value

A `measure_loq` object containing:

- `value`: The LOQ value

- `method`: Method used

- `parameters`: Method-specific parameters

- `uncertainty`: Uncertainty estimate (when available)

## Details

### Blank SD Method

LOQ = mean(blank) + k \* SD(blank)

Where k is typically 10. This is a simple but widely accepted approach.

### Calibration Method

LOQ = k \* sigma / slope

Where sigma is the residual standard error of the calibration curve and
slope is the calibration slope. k is typically 10 for LOQ.

### Signal-to-Noise Method

LOQ is the concentration where S/N = threshold (typically 10:1).

### Precision-Based Method

LOQ is the lowest concentration where precision (CV) is \<= the
specified criterion (typically 20% for bioanalytical methods).

## See also

[`measure_lod()`](https://jameshwade.github.io/measure/dev/reference/measure_lod.md)
for limit of detection,
[`measure_lod_loq()`](https://jameshwade.github.io/measure/dev/reference/measure_lod_loq.md)
for calculating both together.

## Examples

``` r
# Create sample data with blanks
data <- data.frame(
  sample_type = c(rep("blank", 10), rep("standard", 5)),
  response = c(rnorm(10, mean = 0.5, sd = 0.1),
               c(5, 15, 35, 70, 150)),
  nominal_conc = c(rep(0, 10), c(10, 25, 50, 100, 200))
)

# LOQ from blank SD
measure_loq(data, "response", method = "blank_sd")
#> <measure_loq>
#>   Value: 1.537
#>   Method: blank_sd
#>   k: 10
#>   Uncertainty: 0.3326
#>   Parameters:
#>     blank_mean: 0.4853
#>     blank_sd: 0.1052
#>     n_blanks: 10
```
