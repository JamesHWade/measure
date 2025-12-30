# Calculate Limit of Detection (LOD)

Calculates the limit of detection using one of several accepted methods.
The method used is explicitly documented in the output.

## Usage

``` r
measure_lod(
  data,
  response_col,
  method = c("blank_sd", "calibration", "sn", "precision"),
  conc_col = "nominal_conc",
  sample_type_col = "sample_type",
  calibration = NULL,
  k = 3,
  sn_col = NULL,
  noise = NULL,
  sn_threshold = 3,
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

  Multiplier for SD. Default is 3 for LOD.

- sn_col:

  Column containing S/N ratios (for `"sn"` method).

- noise:

  Noise estimate for S/N calculation (alternative to `sn_col`).

- sn_threshold:

  S/N threshold for LOD (default 3).

- ...:

  Additional arguments passed to method-specific calculations.

## Value

A `measure_lod` object containing:

- `value`: The LOD value

- `method`: Method used

- `parameters`: Method-specific parameters

- `uncertainty`: Uncertainty estimate (when available)

## Details

### Blank SD Method

LOD = mean(blank) + k \* SD(blank)

Where k is typically 3. This is a simple but widely accepted approach.

### Calibration Method

LOD = k \* sigma / slope

Where sigma is the residual standard error of the calibration curve and
slope is the calibration slope. k is typically 3.3 for LOD.

### Signal-to-Noise Method

LOD is the concentration where S/N = threshold (typically 3:1).

### Precision-Based Method

LOD is the lowest concentration where precision (CV) meets a specified
criterion.

## See also

[`measure_loq()`](https://jameshwade.github.io/measure/dev/reference/measure_loq.md)
for limit of quantitation,
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

# LOD from blank SD
measure_lod(data, "response", method = "blank_sd")
#> <measure_lod>
#>   Value: 0.6889
#>   Method: blank_sd
#>   k: 3
#>   Uncertainty: 0.08057
#>   Parameters:
#>     blank_mean: 0.4341
#>     blank_sd: 0.08493
#>     n_blanks: 10

# LOD from calibration curve
cal <- measure_calibration_fit(
  data[data$sample_type == "standard", ],
  response ~ nominal_conc
)
measure_lod(data, "response", method = "calibration", calibration = cal)
#> <measure_lod>
#>   Value: 7.71
#>   Method: calibration
#>   k: 3
#>   Parameters:
#>     sigma: 1.783
#>     slope: 0.7634
```
