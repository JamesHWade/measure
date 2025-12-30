# Verify Calibration Curve Performance

Evaluates the performance of a calibration curve using verification
samples (continuing calibration verification - CCV, or independent QC
samples). This function assesses whether the calibration remains valid
during or between analytical runs.

## Usage

``` r
measure_calibration_verify(
  calibration,
  verification_data,
  nominal_col = "nominal_conc",
  acceptance_pct = 15,
  acceptance_pct_lloq = 20,
  lloq = NULL,
  sample_type_col = NULL,
  criteria = NULL
)
```

## Arguments

- calibration:

  A
  [measure_calibration](https://jameshwade.github.io/measure/dev/reference/measure_calibration.md)
  object from
  [`measure_calibration_fit()`](https://jameshwade.github.io/measure/dev/reference/measure_calibration_fit.md).

- verification_data:

  A data frame containing verification samples with known
  concentrations.

- nominal_col:

  Name of the column containing nominal (known) concentrations. Default
  is `"nominal_conc"`.

- acceptance_pct:

  Acceptance criterion as percent deviation from nominal. Default is 15
  (i.e., ±15%).

- acceptance_pct_lloq:

  Acceptance criterion for samples at the lower limit of quantitation
  (LLOQ). Default is 20 (i.e., ±20%).

- lloq:

  Lower limit of quantitation. Samples at or near this level use
  `acceptance_pct_lloq`. Default is NULL (use same criterion for all).

- sample_type_col:

  Optional column indicating sample types. Only samples with type
  containing "qc" or "ccv" will be used if specified.

- criteria:

  Optional
  [measure_criteria](https://jameshwade.github.io/measure/dev/reference/measure_criteria.md)
  object for custom acceptance criteria. If provided, overrides
  `acceptance_pct` settings.

## Value

A `measure_calibration_verify` object (a tibble) containing:

- Predicted concentrations

- Accuracy (%nominal)

- Deviation from nominal (%)

- Pass/fail status for each sample

- Overall verification status

## Details

### Verification Workflow

Calibration verification is typically performed:

1.  At the beginning and end of analytical batches

2.  After every N unknown samples (e.g., every 10)

3.  When instrument performance is in question

### Acceptance Criteria

Default criteria are based on bioanalytical guidelines:

- Standard samples: ±15% of nominal

- LLOQ samples: ±20% of nominal

For more stringent applications (e.g., clinical chemistry), consider
using ±10% or providing custom criteria.

## See also

[`measure_calibration_fit()`](https://jameshwade.github.io/measure/dev/reference/measure_calibration_fit.md)
for fitting calibration curves,
[`measure_calibration_predict()`](https://jameshwade.github.io/measure/dev/reference/measure_calibration_predict.md)
for prediction,
[`measure_criteria()`](https://jameshwade.github.io/measure/dev/reference/measure_criteria.md)
for custom acceptance criteria.

## Examples

``` r
# Fit calibration
cal_data <- data.frame(
  nominal_conc = c(1, 5, 10, 50, 100, 500),
  response = c(1.2, 5.8, 11.3, 52.1, 105.2, 498.7)
)
cal <- measure_calibration_fit(cal_data, response ~ nominal_conc)

# Verify with QC samples
qc_data <- data.frame(
  sample_id = c("QC_Low", "QC_Mid", "QC_High"),
  nominal_conc = c(3, 75, 400),
  response = c(3.3, 77.2, 385.1)
)

verify_result <- measure_calibration_verify(cal, qc_data)
print(verify_result)
#> 
#> ── Calibration Verification ────────────────────────────────────────────────────
#> ✖ Overall: FAIL (1/3 samples out of specification)
#> 
#> ── Sample Results ──
#> 
#> # A tibble: 3 × 8
#>   sample_id nominal_conc response predicted_conc accuracy_pct deviation_pct
#>   <chr>            <dbl>    <dbl>          <dbl>        <dbl>         <dbl>
#> 1 QC_Low               3      3.3           1.38         45.9       -54.1  
#> 2 QC_Mid              75     77.2          75.6         101.          0.857
#> 3 QC_High            400    385.          385.           96.3        -3.73 
#> # ℹ 2 more variables: acceptance_limit <dbl>, pass <lgl>
```
