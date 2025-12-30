# Matrix Effect Analysis

Quantifies matrix effects (ion suppression/enhancement) by comparing
analyte response in matrix versus neat solution. This is essential for
validating LC-MS/MS and other analytical methods where matrix
interference is a concern.

## Usage

``` r
measure_matrix_effect(
  data,
  response_col,
  sample_type_col,
  matrix_level,
  neat_level,
  concentration_col = NULL,
  analyte_col = NULL,
  group_cols = NULL,
  conf_level = 0.95
)
```

## Arguments

- data:

  A data frame containing response data.

- response_col:

  Name of the column containing analyte responses.

- sample_type_col:

  Name of the column indicating sample type (matrix vs neat/standard).

- matrix_level:

  Value in `sample_type_col` indicating matrix samples.

- neat_level:

  Value in `sample_type_col` indicating neat/standard samples.

- concentration_col:

  Optional column for concentration levels. If provided, matrix effects
  are calculated per concentration.

- analyte_col:

  Optional column for analyte names. If provided, matrix effects are
  calculated per analyte.

- group_cols:

  Additional grouping columns (e.g., batch, matrix source).

- conf_level:

  Confidence level for intervals. Default is 0.95.

## Value

A `measure_matrix_effect` object containing:

- `results`: Tibble with matrix effect percentages per group

- `statistics`: Overall summary statistics

- `raw_data`: Data used for calculations

## Details

### Matrix Effect Calculation

Matrix effect (ME%) is calculated as:
`ME% = (response_in_matrix / response_in_neat) * 100`

Or equivalently:
`ME% = 100 + ((response_in_matrix - response_in_neat) / response_in_neat) * 100`

### Interpretation

- **ME = 100%**: No matrix effect

- **ME \> 100%**: Ion enhancement

- **ME \< 100%**: Ion suppression

### Acceptance Criteria (typical)

According to ICH M10 and FDA guidance:

- ME should be between 80-120% (±20%)

- CV of ME should be ≤15%

### Experimental Design

To assess matrix effects:

1.  Prepare blank matrix (e.g., plasma) from multiple sources

2.  Spike analyte post-extraction at known concentration

3.  Compare to analyte in neat solvent at same concentration

## See also

[`step_measure_standard_addition()`](https://jameshwade.github.io/measure/dev/reference/step_measure_standard_addition.md),
[`measure_accuracy()`](https://jameshwade.github.io/measure/dev/reference/measure_accuracy.md)

Other calibration:
[`step_measure_dilution_correct()`](https://jameshwade.github.io/measure/dev/reference/step_measure_dilution_correct.md),
[`step_measure_standard_addition()`](https://jameshwade.github.io/measure/dev/reference/step_measure_standard_addition.md),
[`step_measure_surrogate_recovery()`](https://jameshwade.github.io/measure/dev/reference/step_measure_surrogate_recovery.md)

## Examples

``` r
# Matrix effect study data
me_data <- data.frame(
  sample_type = rep(c("matrix", "neat"), each = 6),
  matrix_lot = rep(c("Lot1", "Lot2", "Lot3", "Lot1", "Lot2", "Lot3"), 2),
  concentration = rep(c("low", "high"), each = 3, times = 2),
  response = c(
    # Matrix samples (some suppression)
    9500, 9800, 9200, 48000, 49500, 47000,
    # Neat samples
    10000, 10000, 10000, 50000, 50000, 50000
  )
)

me <- measure_matrix_effect(
  me_data,
  response_col = "response",
  sample_type_col = "sample_type",
  matrix_level = "matrix",
  neat_level = "neat",
  concentration_col = "concentration"
)

print(me)
#> measure_matrix_effect
#> ──────────────────────────────────────────────────────────────────────────────── 
#> 
#> Overall Matrix Effect Summary:
#>   Groups evaluated: 2 
#>   Mean ME: 96 %
#>   SD ME: 0.9 %
#>   CV ME: 1 %
#>   Range: 95 - 96 %
#> 
#> Effect Classification:
#>   Ion suppression (ME < 100%): 2 
#>   Ion enhancement (ME > 100%): 0 
#>   Acceptable (80-120%): 2 / 2 
tidy(me)
#>   concentration n_matrix n_neat mean_matrix_response mean_neat_response
#> 1           low        3      3              9500.00              10000
#> 2          high        3      3             48166.67              50000
#>   matrix_effect_pct me_ci_lower me_ci_upper interpretation
#> 1          95.00000    87.54759    102.4524    suppression
#> 2          96.33333    90.08172    102.5849    suppression
```
