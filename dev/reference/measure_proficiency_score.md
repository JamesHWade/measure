# Proficiency Testing Scores

Calculates proficiency testing scores (z-scores, En scores, or zeta
scores) for evaluating laboratory performance in interlaboratory
comparisons.

## Usage

``` r
measure_proficiency_score(
  data,
  measured_col,
  reference_col,
  uncertainty_col = NULL,
  reference_uncertainty_col = NULL,
  score_type = c("z_score", "en_score", "zeta_score"),
  sigma = NULL,
  group_col = NULL
)
```

## Arguments

- data:

  A data frame containing measurement data.

- measured_col:

  Name of column with measured/reported values.

- reference_col:

  Name of column with reference/assigned values.

- uncertainty_col:

  Name of column with measurement uncertainties. Required for En and
  zeta scores.

- reference_uncertainty_col:

  Name of column with reference value uncertainties. Optional for
  En/zeta scores.

- score_type:

  Type of score to calculate:

  - `"z_score"` (default): (measured - reference) / sigma

  - `"en_score"`: (measured - reference) / sqrt(U_meas^2 + U_ref^2)

  - `"zeta_score"`: Similar to En, for correlated uncertainties

- sigma:

  Standard deviation for z-score calculation. If NULL, estimated from
  the data.

- group_col:

  Optional grouping column for separate assessments.

## Value

A `measure_proficiency_score` object containing:

- `scores`: Tibble with individual scores and flags

- `statistics`: Summary statistics and counts

## Details

### Score Interpretation

\| \|Score\| \| Status \| Action \| \|———\|—————\|——–\| \| \<= 2 \|
Satisfactory \| None \| \| 2-3 \| Questionable \| Review \| \| \> 3 \|
Unsatisfactory\| Investigate \|

### Score Types

**z-score**: Uses a fixed standard deviation (sigma), typically derived
from historical data or consensus of participants.

**En score**: Uses expanded uncertainties of both the lab result and
reference value. Appropriate when uncertainties are well-characterized.

**zeta score**: Similar to En, but accounts for potential correlation
between lab and reference uncertainties.

## See also

[`measure_accuracy()`](https://jameshwade.github.io/measure/dev/reference/measure_accuracy.md),
[`criteria_proficiency_testing()`](https://jameshwade.github.io/measure/dev/reference/criteria_presets.md)

Other method-comparison:
[`measure_bland_altman()`](https://jameshwade.github.io/measure/dev/reference/measure_bland_altman.md),
[`measure_deming_regression()`](https://jameshwade.github.io/measure/dev/reference/measure_deming_regression.md),
[`measure_passing_bablok()`](https://jameshwade.github.io/measure/dev/reference/measure_passing_bablok.md)

## Examples

``` r
# Proficiency testing results from multiple labs
pt_data <- data.frame(
  lab_id = paste0("Lab_", 1:10),
  measured = c(99.2, 100.5, 98.8, 101.2, 97.5, 100.1, 99.8, 102.3, 100.6, 94.0),
  assigned = rep(100, 10),
  uncertainty = c(1.5, 2.0, 1.8, 1.6, 2.2, 1.9, 1.7, 2.1, 1.5, 2.0)
)

# z-scores with known sigma
z_result <- measure_proficiency_score(
  pt_data,
  measured_col = "measured",
  reference_col = "assigned",
  score_type = "z_score",
  sigma = 2.5
)

print(z_result)
#> measure_proficiency_score
#> ──────────────────────────────────────────────────────────────────────────────── 
#> 
#> Score Type: z_score 
#> Sigma: 2.5 
#> 
#> Results (n = 10 ):
#>   Satisfactory (|z| ≤ 2): 9 ( 90 %)
#>   Questionable (2 < |z| ≤ 3): 1 
#>   Unsatisfactory (|z| > 3): 0 
#> 
#> Score Statistics:
#>   Mean score: -0.24 
#>   SD score: 0.925 
#>   Max |score|: 2.4 

# En scores using uncertainties
en_result <- measure_proficiency_score(
  pt_data,
  measured_col = "measured",
  reference_col = "assigned",
  uncertainty_col = "uncertainty",
  score_type = "en_score"
)

print(en_result)
#> measure_proficiency_score
#> ──────────────────────────────────────────────────────────────────────────────── 
#> 
#> Score Type: en_score 
#> 
#> Results (n = 10 ):
#>   Satisfactory (|z| ≤ 2): 9 ( 90 %)
#>   Questionable (2 < |z| ≤ 3): 1 
#>   Unsatisfactory (|z| > 3): 0 
#> 
#> Score Statistics:
#>   Mean score: -0.291 
#>   SD score: 1.16 
#>   Max |score|: 3 
```
