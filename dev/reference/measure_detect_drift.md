# Detect Drift in Analytical Data

Detects significant drift in feature responses across run order using
trend tests and/or slope analysis.

## Usage

``` r
measure_detect_drift(
  data,
  features,
  run_order_col = "run_order",
  sample_type_col = "sample_type",
  qc_type = NULL,
  method = c("slope", "mann_kendall", "both")
)
```

## Arguments

- data:

  A data frame containing the measurement data.

- features:

  Character vector of feature column names to analyze.

- run_order_col:

  Name of the run order column.

- sample_type_col:

  Name of the sample type column.

- qc_type:

  Value(s) identifying QC samples. If provided, analysis is restricted
  to QC samples.

- method:

  Detection method:

  - `"slope"` (default): Linear regression slope test

  - `"mann_kendall"`: Mann-Kendall trend test

  - `"both"`: Both methods

## Value

A tibble with drift statistics for each feature:

- `feature`: Feature name

- `slope`: Regression slope (change per run)

- `slope_pvalue`: P-value for slope != 0

- `percent_change`: Total percent change over run

- `significant`: Logical, TRUE if drift is statistically significant

## Examples

``` r
# Create data with drift
data <- data.frame(
  sample_type = rep("qc", 20),
  run_order = 1:20,
  feature1 = 100 + (1:20) * 0.5 + rnorm(20, sd = 2),
  feature2 = 50 + rnorm(20, sd = 1)  # No drift
)

measure_detect_drift(data, c("feature1", "feature2"))
#> # A tibble: 2 × 5
#>   feature    slope slope_pvalue percent_change significant
#>   <chr>      <dbl>        <dbl>          <dbl> <lgl>      
#> 1 feature1 0.351       0.000331         6.34   TRUE       
#> 2 feature2 0.00136     0.973            0.0519 FALSE      
```
