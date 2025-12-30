# Analytical Method Validation

``` r
library(measure)
library(dplyr)
library(ggplot2)
```

## Overview

The `measure` package provides a comprehensive suite of functions for
analytical method validation. These functions are designed to be
compatible with regulatory frameworks including: - **ICH Q2(R2)**:
Validation of Analytical Procedures - **ISO/IEC 17025**: General
requirements for testing and calibration laboratories - **USP
\<1225\>**: Validation of Compendial Procedures - **ICH M10**:
Bioanalytical Method Validation (for applicable workflows)

This vignette demonstrates key validation workflows including
calibration, precision, accuracy, uncertainty, and quality control.

## Calibration Curves

### Fitting Calibration Curves

The
[`measure_calibration_fit()`](https://jameshwade.github.io/measure/dev/reference/measure_calibration_fit.md)
function fits weighted or unweighted calibration curves with
comprehensive diagnostics.

``` r
# Create calibration data
set.seed(42)
cal_data <- data.frame(
  nominal_conc = c(1, 5, 10, 25, 50, 100, 250, 500),
  response = c(1, 5, 10, 25, 50, 100, 250, 500) * 1.05 +
             rnorm(8, sd = c(0.1, 0.3, 0.5, 1, 2, 4, 10, 20))
)

# Fit with 1/x^2 weighting (common for bioanalytical methods)
cal <- measure_calibration_fit(
  cal_data,
  response ~ nominal_conc,
  weights = "1/x2"
)

print(cal)
#> <measure_calibration>
#>   Model: linear
#>   Weighting: 1/x2
#>   Formula: response ~ nominal_conc
#>   N points: 8
#>   R²: 0.9989
```

### Visualizing the Calibration

``` r
autoplot(cal, type = "curve")
```

![](validation_files/figure-html/calibration-plot-1.png)

### Checking Residuals

``` r
autoplot(cal, type = "residuals")
```

![](validation_files/figure-html/calibration-residuals-1.png)

### Predicting Unknown Concentrations

``` r
unknowns <- data.frame(
  sample_id = c("Sample_1", "Sample_2", "Sample_3"),
  response = c(52.3, 125.8, 280.5)
)

predictions <- measure_calibration_predict(
  cal,
  newdata = unknowns,
  interval = "confidence"
)

cbind(unknowns, predictions)
#>   sample_id response .pred_conc .pred_lower .pred_upper
#> 1  Sample_1     52.3    49.3896    49.30708    49.47213
#> 2  Sample_2    125.8   118.9571   118.87457   119.03962
#> 3  Sample_3    280.5   265.3801   265.29758   265.46263
```

### Calibration Verification

Verify that the calibration remains valid using QC samples:

``` r
qc_data <- data.frame(
  sample_id = c("QC_Low", "QC_Mid", "QC_High"),
  nominal_conc = c(3, 75, 400),
  response = c(3.1, 77.5, 395.2)
)

verification <- measure_calibration_verify(cal, qc_data)
print(verification)
#> 
#> ── Calibration Verification ────────────────────────────────────────────────────
#> ✔ Overall: PASS (3/3 samples within 15%)
#> 
#> ── Sample Results ──
#> 
#> # A tibble: 3 × 8
#>   sample_id nominal_conc response predicted_conc accuracy_pct deviation_pct
#>   <chr>            <dbl>    <dbl>          <dbl>        <dbl>         <dbl>
#> 1 QC_Low               3      3.1           2.82         94.1         -5.93
#> 2 QC_Mid              75     77.5          73.2          97.7         -2.34
#> 3 QC_High            400    395.          374.           93.5         -6.51
#> # ℹ 2 more variables: acceptance_limit <dbl>, pass <lgl>
```

## Limits of Detection and Quantitation

### Multiple Methods

`measure` supports multiple approaches for calculating LOD/LOQ:

``` r
# Blank-based approach (3σ/10σ)
blank_data <- data.frame(
  sample_type = rep("blank", 10),
  response = rnorm(10, mean = 0.5, sd = 0.08)
)

lod_result <- measure_lod(
  blank_data,
  "response",
  method = "blank_sd",
  calibration = cal
)
print(lod_result)
#> <measure_lod>
#>   Value: 0.8726
#>   Method: blank_sd
#>   k: 3
#>   Uncertainty: 0.1142
#>   Parameters:
#>     blank_mean: 0.5115
#>     blank_sd: 0.1203
#>     n_blanks: 10

# Or calculate both together
lod_loq <- measure_lod_loq(
  blank_data,
  "response",
  method = "blank_sd",
  calibration = cal
)
tidy(lod_loq)
#> # A tibble: 2 × 5
#>   limit_type value method       k uncertainty
#>   <chr>      <dbl> <chr>    <dbl>       <dbl>
#> 1 LOD        0.873 blank_sd     3       0.114
#> 2 LOQ        1.72  blank_sd    10       0.381
```

## Precision Studies

### Repeatability (Within-Run Precision)

``` r
# Data from replicate measurements
repeat_data <- data.frame(
  sample_id = rep(c("Low", "Mid", "High"), each = 6),
  concentration = c(
    rnorm(6, 10, 0.5),
    rnorm(6, 100, 4),
    rnorm(6, 500, 18)
  )
)

repeatability <- measure_repeatability(
  repeat_data,
  "concentration",
  group_col = "sample_id"
)
print(repeatability)
#> measure_precision: repeatability 
#> ──────────────────────────────────────────────────────────────────────────────── 
#> 
#> Group: Low 
#>   n = 6 
#>   Mean = 9.82 
#>   SD = 0.7645 
#>   CV = 7.8 %
#>   95% CI: [9.017, 10.62]
#> 
#> Group: Mid 
#>   n = 6 
#>   Mean = 99.51 
#>   SD = 4.893 
#>   CV = 4.9 %
#>   95% CI: [94.37, 104.6]
#> 
#> Group: High 
#>   n = 6 
#>   Mean = 501.1 
#>   SD = 18.58 
#>   CV = 3.7 %
#>   95% CI: [481.6, 520.6]
```

### Intermediate Precision

``` r
# Data from multiple days
ip_data <- data.frame(
  day = rep(1:3, each = 6),
  analyst = rep(c("A", "A", "A", "B", "B", "B"), 3),
  concentration = 100 +
    rep(c(-2, 0, 2), each = 6) +  # Day effect
    rep(c(-1, 1), 9) +            # Analyst effect
    rnorm(18, sd = 3)             # Residual
)

ip_result <- measure_intermediate_precision(
  ip_data,
  "concentration",
  factors = c("day", "analyst")
)
print(ip_result)
#> measure_precision: intermediate 
#> ──────────────────────────────────────────────────────────────────────────────── 
#> 
#> Variance Components:
#>   day: 15.78 (57%)
#>   analyst: 3.82 (14%)
#>   Residual: 8.135 (29%)
#> 
#> CV by component:
#>   day: 4%
#>   analyst: 2%
#>   Residual: 2.9%
```

### Gage R&R Analysis

For measurement system analysis:

``` r
# Gage R&R data
grr_data <- data.frame(
  part = rep(1:5, each = 6),
  operator = rep(rep(c("Op1", "Op2"), each = 3), 5),
  measurement = c(
    # Part 1
    10.1, 10.2, 10.0, 10.3, 10.1, 10.2,
    # Part 2
    20.2, 20.1, 20.3, 20.0, 20.2, 20.1,
    # Part 3
    15.1, 15.0, 15.2, 15.3, 15.1, 15.0,
    # Part 4
    25.0, 25.1, 24.9, 25.2, 25.0, 25.1,
    # Part 5
    30.1, 30.2, 30.0, 30.1, 30.0, 30.2
  )
)

grr_result <- measure_gage_rr(
  grr_data,
  "measurement",
  part_col = "part",
  operator_col = "operator"
)
print(grr_result)
#> measure_gage_rr: Measurement System Analysis
#> ──────────────────────────────────────────────────────────────────────────────── 
#> 
#> Study design:
#>   Parts: 5 
#>   Operators: 2 
#>   Replicates: 3 
#> 
#> Variance Components:
#>   Repeatability: 0.01133 (0.02% contribution)
#>   Reproducibility: 0 (0% contribution)
#>   Total R&R: 0.01133 (0.02% contribution)
#>   Part-to-Part: 62.08 (100% contribution)
#> 
#> % Study Variation:
#>   Repeatability: 1%
#>   Reproducibility: 0%
#>   Total R&R: 1%
#>   Part-to-Part: 100%
#> 
#> Number of Distinct Categories (ndc): 104 
#> 
#> Assessment:
#>   Measurement system is ACCEPTABLE (%R&R < 10%)
```

## Accuracy Assessment

### Bias and Recovery

``` r
accuracy_data <- data.frame(
  level = rep(c("Low", "Mid", "High"), each = 5),
  measured = c(
    rnorm(5, 10.2, 0.3),   # Low level, slight positive bias
    rnorm(5, 100, 2.5),    # Mid level, no bias
    rnorm(5, 498, 8)       # High level, slight negative bias
  ),
  reference = rep(c(10, 100, 500), each = 5)
)

accuracy <- measure_accuracy(
  accuracy_data,
  "measured",
  "reference",
  group_col = "level"
)
print(accuracy)
#> measure_accuracy
#> ──────────────────────────────────────────────────────────────────────────────── 
#> 
#> Group: Low 
#>   n = 5 
#>   Mean measured = 10.09 
#>   Mean reference = 10 
#>   Bias = 0.08854 ( 0.89 %)
#>   Recovery = 101 %
#>   Recovery 95% CI: [95%, 106%]
#> 
#> Group: Mid 
#>   n = 5 
#>   Mean measured = 101 
#>   Mean reference = 100 
#>   Bias = 1.042 ( 1 %)
#>   Recovery = 101 %
#>   Recovery 95% CI: [99%, 103%]
#> 
#> Group: High 
#>   n = 5 
#>   Mean measured = 502.6 
#>   Mean reference = 500 
#>   Bias = 2.593 ( 0.52 %)
#>   Recovery = 101 %
#>   Recovery 95% CI: [99%, 102%]
```

### Linearity Assessment

``` r
linearity_data <- data.frame(
  concentration = rep(c(10, 25, 50, 75, 100), each = 3),
  response = rep(c(10, 25, 50, 75, 100), each = 3) * 1.02 +
             rnorm(15, sd = 1.5)
)

linearity <- measure_linearity(
  linearity_data,
  "concentration",
  "response"
)
print(linearity)
#> measure_linearity
#> ──────────────────────────────────────────────────────────────────────────────── 
#> 
#> Data:
#>   n = 15 ( 5 levels )
#>   Range: 10 - 100 
#> 
#> Regression:
#>   Slope = 1.023 
#>     95% CI: [1.003, 1.044]
#>   Intercept = -0.1535 
#>     95% CI: [-1.436, 1.129]
#> 
#> Fit Quality:
#>   R-squared = 0.99884 
#>   Adj. R-squared = 0.99875 
#>   Residual SD = 1.222 
#>   Residual CV = 2.3 %
#> 
#> Lack-of-Fit Test:
#>   F = 0.649 
#>   p-value = 0.6015 
#>   Result: Not significant (linearity acceptable)

# Plot with fit line
autoplot(linearity, type = "fit")
```

![](validation_files/figure-html/linearity-1.png)

## Uncertainty Budgets

### ISO GUM Uncertainty

Create uncertainty budgets following the GUM (Guide to the Expression of
Uncertainty in Measurement):

``` r
# Define uncertainty components
components <- list(
  uncertainty_component(
    name = "Repeatability",
    type = "A",
    value = 0.5,
    df = 9
  ),
  uncertainty_component(
    name = "Calibration",
    type = "B",
    value = 0.3,
    distribution = "normal"
  ),
  uncertainty_component(
    name = "Reference Standard",
    type = "B",
    value = 0.1,
    distribution = "rectangular"
  ),
  uncertainty_component(
    name = "Temperature",
    type = "B",
    value = 0.2,
    sensitivity = 0.5  # Sensitivity coefficient
  )
)

budget <- measure_uncertainty_budget(.list = components)
print(budget)
#> <measure_uncertainty_budget>
#>   Components: 4 (1 Type A, 3 Type B)
#>   Combined u: 0.6
#>   Effective df: 19
#>   Coverage k: 2
#>   Expanded U: 1.2
```

### Visualizing Uncertainty Contributions

``` r
autoplot(budget)
```

![](validation_files/figure-html/uncertainty-plot-1.png)

## Control Charts

### Setting Up Control Limits

``` r
# Historical QC data
qc_history <- data.frame(
  run_order = 1:30,
  qc_value = rnorm(30, mean = 100, sd = 2)
)

limits <- measure_control_limits(qc_history, "qc_value")
print(limits)
#> measure_control_limits: shewhart chart
#> ──────────────────────────────────────────────────────────────────────────────── 
#> 
#>   n = 30 
#>   Center = 100.1 
#>   Sigma = 1.737 
#>   UCL (+3s) = 105.3 
#>   UWL (+2s) = 103.6 
#>   LWL (-2s) = 96.61 
#>   LCL (-3s) = 94.88
```

### Monitoring with Westgard Rules

``` r
# New run data including potential out-of-control point
new_run <- data.frame(
  run_order = 1:20,
  qc_value = c(rnorm(19, 100, 2), 108)  # Last point is high
)

chart <- measure_control_chart(
  new_run,
  "qc_value",
  "run_order",
  limits = limits,
  rules = c("1_3s", "2_2s", "R_4s", "10x")
)
print(chart)
#> measure_control_chart
#> ──────────────────────────────────────────────────────────────────────────────── 
#> 
#> Observations: 20 
#> Rules applied: 1_3s, 2_2s, R_4s, 10x 
#> Violations detected: 4 
#> 
#> Status: OUT OF CONTROL
#> 
#> Violation summary:
#> # A tibble: 4 × 3
#>   run_order qc_value violation
#>       <int>    <dbl> <chr>    
#> 1         4    105.  1:3s R:4s
#> 2         5     97.3 R:4s     
#> 3        19    100.  R:4s     
#> 4        20    108   1:3s R:4s
```

``` r
autoplot(chart)
```

![](validation_files/figure-html/control-chart-plot-1.png)

## Acceptance Criteria

### Defining Criteria

``` r
# Create custom criteria
my_criteria <- measure_criteria(
  criterion("cv", "<=", 15, description = "Precision CV"),
  criterion("bias_pct", "between", c(-10, 10), description = "Bias"),
  criterion("recovery", "between", c(85, 115), description = "Recovery %")
)
print(my_criteria)
#> <measure_criteria> with 3 criteria
#>   • Precision CV
#>   • Bias
#>   • Recovery %
```

### Using Preset Criteria

``` r
# ICH Q2 presets
ich_criteria <- criteria_ich_q2()
print(ich_criteria)
#> <measure_criteria> with 4 criteria
#>   • Repeatability RSD <= 2%
#>   • Intermediate precision RSD <= 5%
#>   • Recovery 98-102%
#>   • R² >= 0.999

# Bioanalytical presets
bio_criteria <- criteria_bioanalytical()
print(bio_criteria)
#> <measure_criteria> with 5 criteria
#>   • QC CV <= 15%
#>   • Calibration CV <= 20%
#>   • R² >= 0.99
#>   • Recovery 80-120%
#>   • Bias within +/-15%
```

### Assessing Results

``` r
# Sample results to assess (single summary values per criterion)
# For example, from a method validation summary
results <- list(
  cv = 5.2,          # Overall precision CV
  bias_pct = 1.3,    # Overall bias
  recovery = 101.3   # Mean recovery
)

assessment <- measure_assess(results, my_criteria)
print(assessment)
#> <measure_assessment> [PASS]
#>   3 passed, 0 failed
#> 
#>   ✓ cv: 5.2 (<= 15)
#>   ✓ bias_pct: 1.3 (between [-10, 10])
#>   ✓ recovery: 101.3 (between [85, 115])

# Check if all criteria passed
all_pass(assessment)
#> [1] TRUE
```

## Drift Correction

### Detecting Drift

``` r
# Data with drift
drift_data <- data.frame(
  sample_type = rep("qc", 20),
  run_order = 1:20,
  feature1 = 100 + (1:20) * 0.8 + rnorm(20, sd = 2),  # Has drift

  feature2 = 100 + rnorm(20, sd = 2)                   # No drift
)

drift_result <- measure_detect_drift(
  drift_data,
  features = c("feature1", "feature2"),
  qc_type = "qc"
)
print(drift_result)
#> # A tibble: 2 × 5
#>   feature   slope  slope_pvalue percent_change significant
#>   <chr>     <dbl>         <dbl>          <dbl> <lgl>      
#> 1 feature1 0.730  0.00000000258          12.8  TRUE       
#> 2 feature2 0.0545 0.423                   1.04 FALSE
```

### Correcting Drift

``` r
library(recipes)

# Using QC-LOESS correction in a recipe
rec <- recipe(~ ., data = drift_data) |>
  step_measure_drift_qc_loess(
    feature1, feature2,
    qc_type = "qc"
  ) |>
  prep()

corrected <- bake(rec, new_data = NULL)
```

## Summary

The `measure` package provides a complete toolkit for analytical method
validation:

| Category           | Key Functions                                                                                                                                                                                                                                                                                                                                          |
|--------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **Calibration**    | [`measure_calibration_fit()`](https://jameshwade.github.io/measure/dev/reference/measure_calibration_fit.md), [`measure_calibration_predict()`](https://jameshwade.github.io/measure/dev/reference/measure_calibration_predict.md), [`measure_calibration_verify()`](https://jameshwade.github.io/measure/dev/reference/measure_calibration_verify.md) |
| **LOD/LOQ**        | [`measure_lod()`](https://jameshwade.github.io/measure/dev/reference/measure_lod.md), [`measure_loq()`](https://jameshwade.github.io/measure/dev/reference/measure_loq.md), [`measure_lod_loq()`](https://jameshwade.github.io/measure/dev/reference/measure_lod_loq.md)                                                                               |
| **Precision**      | [`measure_repeatability()`](https://jameshwade.github.io/measure/dev/reference/measure_repeatability.md), [`measure_intermediate_precision()`](https://jameshwade.github.io/measure/dev/reference/measure_intermediate_precision.md), [`measure_gage_rr()`](https://jameshwade.github.io/measure/dev/reference/measure_gage_rr.md)                     |
| **Accuracy**       | [`measure_accuracy()`](https://jameshwade.github.io/measure/dev/reference/measure_accuracy.md), [`measure_linearity()`](https://jameshwade.github.io/measure/dev/reference/measure_linearity.md), [`measure_carryover()`](https://jameshwade.github.io/measure/dev/reference/measure_carryover.md)                                                     |
| **Uncertainty**    | [`measure_uncertainty_budget()`](https://jameshwade.github.io/measure/dev/reference/measure_uncertainty_budget.md), [`measure_uncertainty()`](https://jameshwade.github.io/measure/dev/reference/measure_uncertainty.md)                                                                                                                               |
| **Control Charts** | [`measure_control_limits()`](https://jameshwade.github.io/measure/dev/reference/measure_control_limits.md), [`measure_control_chart()`](https://jameshwade.github.io/measure/dev/reference/measure_control_chart.md)                                                                                                                                   |
| **Criteria**       | [`measure_criteria()`](https://jameshwade.github.io/measure/dev/reference/measure_criteria.md), [`measure_assess()`](https://jameshwade.github.io/measure/dev/reference/measure_assess.md), [`criteria_ich_q2()`](https://jameshwade.github.io/measure/dev/reference/criteria_presets.md)                                                              |
| **Drift**          | [`measure_detect_drift()`](https://jameshwade.github.io/measure/dev/reference/measure_detect_drift.md), [`step_measure_drift_qc_loess()`](https://jameshwade.github.io/measure/dev/reference/step_measure_drift_qc_loess.md)                                                                                                                           |

All functions follow a consistent design philosophy: - **Tidy outputs**:
Results are tibbles with
[`tidy()`](https://generics.r-lib.org/reference/tidy.html),
[`glance()`](https://generics.r-lib.org/reference/glance.html), and
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
methods - **Transparent diagnostics**: No hidden decisions; all
parameters and flags are visible - **Regulatory compatibility**:
Designed with ICH, ISO, and FDA guidelines in mind - **Provenance
tracking**: Audit trails for outlier handling and data modifications

For more details on any function, see the package documentation with
`?function_name`.
