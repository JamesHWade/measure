# Passing-Bablok Regression for Method Comparison

Performs Passing-Bablok regression, a non-parametric method for
comparing two analytical methods. This is robust to outliers and does
not require normal distribution of residuals.

## Usage

``` r
measure_passing_bablok(
  data,
  method1_col,
  method2_col,
  conf_level = 0.95,
  alpha = 0.05
)
```

## Arguments

- data:

  A data frame containing paired measurements.

- method1_col:

  Name of column for method 1 (reference/comparator).

- method2_col:

  Name of column for method 2 (test method).

- conf_level:

  Confidence level for intervals. Default is 0.95.

- alpha:

  Significance level for CUSUM linearity test. Default is 0.05.

## Value

A `measure_passing_bablok` object containing:

- `coefficients`: Tibble with intercept and slope estimates and CIs

- `linearity`: CUSUM test results for linearity assumption

- `statistics`: Summary statistics

## Details

### Method

Passing-Bablok regression:

1.  Calculates slopes between all pairs of points

2.  Uses median slope as the estimate (robust to outliers)

3.  Calculates intercept from median slope

4.  Uses non-parametric confidence intervals

### CUSUM Linearity Test

Tests the assumption of linear relationship. If significant (p \<
alpha), the linear model may not be appropriate.

### Interpretation

For equivalent methods:

- 95% CI for slope includes 1

- 95% CI for intercept includes 0

### Requirements

This function requires the `mcr` package. Install with:
`install.packages("mcr")`

## See also

[`measure_bland_altman()`](https://jameshwade.github.io/measure/dev/reference/measure_bland_altman.md),
[`measure_deming_regression()`](https://jameshwade.github.io/measure/dev/reference/measure_deming_regression.md)

Other method-comparison:
[`measure_bland_altman()`](https://jameshwade.github.io/measure/dev/reference/measure_bland_altman.md),
[`measure_deming_regression()`](https://jameshwade.github.io/measure/dev/reference/measure_deming_regression.md),
[`measure_proficiency_score()`](https://jameshwade.github.io/measure/dev/reference/measure_proficiency_score.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Requires mcr package
data <- data.frame(
  reference = c(5.2, 10.5, 15.8, 25.3, 50.1, 75.4, 100.2),
  new_method = c(5.1, 10.8, 16.2, 25.9, 49.8, 76.1, 101.3)
)

result <- measure_passing_bablok(
  data,
  method1_col = "reference",
  method2_col = "new_method"
)

print(result)
} # }
```
