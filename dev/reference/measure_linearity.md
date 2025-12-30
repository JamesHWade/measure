# Linearity Assessment

Assesses linearity of a method by evaluating the relationship between
response and concentration across the specified range.

## Usage

``` r
measure_linearity(
  data,
  conc_col,
  response_col,
  method = c("regression", "residual"),
  conf_level = 0.95
)
```

## Arguments

- data:

  A data frame containing concentration and response data.

- conc_col:

  Name of the column containing concentrations.

- response_col:

  Name of the column containing responses.

- method:

  Linearity assessment method:

  - `"regression"` (default): Linear regression with diagnostics

  - `"residual"`: Residual analysis and lack-of-fit test

- conf_level:

  Confidence level for intervals. Default is 0.95.

## Value

A `measure_linearity` object containing:

- `r_squared`: Coefficient of determination

- `adj_r_squared`: Adjusted R-squared

- `slope`: Regression slope with CI

- `intercept`: Regression intercept with CI

- `residual_sd`: Residual standard deviation

- `lack_of_fit`: Lack-of-fit test results (if replicates exist)

- `range`: Concentration range evaluated

## Details

Linearity demonstrates that the method produces results that are
directly proportional to analyte concentration within a given range.

### Assessment Criteria

- R-squared \>= 0.99 (typical for many applications)

- Residuals randomly distributed around zero

- No systematic pattern in residual plots

- Lack-of-fit test not significant (p \> 0.05)

### ICH Q2 Requirements

Linearity should be evaluated across the range with at least 5
concentration levels. Report the regression equation, correlation
coefficient, and visual inspection of residual plots.

## See also

[`measure_accuracy()`](https://jameshwade.github.io/measure/dev/reference/measure_accuracy.md),
[`measure_calibration_fit()`](https://jameshwade.github.io/measure/dev/reference/measure_calibration_fit.md)

Other accuracy:
[`measure_accuracy()`](https://jameshwade.github.io/measure/dev/reference/measure_accuracy.md),
[`measure_carryover()`](https://jameshwade.github.io/measure/dev/reference/measure_carryover.md)

## Examples

``` r
# Linearity assessment
set.seed(123)
data <- data.frame(
  concentration = rep(c(10, 25, 50, 75, 100), each = 3),
  response = rep(c(10, 25, 50, 75, 100), each = 3) * 1.5 + rnorm(15, 0, 2)
)

result <- measure_linearity(data, "concentration", "response")
print(result)
#> measure_linearity
#> ──────────────────────────────────────────────────────────────────────────────── 
#> 
#> Data:
#>   n = 15 ( 5 levels )
#>   Range: 10 - 100 
#> 
#> Regression:
#>   Slope = 1.493 
#>     95% CI: [1.463, 1.523]
#>   Intercept = 0.675 
#>     95% CI: [-1.147, 2.497]
#> 
#> Fit Quality:
#>   R-squared = 0.9989 
#>   Adj. R-squared = 0.99882 
#>   Residual SD = 1.737 
#>   Residual CV = 2.2 %
#> 
#> Lack-of-Fit Test:
#>   F = 0.877 
#>   p-value = 0.4853 
#>   Result: Not significant (linearity acceptable)
```
