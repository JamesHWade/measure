# Predict Concentrations from Calibration Curve

Uses a fitted calibration curve to predict concentrations from
responses.

## Usage

``` r
measure_calibration_predict(
  object,
  newdata,
  interval = c("none", "confidence", "prediction"),
  level = 0.95,
  ...
)
```

## Arguments

- object:

  A
  [measure_calibration](https://jameshwade.github.io/measure/dev/reference/measure_calibration.md)
  object from
  [`measure_calibration_fit()`](https://jameshwade.github.io/measure/dev/reference/measure_calibration_fit.md).

- newdata:

  A data frame containing the response values to predict from. Must
  contain a column with the same name as the response variable in the
  calibration formula.

- interval:

  Type of interval to calculate:

  - `"none"` (default): Point estimates only

  - `"confidence"`: Confidence intervals

  - `"prediction"`: Prediction intervals

- level:

  Confidence level for intervals (default 0.95).

- ...:

  Additional arguments (unused).

## Value

A tibble with columns:

- `.pred_conc`: Predicted concentration

- `.pred_lower`: Lower bound (if intervals requested)

- `.pred_upper`: Upper bound (if intervals requested)

## Details

For inverse prediction (response -\> concentration), the function uses
root-finding when the model is quadratic. For linear models, direct
algebraic inversion is used.

### Interval Calculation

Intervals are calculated using the delta method for the inverse
prediction. For quadratic models, intervals are approximate.

## See also

[`measure_calibration_fit()`](https://jameshwade.github.io/measure/dev/reference/measure_calibration_fit.md)
for fitting calibration curves.

## Examples

``` r
# Fit calibration curve
cal_data <- data.frame(
  nominal_conc = c(0, 10, 25, 50, 100),
  response = c(0.5, 15.2, 35.8, 72.1, 148.3)
)
cal <- measure_calibration_fit(cal_data, response ~ nominal_conc)

# Predict concentrations from new responses
unknowns <- data.frame(response = c(45, 85, 120))
measure_calibration_predict(cal, unknowns)
#> # A tibble: 3 × 1
#>   .pred_conc
#>        <dbl>
#> 1       30.6
#> 2       57.7
#> 3       81.4

# With prediction intervals
measure_calibration_predict(cal, unknowns, interval = "prediction")
#> # A tibble: 3 × 3
#>   .pred_conc .pred_lower .pred_upper
#>        <dbl>       <dbl>       <dbl>
#> 1       30.6        27.7        33.6
#> 2       57.7        54.8        60.7
#> 3       81.4        78.5        84.4
```
