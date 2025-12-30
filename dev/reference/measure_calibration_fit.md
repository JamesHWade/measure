# Fit a Calibration Curve

Fits a weighted or unweighted calibration curve for quantitation.
Supports linear and quadratic models with various weighting schemes.

## Usage

``` r
measure_calibration_fit(
  data,
  formula,
  model = c("linear", "quadratic"),
  weights = c("none", "1/x", "1/x2", "1/y", "1/y2"),
  origin = FALSE,
  outlier_method = c("none", "studentized", "cook"),
  outlier_threshold = NULL,
  outlier_action = c("flag", "remove"),
  sample_type_col = NULL
)
```

## Arguments

- data:

  A data frame containing calibration data.

- formula:

  A formula specifying the model. The left-hand side should be the
  response variable, and the right-hand side should be the concentration
  variable (e.g., `response ~ nominal_conc`).

- model:

  Model type: `"linear"` (default) or `"quadratic"`.

- weights:

  Weighting scheme:

  - `"none"` (default): Unweighted regression

  - `"1/x"`: Weight by 1/concentration

  - `"1/x2"`: Weight by 1/concentration^2

  - `"1/y"`: Weight by 1/response

  - `"1/y2"`: Weight by 1/response^2

  - A numeric vector of custom weights (must match data rows)

- origin:

  Logical. If TRUE, force the curve through the origin (zero intercept).
  Default is FALSE.

- outlier_method:

  Method for flagging outliers:

  - `"none"` (default): No outlier detection

  - `"studentized"`: Flag points with \|studentized residual\| \>
    `outlier_threshold`

  - `"cook"`: Flag points with Cook's distance \> `outlier_threshold`

- outlier_threshold:

  Threshold for outlier detection. Default is 2.5 for studentized
  residuals or 1 for Cook's distance.

- outlier_action:

  What to do with outliers:

  - `"flag"` (default): Flag but include in fit

  - `"remove"`: Remove from fit (with audit trail)

- sample_type_col:

  Optional column name for sample type. If provided, only rows with
  `sample_type == "standard"` are used for fitting.

## Value

A
[measure_calibration](https://jameshwade.github.io/measure/dev/reference/measure_calibration.md)
object containing the fitted model, diagnostics, and metadata.

## Details

### Weighting

Weighting is essential when response variance changes with concentration
(heteroscedasticity). Common patterns:

- Constant CV: Use `"1/x2"` or `"1/y2"`

- Constant absolute error: Use `"none"`

- Proportional error: Use `"1/x"` or `"1/y"`

### Outlier Handling

By default, outliers are flagged but NOT removed. This follows the
principle of "flag, don't drop" for analytical data. If removal is
enabled, the removed points are stored in the result for audit purposes.

## See also

[`measure_calibration_predict()`](https://jameshwade.github.io/measure/dev/reference/measure_calibration_predict.md)
for prediction,
[`autoplot.measure_calibration()`](https://jameshwade.github.io/measure/dev/reference/autoplot.measure_calibration.md)
for diagnostic plots,
[`tidy.measure_calibration()`](https://jameshwade.github.io/measure/dev/reference/tidy.measure_calibration.md)
for extracting coefficients.

## Examples

``` r
# Simple linear calibration
data <- data.frame(
  nominal_conc = c(0, 10, 25, 50, 100, 200),
  response = c(0.5, 15.2, 35.8, 72.1, 148.3, 295.7)
)
cal <- measure_calibration_fit(data, response ~ nominal_conc)
print(cal)
#> <measure_calibration>
#>   Model: linear
#>   Weighting: none
#>   Formula: response ~ nominal_conc
#>   N points: 6
#>   R²: 0.99992

# Weighted calibration (1/x^2)
cal_weighted <- measure_calibration_fit(
  data,
  response ~ nominal_conc,
  weights = "1/x2"
)

# Quadratic model
cal_quad <- measure_calibration_fit(
  data,
  response ~ nominal_conc,
  model = "quadratic"
)
```
