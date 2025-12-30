# Calibration Curve Object

A calibration curve object stores the fitted model, diagnostics, and
metadata for quantitation workflows. Created by
[`measure_calibration_fit()`](https://jameshwade.github.io/measure/dev/reference/measure_calibration_fit.md).

## Structure

A `measure_calibration` object is a list containing:

- model:

  The underlying fitted model (lm object)

- model_type:

  Character: "linear" or "quadratic"

- weights_type:

  Character: weighting scheme used

- formula:

  The model formula

- data:

  The calibration data used for fitting

- diagnostics:

  List of diagnostic statistics

- outliers:

  Data frame of flagged outliers (if any)

- call:

  The original function call

## See also

[`measure_calibration_fit()`](https://jameshwade.github.io/measure/dev/reference/measure_calibration_fit.md)
for creating calibration objects,
[`measure_calibration_predict()`](https://jameshwade.github.io/measure/dev/reference/measure_calibration_predict.md)
for prediction,
[`tidy.measure_calibration()`](https://jameshwade.github.io/measure/dev/reference/tidy.measure_calibration.md)
for extracting coefficients,
[`autoplot.measure_calibration()`](https://jameshwade.github.io/measure/dev/reference/autoplot.measure_calibration.md)
for diagnostic plots.
