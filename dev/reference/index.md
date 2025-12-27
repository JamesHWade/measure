# Package index

## Input Steps

Convert raw data to measure’s internal format

- [`step_measure_input_wide()`](https://jameshwade.github.io/measure/dev/reference/step_measure_input_wide.md)
  : Ingest Measurements in Separate Columns
- [`step_measure_input_long()`](https://jameshwade.github.io/measure/dev/reference/step_measure_input_long.md)
  : Ingest Measurements from a Single Column

## Processing Steps

Apply spectral preprocessing transformations

- [`step_measure_savitzky_golay()`](https://jameshwade.github.io/measure/dev/reference/step_measure_savitzky_golay.md)
  : Savitzky-Golay Pre-Processing
- [`step_measure_snv()`](https://jameshwade.github.io/measure/dev/reference/step_measure_snv.md)
  : Standard Normal Variate (SNV) Transformation
- [`step_measure_msc()`](https://jameshwade.github.io/measure/dev/reference/step_measure_msc.md)
  : Multiplicative Scatter Correction (MSC)
- [`step_measure_map()`](https://jameshwade.github.io/measure/dev/reference/step_measure_map.md)
  : Apply a Custom Function to Measurements

## Output Steps

Convert internal format back to modeling-ready data

- [`step_measure_output_wide()`](https://jameshwade.github.io/measure/dev/reference/step_measure_output_wide.md)
  : Reorganize Measurements to Separate Columns
- [`step_measure_output_long()`](https://jameshwade.github.io/measure/dev/reference/step_measure_output_long.md)
  : Reorganize Measurements to Two Columns

## Tunable Parameters

Parameters for hyperparameter tuning with dials

- [`window_side()`](https://jameshwade.github.io/measure/dev/reference/window_side.md)
  [`differentiation_order()`](https://jameshwade.github.io/measure/dev/reference/window_side.md)
  : Parameter for measure steps

## Data

Included datasets for examples and testing

- [`meats_long`](https://jameshwade.github.io/measure/dev/reference/meats_long.md)
  : Fat, water and protein content of meat samples
- [`glucose_bioreactors`](https://jameshwade.github.io/measure/dev/reference/glucose_bioreactors.md)
  [`bioreactors_large`](https://jameshwade.github.io/measure/dev/reference/glucose_bioreactors.md)
  [`bioreactors_small`](https://jameshwade.github.io/measure/dev/reference/glucose_bioreactors.md)
  : Raman Spectra Bioreactor Data

## Baseline Correction

Baseline correction utilities

- [`step_baseline()`](https://jameshwade.github.io/measure/dev/reference/step_baseline.md)
  : Fit and subtract a baseline from a measurement signal
- [`subtract_rf_baseline()`](https://jameshwade.github.io/measure/dev/reference/subtract_rf_baseline.md)
  : Subtract baseline using robust fitting method

## Utilities

Helper functions and utilities

- [`is_measure_tbl()`](https://jameshwade.github.io/measure/dev/reference/is_measure_tbl.md)
  : Test if object is a measure tibble
- [`is_measure_list()`](https://jameshwade.github.io/measure/dev/reference/is_measure_list.md)
  : Test if object is a measure list
- [`find_measure_cols()`](https://jameshwade.github.io/measure/dev/reference/find_measure_cols.md)
  : Find measure columns in a data frame
- [`has_measure_col()`](https://jameshwade.github.io/measure/dev/reference/has_measure_col.md)
  : Check if data frame has measure column(s)
- [`required_pkgs(`*`<step_measure_savitzky_golay>`*`)`](https://jameshwade.github.io/measure/dev/reference/required_pkgs.recipe.md)
  : Set package dependencies

## Exploration & Analysis

Interactive exploration and summary functions for prototyping

- [`measure_map()`](https://jameshwade.github.io/measure/dev/reference/measure_map.md)
  : Apply a Function to Each Sample's Measurements
- [`measure_map_safely()`](https://jameshwade.github.io/measure/dev/reference/measure_map_safely.md)
  : Apply a Function Safely to Each Sample's Measurements
- [`measure_summarize()`](https://jameshwade.github.io/measure/dev/reference/measure_summarize.md)
  : Summarize Measurements Across Samples
