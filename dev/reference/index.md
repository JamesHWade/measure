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

## Sample-wise Normalization

Normalize each spectrum independently

- [`step_measure_normalize_auc()`](https://jameshwade.github.io/measure/dev/reference/step_measure_normalize_auc.md)
  : Normalize by Area Under Curve
- [`step_measure_normalize_max()`](https://jameshwade.github.io/measure/dev/reference/step_measure_normalize_max.md)
  : Normalize by Maximum Value
- [`step_measure_normalize_peak()`](https://jameshwade.github.io/measure/dev/reference/step_measure_normalize_peak.md)
  : Normalize to a Specific Peak Region
- [`step_measure_normalize_range()`](https://jameshwade.github.io/measure/dev/reference/step_measure_normalize_range.md)
  : Normalize to Range 0-1
- [`step_measure_normalize_sum()`](https://jameshwade.github.io/measure/dev/reference/step_measure_normalize_sum.md)
  : Normalize by Sum (Total Intensity)
- [`step_measure_normalize_vector()`](https://jameshwade.github.io/measure/dev/reference/step_measure_normalize_vector.md)
  : Normalize by L2 (Euclidean) Norm

## Variable-wise Scaling

Scale across samples at each measurement location

- [`step_measure_center()`](https://jameshwade.github.io/measure/dev/reference/step_measure_center.md)
  : Mean Centering
- [`step_measure_scale_auto()`](https://jameshwade.github.io/measure/dev/reference/step_measure_scale_auto.md)
  : Auto-Scaling (Z-Score Normalization)
- [`step_measure_scale_pareto()`](https://jameshwade.github.io/measure/dev/reference/step_measure_scale_pareto.md)
  : Pareto Scaling
- [`step_measure_scale_range()`](https://jameshwade.github.io/measure/dev/reference/step_measure_scale_range.md)
  : Range Scaling
- [`step_measure_scale_vast()`](https://jameshwade.github.io/measure/dev/reference/step_measure_scale_vast.md)
  : VAST Scaling (Variable Stability Scaling)

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
- [`baseline_lambda()`](https://jameshwade.github.io/measure/dev/reference/baseline_lambda.md)
  [`baseline_asymmetry()`](https://jameshwade.github.io/measure/dev/reference/baseline_lambda.md)
  [`baseline_degree()`](https://jameshwade.github.io/measure/dev/reference/baseline_lambda.md)
  [`baseline_half_window()`](https://jameshwade.github.io/measure/dev/reference/baseline_lambda.md)
  [`baseline_span()`](https://jameshwade.github.io/measure/dev/reference/baseline_lambda.md)
  : Parameters for baseline correction steps
- [`peak_location_min()`](https://jameshwade.github.io/measure/dev/reference/peak_location_min.md)
  [`peak_location_max()`](https://jameshwade.github.io/measure/dev/reference/peak_location_min.md)
  : Parameters for peak normalization

## Data

Included datasets for examples and testing

- [`meats_long`](https://jameshwade.github.io/measure/dev/reference/meats_long.md)
  : Fat, water and protein content of meat samples
- [`glucose_bioreactors`](https://jameshwade.github.io/measure/dev/reference/glucose_bioreactors.md)
  [`bioreactors_large`](https://jameshwade.github.io/measure/dev/reference/glucose_bioreactors.md)
  [`bioreactors_small`](https://jameshwade.github.io/measure/dev/reference/glucose_bioreactors.md)
  : Raman Spectra Bioreactor Data

## Baseline Correction

Baseline correction steps and utilities

- [`step_measure_baseline_als()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_als.md)
  : Asymmetric Least Squares (ALS) Baseline Correction
- [`step_measure_baseline_custom()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_custom.md)
  : Custom Baseline Correction with User-Provided Function
- [`step_measure_baseline_gpc()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_gpc.md)
  : GPC/SEC Baseline Correction
- [`step_measure_baseline_poly()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_poly.md)
  : Polynomial Baseline Correction
- [`step_measure_baseline_py()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_py.md)
  : Python-Based Baseline Correction via pybaselines
- [`step_measure_baseline_rf()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_rf.md)
  : Robust Fitting Baseline Correction
- [`step_measure_detrend()`](https://jameshwade.github.io/measure/dev/reference/step_measure_detrend.md)
  : Remove Trend from Measurements
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
