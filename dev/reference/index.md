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

## Spectral Math

Mathematical transformations for spectral data

- [`step_measure_absorbance()`](https://jameshwade.github.io/measure/dev/reference/step_measure_absorbance.md)
  : Convert Transmittance to Absorbance
- [`step_measure_transmittance()`](https://jameshwade.github.io/measure/dev/reference/step_measure_transmittance.md)
  : Convert Absorbance to Transmittance
- [`step_measure_log()`](https://jameshwade.github.io/measure/dev/reference/step_measure_log.md)
  : Log Transformation
- [`step_measure_kubelka_munk()`](https://jameshwade.github.io/measure/dev/reference/step_measure_kubelka_munk.md)
  : Kubelka-Munk Transformation
- [`step_measure_derivative()`](https://jameshwade.github.io/measure/dev/reference/step_measure_derivative.md)
  : Simple Finite Difference Derivatives
- [`step_measure_derivative_gap()`](https://jameshwade.github.io/measure/dev/reference/step_measure_derivative_gap.md)
  : Gap (Norris-Williams) Derivatives

## Region Operations

Trim, exclude, and resample measurement regions

- [`step_measure_trim()`](https://jameshwade.github.io/measure/dev/reference/step_measure_trim.md)
  : Trim Measurements to Specified Range
- [`step_measure_exclude()`](https://jameshwade.github.io/measure/dev/reference/step_measure_exclude.md)
  : Exclude Measurement Ranges
- [`step_measure_resample()`](https://jameshwade.github.io/measure/dev/reference/step_measure_resample.md)
  : Resample Measurements to New Grid
- [`step_measure_interpolate()`](https://jameshwade.github.io/measure/dev/reference/step_measure_interpolate.md)
  : Interpolate Gaps in Measurement Data

## Sample-wise Normalization

Normalize each spectrum independently

- [`step_measure_normalize_auc()`](https://jameshwade.github.io/measure/dev/reference/step_measure_normalize_auc.md)
  : Normalize by Area Under Curve
- [`step_measure_normalize_istd()`](https://jameshwade.github.io/measure/dev/reference/step_measure_normalize_istd.md)
  : Internal Standard Normalization
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

## Reference-Based Corrections

Blank subtraction and reference corrections

- [`step_measure_subtract_blank()`](https://jameshwade.github.io/measure/dev/reference/step_measure_subtract_blank.md)
  : Subtract Blank Measurement
- [`step_measure_subtract_reference()`](https://jameshwade.github.io/measure/dev/reference/step_measure_subtract_reference.md)
  : Subtract or Divide by Reference Spectrum
- [`step_measure_ratio_reference()`](https://jameshwade.github.io/measure/dev/reference/step_measure_ratio_reference.md)
  : Compute Ratio to Reference Spectrum

## Calibration

X-axis and Y-axis calibration steps

- [`step_measure_calibrate_x()`](https://jameshwade.github.io/measure/dev/reference/step_measure_calibrate_x.md)
  : Apply X-Axis Calibration
- [`step_measure_calibrate_y()`](https://jameshwade.github.io/measure/dev/reference/step_measure_calibrate_y.md)
  : Apply Y-Axis Calibration (Response Factor)

## Output Steps

Convert internal format back to modeling-ready data

- [`step_measure_output_wide()`](https://jameshwade.github.io/measure/dev/reference/step_measure_output_wide.md)
  : Reorganize Measurements to Separate Columns
- [`step_measure_output_long()`](https://jameshwade.github.io/measure/dev/reference/step_measure_output_long.md)
  : Reorganize Measurements to Two Columns

## Peak Operations

Peak detection, integration, and analysis for chromatography

- [`step_measure_peaks_detect()`](https://jameshwade.github.io/measure/dev/reference/step_measure_peaks_detect.md)
  : Detect Peaks in Measurements
- [`step_measure_peaks_integrate()`](https://jameshwade.github.io/measure/dev/reference/step_measure_peaks_integrate.md)
  : Integrate Peak Areas
- [`step_measure_peaks_filter()`](https://jameshwade.github.io/measure/dev/reference/step_measure_peaks_filter.md)
  : Filter Peaks by Criteria
- [`step_measure_peaks_deconvolve()`](https://jameshwade.github.io/measure/dev/reference/step_measure_peaks_deconvolve.md)
  : Deconvolve Overlapping Peaks
- [`step_measure_peaks_to_table()`](https://jameshwade.github.io/measure/dev/reference/step_measure_peaks_to_table.md)
  : Convert Peaks to Tidy Table
- [`is_peaks_list()`](https://jameshwade.github.io/measure/dev/reference/is_peaks_list.md)
  : Test if object is a peaks list
- [`find_peaks_cols()`](https://jameshwade.github.io/measure/dev/reference/find_peaks_cols.md)
  : Find peaks columns in a data frame

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
- [`derivative_order()`](https://jameshwade.github.io/measure/dev/reference/derivative_order.md)
  [`derivative_gap()`](https://jameshwade.github.io/measure/dev/reference/derivative_order.md)
  [`derivative_segment()`](https://jameshwade.github.io/measure/dev/reference/derivative_order.md)
  : Parameters for derivative steps

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

- [`step_measure_baseline_airpls()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_airpls.md)
  : Adaptive Iteratively Reweighted Penalized Least Squares Baseline
- [`step_measure_baseline_als()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_als.md)
  : Asymmetric Least Squares (ALS) Baseline Correction
- [`step_measure_baseline_arpls()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_arpls.md)
  : Asymmetrically Reweighted Penalized Least Squares Baseline
  Correction
- [`step_measure_baseline_auto()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_auto.md)
  : Automatic Baseline Correction Method Selection
- [`step_measure_baseline_custom()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_custom.md)
  : Custom Baseline Correction with User-Provided Function
- [`step_measure_baseline_gpc()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_gpc.md)
  : GPC/SEC Baseline Correction
- [`step_measure_baseline_minima()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_minima.md)
  : Local Minima Interpolation Baseline Correction
- [`step_measure_baseline_morph()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_morph.md)
  : Iterative Morphological Baseline Correction
- [`step_measure_baseline_poly()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_poly.md)
  : Polynomial Baseline Correction
- [`step_measure_baseline_py()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_py.md)
  : Python-Based Baseline Correction via pybaselines
- [`step_measure_baseline_rf()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_rf.md)
  : Robust Fitting Baseline Correction
- [`step_measure_baseline_rolling()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_rolling.md)
  : Rolling Ball Baseline Correction
- [`step_measure_baseline_snip()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_snip.md)
  : SNIP Baseline Correction
- [`step_measure_baseline_tophat()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_tophat.md)
  : Top-Hat Morphological Baseline Correction
- [`step_measure_detrend()`](https://jameshwade.github.io/measure/dev/reference/step_measure_detrend.md)
  : Remove Trend from Measurements
- [`subtract_rf_baseline()`](https://jameshwade.github.io/measure/dev/reference/subtract_rf_baseline.md)
  : Subtract baseline using robust fitting method

## SEC/GPC Analysis

Molecular weight calculations for size exclusion chromatography

- [`step_measure_mw_averages()`](https://jameshwade.github.io/measure/dev/reference/step_measure_mw_averages.md)
  : Calculate Molecular Weight Averages for SEC/GPC
- [`step_measure_mw_distribution()`](https://jameshwade.github.io/measure/dev/reference/step_measure_mw_distribution.md)
  : Generate Molecular Weight Distribution Curve
- [`step_measure_mw_fractions()`](https://jameshwade.github.io/measure/dev/reference/step_measure_mw_fractions.md)
  : Calculate Molecular Weight Fractions for SEC/GPC

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
- [`required_pkgs(`*`<step_measure_mw_averages>`*`)`](https://jameshwade.github.io/measure/dev/reference/required_pkgs.recipe.md)
  [`required_pkgs(`*`<step_measure_mw_fractions>`*`)`](https://jameshwade.github.io/measure/dev/reference/required_pkgs.recipe.md)
  [`required_pkgs(`*`<step_measure_mw_distribution>`*`)`](https://jameshwade.github.io/measure/dev/reference/required_pkgs.recipe.md)
  [`required_pkgs(`*`<step_measure_resample>`*`)`](https://jameshwade.github.io/measure/dev/reference/required_pkgs.recipe.md)
  [`required_pkgs(`*`<step_measure_savitzky_golay>`*`)`](https://jameshwade.github.io/measure/dev/reference/required_pkgs.recipe.md)
  : Set package dependencies

## Exploration & Analysis

Interactive exploration and summary functions for prototyping

- [`measure_map()`](https://jameshwade.github.io/measure/dev/reference/measure_map.md)
  : Apply a Function to Each Sample's Measurements
- [`measure_map_safely()`](https://jameshwade.github.io/measure/dev/reference/measure_map_safely.md)
  : Apply a Function Safely to Each Sample's Measurements
- [`measure_summarize()`](https://jameshwade.github.io/measure/dev/reference/measure_summarize.md)
  : Summarize Measurements Across Samples
