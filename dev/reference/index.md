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

## Smoothing & Noise Reduction

Noise reduction and spike removal steps

- [`step_measure_smooth_ma()`](https://jameshwade.github.io/measure/dev/reference/step_measure_smooth_ma.md)
  : Moving Average Smoothing
- [`step_measure_smooth_median()`](https://jameshwade.github.io/measure/dev/reference/step_measure_smooth_median.md)
  : Median Filter Smoothing
- [`step_measure_smooth_gaussian()`](https://jameshwade.github.io/measure/dev/reference/step_measure_smooth_gaussian.md)
  : Gaussian Kernel Smoothing
- [`step_measure_smooth_wavelet()`](https://jameshwade.github.io/measure/dev/reference/step_measure_smooth_wavelet.md)
  : Wavelet Denoising
- [`step_measure_filter_fourier()`](https://jameshwade.github.io/measure/dev/reference/step_measure_filter_fourier.md)
  : Fourier Low-Pass Filtering
- [`step_measure_despike()`](https://jameshwade.github.io/measure/dev/reference/step_measure_despike.md)
  : Remove Spikes and Outliers from Measurements

## Alignment & Registration

Spectral alignment and shift correction

- [`step_measure_align_shift()`](https://jameshwade.github.io/measure/dev/reference/step_measure_align_shift.md)
  : Shift Alignment via Cross-Correlation
- [`step_measure_align_reference()`](https://jameshwade.github.io/measure/dev/reference/step_measure_align_reference.md)
  : Align to Reference Spectrum
- [`step_measure_align_dtw()`](https://jameshwade.github.io/measure/dev/reference/step_measure_align_dtw.md)
  : Dynamic Time Warping Alignment
- [`step_measure_align_ptw()`](https://jameshwade.github.io/measure/dev/reference/step_measure_align_ptw.md)
  : Parametric Time Warping Alignment
- [`step_measure_align_cow()`](https://jameshwade.github.io/measure/dev/reference/step_measure_align_cow.md)
  : Correlation Optimized Warping Alignment

## Quality Control

Quality metrics and data cleaning

- [`step_measure_qc_snr()`](https://jameshwade.github.io/measure/dev/reference/step_measure_qc_snr.md)
  : Calculate Signal-to-Noise Ratio
- [`step_measure_qc_saturated()`](https://jameshwade.github.io/measure/dev/reference/step_measure_qc_saturated.md)
  : Detect Saturated Measurements
- [`step_measure_qc_outlier()`](https://jameshwade.github.io/measure/dev/reference/step_measure_qc_outlier.md)
  : Detect Outlier Samples
- [`step_measure_impute()`](https://jameshwade.github.io/measure/dev/reference/step_measure_impute.md)
  : Impute Missing Values in Measurements

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
- [`smooth_window()`](https://jameshwade.github.io/measure/dev/reference/smooth_window.md)
  [`smooth_sigma()`](https://jameshwade.github.io/measure/dev/reference/smooth_window.md)
  [`fourier_cutoff()`](https://jameshwade.github.io/measure/dev/reference/smooth_window.md)
  [`despike_threshold()`](https://jameshwade.github.io/measure/dev/reference/smooth_window.md)
  : Parameters for smoothing steps
- [`align_max_shift()`](https://jameshwade.github.io/measure/dev/reference/align_max_shift.md)
  [`align_segment_length()`](https://jameshwade.github.io/measure/dev/reference/align_max_shift.md)
  : Parameters for alignment steps
- [`outlier_threshold()`](https://jameshwade.github.io/measure/dev/reference/outlier_threshold.md)
  : Parameters for quality control steps
- [`bin_width()`](https://jameshwade.github.io/measure/dev/reference/bin_width.md)
  [`emsc_degree()`](https://jameshwade.github.io/measure/dev/reference/bin_width.md)
  [`osc_n_components()`](https://jameshwade.github.io/measure/dev/reference/bin_width.md)
  : Parameters for feature engineering and scatter correction

## Data

Included datasets for examples and testing

- [`meats_long`](https://jameshwade.github.io/measure/dev/reference/meats_long.md)
  : Fat, water and protein content of meat samples
- [`glucose_bioreactors`](https://jameshwade.github.io/measure/dev/reference/glucose_bioreactors.md)
  [`bioreactors_large`](https://jameshwade.github.io/measure/dev/reference/glucose_bioreactors.md)
  [`bioreactors_small`](https://jameshwade.github.io/measure/dev/reference/glucose_bioreactors.md)
  : Raman Spectra Bioreactor Data
- [`hplc_chromatograms`](https://jameshwade.github.io/measure/dev/reference/hplc_chromatograms.md)
  : Simulated HPLC Chromatography Data
- [`sec_chromatograms`](https://jameshwade.github.io/measure/dev/reference/sec_chromatograms.md)
  : Simulated SEC/GPC Chromatography Data
- [`sec_calibration`](https://jameshwade.github.io/measure/dev/reference/sec_calibration.md)
  : SEC/GPC Calibration Standards Summary
- [`maldi_spectra`](https://jameshwade.github.io/measure/dev/reference/maldi_spectra.md)
  : Simulated MALDI-TOF Mass Spectrometry Data

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

## Feature Engineering

Extract features from spectral data

- [`step_measure_integrals()`](https://jameshwade.github.io/measure/dev/reference/step_measure_integrals.md)
  : Calculate Region Integrals
- [`step_measure_ratios()`](https://jameshwade.github.io/measure/dev/reference/step_measure_ratios.md)
  : Calculate Region Ratios
- [`step_measure_moments()`](https://jameshwade.github.io/measure/dev/reference/step_measure_moments.md)
  : Calculate Statistical Moments
- [`step_measure_bin()`](https://jameshwade.github.io/measure/dev/reference/step_measure_bin.md)
  : Spectral Binning

## Extended Scatter Correction

Advanced scatter correction methods

- [`step_measure_emsc()`](https://jameshwade.github.io/measure/dev/reference/step_measure_emsc.md)
  : Extended Multiplicative Scatter Correction (EMSC)
- [`step_measure_osc()`](https://jameshwade.github.io/measure/dev/reference/step_measure_osc.md)
  : Orthogonal Signal Correction (OSC)

## Data Augmentation

Training data augmentation for improved model robustness

- [`step_measure_augment_noise()`](https://jameshwade.github.io/measure/dev/reference/step_measure_augment_noise.md)
  : Add Random Noise to Measurements
- [`step_measure_augment_shift()`](https://jameshwade.github.io/measure/dev/reference/step_measure_augment_shift.md)
  : Add Random X-axis Shifts
- [`step_measure_augment_scale()`](https://jameshwade.github.io/measure/dev/reference/step_measure_augment_scale.md)
  : Random Intensity Scaling

## Drift & Batch Correction

Correct for signal drift and batch effects

- [`step_measure_drift_qc_loess()`](https://jameshwade.github.io/measure/dev/reference/step_measure_drift_qc_loess.md)
  : QC-Based Drift Correction Using LOESS
- [`step_measure_drift_linear()`](https://jameshwade.github.io/measure/dev/reference/step_measure_drift_linear.md)
  : Linear Drift Correction
- [`step_measure_drift_spline()`](https://jameshwade.github.io/measure/dev/reference/step_measure_drift_spline.md)
  : Spline-Based Drift Correction
- [`step_measure_qc_bracket()`](https://jameshwade.github.io/measure/dev/reference/step_measure_qc_bracket.md)
  : QC Bracketing Interpolation
- [`step_measure_batch_reference()`](https://jameshwade.github.io/measure/dev/reference/step_measure_batch_reference.md)
  : Reference-Based Batch Correction
- [`measure_detect_drift()`](https://jameshwade.github.io/measure/dev/reference/measure_detect_drift.md)
  : Detect Drift in Analytical Data

## Calibration & Quantitation

Calibration curves, LOD/LOQ, and quantitation

- [`measure_calibration`](https://jameshwade.github.io/measure/dev/reference/measure_calibration.md)
  [`measure_calibration-class`](https://jameshwade.github.io/measure/dev/reference/measure_calibration.md)
  : Calibration Curve Object
- [`measure_calibration_fit()`](https://jameshwade.github.io/measure/dev/reference/measure_calibration_fit.md)
  : Fit a Calibration Curve
- [`measure_calibration_predict()`](https://jameshwade.github.io/measure/dev/reference/measure_calibration_predict.md)
  : Predict Concentrations from Calibration Curve
- [`measure_calibration_verify()`](https://jameshwade.github.io/measure/dev/reference/measure_calibration_verify.md)
  : Verify Calibration Curve Performance
- [`is_measure_calibration()`](https://jameshwade.github.io/measure/dev/reference/is_measure_calibration.md)
  : Test if Object is a Calibration Curve
- [`tidy(`*`<measure_calibration>`*`)`](https://jameshwade.github.io/measure/dev/reference/tidy.measure_calibration.md)
  [`tidy(`*`<measure_calibration_verify>`*`)`](https://jameshwade.github.io/measure/dev/reference/tidy.measure_calibration.md)
  : Tidy a Calibration Curve
- [`glance(`*`<measure_calibration>`*`)`](https://jameshwade.github.io/measure/dev/reference/glance.measure_calibration.md)
  : Glance at Calibration Curve Summary
- [`augment(`*`<measure_calibration>`*`)`](https://jameshwade.github.io/measure/dev/reference/augment.measure_calibration.md)
  : Augment Calibration Data
- [`measure_lod()`](https://jameshwade.github.io/measure/dev/reference/measure_lod.md)
  : Calculate Limit of Detection (LOD)
- [`measure_loq()`](https://jameshwade.github.io/measure/dev/reference/measure_loq.md)
  : Calculate Limit of Quantitation (LOQ)
- [`measure_lod_loq()`](https://jameshwade.github.io/measure/dev/reference/measure_lod_loq.md)
  : Calculate LOD and LOQ Together
- [`tidy(`*`<measure_lod>`*`)`](https://jameshwade.github.io/measure/dev/reference/tidy.measure_lod.md)
  : Tidy LOD/LOQ Results

## Precision & Accuracy

Method validation for precision and accuracy

- [`measure_repeatability()`](https://jameshwade.github.io/measure/dev/reference/measure_repeatability.md)
  : Repeatability (Within-Run Precision)
- [`measure_intermediate_precision()`](https://jameshwade.github.io/measure/dev/reference/measure_intermediate_precision.md)
  : Intermediate Precision (Between-Run Precision)
- [`measure_reproducibility()`](https://jameshwade.github.io/measure/dev/reference/measure_reproducibility.md)
  : Reproducibility (Between-Lab Precision)
- [`measure_gage_rr()`](https://jameshwade.github.io/measure/dev/reference/measure_gage_rr.md)
  : Gage R&R (Measurement System Analysis)
- [`measure_accuracy()`](https://jameshwade.github.io/measure/dev/reference/measure_accuracy.md)
  : Accuracy Assessment
- [`measure_linearity()`](https://jameshwade.github.io/measure/dev/reference/measure_linearity.md)
  : Linearity Assessment
- [`measure_carryover()`](https://jameshwade.github.io/measure/dev/reference/measure_carryover.md)
  : Carryover Assessment

## Uncertainty

ISO GUM uncertainty budgets

- [`measure_uncertainty_budget()`](https://jameshwade.github.io/measure/dev/reference/measure_uncertainty_budget.md)
  : Create an Uncertainty Budget
- [`measure_uncertainty()`](https://jameshwade.github.io/measure/dev/reference/measure_uncertainty.md)
  : Quick Uncertainty Calculation
- [`uncertainty_component()`](https://jameshwade.github.io/measure/dev/reference/uncertainty_component.md)
  : Create an Uncertainty Component
- [`uncertainty_type_a()`](https://jameshwade.github.io/measure/dev/reference/uncertainty_type_a.md)
  : Create Type A Uncertainty from Repeated Measurements
- [`uncertainty_type_b_rectangular()`](https://jameshwade.github.io/measure/dev/reference/uncertainty_type_b_rectangular.md)
  : Create Type B Uncertainty from Rectangular Distribution
- [`uncertainty_type_b_expanded()`](https://jameshwade.github.io/measure/dev/reference/uncertainty_type_b_expanded.md)
  : Create Type B Uncertainty from Expanded Uncertainty
- [`tidy(`*`<measure_uncertainty_budget>`*`)`](https://jameshwade.github.io/measure/dev/reference/tidy.measure_uncertainty_budget.md)
  : Tidy an Uncertainty Budget

## Control Charts & QC

Statistical process control and system suitability

- [`measure_control_limits()`](https://jameshwade.github.io/measure/dev/reference/measure_control_limits.md)
  : Calculate Control Limits
- [`measure_control_chart()`](https://jameshwade.github.io/measure/dev/reference/measure_control_chart.md)
  : Generate Control Chart
- [`measure_system_suitability()`](https://jameshwade.github.io/measure/dev/reference/measure_system_suitability.md)
  : System Suitability Check

## Method Comparison

Compare analytical methods using Bland-Altman, Deming, and
Passing-Bablok

- [`measure_bland_altman()`](https://jameshwade.github.io/measure/dev/reference/measure_bland_altman.md)
  : Bland-Altman Method Comparison
- [`measure_deming_regression()`](https://jameshwade.github.io/measure/dev/reference/measure_deming_regression.md)
  : Deming Regression for Method Comparison
- [`measure_passing_bablok()`](https://jameshwade.github.io/measure/dev/reference/measure_passing_bablok.md)
  : Passing-Bablok Regression for Method Comparison
- [`measure_proficiency_score()`](https://jameshwade.github.io/measure/dev/reference/measure_proficiency_score.md)
  : Proficiency Testing Scores

## Matrix Effects & Sample Prep

Matrix effect quantification, standard addition, and sample prep QC

- [`measure_matrix_effect()`](https://jameshwade.github.io/measure/dev/reference/measure_matrix_effect.md)
  : Matrix Effect Analysis
- [`step_measure_standard_addition()`](https://jameshwade.github.io/measure/dev/reference/step_measure_standard_addition.md)
  : Standard Addition Correction
- [`step_measure_dilution_correct()`](https://jameshwade.github.io/measure/dev/reference/step_measure_dilution_correct.md)
  : Dilution Factor Correction
- [`step_measure_surrogate_recovery()`](https://jameshwade.github.io/measure/dev/reference/step_measure_surrogate_recovery.md)
  : Surrogate/Internal Standard Recovery

## Criteria & Assessment

Acceptance criteria and pass/fail evaluation

- [`criterion()`](https://jameshwade.github.io/measure/dev/reference/criterion.md)
  : Create an Acceptance Criterion
- [`measure_criteria()`](https://jameshwade.github.io/measure/dev/reference/measure_criteria.md)
  : Create a Set of Acceptance Criteria
- [`measure_assess()`](https://jameshwade.github.io/measure/dev/reference/measure_assess.md)
  : Assess Data Against Acceptance Criteria
- [`all_pass()`](https://jameshwade.github.io/measure/dev/reference/all_pass.md)
  : Check if All Criteria Pass
- [`get_failures()`](https://jameshwade.github.io/measure/dev/reference/get_failures.md)
  : Extract Failed Criteria
- [`criteria_bioanalytical()`](https://jameshwade.github.io/measure/dev/reference/criteria_presets.md)
  [`criteria_ich_q2()`](https://jameshwade.github.io/measure/dev/reference/criteria_presets.md)
  [`criteria_bland_altman()`](https://jameshwade.github.io/measure/dev/reference/criteria_presets.md)
  [`criteria_method_comparison()`](https://jameshwade.github.io/measure/dev/reference/criteria_presets.md)
  [`criteria_proficiency_testing()`](https://jameshwade.github.io/measure/dev/reference/criteria_presets.md)
  [`criteria_matrix_effects()`](https://jameshwade.github.io/measure/dev/reference/criteria_presets.md)
  [`criteria_surrogate_recovery()`](https://jameshwade.github.io/measure/dev/reference/criteria_presets.md)
  : Preset Acceptance Criteria

## Metadata Validation

Validate analytical metadata columns

- [`measure_validate_metadata()`](https://jameshwade.github.io/measure/dev/reference/measure_validate_metadata.md)
  : Validate Analytical Metadata
- [`measure_standardize_sample_type()`](https://jameshwade.github.io/measure/dev/reference/measure_standardize_sample_type.md)
  : Standardize Sample Type Values
- [`measure_sample_types`](https://jameshwade.github.io/measure/dev/reference/measure_sample_types.md)
  : Canonical Sample Types

## Utilities

Helper functions and utilities

- [`new_measure_tbl()`](https://jameshwade.github.io/measure/dev/reference/new_measure_tbl.md)
  : Create a new measure tibble
- [`new_measure_list()`](https://jameshwade.github.io/measure/dev/reference/new_measure_list.md)
  : Create a new measure list
- [`is_measure_tbl()`](https://jameshwade.github.io/measure/dev/reference/is_measure_tbl.md)
  : Test if object is a measure tibble
- [`is_measure_list()`](https://jameshwade.github.io/measure/dev/reference/is_measure_list.md)
  : Test if object is a measure list
- [`find_measure_cols()`](https://jameshwade.github.io/measure/dev/reference/find_measure_cols.md)
  : Find measure columns in a data frame
- [`has_measure_col()`](https://jameshwade.github.io/measure/dev/reference/has_measure_col.md)
  : Check if data frame has measure column(s)
- [`required_pkgs(`*`<step_measure_align_dtw>`*`)`](https://jameshwade.github.io/measure/dev/reference/required_pkgs.recipe.md)
  [`required_pkgs(`*`<step_measure_align_ptw>`*`)`](https://jameshwade.github.io/measure/dev/reference/required_pkgs.recipe.md)
  [`required_pkgs(`*`<step_measure_augment_noise>`*`)`](https://jameshwade.github.io/measure/dev/reference/required_pkgs.recipe.md)
  [`required_pkgs(`*`<step_measure_augment_shift>`*`)`](https://jameshwade.github.io/measure/dev/reference/required_pkgs.recipe.md)
  [`required_pkgs(`*`<step_measure_augment_scale>`*`)`](https://jameshwade.github.io/measure/dev/reference/required_pkgs.recipe.md)
  [`required_pkgs(`*`<step_measure_integrals>`*`)`](https://jameshwade.github.io/measure/dev/reference/required_pkgs.recipe.md)
  [`required_pkgs(`*`<step_measure_ratios>`*`)`](https://jameshwade.github.io/measure/dev/reference/required_pkgs.recipe.md)
  [`required_pkgs(`*`<step_measure_moments>`*`)`](https://jameshwade.github.io/measure/dev/reference/required_pkgs.recipe.md)
  [`required_pkgs(`*`<step_measure_bin>`*`)`](https://jameshwade.github.io/measure/dev/reference/required_pkgs.recipe.md)
  [`required_pkgs(`*`<step_measure_mw_averages>`*`)`](https://jameshwade.github.io/measure/dev/reference/required_pkgs.recipe.md)
  [`required_pkgs(`*`<step_measure_mw_fractions>`*`)`](https://jameshwade.github.io/measure/dev/reference/required_pkgs.recipe.md)
  [`required_pkgs(`*`<step_measure_mw_distribution>`*`)`](https://jameshwade.github.io/measure/dev/reference/required_pkgs.recipe.md)
  [`required_pkgs(`*`<step_measure_resample>`*`)`](https://jameshwade.github.io/measure/dev/reference/required_pkgs.recipe.md)
  [`required_pkgs(`*`<step_measure_savitzky_golay>`*`)`](https://jameshwade.github.io/measure/dev/reference/required_pkgs.recipe.md)
  [`required_pkgs(`*`<step_measure_emsc>`*`)`](https://jameshwade.github.io/measure/dev/reference/required_pkgs.recipe.md)
  [`required_pkgs(`*`<step_measure_osc>`*`)`](https://jameshwade.github.io/measure/dev/reference/required_pkgs.recipe.md)
  [`required_pkgs(`*`<step_measure_smooth_wavelet>`*`)`](https://jameshwade.github.io/measure/dev/reference/required_pkgs.recipe.md)
  : Set package dependencies

## Exploration & Analysis

Interactive exploration and summary functions for prototyping

- [`measure_map()`](https://jameshwade.github.io/measure/dev/reference/measure_map.md)
  : Apply a Function to Each Sample's Measurements
- [`measure_map_safely()`](https://jameshwade.github.io/measure/dev/reference/measure_map_safely.md)
  : Apply a Function Safely to Each Sample's Measurements
- [`measure_summarize()`](https://jameshwade.github.io/measure/dev/reference/measure_summarize.md)
  : Summarize Measurements Across Samples

## Data Organization

Column detection and role assignment helpers

- [`measure_identify_columns()`](https://jameshwade.github.io/measure/dev/reference/measure_identify_columns.md)
  : Identify Column Types in Analytical Data
- [`measure_column_summary()`](https://jameshwade.github.io/measure/dev/reference/measure_column_summary.md)
  : Get Column Summary by Type
- [`measure_column_patterns`](https://jameshwade.github.io/measure/dev/reference/measure_column_patterns.md)
  : Common column naming patterns for analytical data
- [`set_measure_roles()`](https://jameshwade.github.io/measure/dev/reference/set_measure_roles.md)
  : Set Measure Roles in a Recipe
- [`check_measure_recipe()`](https://jameshwade.github.io/measure/dev/reference/check_measure_recipe.md)
  : Check Measure Recipe Structure

## Axis Validation

Axis semantics and data quality validation

- [`validate_measure()`](https://jameshwade.github.io/measure/dev/reference/validate_measure.md)
  : Validate measure data
- [`measure_axis_info()`](https://jameshwade.github.io/measure/dev/reference/measure_axis_info.md)
  : Get axis information from measure data
- [`infer_axis_type()`](https://jameshwade.github.io/measure/dev/reference/infer_axis_type.md)
  : Infer axis type from location values
- [`check_axis_consistency()`](https://jameshwade.github.io/measure/dev/reference/check_axis_consistency.md)
  : Check axis consistency across samples
- [`measure_quality_summary()`](https://jameshwade.github.io/measure/dev/reference/measure_quality_summary.md)
  : Summarize measure data quality

## Validation Reporting

Generate reproducible analytical method validation reports

- [`measure_validation_report()`](https://jameshwade.github.io/measure/dev/reference/measure_validation_report.md)
  : Create an Analytical Method Validation Report
- [`render_validation_report()`](https://jameshwade.github.io/measure/dev/reference/render_validation_report.md)
  : Render a Validation Report to Document Format
- [`has_validation_section()`](https://jameshwade.github.io/measure/dev/reference/has_validation_section.md)
  : Check if validation report has a section
- [`get_validation_section()`](https://jameshwade.github.io/measure/dev/reference/get_validation_section.md)
  : Get validation section data
- [`add_validation_section()`](https://jameshwade.github.io/measure/dev/reference/add_validation_section.md)
  : Add or update a validation section
- [`print(`*`<measure_validation_report>`*`)`](https://jameshwade.github.io/measure/dev/reference/print.measure_validation_report.md)
  : Print a Validation Report
- [`summary(`*`<measure_validation_report>`*`)`](https://jameshwade.github.io/measure/dev/reference/summary.measure_validation_report.md)
  : Summarize a Validation Report
- [`tidy(`*`<measure_validation_report>`*`)`](https://jameshwade.github.io/measure/dev/reference/tidy.measure_validation_report.md)
  : Tidy a Validation Report

## Visualization & Diagnostics

Plotting and diagnostic functions for measure data

- [`autoplot(`*`<measure_tbl>`*`)`](https://jameshwade.github.io/measure/dev/reference/autoplot-measure.md)
  [`autoplot(`*`<measure_list>`*`)`](https://jameshwade.github.io/measure/dev/reference/autoplot-measure.md)
  [`autoplot(`*`<recipe>`*`)`](https://jameshwade.github.io/measure/dev/reference/autoplot-measure.md)
  : Autoplot Methods for Measure Objects
- [`autoplot(`*`<measure_bland_altman>`*`)`](https://jameshwade.github.io/measure/dev/reference/autoplot.measure_bland_altman.md)
  : Plot Bland-Altman Analysis
- [`autoplot(`*`<measure_calibration>`*`)`](https://jameshwade.github.io/measure/dev/reference/autoplot.measure_calibration.md)
  : Plot Calibration Curve Diagnostics
- [`autoplot(`*`<measure_control_chart>`*`)`](https://jameshwade.github.io/measure/dev/reference/autoplot.measure_control_chart.md)
  : Plot Control Chart
- [`autoplot(`*`<measure_deming_regression>`*`)`](https://jameshwade.github.io/measure/dev/reference/autoplot.measure_deming_regression.md)
  [`autoplot(`*`<measure_passing_bablok>`*`)`](https://jameshwade.github.io/measure/dev/reference/autoplot.measure_deming_regression.md)
  : Plot Method Comparison Regression
- [`autoplot(`*`<measure_linearity>`*`)`](https://jameshwade.github.io/measure/dev/reference/autoplot.measure_linearity.md)
  : Plot Linearity Assessment Results
- [`autoplot(`*`<measure_matrix_effect>`*`)`](https://jameshwade.github.io/measure/dev/reference/autoplot.measure_matrix_effect.md)
  : Plot Matrix Effects
- [`autoplot(`*`<measure_proficiency_score>`*`)`](https://jameshwade.github.io/measure/dev/reference/autoplot.measure_proficiency_score.md)
  : Plot Proficiency Test Scores
- [`autoplot(`*`<measure_uncertainty_budget>`*`)`](https://jameshwade.github.io/measure/dev/reference/autoplot.measure_uncertainty_budget.md)
  : Plot Uncertainty Budget
- [`fortify(`*`<measure_tbl>`*`)`](https://jameshwade.github.io/measure/dev/reference/fortify-measure.md)
  [`fortify(`*`<measure_list>`*`)`](https://jameshwade.github.io/measure/dev/reference/fortify-measure.md)
  : Convert Measure Objects to Data Frames for Plotting
- [`fortify(`*`<measure_calibration>`*`)`](https://jameshwade.github.io/measure/dev/reference/fortify.measure_calibration.md)
  : Extract Calibration Curve Data
- [`plot_measure_comparison()`](https://jameshwade.github.io/measure/dev/reference/plot_measure_comparison.md)
  : Compare Multiple Preprocessing Recipes
- [`measure_plot_summary()`](https://jameshwade.github.io/measure/dev/reference/measure_plot_summary.md)
  : Plot Summary Statistics for Measure Data
