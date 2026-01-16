# Tidiers for measure steps

Tidiers for measure steps

## Usage

``` r
# S3 method for class 'measure_accuracy'
tidy(x, ...)

# S3 method for class 'measure_linearity'
tidy(x, ...)

# S3 method for class 'measure_carryover'
tidy(x, ...)

# S3 method for class 'step_measure_align_shift'
tidy(x, ...)

# S3 method for class 'step_measure_align_reference'
tidy(x, ...)

# S3 method for class 'step_measure_align_dtw'
tidy(x, ...)

# S3 method for class 'step_measure_align_ptw'
tidy(x, ...)

# S3 method for class 'step_measure_align_cow'
tidy(x, ...)

# S3 method for class 'step_measure_augment_noise'
tidy(x, ...)

# S3 method for class 'step_measure_augment_shift'
tidy(x, ...)

# S3 method for class 'step_measure_augment_scale'
tidy(x, ...)

# S3 method for class 'step_measure_baseline_als'
tidy(x, ...)

# S3 method for class 'step_measure_baseline_custom'
tidy(x, ...)

# S3 method for class 'step_measure_baseline_rolling'
tidy(x, ...)

# S3 method for class 'step_measure_baseline_airpls'
tidy(x, ...)

# S3 method for class 'step_measure_baseline_snip'
tidy(x, ...)

# S3 method for class 'step_measure_baseline_arpls'
tidy(x, ...)

# S3 method for class 'step_measure_baseline_tophat'
tidy(x, ...)

# S3 method for class 'step_measure_baseline_morph'
tidy(x, ...)

# S3 method for class 'step_measure_baseline_minima'
tidy(x, ...)

# S3 method for class 'step_measure_baseline_auto'
tidy(x, ...)

# S3 method for class 'step_measure_baseline_gpc'
tidy(x, ...)

# S3 method for class 'step_measure_baseline_morphological'
tidy(x, ...)

# S3 method for class 'step_measure_baseline_aspls'
tidy(x, ...)

# S3 method for class 'step_measure_baseline_iarpls'
tidy(x, ...)

# S3 method for class 'step_measure_baseline_fastchrom'
tidy(x, ...)

# S3 method for class 'step_measure_baseline_poly'
tidy(x, ...)

# S3 method for class 'step_measure_baseline_py'
tidy(x, ...)

# S3 method for class 'step_measure_baseline_rf'
tidy(x, ...)

# S3 method for class 'step_measure_batch_reference'
tidy(x, ...)

# S3 method for class 'step_measure_calibrate_x'
tidy(x, ...)

# S3 method for class 'step_measure_calibrate_y'
tidy(x, ...)

# S3 method for class 'measure_control_limits'
tidy(x, ...)

# S3 method for class 'measure_control_chart'
tidy(x, type = c("data", "violations", "limits"), ...)

# S3 method for class 'measure_sst'
tidy(x, ...)

# S3 method for class 'step_measure_despike'
tidy(x, ...)

# S3 method for class 'step_measure_detrend'
tidy(x, ...)

# S3 method for class 'step_measure_dilution_correct'
tidy(x, ...)

# S3 method for class 'step_measure_drift_qc_loess'
tidy(x, ...)

# S3 method for class 'step_measure_drift_linear'
tidy(x, ...)

# S3 method for class 'step_measure_drift_spline'
tidy(x, ...)

# S3 method for class 'step_measure_qc_bracket'
tidy(x, ...)

# S3 method for class 'step_measure_integrals'
tidy(x, ...)

# S3 method for class 'step_measure_ratios'
tidy(x, ...)

# S3 method for class 'step_measure_moments'
tidy(x, ...)

# S3 method for class 'step_measure_bin'
tidy(x, ...)

# S3 method for class 'step_measure_input_long'
tidy(x, ...)

# S3 method for class 'step_measure_input_wide'
tidy(x, ...)

# S3 method for class 'measure_matrix_effect'
tidy(x, ...)

# S3 method for class 'measure_matrix_effect'
glance(x, ...)

# S3 method for class 'step_measure_standard_addition'
tidy(x, ...)

# S3 method for class 'step_measure_map'
tidy(x, ...)

# S3 method for class 'measure_bland_altman'
tidy(x, ...)

# S3 method for class 'measure_deming_regression'
tidy(x, ...)

# S3 method for class 'measure_passing_bablok'
tidy(x, ...)

# S3 method for class 'measure_proficiency_score'
tidy(x, type = c("scores", "summary"), ...)

# S3 method for class 'measure_bland_altman'
glance(x, ...)

# S3 method for class 'measure_deming_regression'
glance(x, ...)

# S3 method for class 'measure_passing_bablok'
glance(x, ...)

# S3 method for class 'measure_proficiency_score'
glance(x, ...)

# S3 method for class 'step_measure_msc'
tidy(x, ...)

# S3 method for class 'step_measure_mw_averages'
tidy(x, ...)

# S3 method for class 'step_measure_mw_fractions'
tidy(x, ...)

# S3 method for class 'step_measure_mw_distribution'
tidy(x, ...)

# S3 method for class 'step_measure_normalize_sum'
tidy(x, ...)

# S3 method for class 'step_measure_normalize_max'
tidy(x, ...)

# S3 method for class 'step_measure_normalize_range'
tidy(x, ...)

# S3 method for class 'step_measure_normalize_vector'
tidy(x, ...)

# S3 method for class 'step_measure_normalize_auc'
tidy(x, ...)

# S3 method for class 'step_measure_normalize_peak'
tidy(x, ...)

# S3 method for class 'step_measure_output_long'
tidy(x, ...)

# S3 method for class 'step_measure_output_wide'
tidy(x, ...)

# S3 method for class 'step_measure_peaks_detect'
tidy(x, ...)

# S3 method for class 'step_measure_peaks_integrate'
tidy(x, ...)

# S3 method for class 'step_measure_peaks_filter'
tidy(x, ...)

# S3 method for class 'step_measure_peaks_to_table'
tidy(x, ...)

# S3 method for class 'step_measure_peaks_deconvolve'
tidy(x, ...)

# S3 method for class 'measure_precision'
tidy(x, ...)

# S3 method for class 'measure_gage_rr'
tidy(x, ...)

# S3 method for class 'step_measure_qc_snr'
tidy(x, ...)

# S3 method for class 'step_measure_qc_saturated'
tidy(x, ...)

# S3 method for class 'step_measure_impute'
tidy(x, ...)

# S3 method for class 'step_measure_qc_outlier'
tidy(x, ...)

# S3 method for class 'step_measure_subtract_blank'
tidy(x, ...)

# S3 method for class 'step_measure_subtract_reference'
tidy(x, ...)

# S3 method for class 'step_measure_ratio_reference'
tidy(x, ...)

# S3 method for class 'step_measure_trim'
tidy(x, ...)

# S3 method for class 'step_measure_exclude'
tidy(x, ...)

# S3 method for class 'step_measure_resample'
tidy(x, ...)

# S3 method for class 'step_measure_interpolate'
tidy(x, ...)

# S3 method for class 'step_measure_savitzky_golay'
tidy(x, ...)

# S3 method for class 'step_measure_center'
tidy(x, ...)

# S3 method for class 'step_measure_scale_auto'
tidy(x, ...)

# S3 method for class 'step_measure_scale_pareto'
tidy(x, ...)

# S3 method for class 'step_measure_scale_range'
tidy(x, ...)

# S3 method for class 'step_measure_scale_vast'
tidy(x, ...)

# S3 method for class 'step_measure_emsc'
tidy(x, ...)

# S3 method for class 'step_measure_osc'
tidy(x, ...)

# S3 method for class 'step_measure_smooth_wavelet'
tidy(x, ...)

# S3 method for class 'step_measure_smooth_ma'
tidy(x, ...)

# S3 method for class 'step_measure_smooth_median'
tidy(x, ...)

# S3 method for class 'step_measure_smooth_gaussian'
tidy(x, ...)

# S3 method for class 'step_measure_filter_fourier'
tidy(x, ...)

# S3 method for class 'step_measure_snv'
tidy(x, ...)

# S3 method for class 'step_measure_absorbance'
tidy(x, ...)

# S3 method for class 'step_measure_transmittance'
tidy(x, ...)

# S3 method for class 'step_measure_log'
tidy(x, ...)

# S3 method for class 'step_measure_kubelka_munk'
tidy(x, ...)

# S3 method for class 'step_measure_derivative'
tidy(x, ...)

# S3 method for class 'step_measure_derivative_gap'
tidy(x, ...)

# S3 method for class 'step_measure_channel_align'
tidy(x, ...)

# S3 method for class 'step_measure_channel_combine'
tidy(x, ...)

# S3 method for class 'step_measure_channel_ratio'
tidy(x, ...)

# S3 method for class 'step_measure_mcr_als'
tidy(x, type = "parameters", ...)

# S3 method for class 'step_measure_parafac'
tidy(x, type = "parameters", ...)

# S3 method for class 'step_measure_tucker'
tidy(x, type = "parameters", ...)

# S3 method for class 'step_measure_surrogate_recovery'
tidy(x, ...)
```

## Arguments

- x:

  A step object.

- ...:

  Not used.

- type:

  For PARAFAC steps, either `"loadings"` or `"parameters"`.
