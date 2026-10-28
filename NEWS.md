# measure 0.0.1.9003

* `emg_peak_model()` now evaluates the correctly signed Gaussian-exponential
  convolution and remains numerically stable as `tau` approaches zero (#119).
* `new_peaks_tbl()` and `new_peaks_list()` are now public constructors for
  technique packs. Peak tables can include additional technique-specific columns
  (#110).
* `optimize_deconvolution()` now returns Nelder-Mead parameters, predictions,
  and objective values from the same constrained fit (#119).
* `step_measure_peaks_detect()` and `step_measure_peaks_deconvolve()` now expose
  detection thresholds, fit quality, and iteration limits to `tune` through
  dials parameters (#116). Unresolved tuning calls are retained in list columns
  when inspecting untrained steps with `tidy()`.
* `step_measure_savitzky_golay()` now preserves the centered input locations
  after trimming edge points and rejects measurements with inconsistent axes
  (#113).
