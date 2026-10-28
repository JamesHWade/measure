# measure 0.0.1.9003

* Fitted peak areas now integrate the fitted component over explicit finite
  bounds. Detected bases remain available separately, and stored row metadata
  preserves component-aware reintegration after filtering or serialization.
  Built-in models respect finite area ranges, including translated narrow EMG
  and Lorentzian peaks (#121).

* `optimize_deconvolution()` now preserves the initial component order with
  every optimizer when `constrain_positions = TRUE`, including model-specific
  bounds and fixed parameters. Final centers may coincide; component identities
  and model assignments remain attached to their original slots (#117).

* EMG deconvolution no longer caps precursor amplitude using the observed apex. Reported `location` and `height` now describe the fitted component apex within the measured range.

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
