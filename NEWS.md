# measure 0.0.1.9003

* `emg_peak_model()` now evaluates the correctly signed Gaussian-exponential
  convolution and remains numerically stable as `tau` approaches zero (#89).
* `optimize_deconvolution()` now returns Nelder-Mead parameters, predictions,
  and objective values from the same constrained fit (#89).
* `new_peaks_tbl()` and `new_peaks_list()` are now public constructors for
  technique packs. Peak tables can include additional technique-specific columns.
* `step_measure_savitzky_golay()` now preserves the centered input locations
  after trimming edge points and rejects measurements with inconsistent axes.
