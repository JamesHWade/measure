# Changelog

## measure 0.0.1.9003

- EMG deconvolution no longer caps precursor amplitude using the
  observed apex. Reported `location` and `height` now describe the
  fitted component apex within the measured range.

- [`emg_peak_model()`](https://jameshwade.github.io/measure/dev/reference/emg_peak_model.md)
  now evaluates the correctly signed Gaussian-exponential convolution
  and remains numerically stable as `tau` approaches zero
  ([\#119](https://github.com/JamesHWade/measure/issues/119)).

- [`new_peaks_tbl()`](https://jameshwade.github.io/measure/dev/reference/new_peaks_tbl.md)
  and
  [`new_peaks_list()`](https://jameshwade.github.io/measure/dev/reference/new_peaks_list.md)
  are now public constructors for technique packs. Peak tables can
  include additional technique-specific columns
  ([\#110](https://github.com/JamesHWade/measure/issues/110)).

- [`optimize_deconvolution()`](https://jameshwade.github.io/measure/dev/reference/optimize_deconvolution.md)
  now returns Nelder-Mead parameters, predictions, and objective values
  from the same constrained fit
  ([\#119](https://github.com/JamesHWade/measure/issues/119)).

- [`step_measure_peaks_detect()`](https://jameshwade.github.io/measure/dev/reference/step_measure_peaks_detect.md)
  and
  [`step_measure_peaks_deconvolve()`](https://jameshwade.github.io/measure/dev/reference/step_measure_peaks_deconvolve.md)
  now expose detection thresholds, fit quality, and iteration limits to
  `tune` through dials parameters
  ([\#116](https://github.com/JamesHWade/measure/issues/116)).
  Unresolved tuning calls are retained in list columns when inspecting
  untrained steps with
  [`tidy()`](https://generics.r-lib.org/reference/tidy.html).

- [`step_measure_savitzky_golay()`](https://jameshwade.github.io/measure/dev/reference/step_measure_savitzky_golay.md)
  now preserves the centered input locations after trimming edge points
  and rejects measurements with inconsistent axes
  ([\#113](https://github.com/JamesHWade/measure/issues/113)).
