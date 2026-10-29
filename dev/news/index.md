# Changelog

## measure 0.0.1.9003

- [`step_measure_baseline_custom()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_custom.md)
  now supports declared custom arguments through tuning discovery,
  custom IDs, parameter extraction, finalization, and prep/bake.
  Unresolved markers and malformed declarations fail clearly. Formula
  arguments use a private invocation environment, preserving caller
  bindings and ordinary function scoping across repeated baking
  ([\#124](https://github.com/JamesHWade/measure/issues/124)).

- The preprocessing, baseline, internal-class, and multidimensional
  guides now run their examples. A new technique-pack guide demonstrates
  public extension APIs. Python examples check the optional backend, and
  loading measure no longer overwrites `RETICULATE_PYTHON`. The baseline
  guide uses fixed custom arguments while nested custom-argument tuning
  is tracked in
  [\#124](https://github.com/JamesHWade/measure/issues/124).

- `step_measure_peaks_detect(algorithm = "cwt")` now detects peaks
  through Mexican-hat wavelets and persistent scale ridges, with local
  noise scoring, explicit sample-width controls, missing-data
  separation, and bounded work. See
  [`?peak_detection_cwt`](https://jameshwade.github.io/measure/dev/reference/peak_detection_cwt.md)
  for parameter units and practical limits.

- Fitted peak areas now integrate the fitted component over explicit
  finite bounds. Detected bases remain available separately, and stored
  row metadata preserves component-aware reintegration after filtering
  or serialization. Built-in models respect finite area ranges,
  including translated narrow EMG and Lorentzian peaks
  ([\#121](https://github.com/JamesHWade/measure/issues/121)).

- [`optimize_deconvolution()`](https://jameshwade.github.io/measure/dev/reference/optimize_deconvolution.md)
  now preserves the initial component order with every optimizer when
  `constrain_positions = TRUE`, including model-specific bounds and
  fixed parameters. Final centers may coincide; component identities and
  model assignments remain attached to their original slots
  ([\#117](https://github.com/JamesHWade/measure/issues/117)).

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
