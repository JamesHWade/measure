# Changelog

## measure 0.0.1.9003

- [`measure_fold()`](https://jameshwade.github.io/measure/dev/reference/measure_fold.md)
  retains each measurement’s coordinates and metadata after unfolding a
  collection, including unequal grids and lengths. Valid reordered point
  indices restore their original coordinate/value association;
  incomplete mappings and malformed metadata fail clearly
  ([\#145](https://github.com/JamesHWade/measure/issues/145)).

- [`measure_ndim()`](https://jameshwade.github.io/measure/dev/reference/measure_ndim.md)
  and empty nD list subsets retain their validated dimensional prototype
  through list filtering, vctrs slicing and recipe baking. Empty 3D
  collections print as 3D; subsetting keeps strict vctrs index rules
  ([\#151](https://github.com/JamesHWade/measure/issues/151)).

- [`measure_project()`](https://jameshwade.github.io/measure/dev/reference/measure_project.md)
  validates dimensions, flags and scalar summaries before combining
  results. List outputs retain compatible axes; ordinary R all-missing
  mean/sum behavior is explicit
  ([\#145](https://github.com/JamesHWade/measure/issues/145)).

- [`measure_slice()`](https://jameshwade.github.io/measure/dev/reference/measure_slice.md)
  rejects ambiguous selectors and incomplete predicate masks instead of
  recycling them or inserting missing rows. Heterogeneous list
  reductions fail with guidance for retaining axes or mapping separately
  ([\#145](https://github.com/JamesHWade/measure/issues/145)).

- [`new_measure_nd_tbl()`](https://jameshwade.github.io/measure/dev/reference/new_measure_nd_tbl.md)
  rejects malformed numbered dimensions and ambiguous dimension
  metadata. nD list construction and grid inspection validate mutable
  payloads; unknown coordinates stay visible in grid summaries and
  cannot establish a regular grid
  ([\#145](https://github.com/JamesHWade/measure/issues/145)).

- A runnable analytical-records guide demonstrates reinjections,
  detector channels, interleaved curve import, source references, native
  roundtrips, and direct use of independent curves in recipes.

- [`write_measure_record()`](https://jameshwade.github.io/measure/dev/reference/record-bundle.md)
  and
  [`read_measure_record()`](https://jameshwade.github.io/measure/dev/reference/record-bundle.md)
  provide a versioned CSV/TSV bundle with explicit schemas, payload
  hashes, total read limits, and exact numeric roundtrips. Writes
  preserve existing destinations and commit the manifest last;
  incomplete and structurally invalid bundles fail clearly
  ([\#107](https://github.com/JamesHWade/measure/issues/107)).

- [`measure_record_from_long()`](https://jameshwade.github.io/measure/dev/reference/measure_record_from_long.md)
  and
  [`read_measure_record_delim()`](https://jameshwade.github.io/measure/dev/reference/read_measure_record_delim.md)
  import explicitly mapped scalar curves. CSV/TSV imports preserve text
  IDs, hash the exact source bytes, and retain parsed-record row
  references. Numeric parsing rejects malformed, nonfinite, overflowing,
  or underflowing tokens
  ([\#107](https://github.com/JamesHWade/measure/issues/107)).

- [`measure_record()`](https://jameshwade.github.io/measure/dev/reference/measure_record.md)
  connects explicit sample, injection, channel, source, and curve
  identities. Independent one-dimensional grids, declared units, missing
  metadata, and source-row references are validated without aligning
  points or inferring identities
  ([\#81](https://github.com/JamesHWade/measure/issues/81),
  [\#107](https://github.com/JamesHWade/measure/issues/107)).

- ALS and airPLS use a sparse mixed-system solver to bound memory for
  long traces and preserve affine signals at large penalties. Their
  defaults and iteration rules are unchanged. Numerically unresolved
  systems fail with a clear error
  ([\#136](https://github.com/JamesHWade/measure/issues/136)).

- Prominence-based peak detection clips integration bounds between
  adjacent retained peaks. Original prominence contours are stored
  separately, preserving prominence calculations and refined SEC
  integration windows. Filtered-out neighbors and baseline/noise effects
  still require care
  ([\#131](https://github.com/JamesHWade/measure/issues/131)).

- `measure` now requires R 4.5.0 or later
  ([\#132](https://github.com/JamesHWade/measure/issues/132)).

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
