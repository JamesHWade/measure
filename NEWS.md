# measure 0.0.1.9004

* Preprocessing steps that learn a value at each measurement location now
  check the ordered coordinates at training and prediction. This prevents
  shifted spectra from being processed or labelled on the wrong grid. For
  older fitted steps that did not record their grid, prep the recipe again
  with its training data (#192).

* `step_measure_channel_combine(strategy = "stack")` preserves each channel's
  coordinates, including when channels have different lengths. Weighted
  sums and means require matching locations within each sample (#192).

* `step_measure_parafac()` now fits all component scores jointly when baking.
  Scores can change for overlapping component loadings. Linearly dependent
  loadings use the least-squares solution with the smallest Euclidean norm
  (#194).

* `step_measure_parafac()` and `step_measure_tucker()` now fit and project
  measurements with two or three axes. Both steps and the two-axis
  `step_measure_mcr_als()` use training coordinates to place each prediction
  value, so point rows can be reordered without changing scores (#192, #195).

* `render_validation_report()` uses the caller's R libraries when Quarto
  renders a report. This lets both templates find `measure` when it is
  installed in a temporary or project-specific library (#186).

* Matrix-based preprocessing steps now reject samples with different point
  counts. Previously, a short signal could be repeated to fill a matrix row
  and produce results for points that were never measured (#180).

* `step_measure_msc()` now centers a flat spectrum reliably when its fitted
  slope is zero. `step_measure_savitzky_golay()` honors `skip = TRUE` on
  new data and checks that its filter settings are finite integers (#180).

* `step_measure_peaks_detect(algorithm = "cwt")` supports tuning the ridge
  SNR, minimum ridge length, allowed gap, noise window and noise quantile.
  Put `tune::tune("ridge_snr")` inside `algorithm_params$min_snr` to vary
  the wavelet threshold in a recipe (#181).

* Prominence detection can end an integration window when the signal returns
  to a known baseline. Supply `boundary_baseline` and `boundary_tolerance`
  in `algorithm_params` to keep a separated, filtered-out neighbor from
  contributing to the area. The default still uses valleys between retained
  peaks (#131).

* `measure_slice_table()` returns one row per measurement point, and
  `measure_summary_table()` summarizes the values for each sample (#189).

* `step_measure_input_long(pad = TRUE)` brings multidimensional samples
  onto the coordinate tuples observed in the current batch. An absent tuple
  receives a missing signal. The step checks for repeated tuples before
  padding (#182).

* `step_measure_input_wide()` keeps each input row as a separate sample and
  pairs its values with the axis in the column order learned at prep time
  (#183).

* Use `set_measure_value_metadata()` to declare what the values in a
  measurement column represent and `measure_value_metadata()` to read the
  declaration. Compatible declarations survive row slicing and binding.

* Moving-average, median, Gaussian and Fourier smoothing, and ALS and
  polynomial baseline correction, retain a declared value unit. Sample-wise
  normalization marks declared values as normalized. If a divisor is unusable,
  the step reports the affected sample before returning a mislabeled column.

* Savitzky–Golay smoothing at order zero preserves a declared signal unit.
  Positive-order Savitzky–Golay, finite-difference and gap derivatives mark
  the result as a signal derivative. Their output unit is left unspecified.

# measure 0.0.1.9003

* Guides and the homepage now pair preprocessing, records, acquisitions and
  validation examples with labeled plots, captions and descriptive alt text.
  Revised prose distinguishes source absorbance, arbitrary coordinates and
  simulated results from physical qualification (#169).

* Native bundle readers recognize symbolic links on Windows as well as Unix-like
  systems. Scalar and nD writers preserve existing dangling-link destinations.

* `measure_acquisition()` links raw ragged MS scans and two-dimensional arrays
  to declared samples, injections, channels and sources. Scan order, raw numeric
  storage, missing versus acquired-empty scans and per-point saturation and
  source references remain explicit (#162).

* The acquisition/MS guide maps raw source records to ragged MS scans and an
  independently sampled array, saves and reloads the acquisition, then traces
  each selected centroid result to its contributing points and source ordinals.
  Missingness, saturation, declared m/z conventions and interpretation limits
  remain visible (#162).

* Acquisition interchange has a public installed-R/Python qualification profile
  with fixed binary64 truth, typed raw acquisitions, independent trace checks,
  reverse/reorder controls and corruption rejection. CI also runs the tools
  outside the checkout from paths containing spaces (#162).

* `write_measure_acquisition()` and `read_measure_acquisition()` preserve linked
  raw acquisitions in a fixed native CSV/TSV profile. Typed values, numerical
  states, masks, point/source links and metadata survive readback; publication
  is exclusive and selected-file reads use one cumulative byte limit (#162).

* `measure_ms_trace()` calculates explicitly selected centroid TIC/XIC traces
  with scan and point contributor links. Missing scans, empty spectra/windows,
  missing intensities and known or unknown saturation remain distinguishable;
  profile integration and detector synchronization are not inferred (#162).

* `write_measure_nd()` and `read_measure_nd()` preserve supported nD tables and
  collections in a native CSV or TSV bundle, including column storage types,
  finite numerical values, signed zero, missing/nonfinite states, member and
  point order, dimension metadata, list labels and empty prototypes (#154).

* `measure_fold()` retains each measurement's coordinates and metadata after
  unfolding a collection, including unequal grids and lengths. Valid reordered
  point indices restore their original coordinate/value association; incomplete
  mappings and malformed metadata fail clearly (#145).

* `measure_grid_info()` reports dimensionality from the validated prototype of
  an empty nD collection, agreeing with `measure_ndim()` after filtering or
  restoring an empty 3D list (#153).

* `measure_ndim()` and empty nD list subsets retain their validated dimensional
  prototype through list filtering, vctrs slicing and recipe baking. Empty 3D
  collections print as 3D; subsetting keeps strict vctrs index rules (#151).

* `measure_project()` validates dimensions, flags and scalar summaries before
  combining results. List outputs retain compatible axes; ordinary R all-missing
  mean/sum behavior is explicit (#145).

* `measure_slice()` rejects ambiguous selectors and incomplete predicate masks
  instead of recycling them or inserting missing rows. Heterogeneous list
  reductions fail with guidance for retaining axes or mapping separately (#145).

* `new_measure_nd_tbl()` rejects malformed numbered dimensions and ambiguous
  dimension metadata. nD list construction and grid inspection validate mutable
  payloads; unknown coordinates stay visible in grid summaries and cannot
  establish a regular grid (#145).

* A runnable analytical-records guide demonstrates reinjections, detector
  channels, interleaved curve import, source references, native roundtrips,
  and direct use of independent curves in recipes.

* `write_measure_record()` and `read_measure_record()` provide a versioned
  CSV/TSV bundle with explicit schemas, payload hashes, total read limits, and
  exact numeric roundtrips. Writes preserve existing destinations and commit
  the manifest last; incomplete and structurally invalid bundles fail clearly
  (#107).

* `measure_record_from_long()` and `read_measure_record_delim()` import
  explicitly mapped scalar curves. CSV/TSV imports preserve text IDs, hash the
  exact source bytes, and retain parsed-record row references. Numeric parsing
  rejects malformed, nonfinite, overflowing, or underflowing tokens (#107).

* `measure_record()` connects explicit sample, injection, channel, source, and
  curve identities. Independent one-dimensional grids, declared units, missing
  metadata, and source-row references are validated without aligning points
  or inferring identities (#81, #107).

* ALS and airPLS use a sparse mixed-system solver to bound memory for long
  traces and preserve affine signals at large penalties. Their defaults and
  iteration rules are unchanged. Numerically unresolved systems fail with a
  clear error (#136).

* Prominence-based peak detection clips integration bounds between adjacent
  retained peaks. Original prominence contours are stored separately, preserving
  prominence calculations and refined SEC integration windows. Filtered-out
  neighbors and baseline/noise effects still require care (#131).

* `measure` now requires R 4.5.0 or later (#132).

* `step_measure_baseline_custom()` now supports declared custom arguments
  through tuning discovery, custom IDs, parameter extraction, finalization,
  and prep/bake. Unresolved markers and malformed declarations fail clearly.
  Formula arguments use a private invocation environment, preserving caller
  bindings and ordinary function scoping across repeated baking (#124).

* The preprocessing, baseline, internal-class, and multidimensional guides now
  run their examples. A new technique-pack guide demonstrates public extension
  APIs. Python examples check the optional backend, and loading measure no
  longer overwrites `RETICULATE_PYTHON`. The baseline guide uses fixed custom
  arguments while nested custom-argument tuning is tracked in #124.

* `step_measure_peaks_detect(algorithm = "cwt")` now detects peaks through
  Mexican-hat wavelets and persistent scale ridges, with local noise scoring,
  explicit sample-width controls, missing-data separation, and bounded work.
  See `?peak_detection_cwt` for parameter units and practical limits.

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
