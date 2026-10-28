# measure 0.0.1.9003

* `new_peaks_tbl()` and `new_peaks_list()` are now public constructors for
  technique packs. Peak tables can include additional technique-specific columns.
* `step_measure_peaks_detect()` and `step_measure_peaks_deconvolve()` now expose
  detection thresholds, fit quality, and iteration limits to `tune` through
  dials parameters.
