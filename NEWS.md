# measure 0.0.1.9003

* `new_peaks_tbl()` and `new_peaks_list()` are now public constructors for
  technique packs. Peak tables can include additional technique-specific columns.
* `step_measure_savitzky_golay()` now preserves the centered input locations
  after trimming edge points and rejects measurements with inconsistent axes.
