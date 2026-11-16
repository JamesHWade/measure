# Extract a centroid total-ion or selected-ion trace

Extract one explicit polarity and MS level from one declared MS stream.
Centroid intensities are summed in their original point order. A closed
`mz_range` selects an inclusive extracted-ion window in the stream's
declared m/z unit; `NULL` extracts a total-ion trace.

## Usage

``` r
measure_ms_trace(x, stream_id, polarity, ms_level, mz_range = NULL)
```

## Arguments

- x:

  A validated `measure_acquisition`.

- stream_id:

  One exact MS stream identifier.

- polarity:

  One of `"positive"` or `"negative"`.

- ms_level:

  One positive MS level.

- mz_range:

  `NULL` for a total-ion trace, or a two-value closed m/z interval in
  the stream's declared unit.

## Value

An ordinary tibble with one row per selected scan. The columns are
`scan_id`, `scan_index`, `source_scan_id`, `retention_time`, `value`,
`status`, `selected_points`, `saturation_unknown`, and `.point_indices`.
The `processing` attribute records the declared extraction parameters.
The returned table does not require a monotonic time axis; converting it
to a scalar record requires that record's checks.

## Details

One stream, polarity, and MS level must be selected explicitly. A scan
whose unknown settings could match the requested combination is
ambiguous and causes an error. Unknown settings on scans excluded by
another known selector do not block the calculation. An empty selection
is an error.

Selected rows follow their declared `scan_index`, retaining repeated or
nonmonotonic retention times. Times must be known and finite and the
stream must declare a time unit. XIC bounds require a known declared m/z
unit. No units are converted and no samples, polarities or MS levels are
pooled.

Acquired selected scans must declare centroid representation. Every m/z
in those scans must be finite and positive, including points outside an
XIC window. Selected intensities must be finite and nonnegative or
ordinary NA; NaN, infinities, negative values and unrepresentable sums
cause errors. Unselected intensities do not contribute to an XIC.
Duplicate m/z points contribute separately, in their original order.

## Missingness and saturation

The following statuses describe the declared centroid calculation:

- `missing_scan`: an explicitly missing scan yields NA, with no points.

- `empty_scan`: an acquired-empty centroid spectrum yields zero.

- `empty_window`: a nonempty spectrum with no XIC matches yields zero.

- `saturated`: any selected point declared saturated yields NA.

- `missing_intensity`: any selected ordinary NA yields NA.

- `calculated`: the selected finite intensities were summed.

Saturation takes precedence over ordinary missing intensity. Invalid
selected numerical states still cause errors before either policy is
applied. `saturation_unknown` independently records whether any selected
mask entry is unknown; it is not silently treated as FALSE. A calculated
value does not certify an instrument, method, absence of saturation, or
analyte identity.

## Contributor links and processing

`.point_indices` retains every selected original point position,
including those responsible for an NA result. Together with `scan_id`,
these indices link each output row to its retained acquisition input.
They are not source file line numbers. Source record ordinals remain in
the raw scan's `.source_rows` and original source scan identifiers
remain opaque text.

The `processing` attribute is a named list recording version 1, the
operation, package version, explicit stream/injection/channel/source IDs
and available source digest, selectors, bounds, units, centroid sum,
propagation policies and absence of synchronization. Unknown source
evidence remains unknown. Keep this metadata and contributor links with
the raw acquisition. A scalar record export alone does not preserve
many-to-one extraction ancestry.

Profile spectra may be stored but are not calculated here: an unweighted
profile-point sum depends on sampling density and is not physical
integration. This function performs no centroiding, interpolation,
binning or annotation.

## See also

[`measure_acquisition()`](https://jameshwade.github.io/measure/dev/reference/measure_acquisition.md)
