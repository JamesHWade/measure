# Detect Peaks in Measurements

`step_measure_peaks_detect()` creates a *specification* of a recipe step
that detects peaks in measurement data and stores them in a new `.peaks`
column.

## Usage

``` r
step_measure_peaks_detect(
  recipe,
  algorithm = "prominence",
  min_height = 0,
  min_distance = 0,
  min_prominence = 0,
  snr_threshold = FALSE,
  algorithm_params = list(),
  measures = NULL,
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_peaks_detect")
)
```

## Arguments

- recipe:

  A recipe object.

- algorithm:

  Peak detection algorithm. One of `"prominence"` (default),
  `"derivative"`, `"local_maxima"`, `"cwt"`, or any algorithm registered
  via
  [`register_peak_algorithm()`](https://jameshwade.github.io/measure/dev/reference/register_peak_algorithm.md).
  Use
  [`peak_algorithms()`](https://jameshwade.github.io/measure/dev/reference/peak_algorithms.md)
  to see available algorithms.

- min_height:

  Minimum peak height in the measurement's signal units. If
  `snr_threshold = TRUE`, the existing logical mode interprets this as a
  multiple of the signal's median absolute deviation (MAD).

- min_distance:

  Minimum distance between peaks in x-axis units.

- min_prominence:

  Minimum peak prominence (only for `algorithm = "prominence"`).

- snr_threshold:

  `FALSE` (default) disables SNR filtering; `TRUE` retains the legacy
  behavior of interpreting `min_height` as an SNR multiple. A
  non-negative number sets an additional minimum SNR threshold in MAD
  units. In numeric mode, a candidate must meet both the absolute
  `min_height` and SNR thresholds. A numeric value of zero adds no SNR
  filtering.

- algorithm_params:

  Named list of additional algorithm-specific parameters. These are
  passed to the algorithm function along with the standard parameters.

- measures:

  Optional character vector of measure column names.

- role:

  Not used.

- trained:

  Logical indicating if the step has been trained.

- skip:

  Logical. Should the step be skipped when baking?

- id:

  Unique step identifier.

## Value

An updated recipe with the new step added.

## Details

This step detects peaks in measurement data and creates a new `.peaks`
column containing the detected peaks for each sample. The original
`.measures` column is preserved.

**Detection algorithms:**

- `"prominence"` (default): Finds local maxima and calculates their
  prominence (the height above the surrounding contour). Use
  `min_prominence` to exclude maxima below the chosen prominence
  threshold.

- `"derivative"`: Finds peaks by detecting zero-crossings in the first
  derivative. Noise can introduce additional zero-crossings.

- `"local_maxima"`: Finds local maxima above the chosen height
  threshold, including maxima caused by noise.

- `"cwt"`: Links Mexican-hat wavelet maxima across widths to identify
  persistent peaks. Requires a uniformly spaced axis. See
  [peak_detection_cwt](https://jameshwade.github.io/measure/dev/reference/peak_detection_cwt.md)
  for widths, local SNR, missing data, and limits.

Additional algorithms can be registered by technique packs using
[`register_peak_algorithm()`](https://jameshwade.github.io/measure/dev/reference/register_peak_algorithm.md).

The detection thresholds are tunable with
[`peak_min_height()`](https://jameshwade.github.io/measure/dev/reference/peak_min_height.md),
[`peak_min_distance()`](https://jameshwade.github.io/measure/dev/reference/peak_min_height.md),
[`peak_min_prominence()`](https://jameshwade.github.io/measure/dev/reference/peak_min_height.md),
and
[`peak_snr_threshold()`](https://jameshwade.github.io/measure/dev/reference/peak_min_height.md).
The default search ranges are only starting points: height and
prominence use signal units, distance uses x-axis units, and SNR uses
MAD units. Adjust the ranges to the measurement scale. Prominence is
offered as a tunable threshold only for the built-in prominence
algorithm. For the built-in CWT detector, use, for example,
`algorithm_params = list(min_snr = tune::tune("ridge_snr"))` to search
over wavelet ridge SNR. A grid column named `ridge_snr` supplies the
candidate values. See
[`cwt_min_snr()`](https://jameshwade.github.io/measure/dev/reference/cwt_min_snr.md)
for the supported scalar controls.

**Peak properties stored:**

- `peak_id`: Integer identifier

- `location`: X-axis position of peak apex

- `height`: Y-value at peak apex

- `left_base`, `right_base`: X-axis positions of peak integration bounds

- `prominence_left_base`, `prominence_right_base`: Original contour
  bases for prominence-based detection

- `area`: Initially NA; use
  [`step_measure_peaks_integrate()`](https://jameshwade.github.io/measure/dev/reference/step_measure_peaks_integrate.md)
  to calculate

For `algorithm = "prominence"`, each pair of adjacent retained peaks is
bounded at the lowest sampled value between their apexes. If values tie,
the first sample in acquisition order is used. These bounds do not
overlap in their interiors. The original prominence contour bases are
retained in `prominence_left_base` and `prominence_right_base` for
prominence calculations. This rule considers retained peaks only, so a
filtered-out neighboring peak does not create a boundary. A low sample
can also be far from its peak on a noisy or sloped baseline, so these
bounds do not by themselves remove baseline or noise bias.

To end a prominence peak when the signal returns to a known baseline,
supply both `boundary_baseline` and `boundary_tolerance` in
`algorithm_params`. The baseline is a finite signal level or a function
of the location vector returning a level or one value per location. Set
the positive tolerance in signal units, for example from independent
blank or noise measurements. For a zero-corrected trace with a noise
standard deviation of 0.05,
`list(boundary_baseline = 0, boundary_tolerance = 0.15)` places the band
three standard deviations above zero.

Starting at the apex, each boundary is the nearest sample on that side
at or below baseline plus tolerance, inside the default clipped bounds.
This requires a finite signal on a strictly monotonic axis, an apex
above the band and a sampled return on both sides. An unresolved peak
raises an error with its location. A noise excursion can end a window
early, and unresolved neighbors can contribute to the same window if the
signal stays above the band between them. The original contours still
determine prominence. Integration uses its own `baseline` setting.

Deconvolution uses current `left_base` and `right_base` as finite
limits, so fitted areas omit model tails outside them.

Use
[`step_measure_peaks_properties()`](https://jameshwade.github.io/measure/dev/reference/step_measure_peaks_properties.md)
to calculate additional peak metrics such as prominence and full width
at half maximum (FWHM).

## See also

[`peak_algorithms()`](https://jameshwade.github.io/measure/dev/reference/peak_algorithms.md),
[`register_peak_algorithm()`](https://jameshwade.github.io/measure/dev/reference/register_peak_algorithm.md)

Other peak-operations:
[`step_measure_peaks_deconvolve()`](https://jameshwade.github.io/measure/dev/reference/step_measure_peaks_deconvolve.md),
[`step_measure_peaks_filter()`](https://jameshwade.github.io/measure/dev/reference/step_measure_peaks_filter.md),
[`step_measure_peaks_integrate()`](https://jameshwade.github.io/measure/dev/reference/step_measure_peaks_integrate.md),
[`step_measure_peaks_properties()`](https://jameshwade.github.io/measure/dev/reference/step_measure_peaks_properties.md),
[`step_measure_peaks_to_table()`](https://jameshwade.github.io/measure/dev/reference/step_measure_peaks_to_table.md)

## Examples

``` r
library(recipes)

rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
  update_role(id, new_role = "id") |>
  step_measure_input_long(transmittance, location = vars(channel)) |>
  step_measure_peaks_detect(min_height = 0.5, min_distance = 5) |>
  prep()

result <- bake(rec, new_data = NULL)
# Result now has .peaks column alongside .measures

# Use a different algorithm
rec2 <- recipe(water + fat + protein ~ ., data = meats_long) |>
  update_role(id, new_role = "id") |>
  step_measure_input_long(transmittance, location = vars(channel)) |>
  step_measure_peaks_detect(algorithm = "derivative", min_height = 0.5) |>
  prep()
```
