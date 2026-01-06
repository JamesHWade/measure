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
  `"derivative"`, `"local_maxima"`, or any algorithm registered via

  [`register_peak_algorithm()`](https://jameshwade.github.io/measure/dev/reference/register_peak_algorithm.md).
  Use
  [`peak_algorithms()`](https://jameshwade.github.io/measure/dev/reference/peak_algorithms.md)
  to see available algorithms.

- min_height:

  Minimum peak height. If `snr_threshold = TRUE`, this is interpreted as
  a signal-to-noise ratio threshold.

- min_distance:

  Minimum distance between peaks in x-axis units.

- min_prominence:

  Minimum peak prominence (only for `algorithm = "prominence"`).

- snr_threshold:

  Logical. If `TRUE`, `min_height` is interpreted as a signal-to-noise
  ratio. Noise is estimated as the MAD of the signal.

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
  prominence (how much a peak stands out from surrounding signal). More
  robust to noise.

- `"derivative"`: Finds peaks by detecting zero-crossings in the first
  derivative. Faster but more sensitive to noise.

- `"local_maxima"`: Finds all local maxima above a threshold. Simple and
  fast but may detect many spurious peaks.

Additional algorithms can be registered by technique packs using
[`register_peak_algorithm()`](https://jameshwade.github.io/measure/dev/reference/register_peak_algorithm.md).

**Peak properties stored:**

- `peak_id`: Integer identifier

- `location`: X-axis position of peak apex

- `height`: Y-value at peak apex

- `left_base`, `right_base`: X-axis positions of peak boundaries

- `area`: Initially NA; use
  [`step_measure_peaks_integrate()`](https://jameshwade.github.io/measure/dev/reference/step_measure_peaks_integrate.md)
  to calculate

## See also

[`peak_algorithms()`](https://jameshwade.github.io/measure/dev/reference/peak_algorithms.md),
[`register_peak_algorithm()`](https://jameshwade.github.io/measure/dev/reference/register_peak_algorithm.md)

Other peak-operations:
[`step_measure_peaks_filter()`](https://jameshwade.github.io/measure/dev/reference/step_measure_peaks_filter.md),
[`step_measure_peaks_integrate()`](https://jameshwade.github.io/measure/dev/reference/step_measure_peaks_integrate.md),
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
