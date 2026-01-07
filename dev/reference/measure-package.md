# measure: A Recipes-Style Interface to Tidymodels for Analytical Measurements

The measure package provides preprocessing steps for analytical
measurement data (spectroscopy, chromatography, etc.) within the
tidymodels framework.

## Input Steps

Convert raw data to measure's internal format:

- [`step_measure_input_wide()`](https://jameshwade.github.io/measure/dev/reference/step_measure_input_wide.md):
  For data with measurements in columns

- [`step_measure_input_long()`](https://jameshwade.github.io/measure/dev/reference/step_measure_input_long.md):
  For data with measurements in rows

## Processing Steps

Apply spectral preprocessing transformations:

- [`step_measure_savitzky_golay()`](https://jameshwade.github.io/measure/dev/reference/step_measure_savitzky_golay.md):
  Smoothing and derivatives

- [`step_measure_snv()`](https://jameshwade.github.io/measure/dev/reference/step_measure_snv.md):
  Standard Normal Variate normalization

- [`step_measure_msc()`](https://jameshwade.github.io/measure/dev/reference/step_measure_msc.md):
  Multiplicative Scatter Correction

- [`step_measure_map()`](https://jameshwade.github.io/measure/dev/reference/step_measure_map.md):
  Custom transformations

## Sample-wise Normalization

Normalize each spectrum independently:

- [`step_measure_normalize_sum()`](https://jameshwade.github.io/measure/dev/reference/step_measure_normalize_sum.md):
  Divide by sum

- [`step_measure_normalize_max()`](https://jameshwade.github.io/measure/dev/reference/step_measure_normalize_max.md):
  Divide by maximum

- [`step_measure_normalize_range()`](https://jameshwade.github.io/measure/dev/reference/step_measure_normalize_range.md):
  Scale to 0-1 range

- [`step_measure_normalize_vector()`](https://jameshwade.github.io/measure/dev/reference/step_measure_normalize_vector.md):
  L2 normalization

- [`step_measure_normalize_auc()`](https://jameshwade.github.io/measure/dev/reference/step_measure_normalize_auc.md):
  Divide by area under curve

- [`step_measure_normalize_peak()`](https://jameshwade.github.io/measure/dev/reference/step_measure_normalize_peak.md):
  Normalize by region (tunable)

## Variable-wise Scaling

Scale across samples at each location (learns from training data):

- [`step_measure_center()`](https://jameshwade.github.io/measure/dev/reference/step_measure_center.md):
  Mean centering

- [`step_measure_scale_auto()`](https://jameshwade.github.io/measure/dev/reference/step_measure_scale_auto.md):
  Z-score normalization

- [`step_measure_scale_pareto()`](https://jameshwade.github.io/measure/dev/reference/step_measure_scale_pareto.md):
  Pareto scaling

- [`step_measure_scale_range()`](https://jameshwade.github.io/measure/dev/reference/step_measure_scale_range.md):
  Range scaling

- [`step_measure_scale_vast()`](https://jameshwade.github.io/measure/dev/reference/step_measure_scale_vast.md):
  VAST scaling

## Output Steps

Convert back to modeling-ready format:

- [`step_measure_output_wide()`](https://jameshwade.github.io/measure/dev/reference/step_measure_output_wide.md):
  Back to wide format

- [`step_measure_output_long()`](https://jameshwade.github.io/measure/dev/reference/step_measure_output_long.md):
  Back to long format

## See also

- `vignette("measure")` for getting started

- `vignette("preprocessing")` for detailed technique descriptions

- [`vignette("recipes")`](https://recipes.tidymodels.org/articles/recipes.html)
  for tidymodels integration

## Author

**Maintainer**: James Wade <github@jameshwade.com>
([ORCID](https://orcid.org/0000-0002-9740-1905))

Other contributors:

- Max Kuhn <max@posit.co>
  ([ORCID](https://orcid.org/0000-0003-2402-136X)) \[contributor\]
