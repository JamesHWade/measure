# Internal Standard Normalization

`step_measure_normalize_istd()` is an alias for
[`step_measure_normalize_peak()`](https://jameshwade.github.io/measure/dev/reference/step_measure_normalize_peak.md)
with domain-specific naming for chromatography and mass spectrometry
users. It normalizes spectra by dividing by a value computed from a
specific region (internal standard peak).

## Usage

``` r
step_measure_normalize_istd(
  recipe,
  location_min,
  location_max,
  method = "mean",
  measures = NULL,
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_normalize_istd")
)
```

## Arguments

- recipe:

  A recipe object. The step will be added to the sequence of operations
  for this recipe.

- location_min:

  Numeric. The lower bound of the region to use for normalization. This
  parameter is tunable with
  [`peak_location_min()`](https://jameshwade.github.io/measure/dev/reference/peak_location_min.md).

- location_max:

  Numeric. The upper bound of the region to use for normalization. This
  parameter is tunable with
  [`peak_location_max()`](https://jameshwade.github.io/measure/dev/reference/peak_location_min.md).

- method:

  Character. The summary statistic to compute from the region. One of
  `"mean"` (default), `"max"`, or `"integral"`.

- measures:

  An optional character vector of measure column names to process. If
  `NULL` (the default), all measure columns (columns with class
  `measure_list`) will be processed. Use this to limit processing to
  specific measure columns when working with multiple measurement types.

- role:

  Not used by this step since no new variables are created.

- trained:

  A logical to indicate if the quantities for preprocessing have been
  estimated.

- skip:

  A logical. Should the step be skipped when the recipe is baked by
  [`recipes::bake()`](https://recipes.tidymodels.org/reference/bake.html)?
  While all operations are baked when
  [`recipes::prep()`](https://recipes.tidymodels.org/reference/prep.html)
  is run, some operations may not be able to be conducted on new data
  (e.g. processing the outcome variable(s)). Care should be taken when
  using `skip = TRUE` as it may affect the computations for subsequent
  operations.

- id:

  A character string that is unique to this step to identify it.

## Value

An updated version of `recipe` with the new step added.

## Details

This function is identical to
[`step_measure_normalize_peak()`](https://jameshwade.github.io/measure/dev/reference/step_measure_normalize_peak.md)
but uses terminology familiar to chromatography and mass spectrometry
practitioners.

Internal standard (ISTD) normalization is commonly used to correct for:

- Injection volume variations

- Ionization efficiency differences

- Matrix effects

- Instrument drift

The internal standard should be a compound that:

- Is chemically stable

- Does not naturally occur in samples

- Elutes in a distinct region

- Has consistent response

## See also

[`step_measure_normalize_peak()`](https://jameshwade.github.io/measure/dev/reference/step_measure_normalize_peak.md)
for the underlying implementation

Other measure-preprocessing:
[`step_measure_absorbance()`](https://jameshwade.github.io/measure/dev/reference/step_measure_absorbance.md),
[`step_measure_calibrate_x()`](https://jameshwade.github.io/measure/dev/reference/step_measure_calibrate_x.md),
[`step_measure_calibrate_y()`](https://jameshwade.github.io/measure/dev/reference/step_measure_calibrate_y.md),
[`step_measure_derivative()`](https://jameshwade.github.io/measure/dev/reference/step_measure_derivative.md),
[`step_measure_derivative_gap()`](https://jameshwade.github.io/measure/dev/reference/step_measure_derivative_gap.md),
[`step_measure_kubelka_munk()`](https://jameshwade.github.io/measure/dev/reference/step_measure_kubelka_munk.md),
[`step_measure_log()`](https://jameshwade.github.io/measure/dev/reference/step_measure_log.md),
[`step_measure_map()`](https://jameshwade.github.io/measure/dev/reference/step_measure_map.md),
[`step_measure_msc()`](https://jameshwade.github.io/measure/dev/reference/step_measure_msc.md),
[`step_measure_ratio_reference()`](https://jameshwade.github.io/measure/dev/reference/step_measure_ratio_reference.md),
[`step_measure_snv()`](https://jameshwade.github.io/measure/dev/reference/step_measure_snv.md),
[`step_measure_subtract_blank()`](https://jameshwade.github.io/measure/dev/reference/step_measure_subtract_blank.md),
[`step_measure_subtract_reference()`](https://jameshwade.github.io/measure/dev/reference/step_measure_subtract_reference.md),
[`step_measure_transmittance()`](https://jameshwade.github.io/measure/dev/reference/step_measure_transmittance.md)

## Examples

``` r
library(recipes)

# Normalize to internal standard peak region (channels 50-60)
rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
  update_role(id, new_role = "id") |>
  step_measure_input_long(transmittance, location = vars(channel)) |>
  step_measure_normalize_istd(
    location_min = 50,
    location_max = 60,
    method = "integral"
  )
```
