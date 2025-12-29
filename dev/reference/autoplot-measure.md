# Autoplot Methods for Measure Objects

Create ggplot2 visualizations of spectral/chromatographic data stored in
measure objects.

## Usage

``` r
# S3 method for class 'measure_tbl'
autoplot(object, ...)

# S3 method for class 'measure_list'
autoplot(object, summary = FALSE, max_spectra = 50, alpha = 0.3, ...)

# S3 method for class 'recipe'
autoplot(object, n_samples = 10, which = c("before_after", "summary"), ...)
```

## Arguments

- object:

  A `measure_tbl`, `measure_list`, or `recipe` object.

- ...:

  Additional arguments passed to specific plot types.

- summary:

  Logical. If TRUE, add mean +/- SD ribbon. Default FALSE.

- max_spectra:

  Maximum number of individual spectra to plot. Default 50. Set to NULL
  for no limit.

- alpha:

  Transparency for individual spectrum lines. Default 0.3.

- n_samples:

  Number of samples to show in before/after comparison. Default 10.

- which:

  Which comparison to show: `"before_after"` (default) shows
  side-by-side before/after comparison, `"summary"` shows summary
  statistics (mean +/- SD) for the processed data.

## Value

A ggplot2 object.

## Details

For `measure_tbl` (single spectrum):

- Plots location vs value as a line

For `measure_list` (multiple spectra):

- Plots all spectra with optional summary ribbon

- Use `summary = TRUE` for mean +/- SD ribbon

- Use `max_spectra` to limit number of individual lines

For `recipe`:

- Shows before/after comparison of preprocessing

- Requires a prepped recipe

- Use `n_samples` to control number of samples shown

## Examples

``` r
if (FALSE) { # \dontrun{
library(ggplot2)

# Single spectrum
spec <- new_measure_tbl(location = 1:100, value = sin(1:100 / 10) + rnorm(100, sd = 0.1))
autoplot(spec)

# Multiple spectra with summary
rec <- recipe(water ~ ., data = meats_long) |>
  step_measure_input_long(transmittance, location = vars(channel)) |>
  prep()
baked <- bake(rec, new_data = NULL)
autoplot(baked$.measures, summary = TRUE)

# Recipe before/after comparison
rec <- recipe(water ~ ., data = meats_long) |>
  update_role(id, new_role = "id") |>
  step_measure_input_long(transmittance, location = vars(channel)) |>
  step_measure_snv() |>
  prep()
autoplot(rec, n_samples = 10)
} # }
```
