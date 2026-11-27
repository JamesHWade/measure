# Plot a quantified chromatography batch

Plot a quantified chromatography batch

## Usage

``` r
# S3 method for class 'measure_batch_results'
autoplot(object, type = c("chromatograms", "calibration"), ...)
```

## Arguments

- object:

  A `measure_batch_results` object from
  [`measure_quantify_batch()`](https://jameshwade.github.io/measure/dev/reference/measure_quantify_batch.md).

- type:

  Plot type: `"chromatograms"` (default) shows overlaid batch traces
  with the target retention window shaded; `"calibration"` shows the
  fitted calibration curve with QC and unknown injections overlaid.

- ...:

  Additional arguments (unused).

## Value

A `ggplot2` object.
