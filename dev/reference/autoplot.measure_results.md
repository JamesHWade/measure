# Plot processed measurement results

Plot processed measurement results

## Usage

``` r
# S3 method for class 'measure_results'
autoplot(
  object,
  type = c("auto", "signal", "map", "summary"),
  max_spectra = 12L,
  summary = FALSE,
  alpha = 0.8,
  point_size = 2.5,
  ...
)
```

## Arguments

- object:

  A
  [`measure_results()`](https://jameshwade.github.io/measure/dev/reference/measure_results.md)
  object.

- type:

  The default, `"auto"`, displays one-dimensional signals as lines,
  two-dimensional signals as maps, and higher-dimensional signals as
  per-sample ranges and means. Choose `"signal"`, `"map"`, or
  `"summary"` to select a view explicitly.

- max_spectra:

  Maximum number of samples to include in a signal overlay, taken in
  input order. Use `NULL` to show all samples.

- summary:

  For a signal overlay, add the mean and standard-deviation ribbon for
  the displayed samples at each location.

- alpha:

  Opacity of the signal traces or map points.

- point_size:

  Size of the points in a map.

- ...:

  Reserved for method extensions.

## Value

A
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.

## Details

Maps draw a point at each measured coordinate pair. Each sample has a
separate panel. Axis names and units must agree across panels.
