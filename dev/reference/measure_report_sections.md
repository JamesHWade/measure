# Build report sections for processed measurements

A technique pack can register a method for its results subclass, call
[`NextMethod()`](https://rdrr.io/r/base/UseMethod.html) to obtain the
core sections, and append its own tables or plots.
[`render_measure_report()`](https://jameshwade.github.io/measure/dev/reference/render_measure_report.md)
calls the method in the caller's R session. The same sections can be
used directly in a pack's own template.

## Usage

``` r
# S3 method for class 'measure_batch_results'
measure_report_sections(x, ...)

measure_report_sections(x, ...)
```

## Arguments

- x:

  A
  [`measure_results()`](https://jameshwade.github.io/measure/dev/reference/measure_results.md)
  object or a technique-pack subclass.

- ...:

  Passed to
  [`ggplot2::autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  by the core method. For example, use `type = "summary"` for a
  per-sample summary plot.

## Value

An ordered, uniquely named list. Each name is a section heading. Each
element is a data frame or a ggplot. In the core method,
`Sample summary` contains sample identifiers, point and missing counts,
the mean, standard deviation, range, and declared signal unit.
`Measurements` contains the plot. Use `summary(x)` for the full
measurement summary, including axis metadata.

## Examples

``` r
signal <- new_measure_tbl(c(0, 1, 2), c(0.1, 0.8, 0.2))
sections <- measure_report_sections(measure_results(signal))
sections[["Sample summary"]]
#> # A tibble: 1 × 7
#>   row_id n_points n_missing  mean    sd   min   max
#>    <int>    <int>     <int> <dbl> <dbl> <dbl> <dbl>
#> 1      1        3         0 0.367 0.379   0.1   0.8
```
