# Create a processed measurement results object

`measure_results()` keeps processed measurements together with the
column and metadata selections used to interpret them. The measurement
data are retained as supplied.

## Usage

``` r
measure_results(data, measures = NULL, id_cols = NULL)

# S3 method for class 'measure_results'
summary(object, ..., na.rm = TRUE)

# S3 method for class 'measure_results'
print(x, ...)
```

## Arguments

- data:

  A baked data frame, a `measure_list` or `measure_nd_list`, or a single
  `measure_tbl` or `measure_nd_tbl`.

- measures:

  Name of the measurement column in `data`. Required when a data frame
  contains more than one measurement column.

- id_cols:

  Names of sample metadata columns to use when summarizing the results.
  The default follows
  [`measure_summary_table()`](https://jameshwade.github.io/measure/dev/reference/measure_summary_table.md)
  and keeps scalar columns. Use
  [`character()`](https://rdrr.io/r/base/character.html) to omit them.

- object:

  A `measure_results` object.

- ...:

  Reserved for method extensions.

- na.rm:

  Whether to omit missing values from the summary statistics.

- x:

  A `measure_results` object.

## Value

An ordinary S3 list with components `data`, `measures`, and `id_cols`,
and class `measure_results`. Subclasses can be added by prefixing the
class vector. The original measurement objects, including n-dimensional
declarations and value metadata, remain in `data`.

## Details

[`summary()`](https://rdrr.io/r/base/summary.html) returns one row per
sample using
[`measure_summary_table()`](https://jameshwade.github.io/measure/dev/reference/measure_summary_table.md).
Use
[`ggplot2::autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
to draw the selected signal and
[`render_measure_report()`](https://jameshwade.github.io/measure/dev/reference/render_measure_report.md)
to export the summary and plot.

## Examples

``` r
samples <- tibble::tibble(
  sample_id = c("A", "B"),
  signal = new_measure_list(list(
    new_measure_tbl(c(210, 254), c(0.12, 0.18)),
    new_measure_tbl(c(210, 254), c(0.20, 0.41))
  ))
)
results <- measure_results(samples, id_cols = "sample_id")
summary(results)
#> # A tibble: 2 × 12
#>   sample_id row_id value_quantity value_unit n_dimensions n_points n_missing
#>   <chr>      <int> <chr>          <chr>             <int>    <int>     <int>
#> 1 A              1 NA             NA                    1        2         0
#> 2 B              2 NA             NA                    1        2         0
#> # ℹ 5 more variables: n_observed <int>, mean <dbl>, sd <dbl>, min <dbl>,
#> #   max <dbl>
results
#> <measure_results>
#>   Members: 2
#>   Dimensions: 1D
#>   Measure column: signal
```
