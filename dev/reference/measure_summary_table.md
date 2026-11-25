# Export processed measurements as tables

After baking a recipe, use `measure_summary_table()` for one row per
sample or `measure_slice_table()` for one row per measurement point.

## Usage

``` r
measure_summary_table(data, measures = NULL, id_cols = NULL, na.rm = TRUE)

measure_slice_table(data, measures = NULL, id_cols = NULL)
```

## Arguments

- data:

  A baked data frame, a `measure_list` or `measure_nd_list`, or a single
  `measure_tbl` or `measure_nd_tbl`.

- measures:

  Name of the measurement column in `data`. Required when the data frame
  contains more than one measurement column.

- id_cols:

  Names of sample metadata columns to carry into the table. The default
  keeps scalar columns such as identifiers and outcomes. Use
  [`character()`](https://rdrr.io/r/base/character.html) to omit them. A
  metadata name that conflicts with an output field receives a `.data`
  suffix and a number if needed.

- na.rm:

  Whether to omit missing values from the summary statistics. Missing
  counts always include both `NA` and `NaN`.

## Value

A tibble. Both tables identify each input row by its one-based `row_id`.
Named collections also include `member_name`. Declared signal quantity
and unit appear in `value_quantity` and `value_unit`. For nD
measurements, the `axis_names` and `axis_units` list columns retain each
member's dimension declarations.

The summary reports `n_dimensions`, `n_points`, `n_missing`,
`n_observed`, `mean`, `sd` (sample standard deviation), `min`, and
`max`. Empty members and members with only missing values yield missing
statistics.

The slice table retains point order, including repeated coordinates and
missing values. It gives the one-based `point` within each sample, the
original `location` or `location_1`, `location_2`, ... coordinates, and
the signal `value`. An empty member contributes zero point rows.

## See also

[`step_measure_output_long()`](https://jameshwade.github.io/measure/dev/reference/step_measure_output_long.md),
[`step_measure_output_wide()`](https://jameshwade.github.io/measure/dev/reference/step_measure_output_wide.md)
to reshape measurements inside a recipe.

## Examples

``` r
samples <- tibble::tibble(
  sample_id = c("A", "B"),
  absorbance = new_measure_list(list(
    new_measure_tbl(c(210, 254), c(0.12, NA)),
    new_measure_tbl(c(210, 254), c(0.20, 0.41))
  ))
)
measure_summary_table(samples)
#> # A tibble: 2 × 12
#>   sample_id row_id value_quantity value_unit n_dimensions n_points n_missing
#>   <chr>      <int> <chr>          <chr>             <int>    <int>     <int>
#> 1 A              1 NA             NA                    1        2         1
#> 2 B              2 NA             NA                    1        2         0
#> # ℹ 5 more variables: n_observed <int>, mean <dbl>, sd <dbl>, min <dbl>,
#> #   max <dbl>
measure_slice_table(samples)
#> # A tibble: 4 × 7
#>   sample_id row_id value_quantity value_unit point location value
#>   <chr>      <int> <chr>          <chr>      <int>    <dbl> <dbl>
#> 1 A              1 NA             NA             1      210  0.12
#> 2 A              1 NA             NA             2      254 NA   
#> 3 B              2 NA             NA             1      210  0.2 
#> 4 B              2 NA             NA             2      254  0.41
```
