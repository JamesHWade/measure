# Explanation: Internal Class System

``` r
library(measure)
library(recipes)
```

## Overview

This vignette describes measure’s internal class system. While most
users won’t need to interact with these internals directly,
understanding them is useful if you’re:

- Debugging unexpected behavior
- Contributing to measure
- Building extensions that work with measure data

## Motivation

Early versions of measure relied on a column named `.measures` to store
spectral data. This worked but had limitations:

- Name clashes if users had their own `.measures` column
- No way to have multiple measure columns
- Detection relied on column names, not types

Following [Issue \#16](https://github.com/JamesHWade/measure/issues/16),
measure now uses custom S3 classes. This enables robust detection via
[`inherits()`](https://rdrr.io/r/base/class.html) and supports multiple
measure columns per dataset (see [Multiple Measure
Columns](#multiple-measure-columns) below).

## The two classes

measure uses a two-level class hierarchy:

### `measure_tbl`

A single measurement - a tibble with `location` and `value` columns:

``` r
# After preprocessing, each row's .measures element is a measure_tbl
rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
  update_role(id, new_role = "id") |>
  step_measure_input_long(transmittance, location = vars(channel)) |>
  prep()

result <- bake(rec, new_data = NULL)

# Extract one measurement
one_measurement <- result$.measures[[1]]
one_measurement
#> <measure_tbl [100 x 2]>
#> # A tibble: 100 × 2
#>    location value
#>       <int> <dbl>
#>  1        1  2.62
#>  2        2  2.62
#>  3        3  2.62
#>  4        4  2.62
#>  5        5  2.62
#>  6        6  2.62
#>  7        7  2.62
#>  8        8  2.62
#>  9        9  2.63
#> 10       10  2.63
#> # ℹ 90 more rows

# Check the class
class(one_measurement)
#> [1] "measure_tbl" "tbl_df"      "tbl"         "data.frame"
is_measure_tbl(one_measurement)
#> [1] TRUE
```

### `measure_list`

A list column containing multiple `measure_tbl` objects - one per row in
your data:

``` r
# The .measures column itself is a measure_list
class(result$.measures)
#> [1] "measure_list"  "vctrs_list_of" "vctrs_vctr"    "list"
is_measure_list(result$.measures)
#> [1] TRUE

# Nice printing in tibbles
result
#> # A tibble: 215 × 6
#>       id water   fat protein .measures channel    
#>    <int> <dbl> <dbl>   <dbl>    <meas> <list>     
#>  1     1  60.5  22.5    16.7 [100 × 2] <int [100]>
#>  2     2  46    40.1    13.5 [100 × 2] <int [100]>
#>  3     3  71     8.4    20.5 [100 × 2] <int [100]>
#>  4     4  72.8   5.9    20.7 [100 × 2] <int [100]>
#>  5     5  58.3  25.5    15.5 [100 × 2] <int [100]>
#>  6     6  44    42.7    13.7 [100 × 2] <int [100]>
#>  7     7  44    42.7    13.7 [100 × 2] <int [100]>
#>  8     8  69.3  10.6    19.3 [100 × 2] <int [100]>
#>  9     9  61.4  19.9    17.7 [100 × 2] <int [100]>
#> 10    10  61.4  19.9    17.7 [100 × 2] <int [100]>
#> # ℹ 205 more rows
```

## Detecting measure columns

measure provides helper functions to find and validate measure columns:

### `is_measure_list()` and `is_measure_tbl()`

Test if an object has the appropriate class:

``` r
is_measure_list(result$.measures)
#> [1] TRUE
is_measure_tbl(result$.measures[[1]])
#> [1] TRUE

# Regular lists and tibbles return FALSE
is_measure_list(list())
#> [1] FALSE
is_measure_tbl(tibble::tibble(location = 1:5, value = rnorm(5)))
#> [1] FALSE
```

### `find_measure_cols()`

Find all measure columns in a data frame:

``` r
find_measure_cols(result)
#> [1] ".measures"
```

### `has_measure_col()`

Check that a data frame has at least one measure column, erroring if
not:

``` r
has_measure_col(result)
```

This is used internally by processing steps to validate input.

## Why this matters

The class-based approach provides several benefits:

1.  **Robust detection**: Steps use `inherits(x, "measure_list")`
    instead of checking column names
2.  **Nice printing**: Tibbles show `<meas [100]>` instead of raw list
    output
3.  **Multiple columns**: You can have multiple measure columns per
    dataset (e.g., UV and MS spectra)
4.  **Validation**: The classes enforce that measurements have the
    expected structure

## For package developers

If you’re writing functions that work with measure data:

``` r
my_function <- function(data) {
  # Validate input has measure columns
  has_measure_col(data)

  # Find measure columns
  meas_cols <- find_measure_cols(data)

  # Work with the measure_list
  for (col in meas_cols) {
    measurements <- data[[col]]
    # Each element is a measure_tbl with $location and $value
  }
}
```

The helper functions `measure_to_matrix()` and `matrix_to_measure()` in
`R/helpers.R` convert between measure lists and matrices for bulk
operations.

## Working with Measure Data Interactively

While recipe steps are the primary interface for production pipelines,
measure provides utility functions for interactive exploration and
debugging.

### measure_map(): Prototyping transformations

When developing a custom transformation, use
[`measure_map()`](https://jameshwade.github.io/measure/dev/reference/measure_map.md)
to test it interactively:

``` r
# Apply a transformation to each sample's measurements
centered <- measure_map(result, ~ {
  .x$value <- .x$value - mean(.x$value)
  .x
})

# Check the result
mean(centered$.measures[[1]]$value)  # Should be ~0
#> [1] -1.599431e-16
```

**Important**:
[`measure_map()`](https://jameshwade.github.io/measure/dev/reference/measure_map.md)
is for exploration only. Once your transformation works, move it to
[`step_measure_map()`](https://jameshwade.github.io/measure/dev/reference/step_measure_map.md)
for reproducible pipelines:

``` r
# For production use
rec <- recipe(...) |>
  step_measure_input_long(...) |>
  step_measure_map(~ { .x$value <- .x$value - mean(.x$value); .x })
```

### measure_map_safely(): Fault-tolerant exploration

When exploring data that might have problematic samples, use the safer
variant:

``` r
result <- measure_map_safely(data, risky_function)

# Check which samples failed
result$errors

# result$result contains data with successful transforms
# (failed samples keep original values)
```

### measure_summarize(): Understanding your data

Compute summary statistics across all samples at each measurement
location:

``` r
# Default: mean and SD at each location
stats <- measure_summarize(result)
head(stats)
#> # A tibble: 6 × 3
#>   location  mean    sd
#>      <int> <dbl> <dbl>
#> 1        1  2.81 0.411
#> 2        2  2.81 0.413
#> 3        3  2.81 0.416
#> 4        4  2.82 0.418
#> 5        5  2.82 0.421
#> 6        6  2.82 0.424
```

This is useful for:

- Computing reference spectra (e.g., for MSC-style corrections)
- Identifying high-variability regions
- Quality control and outlier detection

## Multiple Measure Columns

measure supports multiple measure columns in a single dataset. This is
useful when you have different types of measurements (e.g., UV and MS
spectra) that need separate processing.

### Creating multiple measure columns

Use the `col_name` parameter in input steps:

``` r
rec <- recipe(outcome ~ ., data = my_data) |>
  step_measure_input_wide(
    starts_with("uv_"),
    col_name = ".uv_spectrum"
  ) |>
  step_measure_input_wide(
    starts_with("ms_"),
    col_name = ".ms_spectrum"
  )
```

### Processing steps

By default, processing steps operate on **all** measure columns:

``` r
rec <- rec |>
  step_measure_snv()  # Applies to both .uv_spectrum and .ms_spectrum
```

To process specific columns, use the `measures` parameter:

``` r
rec <- rec |>
  step_measure_snv(measures = ".uv_spectrum")  # Only UV
```

### Output steps

When multiple measure columns exist, output steps require you to specify
which column to output:

``` r
rec <- rec |>
  step_measure_output_wide(measures = ".uv_spectrum", prefix = "uv_") |>
  step_measure_output_wide(measures = ".ms_spectrum", prefix = "ms_")
```

If you don’t specify and multiple columns exist, you’ll get a helpful
error message telling you which columns are available.

## See Also

- [Getting
  Started](https://jameshwade.github.io/measure/dev/articles/tutorial-getting-started.md) -
  Learn the fundamentals of measure
- [Multi-Dimensional
  Measurements](https://jameshwade.github.io/measure/dev/articles/reference-multidimensional.md) -
  Working with nD data
- [Preprocessing
  Reference](https://jameshwade.github.io/measure/dev/articles/reference-preprocessing.md) -
  Guide to preprocessing techniques
