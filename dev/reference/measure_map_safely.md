# Apply a Function Safely to Each Sample's Measurements

`measure_map_safely()` is a fault-tolerant version of
[`measure_map()`](https://jameshwade.github.io/measure/dev/reference/measure_map.md)
that captures errors instead of stopping execution. This is useful when
exploring data that may have problematic samples.

## Usage

``` r
measure_map_safely(
  .data,
  .f,
  .cols = NULL,
  ...,
  .otherwise = NULL,
  .error_call = rlang::caller_env()
)
```

## Arguments

- .data:

  A data frame containing one or more `measure_list` columns.

- .f:

  A function or formula to apply to each sample's measurement tibble.

  - If a **function**, it is used as-is.

  - If a **formula** (e.g., `~ { .x$value <- log(.x$value); .x }`), it
    is converted to a function using
    [`rlang::as_function()`](https://rlang.r-lib.org/reference/as_function.html).

- .cols:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Columns to apply the transformation to. Defaults to all `measure_list`
  columns.

- ...:

  Additional arguments passed to `.f`.

- .otherwise:

  Value to use when `.f` fails for a sample. Default is `NULL`, which
  keeps the original (untransformed) measurement.

- .error_call:

  The execution environment for error reporting.

## Value

A list with two elements:

- `result`: A data frame with transformations applied where successful

- `errors`: A tibble with columns `column`, `sample`, and `error`

## See also

[`measure_map()`](https://jameshwade.github.io/measure/dev/reference/measure_map.md)
for standard (fail-fast) mapping

## Examples

``` r
library(recipes)

rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
  update_role(id, new_role = "id") |>
  step_measure_input_long(transmittance, location = vars(channel)) |>
  prep()

baked_data <- bake(rec, new_data = NULL)

# A function that might fail for some samples
risky_transform <- function(x) {
  if (any(x$value < 0)) stop("Negative values not allowed")
  x$value <- log(x$value)
  x
}

# Errors are captured, not thrown
result <- measure_map_safely(baked_data, risky_transform)

# Check which samples failed
if (nrow(result$errors) > 0) {
  print(result$errors)
}
```
