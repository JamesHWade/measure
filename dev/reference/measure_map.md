# Apply a Function to Each Sample's Measurements

`measure_map()` applies a function to each sample's measurement data.
This function is intended for **exploration and prototyping**, not for
production pipelines. For reproducible preprocessing, use
[`step_measure_map()`](https://jameshwade.github.io/measure/dev/reference/step_measure_map.md)
instead.

## Usage

``` r
measure_map(
  .data,
  .f,
  .cols = NULL,
  ...,
  verbosity = 1L,
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

- verbosity:

  An integer controlling output verbosity:

  - `0`: Silent - suppress all messages and output from `.f`

  - `1`: Normal (default) - show output from `.f`

- .error_call:

  The execution environment for error reporting.

## Value

A data frame with the specified measure columns transformed.

## Details

### Intended Use: Exploration, Not Production

This function is designed for interactive exploration and debugging:

    # Good: Prototyping a new transformation
    baked_data |>
      measure_map(~ { .x$value <- my_experimental_fn(.x$value); .x })

    # Better: Once it works, put it in a recipe step
    recipe(...) |>
      step_measure_map(my_experimental_fn) |>
      prep()

Unlike recipe steps, transformations applied with `measure_map()` are
NOT:

- Automatically applied to new data

- Bundled into workflows

- Reproducible across sessions

### Function Requirements

The function `.f` must:

- Accept a tibble with `location` and `value` columns

- Return a tibble with `location` and `value` columns

- Not change the number of rows

## See also

- [`step_measure_map()`](https://jameshwade.github.io/measure/dev/reference/step_measure_map.md)
  for production use in recipe pipelines

- [`measure_map_safely()`](https://jameshwade.github.io/measure/dev/reference/measure_map_safely.md)
  for fault-tolerant exploration

- [`measure_summarize()`](https://jameshwade.github.io/measure/dev/reference/measure_summarize.md)
  for computing summary statistics

## Examples

``` r
library(recipes)

# First, get data in internal format
rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
  update_role(id, new_role = "id") |>
  step_measure_input_long(transmittance, location = vars(channel)) |>
  prep()

baked_data <- bake(rec, new_data = NULL)

# Explore a custom transformation
result <- measure_map(baked_data, ~ {
  # Subtract the minimum value from each spectrum
  .x$value <- .x$value - min(.x$value)
  .x
})

# Once you're happy with it, use step_measure_map() in your recipe:
# recipe(...) |>
#   step_measure_map(~ { .x$value <- .x$value - min(.x$value); .x })
```
