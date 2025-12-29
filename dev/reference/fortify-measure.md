# Convert Measure Objects to Data Frames for Plotting

These methods convert measure objects to data frames suitable for use
with ggplot2.

## Usage

``` r
# S3 method for class 'measure_tbl'
fortify(model, data = NULL, ...)

# S3 method for class 'measure_list'
fortify(model, data = NULL, ...)
```

## Arguments

- model:

  A `measure_tbl` or `measure_list` object.

- data:

  Ignored. Present for compatibility with generic.

- ...:

  Additional arguments (currently unused).

## Value

A tibble with columns `location` and `value` (for `measure_tbl`) or
`location`, `value`, and `sample` (for `measure_list`).

## Examples

``` r
if (FALSE) { # \dontrun{
library(ggplot2)

# Single spectrum
spec <- new_measure_tbl(location = 1:100, value = rnorm(100))
ggplot(fortify(spec), aes(location, value)) + geom_line()

# Multiple spectra (from recipe output)
rec <- recipe(water ~ ., data = meats_long) |>
  step_measure_input_long(transmittance, location = vars(channel)) |>
  prep()
baked <- bake(rec, new_data = NULL)
ggplot(fortify(baked$.measures), aes(location, value, group = sample)) +
  geom_line(alpha = 0.5)
} # }
```
