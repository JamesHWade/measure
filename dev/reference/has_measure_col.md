# Check if data frame has measure column(s)

Checks whether a data frame contains at least one measure column. This
is the recommended way to validate data in step functions.

## Usage

``` r
has_measure_col(data)
```

## Arguments

- data:

  A data frame.

## Value

Invisibly returns the names of measure columns found. Throws an error if
no measure columns are found.

## Examples

``` r
library(recipes)

rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
  update_role(id, new_role = "id") |>
  step_measure_input_long(transmittance, location = vars(channel)) |>
  prep()

result <- bake(rec, new_data = NULL)
has_measure_col(result)  # TRUE (returns invisibly)
```
