# Find measure columns in a data frame

Finds all columns in a data frame that contain measurement data (i.e.,
are of class `measure_list`).

## Usage

``` r
find_measure_cols(data)
```

## Arguments

- data:

  A data frame.

## Value

Character vector of column names containing measure data. Returns empty
character vector if no measure columns found.

## Examples

``` r
library(recipes)

rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
  update_role(id, new_role = "id") |>
  step_measure_input_long(transmittance, location = vars(channel)) |>
  prep()

result <- bake(rec, new_data = NULL)
find_measure_cols(result)  # ".measures"
#> [1] ".measures"
```
