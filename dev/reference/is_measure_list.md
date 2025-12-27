# Test if object is a measure list

Test if object is a measure list

## Usage

``` r
is_measure_list(x)
```

## Arguments

- x:

  Object to test.

## Value

Logical indicating if `x` inherits from `measure_list`.

## Examples

``` r
# After using step_measure_input_*, the .measures column is a measure_list
library(recipes)

rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
  update_role(id, new_role = "id") |>
  step_measure_input_long(transmittance, location = vars(channel)) |>
  prep()

result <- bake(rec, new_data = NULL)
is_measure_list(result$.measures)
#> [1] TRUE
```
