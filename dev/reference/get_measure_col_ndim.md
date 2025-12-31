# Get the dimensionality of a measure column

Returns the number of dimensions (1 for `measure_list`, 2+ for
`measure_nd_list`) of a measure column in a data frame.

## Usage

``` r
get_measure_col_ndim(data, col)
```

## Arguments

- data:

  A data frame.

- col:

  Character string naming the measure column.

## Value

Integer indicating the number of dimensions.

## Examples

``` r
library(recipes)

rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
  update_role(id, new_role = "id") |>
  step_measure_input_long(transmittance, location = vars(channel)) |>
  prep()

result <- bake(rec, new_data = NULL)
get_measure_col_ndim(result, ".measures")  # 1
#> [1] 1
```
