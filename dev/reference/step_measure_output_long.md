# Reorganize Measurements to Long Format

`step_measure_output_long` creates a *specification* of a recipe

## Usage

``` r
step_measure_output_long(
  recipe,
  values_to = ".measure",
  location_to = ".location",
  measures = NULL,
  role = "predictor",
  trained = FALSE,
  skip = FALSE,
  id = rand_id("measure_output_long")
)
```

## Arguments

- recipe:

  A recipe object. The step will be added to the sequence of operations
  for this recipe.

- values_to:

  A single character string for the column containing the analytical
  measurement.

- location_to:

  A single character string for the column name prefix for location
  columns. For 1D data, this becomes the column name (default:
  `.location`). For nD data, this becomes a prefix with dimension
  suffixes (e.g., `.location_1`, `.location_2`).

- measures:

  An optional single character string specifying which measure column to
  output. If `NULL` (the default) and only one measure column exists,
  that column will be used. If multiple measure columns exist and
  `measures` is `NULL`, an error will be thrown prompting you to specify
  which column to output.

- role:

  Not used by this step since no new variables are created.

- trained:

  A logical to indicate if the quantities for preprocessing have been
  estimated.

- skip:

  A logical. Should the step be skipped when the recipe is baked by
  [`bake()`](https://recipes.tidymodels.org/reference/bake.html)? While
  all operations are baked when
  [`prep()`](https://recipes.tidymodels.org/reference/prep.html) is run,
  some operations may not be able to be conducted on new data (e.g.
  processing the outcome variable(s)). Care should be taken when using
  `skip = TRUE` as it may affect the computations for subsequent
  operations.

- id:

  A character string that is unique to this step to identify it.

## Details

step that converts measures to a format with columns for the measurement
and the corresponding location (i.e., "long" format).

This step is designed convert analytical measurements from their
internal data structure to a long format with explicit location columns.

For 1D data, the output has two columns: the measurement value and a
single location column.

For n-dimensional data (2D, 3D, etc.), the output has n+1 columns: the
measurement value and n location columns named with the `location_to`
prefix followed by dimension numbers (e.g., `.location_1`,
`.location_2`).

## See also

Other input/output steps:
[`step_measure_input_long()`](https://jameshwade.github.io/measure/dev/reference/step_measure_input_long.md),
[`step_measure_input_wide()`](https://jameshwade.github.io/measure/dev/reference/step_measure_input_wide.md),
[`step_measure_output_wide()`](https://jameshwade.github.io/measure/dev/reference/step_measure_output_wide.md)

## Examples

``` r
library(dplyr)

data(glucose_bioreactors)
bioreactors_small$batch_sample <- NULL

small_tr <- bioreactors_small[1:200, ]
small_te <- bioreactors_small[201:210, ]

small_rec <-
  recipe(glucose ~ ., data = small_tr) |>
  update_role(batch_id, day, new_role = "id columns") |>
  step_measure_input_wide(`400`:`3050`) |>
  prep()

# Before reformatting:

small_rec |> bake(new_data = small_te)
#> # A tibble: 10 × 4
#>    batch_id   day glucose   .measures
#>    <chr>    <int>   <dbl>      <meas>
#>  1 S_15         5    2.04 [2,651 × 2]
#>  2 S_15         6    5.56 [2,651 × 2]
#>  3 S_15         7    4.65 [2,651 × 2]
#>  4 S_15         8    9.91 [2,651 × 2]
#>  5 S_15         9    4.96 [2,651 × 2]
#>  6 S_15        10    6.78 [2,651 × 2]
#>  7 S_15        11    6.72 [2,651 × 2]
#>  8 S_15        12    4.69 [2,651 × 2]
#>  9 S_15        13    6.30 [2,651 × 2]
#> 10 S_15        14    3.10 [2,651 × 2]

# After reformatting:

output_rec <-
  small_rec |>
  step_measure_output_long() |>
  prep()

output_rec |> bake(new_data = small_te)
#> # A tibble: 26,510 × 5
#>    batch_id   day glucose .location .measure
#>    <chr>    <int>   <dbl>     <dbl>    <dbl>
#>  1 S_15         5    2.04         1  760094.
#>  2 S_15         5    2.04         2  779053.
#>  3 S_15         5    2.04         3  797154.
#>  4 S_15         5    2.04         4  817226.
#>  5 S_15         5    2.04         5  832725.
#>  6 S_15         5    2.04         6  840075.
#>  7 S_15         5    2.04         7  841721.
#>  8 S_15         5    2.04         8  832112.
#>  9 S_15         5    2.04         9  819494.
#> 10 S_15         5    2.04        10  799601.
#> # ℹ 26,500 more rows
```
