# Reorganize Measurements to Separate Columns

`step_measure_output_wide` creates a *specification* of a recipe step
that converts measures to multiple columns (i.e., "wide" format).

## Usage

``` r
step_measure_output_wide(
  recipe,
  prefix = "measure_",
  measures = NULL,
  role = "predictor",
  trained = FALSE,
  skip = FALSE,
  id = rand_id("measure_output_wide")
)
```

## Arguments

- recipe:

  A recipe object. The step will be added to the sequence of operations
  for this recipe.

- prefix:

  A character string used to name the new columns.

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

This step is designed convert analytical measurements from their
internal data structure to separate columns.

Wide outputs can be helpful when you want to use standard recipes steps
with the measuresments, such as
[`recipes::step_pca()`](https://recipes.tidymodels.org/reference/step_pca.html),
[`recipes::step_pls()`](https://recipes.tidymodels.org/reference/step_pls.html),
and so on.

## See also

Other input/output steps:
[`step_measure_input_long()`](https://jameshwade.github.io/measure/dev/reference/step_measure_input_long.md),
[`step_measure_input_wide()`](https://jameshwade.github.io/measure/dev/reference/step_measure_input_wide.md),
[`step_measure_output_long()`](https://jameshwade.github.io/measure/dev/reference/step_measure_output_long.md)

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
  step_measure_output_wide() |>
  prep()

output_rec |> bake(new_data = small_te)
#> # A tibble: 10 × 2,654
#>    batch_id   day glucose measure_0001 measure_0002 measure_0003 measure_0004
#>    <chr>    <int>   <dbl>        <dbl>        <dbl>        <dbl>        <dbl>
#>  1 S_15         5    2.04      760094.      779053.      797154.      817226.
#>  2 S_15         6    5.56      854812.      874105.      893653.      912190.
#>  3 S_15         7    4.65      961786.      979449.     1000054.     1019400.
#>  4 S_15         8    9.91     1092681.     1110867.     1132371.     1151944.
#>  5 S_15         9    4.96     1243318.     1259961.     1281630.     1301011.
#>  6 S_15        10    6.78     1500414.     1517867.     1536436.     1557016.
#>  7 S_15        11    6.72     1818328.     1836446.     1856983.     1876906.
#>  8 S_15        12    4.69     2035750.     2053903.     2073901.     2093514.
#>  9 S_15        13    6.30     2192191.     2211502.     2230969.     2251089.
#> 10 S_15        14    3.10     2353638.     2371877.     2392865.     2412822.
#> # ℹ 2,647 more variables: measure_0005 <dbl>, measure_0006 <dbl>,
#> #   measure_0007 <dbl>, measure_0008 <dbl>, measure_0009 <dbl>,
#> #   measure_0010 <dbl>, measure_0011 <dbl>, measure_0012 <dbl>,
#> #   measure_0013 <dbl>, measure_0014 <dbl>, measure_0015 <dbl>,
#> #   measure_0016 <dbl>, measure_0017 <dbl>, measure_0018 <dbl>,
#> #   measure_0019 <dbl>, measure_0020 <dbl>, measure_0021 <dbl>,
#> #   measure_0022 <dbl>, measure_0023 <dbl>, measure_0024 <dbl>, …
```
