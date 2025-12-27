# Normalize by Maximum Value

`step_measure_normalize_max()` creates a *specification* of a recipe
step that divides each spectrum by its maximum value. This is useful for
peak-focused analysis where you want the highest peak to equal 1.

## Usage

``` r
step_measure_normalize_max(
  recipe,
  measures = NULL,
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_normalize_max")
)
```

## Arguments

- recipe:

  A recipe object. The step will be added to the sequence of operations
  for this recipe.

- measures:

  An optional character vector of measure column names to process. If
  `NULL` (the default), all measure columns (columns with class
  `measure_list`) will be processed. Use this to limit processing to
  specific measure columns when working with multiple measurement types.

- role:

  Not used by this step since no new variables are created.

- trained:

  A logical to indicate if the quantities for preprocessing have been
  estimated.

- skip:

  A logical. Should the step be skipped when the recipe is baked by
  [`recipes::bake()`](https://recipes.tidymodels.org/reference/bake.html)?
  While all operations are baked when
  [`recipes::prep()`](https://recipes.tidymodels.org/reference/prep.html)
  is run, some operations may not be able to be conducted on new data
  (e.g. processing the outcome variable(s)). Care should be taken when
  using `skip = TRUE` as it may affect the computations for subsequent
  operations.

- id:

  A character string that is unique to this step to identify it.

## Value

An updated version of `recipe` with the new step added to the sequence
of any existing operations.

## Details

For each spectrum \\x\\, the transformation is:

\$\$x\_{norm} = \frac{x}{\max(x)}\$\$

After transformation, the maximum value of each spectrum will equal 1.

If the maximum is zero or NA, a warning is issued and the original
values are returned unchanged.

**No selectors should be supplied to this step function**. The data
should be in the internal format produced by
[`step_measure_input_wide()`](https://jameshwade.github.io/measure/dev/reference/step_measure_input_wide.md)
or
[`step_measure_input_long()`](https://jameshwade.github.io/measure/dev/reference/step_measure_input_long.md).

## See also

[`step_measure_normalize_sum()`](https://jameshwade.github.io/measure/dev/reference/step_measure_normalize_sum.md),
[`step_measure_normalize_range()`](https://jameshwade.github.io/measure/dev/reference/step_measure_normalize_range.md)

Other measure-normalization:
[`step_measure_normalize_auc()`](https://jameshwade.github.io/measure/dev/reference/step_measure_normalize_auc.md),
[`step_measure_normalize_peak()`](https://jameshwade.github.io/measure/dev/reference/step_measure_normalize_peak.md),
[`step_measure_normalize_range()`](https://jameshwade.github.io/measure/dev/reference/step_measure_normalize_range.md),
[`step_measure_normalize_sum()`](https://jameshwade.github.io/measure/dev/reference/step_measure_normalize_sum.md),
[`step_measure_normalize_vector()`](https://jameshwade.github.io/measure/dev/reference/step_measure_normalize_vector.md)

## Examples

``` r
library(recipes)

rec <-
  recipe(water + fat + protein ~ ., data = meats_long) |>
  update_role(id, new_role = "id") |>
  step_measure_input_long(transmittance, location = vars(channel)) |>
  step_measure_normalize_max() |>
  prep()

bake(rec, new_data = NULL)
#> # A tibble: 215 × 5
#>       id water   fat protein .measures
#>    <int> <dbl> <dbl>   <dbl>    <meas>
#>  1     1  60.5  22.5    16.7 [100 × 2]
#>  2     2  46    40.1    13.5 [100 × 2]
#>  3     3  71     8.4    20.5 [100 × 2]
#>  4     4  72.8   5.9    20.7 [100 × 2]
#>  5     5  58.3  25.5    15.5 [100 × 2]
#>  6     6  44    42.7    13.7 [100 × 2]
#>  7     7  44    42.7    13.7 [100 × 2]
#>  8     8  69.3  10.6    19.3 [100 × 2]
#>  9     9  61.4  19.9    17.7 [100 × 2]
#> 10    10  61.4  19.9    17.7 [100 × 2]
#> # ℹ 205 more rows
```
