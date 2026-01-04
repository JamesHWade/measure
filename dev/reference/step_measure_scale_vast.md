# VAST Scaling (Variable Stability Scaling)

`step_measure_scale_vast()` creates a *specification* of a recipe step
that applies VAST (Variable Stability) scaling at each measurement
location. This focuses on variables with high stability (low coefficient
of variation).

## Usage

``` r
step_measure_scale_vast(
  recipe,
  measures = NULL,
  role = NA,
  trained = FALSE,
  learned_params = NULL,
  skip = FALSE,
  id = recipes::rand_id("measure_scale_vast")
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

- learned_params:

  A named list containing learned means and locations for each measure
  column. This is `NULL` until the step is trained.

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

VAST scaling divides by the product of the standard deviation and the
coefficient of variation (CV = SD/mean). This gives more weight to
variables that are stable across samples (low CV).

For a data matrix \\X\\, the transformation is:

\$\$X\_{scaled} = \frac{X - \bar{X}}{s_X \cdot CV_X}\$\$

where \\\bar{X}\\, \\s_X\\, and \\CV_X = s_X / \|\bar{X}\|\\ are
computed from the training data.

If a column has zero divisor (constant values or zero mean), that column
is only centered, not scaled.

The means, standard deviations, and CVs are learned during
[`prep()`](https://recipes.tidymodels.org/reference/prep.html) from the
training data and stored for use when applying the transformation to new
data during
[`bake()`](https://recipes.tidymodels.org/reference/bake.html).

**No selectors should be supplied to this step function**. The data
should be in the internal format produced by
[`step_measure_input_wide()`](https://jameshwade.github.io/measure/dev/reference/step_measure_input_wide.md)
or
[`step_measure_input_long()`](https://jameshwade.github.io/measure/dev/reference/step_measure_input_long.md).

## References

van den Berg, R.A., Hoefsloot, H.C., Westerhuis, J.A., Smilde, A.K., and
van der Werf, M.J. 2006. Centering, scaling, and transformations:
improving the biological information content of metabolomics data. BMC
Genomics, 7:142.

## See also

[`step_measure_scale_auto()`](https://jameshwade.github.io/measure/dev/reference/step_measure_scale_auto.md),
[`step_measure_scale_pareto()`](https://jameshwade.github.io/measure/dev/reference/step_measure_scale_pareto.md)

Other measure-scaling:
[`step_measure_center()`](https://jameshwade.github.io/measure/dev/reference/step_measure_center.md),
[`step_measure_scale_auto()`](https://jameshwade.github.io/measure/dev/reference/step_measure_scale_auto.md),
[`step_measure_scale_pareto()`](https://jameshwade.github.io/measure/dev/reference/step_measure_scale_pareto.md),
[`step_measure_scale_range()`](https://jameshwade.github.io/measure/dev/reference/step_measure_scale_range.md)

## Examples

``` r
library(recipes)

rec <-
  recipe(water + fat + protein ~ ., data = meats_long) |>
  update_role(id, new_role = "id") |>
  step_measure_input_long(transmittance, location = vars(channel)) |>
  step_measure_scale_vast() |>
  prep()

bake(rec, new_data = NULL)
#> # A tibble: 215 × 6
#>       id water   fat protein .measures channel    
#>    <int> <dbl> <dbl>   <dbl>    <meas> <list>     
#>  1     1  60.5  22.5    16.7 [100 × 2] <int [100]>
#>  2     2  46    40.1    13.5 [100 × 2] <int [100]>
#>  3     3  71     8.4    20.5 [100 × 2] <int [100]>
#>  4     4  72.8   5.9    20.7 [100 × 2] <int [100]>
#>  5     5  58.3  25.5    15.5 [100 × 2] <int [100]>
#>  6     6  44    42.7    13.7 [100 × 2] <int [100]>
#>  7     7  44    42.7    13.7 [100 × 2] <int [100]>
#>  8     8  69.3  10.6    19.3 [100 × 2] <int [100]>
#>  9     9  61.4  19.9    17.7 [100 × 2] <int [100]>
#> 10    10  61.4  19.9    17.7 [100 × 2] <int [100]>
#> # ℹ 205 more rows
```
