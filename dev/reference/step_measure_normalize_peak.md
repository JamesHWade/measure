# Normalize to a Specific Peak Region

`step_measure_normalize_peak()` creates a *specification* of a recipe
step that divides each spectrum by a summary statistic computed from a
specified region. This is commonly used for internal standard
normalization.

## Usage

``` r
step_measure_normalize_peak(
  recipe,
  measures = NULL,
  role = NA,
  trained = FALSE,
  location_min = NULL,
  location_max = NULL,
  method = "mean",
  skip = FALSE,
  id = recipes::rand_id("measure_normalize_peak")
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

- location_min:

  Numeric. The lower bound of the region to use for normalization. This
  parameter is tunable with
  [`peak_location_min()`](https://jameshwade.github.io/measure/dev/reference/peak_location_min.md).

- location_max:

  Numeric. The upper bound of the region to use for normalization. This
  parameter is tunable with
  [`peak_location_max()`](https://jameshwade.github.io/measure/dev/reference/peak_location_min.md).

- method:

  Character. The summary statistic to compute from the region. One of
  `"mean"` (default), `"max"`, or `"integral"`.

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

For each spectrum, this step:

1.  Selects values in the region `[location_min, location_max]`

2.  Computes a summary statistic (mean, max, or integral) from that
    region

3.  Divides the entire spectrum by this value

This is useful when you have an internal standard peak at a known
location and want to normalize all spectra to that peak.

The `location_min` and `location_max` parameters are tunable with
[`peak_location_min()`](https://jameshwade.github.io/measure/dev/reference/peak_location_min.md)
and
[`peak_location_max()`](https://jameshwade.github.io/measure/dev/reference/peak_location_min.md)
for hyperparameter optimization.

If no values fall within the specified region, an error is raised. If
the computed normalizer is zero or NA, a warning is issued and the
original values are returned unchanged.

**No selectors should be supplied to this step function**. The data
should be in the internal format produced by
[`step_measure_input_wide()`](https://jameshwade.github.io/measure/dev/reference/step_measure_input_wide.md)
or
[`step_measure_input_long()`](https://jameshwade.github.io/measure/dev/reference/step_measure_input_long.md).

## See also

[`step_measure_normalize_max()`](https://jameshwade.github.io/measure/dev/reference/step_measure_normalize_max.md),
[`step_measure_normalize_auc()`](https://jameshwade.github.io/measure/dev/reference/step_measure_normalize_auc.md),
[`peak_location_min()`](https://jameshwade.github.io/measure/dev/reference/peak_location_min.md),
[`peak_location_max()`](https://jameshwade.github.io/measure/dev/reference/peak_location_min.md)

Other measure-normalization:
[`step_measure_normalize_auc()`](https://jameshwade.github.io/measure/dev/reference/step_measure_normalize_auc.md),
[`step_measure_normalize_max()`](https://jameshwade.github.io/measure/dev/reference/step_measure_normalize_max.md),
[`step_measure_normalize_range()`](https://jameshwade.github.io/measure/dev/reference/step_measure_normalize_range.md),
[`step_measure_normalize_sum()`](https://jameshwade.github.io/measure/dev/reference/step_measure_normalize_sum.md),
[`step_measure_normalize_vector()`](https://jameshwade.github.io/measure/dev/reference/step_measure_normalize_vector.md)

## Examples

``` r
library(recipes)

# Normalize to mean of region 40-60
rec <-
  recipe(water + fat + protein ~ ., data = meats_long) |>
  update_role(id, new_role = "id") |>
  step_measure_input_long(transmittance, location = vars(channel)) |>
  step_measure_normalize_peak(location_min = 40, location_max = 60) |>
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
