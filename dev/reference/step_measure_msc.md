# Multiplicative Scatter Correction (MSC)

`step_measure_msc()` creates a *specification* of a recipe step that
applies Multiplicative Scatter Correction to spectral data. MSC removes
physical light scatter by accounting for additive and multiplicative
effects.

## Usage

``` r
step_measure_msc(
  recipe,
  measures = NULL,
  role = NA,
  trained = FALSE,
  ref_spectra = NULL,
  skip = FALSE,
  id = recipes::rand_id("measure_msc")
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

- ref_spectra:

  A named list of numeric vectors containing the reference spectra
  computed during training for each measure column. This is `NULL` until
  the step is trained.

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

Multiplicative Scatter Correction (MSC) is a normalization method that
attempts to account for additive and multiplicative effects by aligning
each spectrum to a reference spectrum. For a spectrum \\x_i\\ and
reference \\x_r\\, the transformation is:

\$\$x_i = m_i \cdot x_r + a_i\$\$ \$\$MSC(x_i) = \frac{x_i -
a_i}{m_i}\$\$

where \\a_i\\ and \\m_i\\ are the additive (intercept) and
multiplicative (slope) terms from regressing \\x_i\\ on \\x_r\\.

The reference spectrum is computed as the mean of all training spectra
during [`prep()`](https://recipes.tidymodels.org/reference/prep.html)
and stored for use when applying the transformation to new data.

MSC is commonly used to remove physical light scatter effects in NIR
spectroscopy caused by differences in particle size or path length.

**No selectors should be supplied to this step function**. The data
should be in the internal format produced by
[`step_measure_input_wide()`](https://jameshwade.github.io/measure/dev/reference/step_measure_input_wide.md)
or
[`step_measure_input_long()`](https://jameshwade.github.io/measure/dev/reference/step_measure_input_long.md).

The measurement locations are preserved unchanged.

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/reference/tidy.recipe.html)
this step, a tibble with columns `terms` (set to `".measures"`) and `id`
is returned.

## References

Geladi, P., MacDougall, D., and Martens, H. 1985. Linearization and
Scatter-Correction for Near-Infrared Reflectance Spectra of Meat.
Applied Spectroscopy, 39(3):491-500.

## See also

[`step_measure_snv()`](https://jameshwade.github.io/measure/dev/reference/step_measure_snv.md)
for a simpler scatter correction method

Other measure-preprocessing:
[`step_measure_calibrate_x()`](https://jameshwade.github.io/measure/dev/reference/step_measure_calibrate_x.md),
[`step_measure_calibrate_y()`](https://jameshwade.github.io/measure/dev/reference/step_measure_calibrate_y.md),
[`step_measure_map()`](https://jameshwade.github.io/measure/dev/reference/step_measure_map.md),
[`step_measure_normalize_istd()`](https://jameshwade.github.io/measure/dev/reference/step_measure_normalize_istd.md),
[`step_measure_ratio_reference()`](https://jameshwade.github.io/measure/dev/reference/step_measure_ratio_reference.md),
[`step_measure_snv()`](https://jameshwade.github.io/measure/dev/reference/step_measure_snv.md),
[`step_measure_subtract_blank()`](https://jameshwade.github.io/measure/dev/reference/step_measure_subtract_blank.md),
[`step_measure_subtract_reference()`](https://jameshwade.github.io/measure/dev/reference/step_measure_subtract_reference.md)

## Examples

``` r
library(recipes)

rec <-
  recipe(water + fat + protein ~ ., data = meats_long) |>
  update_role(id, new_role = "id") |>
  step_measure_input_long(transmittance, location = vars(channel)) |>
  step_measure_msc() |>
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
