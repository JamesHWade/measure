# Ingest Measurements from a Single Column

`step_measure_input_long` creates a *specification* of a recipe step
that converts measures organized in a column for the analytical results
(and one or more columns of numeric indices) into an internal format
used by the package.

## Usage

``` r
step_measure_input_long(
  recipe,
  ...,
  location,
  col_name = ".measures",
  dim_names = NULL,
  dim_units = NULL,
  pad = FALSE,
  role = "measure",
  trained = FALSE,
  columns = NULL,
  skip = FALSE,
  id = rand_id("measure_input_long")
)
```

## Arguments

- recipe:

  A recipe object. The step will be added to the sequence of operations
  for this recipe.

- ...:

  One or more selector functions to choose which *single* column
  contains the analytical measurements. The selection should be in the
  order of the measurement's profile.

- location:

  One or more selector functions to choose which column(s) have the
  locations of the analytical values. For 1D data (spectra,
  chromatograms), select a single location column. For 2D or higher
  dimensional data (LC-DAD, 2D NMR, EEM), select multiple location
  columns. Columns will be renamed to `location_1`, `location_2`, etc.
  in order.

- col_name:

  A single character string specifying the name of the output column
  that will contain the measure data. Defaults to `".measures"`. Use
  different names when creating multiple measure columns (e.g.,
  `".uv_spectrum"` and `".ms_spectrum"`).

- dim_names:

  Optional character vector of semantic names for each dimension (e.g.,
  `c("retention_time", "wavelength")`). Only used for multi-dimensional
  data.

- dim_units:

  Optional character vector of units for each dimension (e.g.,
  `c("min", "nm")`). Only used for multi-dimensional data.

- pad:

  Whether to pad the measurements to ensure that they all have the same
  number of values. This is useful when there are missing values in the
  measurements.

- role:

  Not used by this step since no new variables are created.

- trained:

  A logical to indicate if the quantities for preprocessing have been
  estimated.

- columns:

  A character vector of column names determined by the recipe.

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

This step is designed for data in a format where there is a column for
the analytical measurement (e.g., absorption, etc.) and one or more
columns with the location of the value (e.g., wave number, retention
time, wavelength, etc.).

`step_measure_input_long()` will collect those data and put them into a
format used internally by this package. The data structure has a row for
each independent experimental unit and a nested tibble with that
sample's measure (measurement and location). It assumes that there are
unique combinations of the other columns in the data that define
individual patterns associated with the pattern. If this is not the
case, the special values might be inappropriately restructured.

The best advice is to have a column of any type that indicates the
unique sample number for each measure. For example, if there are 200
values in the measure and 7 samples, the input data (in long format)
should have 1,400 rows. We advise having a column with 7 unique values
indicating which of the rows correspond to each sample.

## Multi-Dimensional Data

For 2D or higher dimensional data, provide multiple location columns:

    # LC-DAD data with retention time and wavelength
    step_measure_input_long(
      absorbance,
      location = vars(retention_time, wavelength),
      dim_names = c("time", "wavelength"),
      dim_units = c("min", "nm")
    )

The result will be a `measure_nd_list` column instead of a
`measure_list`.

## Missing Data

Currently, measure assumes that there are equal numbers of values within
a sample. If there are missing values in the measurements, you'll need
to pad them with missing values (as opposed to an absent row in the long
format). If not, an error will occur.

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/reference/tidy.recipe.html)
this step, a tibble indicating which of the original columns were used
to reformat the data.

## See also

Other input/output steps:
[`step_measure_input_wide()`](https://jameshwade.github.io/measure/dev/reference/step_measure_input_wide.md),
[`step_measure_output_long()`](https://jameshwade.github.io/measure/dev/reference/step_measure_output_long.md),
[`step_measure_output_wide()`](https://jameshwade.github.io/measure/dev/reference/step_measure_output_wide.md)

## Examples

``` r
library(recipes)

# 1D data (traditional usage)
rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
  update_role(id, new_role = "id") |>
  step_measure_input_long(transmittance, location = vars(channel)) |>
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
