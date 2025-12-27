# Ingest Measurements in Separate Columns

`step_measure_input_wide` creates a *specification* of a recipe step
that converts measures organized in multiple columns into an internal
format used by the package.

## Usage

``` r
step_measure_input_wide(
  recipe,
  ...,
  role = "measure",
  trained = FALSE,
  columns = NULL,
  location_values = NULL,
  col_name = ".measures",
  skip = FALSE,
  id = rand_id("measure_input_wide")
)
```

## Arguments

- recipe:

  A recipe object. The step will be added to the sequence of operations
  for this recipe.

- ...:

  One or more selector functions to choose variables for this step. See
  [`selections()`](https://recipes.tidymodels.org/reference/selections.html)
  for more details.

- role:

  Not used by this step since no new variables are created.

- trained:

  A logical to indicate if the quantities for preprocessing have been
  estimated.

- columns:

  A character string of the selected variable names. This field is a
  placeholder and will be populated once
  [`recipes::prep()`](https://recipes.tidymodels.org/reference/prep.html)
  is used.

- location_values:

  A numeric vector of values that specify the location of the
  measurements (e.g., wavelength etc.) in the same order as the
  variables selected by `...`. If not specified, a sequence of integers
  (starting at 1L) is used.

- col_name:

  A single character string specifying the name of the output column
  that will contain the measure data. Defaults to `".measures"`. Use
  different names when creating multiple measure columns (e.g.,
  `".uv_spectrum"` and `".ms_spectrum"`).

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

This step is designed for data in a format where the analytical
measurements are in separate columns.

`step_measure_input_wide()` will collect those data and put them into a
format used internally by this package. The data structure has a row for
each independent experimental unit and a nested tibble with that
sample's measure (measurement and location). It assumes that there are
unique combinations of the other columns in the data that define
individual patterns associated with the pattern. If this is not the
case, the special values might be inappropriately restructured.

The best advice is to have a column of any type that indicates the
unique sample number for each measure. For example, if there are 20 rows
in the input data set, the columns that are *not* analytically
measurements show have no duplicate combinations in the 20 rows.

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/reference/tidy.recipe.html)
this step, a tibble indicating which of the original columns were used
to reformat the data.

## See also

Other input/output steps:
[`step_measure_input_long()`](https://jameshwade.github.io/measure/dev/reference/step_measure_input_long.md),
[`step_measure_output_long()`](https://jameshwade.github.io/measure/dev/reference/step_measure_output_long.md),
[`step_measure_output_wide()`](https://jameshwade.github.io/measure/dev/reference/step_measure_output_wide.md)

## Examples

``` r
data(meats, package = "modeldata")

# Outcome data is to the right
names(meats) %>% tail(10)
#>  [1] "x_094"   "x_095"   "x_096"   "x_097"   "x_098"   "x_099"   "x_100"  
#>  [8] "water"   "fat"     "protein"

# ------------------------------------------------------------------------------
# Ingest data without adding the location (i.e. wave number) for the spectra

rec <-
  recipe(water + fat + protein ~ ., data = meats) %>%
  step_measure_input_wide(starts_with("x_")) %>%
  prep()

summary(rec)
#> # A tibble: 4 × 4
#>   variable  type      role    source  
#>   <chr>     <list>    <chr>   <chr>   
#> 1 water     <chr [2]> outcome original
#> 2 fat       <chr [2]> outcome original
#> 3 protein   <chr [2]> outcome original
#> 4 .measures <chr [1]> measure derived 

# ------------------------------------------------------------------------------
# Ingest data without adding the location (i.e. wave number) for the spectra

# Make up some locations for the spectra's x-axis
index <- seq(1, 2, length.out = 100)

rec <-
  recipe(water + fat + protein ~ ., data = meats) %>%
  step_measure_input_wide(starts_with("x_"), location_values = index) %>%
  prep()

summary(rec)
#> # A tibble: 4 × 4
#>   variable  type      role    source  
#>   <chr>     <list>    <chr>   <chr>   
#> 1 water     <chr [2]> outcome original
#> 2 fat       <chr [2]> outcome original
#> 3 protein   <chr [2]> outcome original
#> 4 .measures <chr [1]> measure derived 
```
