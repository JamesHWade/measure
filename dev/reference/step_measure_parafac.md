# PARAFAC Decomposition for Multi-Dimensional Data

`step_measure_parafac()` creates a *specification* of a recipe step that
applies Parallel Factor Analysis (PARAFAC) to multi-dimensional
measurement data, extracting component scores as features for modeling.

## Usage

``` r
step_measure_parafac(
  recipe,
  ...,
  n_components = 3L,
  center = TRUE,
  scale = FALSE,
  max_iter = 500L,
  tol = 1e-06,
  prefix = "parafac_",
  role = "predictor",
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_parafac")
)
```

## Arguments

- recipe:

  A recipe object.

- ...:

  One or more selector functions to choose measure columns. If empty,
  all nD measure columns are used.

- n_components:

  Number of PARAFAC components to extract. Default is 3.

- center:

  Logical. Should data be centered before decomposition? Default is
  `TRUE`.

- scale:

  Logical. Should data be scaled before decomposition? Default is
  `FALSE`.

- max_iter:

  Maximum number of iterations. Default is 500.

- tol:

  Convergence tolerance. Default is 1e-6.

- prefix:

  Prefix for output column names. Default is `"parafac_"`.

- role:

  Not used.

- trained:

  Logical indicating if the step has been trained.

- skip:

  Logical. Should the step be skipped when baking?

- id:

  Unique step identifier.

## Value

An updated recipe with the new step added.

## Details

PARAFAC (also known as CANDECOMP/PARAFAC or CP decomposition) decomposes
a three-way or higher array into a sum of rank-one tensors. For
measurement data like EEM (excitation-emission matrices) or LC-DAD, this
extracts interpretable components corresponding to underlying chemical
species.

### Requirements

- Input must be `measure_nd_list` with 2+ dimensions

- All samples must have the same grid (regular, aligned)

- The `multiway` package must be installed (in Suggests)

### Output

Creates numeric feature columns: `parafac_1`, `parafac_2`, ...,
`parafac_n` representing each sample's scores on the extracted
components.

## Note

This step requires the `multiway` package. Install with:
`install.packages("multiway")`

## See also

[`step_measure_tucker()`](https://jameshwade.github.io/measure/dev/reference/step_measure_tucker.md)
for Tucker decomposition

Other measure-multiway:
[`step_measure_mcr_als()`](https://jameshwade.github.io/measure/dev/reference/step_measure_mcr_als.md),
[`step_measure_tucker()`](https://jameshwade.github.io/measure/dev/reference/step_measure_tucker.md)

## Examples

``` r
if (FALSE) { # \dontrun{
library(recipes)

# After ingesting EEM data as 2D measurements
rec <- recipe(concentration ~ ., data = eem_data) |>
  step_measure_input_long(
    fluorescence,
    location = vars(excitation, emission)
  ) |>
  step_measure_parafac(n_components = 3) |>
  prep()

bake(rec, new_data = NULL)
} # }
```
