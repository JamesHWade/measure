# Tucker Decomposition for Multi-Dimensional Data

`step_measure_tucker()` creates a *specification* of a recipe step that
applies Tucker decomposition to multi-dimensional measurement data,
extracting component scores as features for modeling.

## Usage

``` r
step_measure_tucker(
  recipe,
  ...,
  ranks = 3L,
  center = TRUE,
  scale = FALSE,
  max_iter = 500L,
  tol = 1e-06,
  prefix = "tucker_",
  role = "predictor",
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_tucker")
)
```

## Arguments

- recipe:

  A recipe object.

- ...:

  One or more selector functions to choose measure columns. If empty,
  all nD measure columns are used.

- ranks:

  A vector of ranks for each mode. If a single integer, the same rank is
  used for all modes. Default is 3.

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

  Prefix for output column names. Default is `"tucker_"`.

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

Tucker decomposition (also known as higher-order SVD or multilinear SVD)
decomposes a tensor into a core tensor multiplied by factor matrices
along each mode. Unlike PARAFAC, Tucker allows different ranks for each
mode, providing more flexibility.

### Requirements

- Input must be `measure_nd_list` with 2+ dimensions

- All samples must have the same grid (regular, aligned)

- The `multiway` package must be installed (in Suggests)

### Output

Creates numeric feature columns representing the unfolded core tensor
scores for each sample.

## Note

This step requires the `multiway` package. Install with:
`install.packages("multiway")`

## See also

[`step_measure_parafac()`](https://jameshwade.github.io/measure/dev/reference/step_measure_parafac.md)
for PARAFAC decomposition

Other measure-multiway:
[`step_measure_mcr_als()`](https://jameshwade.github.io/measure/dev/reference/step_measure_mcr_als.md),
[`step_measure_parafac()`](https://jameshwade.github.io/measure/dev/reference/step_measure_parafac.md)

## Examples

``` r
if (FALSE) { # \dontrun{
library(recipes)

# After ingesting 2D data as nD measurements
rec <- recipe(concentration ~ ., data = lc_dad_data) |>
  step_measure_input_long(
    absorbance,
    location = vars(time, wavelength)
  ) |>
  step_measure_tucker(ranks = c(5, 3)) |>
  prep()

bake(rec, new_data = NULL)
} # }
```
