# MCR-ALS Decomposition for Multi-Dimensional Data

`step_measure_mcr_als()` creates a *specification* of a recipe step that
applies Multivariate Curve Resolution - Alternating Least Squares
(MCR-ALS) to multi-dimensional measurement data.

## Usage

``` r
step_measure_mcr_als(
  recipe,
  ...,
  n_components = 3L,
  max_iter = 500L,
  tol = 1e-06,
  non_negativity = TRUE,
  unimodality = FALSE,
  prefix = "mcr_",
  role = "predictor",
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_mcr_als")
)
```

## Arguments

- recipe:

  A recipe object.

- ...:

  One or more selector functions to choose measure columns. If empty,
  all nD measure columns are used.

- n_components:

  Number of components to extract. Default is 3.

- max_iter:

  Maximum number of iterations. Default is 500.

- tol:

  Convergence tolerance. Default is 1e-6.

- non_negativity:

  Logical. Should non-negativity constraints be applied? Default is
  `TRUE`.

- unimodality:

  Logical. Should unimodality constraints be applied? Default is
  `FALSE`.

- prefix:

  Prefix for output column names. Default is `"mcr_"`.

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

MCR-ALS is a powerful technique for resolving mixtures into pure
component contributions. It's particularly useful for:

- Chromatographic data (time x wavelength)

- Spectroscopic mixtures

- Process analytical data

Unlike PARAFAC, MCR-ALS is a bilinear method that accepts exactly two
measurement axes. The sample axis is unfolded alongside the first
measurement axis. It allows flexible constraints like non-negativity and
unimodality, but a three-axis measurement must use PARAFAC or Tucker.

### Experimental Status

This step is experimental and its API may change in future versions.

### Requirements

- Input must be `measure_nd_list` with exactly 2 measurement dimensions.

- All training samples and held-out samples must use the same coordinate
  levels on both axes. Rows may be in any order; the coordinates, rather
  than row positions, determine the array cells.

- The matrix factorization requires complete numeric values on that
  grid. Align and fill measurements upstream when needed.

## Note

This is an **experimental** feature. The implementation uses a simple
ALS algorithm without advanced constraints. For production use, consider
using dedicated MCR-ALS packages.

## See also

[`step_measure_parafac()`](https://jameshwade.github.io/measure/dev/reference/step_measure_parafac.md)
for PARAFAC decomposition

Other measure-multiway:
[`step_measure_parafac()`](https://jameshwade.github.io/measure/dev/reference/step_measure_parafac.md),
[`step_measure_tucker()`](https://jameshwade.github.io/measure/dev/reference/step_measure_tucker.md)

## Examples

``` r
if (FALSE) { # \dontrun{
library(recipes)

# After ingesting chromatographic data
rec <- recipe(concentration ~ ., data = chrom_data) |>
  step_measure_input_long(
    absorbance,
    location = vars(time, wavelength)
  ) |>
  step_measure_mcr_als(n_components = 3) |>
  prep()

bake(rec, new_data = NULL)
} # }
```
