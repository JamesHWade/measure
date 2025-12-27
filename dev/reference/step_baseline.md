# Fit and subtract a baseline from a measurement signal

Fit and subtract a baseline from a measurement signal

## Usage

``` r
step_baseline(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  options = NULL,
  skip = FALSE,
  id = recipes::rand_id("measure")
)
```

## Arguments

- recipe:

  A recipe object. The step will be added to the sequence of operations
  for this recipe.

- ...:

  One or more selector functions to choose variables for this step.

- role:

  Assign the role of new variables.

- trained:

  A logical to indicate if the quantities for preprocessing have been
  estimated.

- options:

  A list of options to the default method for
  [`stats::prcomp()`](https://rdrr.io/r/stats/prcomp.html). Argument
  defaults are set to `retx = FALSE`, `center = FALSE`,
  `scale. = FALSE`, and `tol = NULL`. **Note** that the argument `x`
  should not be passed here (or at all).

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
