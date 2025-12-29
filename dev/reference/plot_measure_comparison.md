# Compare Multiple Preprocessing Recipes

Visualize the effect of different preprocessing recipes side-by-side.
Useful for comparing different parameter settings or preprocessing
strategies.

## Usage

``` r
plot_measure_comparison(..., data = NULL, n_samples = 5, summary_only = FALSE)
```

## Arguments

- ...:

  Named recipe objects to compare. Each must be a prepped recipe.

- data:

  Data to apply recipes to. If NULL, uses the training data from the
  first recipe.

- n_samples:

  Number of samples to show. Default 5.

- summary_only:

  If TRUE, only show summary statistics (mean +/- SD). Default FALSE
  shows individual spectra.

## Value

A ggplot2 object with faceted comparison.

## Examples

``` r
if (FALSE) { # \dontrun{
library(recipes)
library(ggplot2)

# Compare SNV vs MSC preprocessing
base_rec <- recipe(water ~ ., data = meats_long) |>
  update_role(id, new_role = "id") |>
  step_measure_input_long(transmittance, location = vars(channel))

snv_rec <- base_rec |>
  step_measure_snv() |>
  prep()

msc_rec <- base_rec |>
  step_measure_msc() |>
  prep()

plot_measure_comparison(
  "SNV" = snv_rec,
  "MSC" = msc_rec,
  n_samples = 10
)
} # }
```
