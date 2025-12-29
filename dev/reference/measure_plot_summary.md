# Plot Summary Statistics for Measure Data

Create a summary plot showing mean +/- standard deviation across all
samples at each measurement location.

## Usage

``` r
measure_plot_summary(data, measure_col = NULL, show_range = FALSE)
```

## Arguments

- data:

  A data frame with a measure column (`.measures`).

- measure_col:

  Name of the measure column. If NULL, auto-detected.

- show_range:

  Logical. If TRUE, also show min/max range. Default FALSE.

## Value

A ggplot2 object.

## Examples

``` r
if (FALSE) { # \dontrun{
rec <- recipe(water ~ ., data = meats_long) |>
  step_measure_input_long(transmittance, location = vars(channel)) |>
  step_measure_snv() |>
  prep()

baked <- bake(rec, new_data = NULL)
measure_plot_summary(baked)
} # }
```
