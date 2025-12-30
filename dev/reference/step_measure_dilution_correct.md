# Dilution Factor Correction

`step_measure_dilution_correct()` creates a *specification* of a recipe
step that corrects concentration values by applying dilution factors.
This is essential when samples are diluted during preparation and need
to be back-calculated to original concentrations.

## Usage

``` r
step_measure_dilution_correct(
  recipe,
  ...,
  dilution_col = "dilution_factor",
  operation = c("multiply", "divide"),
  handle_zero = c("error", "warn", "skip"),
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_dilution_correct")
)
```

## Arguments

- recipe:

  A recipe object.

- ...:

  One or more selector functions to choose feature columns
  (concentration values) to correct. If empty, all numeric columns
  (excluding metadata columns) will be selected.

- dilution_col:

  Name of the column containing dilution factors. Default is
  `"dilution_factor"`.

- operation:

  How to apply the dilution factor:

  - `"multiply"` (default): `concentration * dilution_factor`
    (back-calculate from diluted to original concentration)

  - `"divide"`: `concentration / dilution_factor` (apply dilution)

- handle_zero:

  How to handle zero dilution factors:

  - `"error"` (default): Stop with an error

  - `"warn"`: Warn and set result to NA

  - `"skip"`: Silently set result to NA

- role:

  Not used by this step.

- trained:

  Logical indicating if the step has been trained.

- skip:

  Logical. Should the step be skipped when baking?

- id:

  Unique step identifier.

## Value

An updated recipe with the new step added.

## Details

### Dilution Factor Interpretation

The dilution factor represents how much the sample was diluted:

- A factor of 1 means no dilution (undiluted)

- A factor of 2 means 1:2 dilution (1 part sample + 1 part diluent)

- A factor of 10 means 1:10 dilution

### Back-Calculation

When using `operation = "multiply"` (the default):
`original_concentration = measured_concentration * dilution_factor`

This corrects for the dilution to get the true concentration in the
original sample.

### When to Use

Use this step after quantitation (calibration) when samples were diluted
to bring concentrations within the calibration range.

## See also

[`step_measure_surrogate_recovery()`](https://jameshwade.github.io/measure/dev/reference/step_measure_surrogate_recovery.md),
[`measure_calibration_predict()`](https://jameshwade.github.io/measure/dev/reference/measure_calibration_predict.md)

Other calibration:
[`measure_matrix_effect()`](https://jameshwade.github.io/measure/dev/reference/measure_matrix_effect.md),
[`step_measure_standard_addition()`](https://jameshwade.github.io/measure/dev/reference/step_measure_standard_addition.md),
[`step_measure_surrogate_recovery()`](https://jameshwade.github.io/measure/dev/reference/step_measure_surrogate_recovery.md)

## Examples

``` r
library(recipes)

# Example: samples diluted to fit calibration range
data <- data.frame(
  sample_id = paste0("S", 1:6),
  dilution_factor = c(1, 2, 5, 10, 1, 1),
  analyte = c(50, 45, 42, 48, 51, 49)  # Measured after dilution
)

rec <- recipe(~ ., data = data) |>
  update_role(sample_id, new_role = "id") |>
  step_measure_dilution_correct(
    analyte,
    dilution_col = "dilution_factor",
    operation = "multiply"
  ) |>
  prep()

# Back-calculated concentrations
bake(rec, new_data = NULL)
#> # A tibble: 6 × 3
#>   sample_id dilution_factor analyte
#>   <chr>               <dbl>   <dbl>
#> 1 S1                      1      50
#> 2 S2                      2      90
#> 3 S3                      5     210
#> 4 S4                     10     480
#> 5 S5                      1      51
#> 6 S6                      1      49
# S1: 50*1=50, S2: 45*2=90, S3: 42*5=210, S4: 48*10=480
```
