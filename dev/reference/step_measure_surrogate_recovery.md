# Surrogate/Internal Standard Recovery

`step_measure_surrogate_recovery()` creates a *specification* of a
recipe step that calculates recovery percentages for surrogate or
internal standards. This is essential for quality control in analytical
workflows where spiked compounds are used to monitor method performance.

## Usage

``` r
step_measure_surrogate_recovery(
  recipe,
  ...,
  expected_col = NULL,
  expected_value = NULL,
  recovery_suffix = "_recovery",
  action = c("add_column", "flag", "filter"),
  flag_col = ".surrogate_pass",
  min_recovery = 70,
  max_recovery = 130,
  role = "surrogate",
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_surrogate_recovery")
)
```

## Arguments

- recipe:

  A recipe object.

- ...:

  One or more selector functions to choose surrogate columns (measured
  concentrations or responses).

- expected_col:

  Name of a column containing expected concentrations for each sample.
  Mutually exclusive with `expected_value`.

- expected_value:

  A fixed numeric value for expected concentration. Used when all
  surrogates have the same expected value. Mutually exclusive with
  `expected_col`.

- recovery_suffix:

  Suffix appended to column names for recovery output. Default is
  `"_recovery"`.

- action:

  What to do with recovery calculations:

  - `"add_column"` (default): Add new columns with recovery percentages

  - `"flag"`: Add a boolean column indicating if recovery is within
    limits

  - `"filter"`: Remove rows where any surrogate is outside limits

- flag_col:

  Name of the flag column when `action = "flag"`. Default is
  `".surrogate_pass"`.

- min_recovery:

  Minimum acceptable recovery percentage. Default is 70.

- max_recovery:

  Maximum acceptable recovery percentage. Default is 130.

- role:

  Recipe role for new recovery columns. Default is `"surrogate"`.

- trained:

  Logical indicating if the step has been trained.

- skip:

  Logical. Should the step be skipped when baking?

- id:

  Unique step identifier.

## Value

An updated recipe with the new step added.

## Details

### Recovery Calculation

Recovery is calculated as: `recovery_pct = (measured / expected) * 100`

Where:

- `measured` is the observed concentration/response of the surrogate

- `expected` is the known spike amount or theoretical value

### Acceptance Criteria

Typical acceptance limits vary by application:

- **ICH M10 (Bioanalytical)**: 70-130% for surrogates

- **EPA Methods**: Often 50-150% or method-specific

- **FDA Guidance**: Application-specific, often 80-120%

### Use Cases

- Monitor extraction efficiency in sample preparation

- Track instrument performance across runs

- Identify samples with matrix effects or procedural errors

## See also

[`step_measure_dilution_correct()`](https://jameshwade.github.io/measure/dev/reference/step_measure_dilution_correct.md),
[`measure_matrix_effect()`](https://jameshwade.github.io/measure/dev/reference/measure_matrix_effect.md)

Other calibration:
[`measure_matrix_effect()`](https://jameshwade.github.io/measure/dev/reference/measure_matrix_effect.md),
[`step_measure_dilution_correct()`](https://jameshwade.github.io/measure/dev/reference/step_measure_dilution_correct.md),
[`step_measure_standard_addition()`](https://jameshwade.github.io/measure/dev/reference/step_measure_standard_addition.md)

## Examples

``` r
library(recipes)

# Example: QC data with spiked surrogates
qc_data <- data.frame(
  sample_id = paste0("QC", 1:10),
  surrogate_1 = rnorm(10, mean = 100, sd = 10),
  surrogate_2 = rnorm(10, mean = 50, sd = 5),
  analyte = rnorm(10, mean = 75, sd = 8)
)

# Add recovery columns for surrogates with expected value of 100 and 50
rec <- recipe(~ ., data = qc_data) |>
  update_role(sample_id, new_role = "id") |>
  step_measure_surrogate_recovery(
    surrogate_1,
    expected_value = 100,
    min_recovery = 80,
    max_recovery = 120
  ) |>
  prep()

bake(rec, new_data = NULL)
#> # A tibble: 10 × 5
#>    sample_id surrogate_1 surrogate_2 analyte surrogate_1_recovery
#>    <chr>           <dbl>       <dbl>   <dbl>                <dbl>
#>  1 QC1              75.3        54.3    74.9                 75.3
#>  2 QC2             126.         47.7    60.7                126. 
#>  3 QC3              97.9        62.1    75.3                 97.9
#>  4 QC4             107.         41.7    76.5                107. 
#>  5 QC5             103.         47.7    76.4                103. 
#>  6 QC6             110.         54.1    66.6                110. 
#>  7 QC7             108.         52.6    78.8                108. 
#>  8 QC8              97.9        47.1    86.0                 97.9
#>  9 QC9             104.         45.0    78.6                104. 
#> 10 QC10             90.5        50.7    65.9                 90.5
```
