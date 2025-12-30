# Standard Addition Correction

`step_measure_standard_addition()` creates a *specification* of a recipe
step that performs standard addition correction to compensate for matrix
effects. This method creates a sample-specific calibration for each
unknown to accurately quantify in the presence of matrix interference.

## Usage

``` r
step_measure_standard_addition(
  recipe,
  ...,
  addition_col = "addition",
  sample_id_col,
  min_points = 3,
  output_suffix = "_corrected",
  diagnostics = TRUE,
  role = "outcome",
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_standard_addition")
)
```

## Arguments

- recipe:

  A recipe object.

- ...:

  One or more selector functions to choose response columns to correct
  using standard addition.

- addition_col:

  Name of the column containing the amount of standard added (spike
  amount). Default is `"addition"`.

- sample_id_col:

  Name of the column identifying unique samples. Each sample gets its
  own standard addition curve.

- min_points:

  Minimum number of addition points required per sample. Default is 3.

- output_suffix:

  Suffix for output concentration columns. Default is `"_corrected"`.

- diagnostics:

  Include diagnostic information (R², slope, intercept)? Default is
  TRUE.

- role:

  Recipe role for new columns. Default is `"outcome"`.

- trained:

  Logical indicating if the step has been trained.

- skip:

  Logical. Should the step be skipped when baking?

- id:

  Unique step identifier.

## Value

An updated recipe with the new step added.

## Details

### Standard Addition Method

Standard addition works by:

1.  Splitting each unknown sample into multiple aliquots

2.  Adding increasing known amounts of analyte to each aliquot

3.  Measuring response for all aliquots

4.  Fitting regression: `response = intercept + slope * addition`

5.  Calculating original concentration from the x-intercept

The x-intercept (where response = 0) is at `-intercept / slope`. Since
intercept is positive (response from original sample) and slope is
positive (response increases with addition), the original concentration
is: `concentration = intercept / slope`

### Data Format

The input data should have:

- A sample identifier column (each unique sample)

- An addition amount column (0 for unspiked, then increasing amounts)

- Response column(s) to be corrected

### When to Use

Use standard addition when:

- Significant matrix effects are present

- Matrix-matched calibrators are not available

- Sample-to-sample matrix variation is expected

### Limitations

- Requires multiple measurements per sample

- Assumes linear response over the addition range

- Does not correct for non-specific interferences

## See also

[`measure_matrix_effect()`](https://jameshwade.github.io/measure/dev/reference/measure_matrix_effect.md),
[`measure_calibration()`](https://jameshwade.github.io/measure/dev/reference/measure_calibration.md)

Other calibration:
[`measure_matrix_effect()`](https://jameshwade.github.io/measure/dev/reference/measure_matrix_effect.md),
[`step_measure_dilution_correct()`](https://jameshwade.github.io/measure/dev/reference/step_measure_dilution_correct.md),
[`step_measure_surrogate_recovery()`](https://jameshwade.github.io/measure/dev/reference/step_measure_surrogate_recovery.md)

## Examples

``` r
library(recipes)

# Standard addition data for two samples
sa_data <- data.frame(
  sample_id = rep(c("Sample1", "Sample2"), each = 4),
  addition = rep(c(0, 10, 20, 30), 2),
  response = c(
    # Sample 1: original conc ~15
    150, 250, 350, 450,
    # Sample 2: original conc ~25
    250, 350, 450, 550
  )
)

rec <- recipe(~ ., data = sa_data) |>
  step_measure_standard_addition(
    response,
    addition_col = "addition",
    sample_id_col = "sample_id"
  ) |>
  prep()
#> Warning: essentially perfect fit: summary may be unreliable
#> Warning: essentially perfect fit: summary may be unreliable

bake(rec, new_data = NULL)
#> # A tibble: 8 × 7
#>   sample_id addition response response_corrected response_sa_slope
#>   <fct>        <dbl>    <dbl>              <dbl>             <dbl>
#> 1 Sample1          0      150                 15                10
#> 2 Sample1         10      250                 15                10
#> 3 Sample1         20      350                 15                10
#> 4 Sample1         30      450                 15                10
#> 5 Sample2          0      250                 25                10
#> 6 Sample2         10      350                 25                10
#> 7 Sample2         20      450                 25                10
#> 8 Sample2         30      550                 25                10
#> # ℹ 2 more variables: response_sa_intercept <dbl>, response_sa_rsquared <dbl>
```
