# Calculate Region Integrals

`step_measure_integrals()` creates a *specification* of a recipe step
that calculates integrated areas for specified x-axis regions.

## Usage

``` r
step_measure_integrals(
  recipe,
  regions,
  method = c("trapezoid", "simpson"),
  measures = NULL,
  prefix = "integral_",
  role = "predictor",
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_integrals")
)
```

## Arguments

- recipe:

  A recipe object.

- regions:

  A named or unnamed list of numeric vectors, each of length 2
  specifying regions as `c(min, max)`. For example:
  `list(peak1 = c(1000, 1100), peak2 = c(1500, 1600))`.

- method:

  Integration method: `"trapezoid"` (default) or `"simpson"`.

- measures:

  An optional character vector of measure column names.

- prefix:

  Prefix for output column names. Default is `"integral_"`.

- role:

  Role for generated columns. Default is `"predictor"`.

- trained:

  Logical indicating if the step has been trained.

- skip:

  Logical. Should the step be skipped when baking?

- id:

  Unique step identifier.

## Value

An updated recipe with the new step added.

## Details

This step calculates the integrated area under the curve for each
specified region. The result is added as new predictor columns, one per
region.

**Column naming:**

- If regions are named: `prefix + name` (e.g., `"integral_peak1"`)

- If regions are unnamed: `prefix + index` (e.g., `"integral_1"`)

**Integration methods:**

- `"trapezoid"`: Trapezoidal rule, fast and accurate for smooth data

- `"simpson"`: Simpson's rule, more accurate for smooth curves

## See also

Other measure-features:
[`step_measure_bin()`](https://jameshwade.github.io/measure/dev/reference/step_measure_bin.md),
[`step_measure_moments()`](https://jameshwade.github.io/measure/dev/reference/step_measure_moments.md),
[`step_measure_ratios()`](https://jameshwade.github.io/measure/dev/reference/step_measure_ratios.md)

## Examples

``` r
library(recipes)

rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
  update_role(id, new_role = "id") |>
  step_measure_input_long(transmittance, location = vars(channel)) |>
  step_measure_integrals(
    regions = list(low = c(1, 30), mid = c(40, 60), high = c(70, 100))
  ) |>
  prep()

bake(rec, new_data = NULL)
#> # A tibble: 215 × 8
#>       id water   fat protein .measures integral_low integral_mid integral_high
#>    <int> <dbl> <dbl>   <dbl>    <meas>        <dbl>        <dbl>         <dbl>
#>  1     1  60.5  22.5    16.7 [100 × 2]         77.1         61.8          93.6
#>  2     2  46    40.1    13.5 [100 × 2]         85.0         67.4         101. 
#>  3     3  71     8.4    20.5 [100 × 2]         76.1         59.4          85.0
#>  4     4  72.8   5.9    20.7 [100 × 2]         83.1         64.6          94.2
#>  5     5  58.3  25.5    15.5 [100 × 2]         82.8         67.6         103. 
#>  6     6  44    42.7    13.7 [100 × 2]         90.6         72.3         109. 
#>  7     7  44    42.7    13.7 [100 × 2]         90.2         72.0         107. 
#>  8     8  69.3  10.6    19.3 [100 × 2]         74.3         59.7          90.5
#>  9     9  61.4  19.9    17.7 [100 × 2]         98.3         78.8         119. 
#> 10    10  61.4  19.9    17.7 [100 × 2]        102.          82.6         124. 
#> # ℹ 205 more rows
```
