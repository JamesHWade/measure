# Orthogonal Signal Correction (OSC)

`step_measure_osc()` creates a *specification* of a recipe step that
applies Orthogonal Signal Correction to remove variation orthogonal to
the outcome.

## Usage

``` r
step_measure_osc(
  recipe,
  n_components = 1L,
  tolerance = 1e-06,
  max_iter = 100L,
  measures = NULL,
  role = NA,
  trained = FALSE,
  weights = NULL,
  loadings = NULL,
  skip = FALSE,
  id = recipes::rand_id("measure_osc")
)
```

## Arguments

- recipe:

  A recipe object.

- n_components:

  Number of orthogonal components to remove. Default is 1.

- tolerance:

  Convergence tolerance for NIPALS algorithm. Default is 1e-6.

- max_iter:

  Maximum iterations for NIPALS. Default is 100.

- measures:

  An optional character vector of measure column names.

- role:

  Not used.

- trained:

  Logical indicating if the step has been trained.

- weights:

  The learned orthogonal weights (after training).

- loadings:

  The learned orthogonal loadings (after training).

- skip:

  Logical. Should the step be skipped when baking?

- id:

  Unique step identifier.

## Value

An updated recipe with the new step added.

## Details

Orthogonal Signal Correction (OSC) removes variation in X that is
orthogonal to Y (the outcome). This is useful for removing systematic
variation that is not related to the response.

**Algorithm:**

1.  Compute initial score t from Y using SVD

2.  Orthogonalize t with respect to Y

3.  Iterate NIPALS to find orthogonal components

4.  Remove orthogonal components from X

**Important:**

- The recipe must have at least one outcome variable with role "outcome"

- Outcomes are automatically detected from the recipe's role definitions

- Multiple outcomes are supported (multivariate Y)

OSC was originally described by Wold et al. (1998) for NIR spectroscopy.

## References

Wold, S., Antti, H., Lindgren, F., and Ohman, J. (1998). Orthogonal
signal correction of near-infrared spectra. Chemometrics and Intelligent
Laboratory Systems, 44(1-2), 175-185.

## See also

Other measure-preprocessing:
[`step_measure_absorbance()`](https://jameshwade.github.io/measure/dev/reference/step_measure_absorbance.md),
[`step_measure_calibrate_x()`](https://jameshwade.github.io/measure/dev/reference/step_measure_calibrate_x.md),
[`step_measure_calibrate_y()`](https://jameshwade.github.io/measure/dev/reference/step_measure_calibrate_y.md),
[`step_measure_derivative()`](https://jameshwade.github.io/measure/dev/reference/step_measure_derivative.md),
[`step_measure_derivative_gap()`](https://jameshwade.github.io/measure/dev/reference/step_measure_derivative_gap.md),
[`step_measure_emsc()`](https://jameshwade.github.io/measure/dev/reference/step_measure_emsc.md),
[`step_measure_kubelka_munk()`](https://jameshwade.github.io/measure/dev/reference/step_measure_kubelka_munk.md),
[`step_measure_log()`](https://jameshwade.github.io/measure/dev/reference/step_measure_log.md),
[`step_measure_map()`](https://jameshwade.github.io/measure/dev/reference/step_measure_map.md),
[`step_measure_msc()`](https://jameshwade.github.io/measure/dev/reference/step_measure_msc.md),
[`step_measure_normalize_istd()`](https://jameshwade.github.io/measure/dev/reference/step_measure_normalize_istd.md),
[`step_measure_ratio_reference()`](https://jameshwade.github.io/measure/dev/reference/step_measure_ratio_reference.md),
[`step_measure_snv()`](https://jameshwade.github.io/measure/dev/reference/step_measure_snv.md),
[`step_measure_subtract_blank()`](https://jameshwade.github.io/measure/dev/reference/step_measure_subtract_blank.md),
[`step_measure_subtract_reference()`](https://jameshwade.github.io/measure/dev/reference/step_measure_subtract_reference.md),
[`step_measure_transmittance()`](https://jameshwade.github.io/measure/dev/reference/step_measure_transmittance.md)

## Examples

``` r
library(recipes)

rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
  update_role(id, new_role = "id") |>
  step_measure_input_long(transmittance, location = vars(channel)) |>
  step_measure_osc(n_components = 2) |>
  prep()

bake(rec, new_data = NULL)
#> # A tibble: 215 × 6
#>       id water   fat protein .measures channel    
#>    <int> <dbl> <dbl>   <dbl>    <meas> <list>     
#>  1     1  60.5  22.5    16.7 [100 × 2] <int [100]>
#>  2     2  46    40.1    13.5 [100 × 2] <int [100]>
#>  3     3  71     8.4    20.5 [100 × 2] <int [100]>
#>  4     4  72.8   5.9    20.7 [100 × 2] <int [100]>
#>  5     5  58.3  25.5    15.5 [100 × 2] <int [100]>
#>  6     6  44    42.7    13.7 [100 × 2] <int [100]>
#>  7     7  44    42.7    13.7 [100 × 2] <int [100]>
#>  8     8  69.3  10.6    19.3 [100 × 2] <int [100]>
#>  9     9  61.4  19.9    17.7 [100 × 2] <int [100]>
#> 10    10  61.4  19.9    17.7 [100 × 2] <int [100]>
#> # ℹ 205 more rows
```
