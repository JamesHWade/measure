# Extended Multiplicative Scatter Correction (EMSC)

`step_measure_emsc()` creates a *specification* of a recipe step that
applies Extended Multiplicative Scatter Correction to spectral data.
EMSC accounts for wavelength-dependent scatter effects using polynomial
terms.

## Usage

``` r
step_measure_emsc(
  recipe,
  degree = 2L,
  reference = "mean",
  measures = NULL,
  role = NA,
  trained = FALSE,
  ref_spectrum = NULL,
  locations = NULL,
  skip = FALSE,
  id = recipes::rand_id("measure_emsc")
)
```

## Arguments

- recipe:

  A recipe object.

- degree:

  Polynomial degree for wavelength-dependent terms. Default is 2. Higher
  values can model more complex scatter effects but risk overfitting.

- reference:

  Reference spectrum method: `"mean"` (default) or `"median"`.
  Alternatively, a numeric vector can be supplied as the reference
  spectrum.

- measures:

  An optional character vector of measure column names.

- role:

  Not used.

- trained:

  Logical indicating if the step has been trained.

- ref_spectrum:

  The learned reference spectrum (after training).

- locations:

  The location values for polynomial terms (after training).

- skip:

  Logical. Should the step be skipped when baking?

- id:

  Unique step identifier.

## Value

An updated recipe with the new step added.

## Details

Extended MSC (EMSC) extends standard MSC by modeling
wavelength-dependent scatter effects. For a spectrum \\x_i\\ and
reference \\x_r\\, the model is:

\$\$x_i = a_i + b_i \cdot x_r + c_i \cdot \lambda + d_i \cdot
\lambda^2 + ... + \epsilon\$\$

The corrected spectrum is:

\$\$EMSC(x_i) = \frac{x_i - a_i - c_i \cdot \lambda - d_i \cdot
\lambda^2 - ...}{b_i}\$\$

The polynomial terms (\\\lambda\\, \\\lambda^2\\, etc.) account for
wavelength-dependent baseline effects that vary between samples.

**When to use EMSC vs MSC:**

- Use MSC for simple additive/multiplicative scatter

- Use EMSC when scatter effects vary with wavelength

- Start with degree=2, increase if needed for complex scatter

## See also

[`step_measure_msc()`](https://jameshwade.github.io/measure/dev/reference/step_measure_msc.md)
for standard MSC

Other measure-preprocessing:
[`step_measure_absorbance()`](https://jameshwade.github.io/measure/dev/reference/step_measure_absorbance.md),
[`step_measure_calibrate_x()`](https://jameshwade.github.io/measure/dev/reference/step_measure_calibrate_x.md),
[`step_measure_calibrate_y()`](https://jameshwade.github.io/measure/dev/reference/step_measure_calibrate_y.md),
[`step_measure_derivative()`](https://jameshwade.github.io/measure/dev/reference/step_measure_derivative.md),
[`step_measure_derivative_gap()`](https://jameshwade.github.io/measure/dev/reference/step_measure_derivative_gap.md),
[`step_measure_kubelka_munk()`](https://jameshwade.github.io/measure/dev/reference/step_measure_kubelka_munk.md),
[`step_measure_log()`](https://jameshwade.github.io/measure/dev/reference/step_measure_log.md),
[`step_measure_map()`](https://jameshwade.github.io/measure/dev/reference/step_measure_map.md),
[`step_measure_msc()`](https://jameshwade.github.io/measure/dev/reference/step_measure_msc.md),
[`step_measure_normalize_istd()`](https://jameshwade.github.io/measure/dev/reference/step_measure_normalize_istd.md),
[`step_measure_osc()`](https://jameshwade.github.io/measure/dev/reference/step_measure_osc.md),
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
  step_measure_emsc(degree = 2) |>
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
