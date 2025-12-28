# GPC/SEC Baseline Correction

`step_measure_baseline_gpc()` creates a *specification* of a recipe step
that applies baseline correction optimized for Gel Permeation
Chromatography (GPC) or Size Exclusion Chromatography (SEC) data. This
method estimates the baseline by interpolating between baseline regions
at the start and end of the chromatogram.

## Usage

``` r
step_measure_baseline_gpc(
  recipe,
  measures = NULL,
  left_frac = 0.05,
  right_frac = 0.05,
  method = "linear",
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_baseline_gpc")
)
```

## Arguments

- recipe:

  A recipe object. The step will be added to the sequence of operations
  for this recipe.

- measures:

  An optional character vector of measure column names to process. If
  `NULL` (the default), all measure columns (columns with class
  `measure_list`) will be processed.

- left_frac:

  Fraction of points from the beginning to use as the left baseline
  region. Default is `0.05` (first 5% of data points).

- right_frac:

  Fraction of points from the end to use as the right baseline region.
  Default is `0.05` (last 5% of data points).

- method:

  Method for baseline estimation. One of:

  - `"linear"` (default): Linear interpolation between left and right
    means

  - `"median"`: Uses median of baseline regions (more robust to
    outliers)

  - `"spline"`: Smooth spline through baseline regions

- role:

  Not used by this step since no new variables are created.

- trained:

  A logical to indicate if the quantities for preprocessing have been
  estimated.

- skip:

  A logical. Should the step be skipped when the recipe is baked?

- id:

  A character string that is unique to this step to identify it.

## Value

An updated version of `recipe` with the new step added to the sequence
of any existing operations.

## Details

GPC/SEC chromatograms typically have distinct baseline regions at the
beginning and end where no polymer elutes. This step leverages this
characteristic by:

1 2. Computing a representative baseline value for each region (mean or
median) 3. Interpolating between these values to estimate the full
baseline 4. Subtracting the estimated baseline from the signal

The `left_frac` and `right_frac` parameters control how much of the
chromatogram is considered "baseline". Choose values that:

- Include only the flat, signal-free regions

- Exclude any polymer peaks or system peaks

- Are large enough to average out noise

Unlike general-purpose baseline methods like ALS or polynomial fitting,
this approach is specifically designed for the characteristic shape of
GPC/SEC chromatograms and is computationally very fast.

**No selectors should be supplied to this step function**. The data
should be in the internal format produced by
[`step_measure_input_wide()`](https://jameshwade.github.io/measure/dev/reference/step_measure_input_wide.md)
or
[`step_measure_input_long()`](https://jameshwade.github.io/measure/dev/reference/step_measure_input_long.md).

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/reference/tidy.recipe.html)
this step, a tibble with columns `terms`, `left_frac`, `right_frac`,
`method`, and `id` is returned.

## See also

[`step_measure_baseline_als()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_als.md)
for general-purpose baseline correction,
[`step_measure_detrend()`](https://jameshwade.github.io/measure/dev/reference/step_measure_detrend.md)
for simple trend removal.

Other measure-baseline:
[`step_measure_baseline_airpls()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_airpls.md),
[`step_measure_baseline_als()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_als.md),
[`step_measure_baseline_arpls()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_arpls.md),
[`step_measure_baseline_auto()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_auto.md),
[`step_measure_baseline_custom()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_custom.md),
[`step_measure_baseline_minima()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_minima.md),
[`step_measure_baseline_morph()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_morph.md),
[`step_measure_baseline_poly()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_poly.md),
[`step_measure_baseline_py()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_py.md),
[`step_measure_baseline_rf()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_rf.md),
[`step_measure_baseline_rolling()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_rolling.md),
[`step_measure_baseline_snip()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_snip.md),
[`step_measure_baseline_tophat()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_tophat.md),
[`step_measure_detrend()`](https://jameshwade.github.io/measure/dev/reference/step_measure_detrend.md)

## Examples

``` r
library(recipes)

# Using meats_long as example (works on any measurement data)
rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
  update_role(id, new_role = "id") |>
  step_measure_input_long(transmittance, location = vars(channel)) |>
  step_measure_baseline_gpc(left_frac = 0.1, right_frac = 0.1) |>
  prep()

bake(rec, new_data = NULL)
#> # A tibble: 215 × 5
#>       id water   fat protein .measures
#>    <int> <dbl> <dbl>   <dbl>    <meas>
#>  1     1  60.5  22.5    16.7 [100 × 2]
#>  2     2  46    40.1    13.5 [100 × 2]
#>  3     3  71     8.4    20.5 [100 × 2]
#>  4     4  72.8   5.9    20.7 [100 × 2]
#>  5     5  58.3  25.5    15.5 [100 × 2]
#>  6     6  44    42.7    13.7 [100 × 2]
#>  7     7  44    42.7    13.7 [100 × 2]
#>  8     8  69.3  10.6    19.3 [100 × 2]
#>  9     9  61.4  19.9    17.7 [100 × 2]
#> 10    10  61.4  19.9    17.7 [100 × 2]
#> # ℹ 205 more rows
```
