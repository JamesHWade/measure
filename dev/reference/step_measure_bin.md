# Spectral Binning

`step_measure_bin()` creates a *specification* of a recipe step that
reduces a spectrum to fewer points by averaging within bins.

## Usage

``` r
step_measure_bin(
  recipe,
  n_bins = NULL,
  bin_width = NULL,
  method = c("mean", "sum", "median", "max"),
  measures = NULL,
  role = NA,
  trained = FALSE,
  bin_breaks = NULL,
  skip = FALSE,
  id = recipes::rand_id("measure_bin")
)
```

## Arguments

- recipe:

  A recipe object.

- n_bins:

  Number of bins (mutually exclusive with `bin_width`).

- bin_width:

  Width of each bin in location units (mutually exclusive with
  `n_bins`).

- method:

  Aggregation method: `"mean"` (default), `"sum"`, `"median"`, or
  `"max"`.

- measures:

  An optional character vector of measure column names.

- role:

  Not used (modifies existing data).

- trained:

  Logical indicating if the step has been trained.

- bin_breaks:

  The computed bin breaks (after training).

- skip:

  Logical. Should the step be skipped when baking?

- id:

  Unique step identifier.

## Value

An updated recipe with the new step added.

## Details

This step reduces the number of points in each spectrum by dividing the
x-axis into bins and aggregating values within each bin. The result
replaces the `.measures` column with the binned data.

This is useful for:

- Reducing data dimensionality

- Decreasing noise through averaging

- Speeding up downstream processing

- Aligning data from different resolutions

The bin boundaries are determined during
[`prep()`](https://recipes.tidymodels.org/reference/prep.html) from the
training data and stored for consistent application to new data.

## See also

Other measure-features:
[`step_measure_integrals()`](https://jameshwade.github.io/measure/dev/reference/step_measure_integrals.md),
[`step_measure_moments()`](https://jameshwade.github.io/measure/dev/reference/step_measure_moments.md),
[`step_measure_ratios()`](https://jameshwade.github.io/measure/dev/reference/step_measure_ratios.md)

## Examples

``` r
library(recipes)

# Bin to 20 points
rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
  update_role(id, new_role = "id") |>
  step_measure_input_long(transmittance, location = vars(channel)) |>
  step_measure_bin(n_bins = 20) |>
  prep()

bake(rec, new_data = NULL)
#> # A tibble: 215 × 6
#>       id water   fat protein .measures channel    
#>    <int> <dbl> <dbl>   <dbl>    <meas> <list>     
#>  1     1  60.5  22.5    16.7  [20 × 2] <int [100]>
#>  2     2  46    40.1    13.5  [20 × 2] <int [100]>
#>  3     3  71     8.4    20.5  [20 × 2] <int [100]>
#>  4     4  72.8   5.9    20.7  [20 × 2] <int [100]>
#>  5     5  58.3  25.5    15.5  [20 × 2] <int [100]>
#>  6     6  44    42.7    13.7  [20 × 2] <int [100]>
#>  7     7  44    42.7    13.7  [20 × 2] <int [100]>
#>  8     8  69.3  10.6    19.3  [20 × 2] <int [100]>
#>  9     9  61.4  19.9    17.7  [20 × 2] <int [100]>
#> 10    10  61.4  19.9    17.7  [20 × 2] <int [100]>
#> # ℹ 205 more rows
```
