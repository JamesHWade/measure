# Correlation Optimized Warping Alignment

`step_measure_align_cow()` creates a *specification* of a recipe step
that aligns spectra using Correlation Optimized Warping (COW). This
method uses piecewise linear warping to correct for non-linear shifts.

## Usage

``` r
step_measure_align_cow(
  recipe,
  measures = NULL,
  reference = c("mean", "median", "first"),
  segment_length = 30L,
  slack = 1L,
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_align_cow")
)
```

## Arguments

- recipe:

  A recipe object.

- measures:

  An optional character vector of measure column names.

- reference:

  How to determine the reference: `"mean"` (default, mean spectrum from
  training), `"median"` (median spectrum from training), or `"first"`
  (first sample).

- segment_length:

  Length of each segment for warping. Default is 30. Tunable via
  [`align_segment_length()`](https://jameshwade.github.io/measure/dev/reference/align_max_shift.md).

- slack:

  Maximum compression/expansion per segment in points. Default is 1. A
  slack of 1 means each segment can shrink or expand by 1 point.

- role:

  Not used.

- trained:

  Logical indicating if the step has been trained.

- skip:

  Logical. Should the step be skipped when baking?

- id:

  Unique step identifier.

## Value

An updated recipe with the new step added.

## Details

Correlation Optimized Warping (COW) divides signals into segments and
uses dynamic programming to find the optimal piecewise linear warping
that maximizes correlation with the reference spectrum.

Key parameters:

- `segment_length`: Controls the resolution of warping. Smaller segments
  allow more local corrections but increase computation.

- `slack`: Controls how much each segment can stretch or compress.
  Larger values allow more flexibility but may introduce artifacts.

This is a pure R implementation based on Nielsen et al. (1998).

## References

Nielsen, N.P.V., Carstensen, J.M., and Smedsgaard, J. (1998). Aligning
of single and multiple wavelength chromatographic profiles for
chemometric data analysis using correlation optimised warping. Journal
of Chromatography A, 805, 17-35.

## See also

Other measure-align:
[`step_measure_align_dtw()`](https://jameshwade.github.io/measure/dev/reference/step_measure_align_dtw.md),
[`step_measure_align_ptw()`](https://jameshwade.github.io/measure/dev/reference/step_measure_align_ptw.md),
[`step_measure_align_reference()`](https://jameshwade.github.io/measure/dev/reference/step_measure_align_reference.md),
[`step_measure_align_shift()`](https://jameshwade.github.io/measure/dev/reference/step_measure_align_shift.md)

## Examples

``` r
library(recipes)

rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
  update_role(id, new_role = "id") |>
  step_measure_input_long(transmittance, location = vars(channel)) |>
  step_measure_align_cow(segment_length = 20, slack = 2) |>
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
