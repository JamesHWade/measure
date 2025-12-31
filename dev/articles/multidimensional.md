# Multi-Dimensional Measurements

``` r
library(measure)
#> Loading required package: recipes
#> Loading required package: dplyr
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
#> 
#> Attaching package: 'recipes'
#> The following object is masked from 'package:stats':
#> 
#>     step
library(recipes)
library(dplyr)
```

## Introduction

Many analytical techniques produce multi-dimensional data. Examples
include:

- **LC-DAD**: Liquid chromatography with diode array detection (time ×
  wavelength)
- **GC×GC**: Comprehensive two-dimensional gas chromatography (time₁ ×
  time₂)
- **EEM**: Excitation-emission matrix fluorescence (excitation ×
  emission wavelength)
- **2D NMR**: Two-dimensional nuclear magnetic resonance (chemical shift
  × chemical shift)

The `measure` package provides native support for n-dimensional
measurement data through the `measure_nd_tbl` and `measure_nd_list`
classes.

## Creating 2D Measurement Data

Let’s create synthetic LC-DAD data with retention time and wavelength
dimensions:

``` r
set.seed(42)

# Simulate 3 samples with LC-DAD measurements
# 10 time points × 4 wavelengths = 40 data points per sample
lc_dad_data <- tibble(
  sample_id = rep(1:3, each = 40),
  retention_time = rep(rep(seq(0, 9, by = 1), each = 4), 3),
  wavelength = rep(c(254, 280, 320, 350), 30),
  absorbance = rnorm(120, mean = 100, sd = 10),
  concentration = rep(c(10, 25, 50), each = 40)
)

head(lc_dad_data, 12)
#> # A tibble: 12 × 5
#>    sample_id retention_time wavelength absorbance concentration
#>        <int>          <dbl>      <dbl>      <dbl>         <dbl>
#>  1         1              0        254      114.             10
#>  2         1              0        280       94.4            10
#>  3         1              0        320      104.             10
#>  4         1              0        350      106.             10
#>  5         1              1        254      104.             10
#>  6         1              1        280       98.9            10
#>  7         1              1        320      115.             10
#>  8         1              1        350       99.1            10
#>  9         1              2        254      120.             10
#> 10         1              2        280       99.4            10
#> 11         1              2        320      113.             10
#> 12         1              2        350      123.             10
```

This is the typical “long format” for 2D analytical data, where each row
represents a single measurement at a specific (time, wavelength)
coordinate.

## Ingesting 2D Data

Use
[`step_measure_input_long()`](https://jameshwade.github.io/measure/dev/reference/step_measure_input_long.md)
with multiple location columns to create a 2D measure column:

``` r
rec <- recipe(concentration ~ ., data = lc_dad_data) |>
  update_role(sample_id, new_role = "id") |>
  step_measure_input_long(
    absorbance,
    location = vars(retention_time, wavelength),
    dim_names = c("time", "wavelength"),
    dim_units = c("min", "nm")
  ) |>
  prep()

result <- bake(rec, new_data = NULL)
result
#> # A tibble: 3 × 3
#>   sample_id concentration .measures
#>       <int>         <dbl>  <meas2d>
#> 1         1            10  [40 × 3]
#> 2         2            25  [40 × 3]
#> 3         3            50  [40 × 3]
```

The `.measures` column now contains `measure_nd_list` objects - one 2D
measurement per sample:

``` r
class(result$.measures)
#> [1] "measure_nd_list" "measure_list"    "vctrs_list_of"   "vctrs_vctr"     
#> [5] "list"
measure_ndim(result$.measures)
#> [1] 2
```

## Inspecting 2D Measurements

Each element of the measure column is a `measure_nd_tbl`:

``` r
# First sample's measurement
m1 <- result$.measures[[1]]
class(m1)
#> [1] "measure_nd_tbl" "measure_tbl"    "tbl_df"         "tbl"           
#> [5] "data.frame"
m1
#> <measure_nd_tbl [40 x 3] time x wavelength>
#> <measure_tbl [40 x 3]>
#> # A tibble: 40 × 3
#>    location_1 location_2 value
#>         <dbl>      <dbl> <dbl>
#>  1          0        254 114. 
#>  2          0        280  94.4
#>  3          0        320 104. 
#>  4          0        350 106. 
#>  5          1        254 104. 
#>  6          1        280  98.9
#>  7          1        320 115. 
#>  8          1        350  99.1
#>  9          2        254 120. 
#> 10          2        280  99.4
#> # ℹ 30 more rows
```

Dimension metadata is preserved:

``` r
measure_dim_names(m1)
#> [1] "time"       "wavelength"
measure_dim_units(m1)
#> [1] "min" "nm"
```

## Grid Information

The
[`measure_grid_info()`](https://jameshwade.github.io/measure/dev/reference/measure_grid_info.md)
function provides detailed information about the measurement grid:

``` r
info <- measure_grid_info(m1)
info$ndim
#> [1] 2
info$shape
#> dim_1 dim_2 
#>    10     4
info$n_points
#> [1] 40
info$is_regular
#> [1] TRUE
```

A “regular” grid means all combinations of location values are present
(complete rectangular grid).

## Applying 1D Operations to 2D Data

The
[`measure_apply()`](https://jameshwade.github.io/measure/dev/reference/measure_apply.md)
function enables existing 1D preprocessing operations to work on
n-dimensional data by applying them along specified dimensions.

``` r
# Define a simple 1D smoothing function
smooth_1d <- function(x, window = 3) {
  if (nrow(x) < window) return(x)
  smoothed <- stats::filter(x$value, rep(1/window, window), sides = 2)
  valid <- !is.na(smoothed)
  new_measure_tbl(
    location = x$location[valid],
    value = as.numeric(smoothed[valid])
  )
}
```

Apply smoothing along the time dimension (dimension 1):

``` r
# Apply to a single 2D measurement
smoothed <- measure_apply(m1, smooth_1d, along = 1, window = 3)

# Original had 40 points (10 time × 4 wavelength)
nrow(m1)
#> [1] 40

# Smoothed has fewer points (edges removed by filter)
nrow(smoothed)
#> [1] 32
```

The function was applied independently to each wavelength slice,
treating time as the 1D axis.

## Converting Back to Long Format

Use
[`step_measure_output_long()`](https://jameshwade.github.io/measure/dev/reference/step_measure_output_long.md)
to convert the nested measure back to long format:

``` r
output_rec <- recipe(concentration ~ ., data = lc_dad_data) |>
  update_role(sample_id, new_role = "id") |>
  step_measure_input_long(
    absorbance,
    location = vars(retention_time, wavelength)
  ) |>
  step_measure_output_long(
    values_to = "absorbance",
    location_to = "loc"
  ) |>
  prep()

output_result <- bake(output_rec, new_data = NULL)
head(output_result)
#> # A tibble: 6 × 5
#>   sample_id concentration loc_1 loc_2 absorbance
#>       <int>         <dbl> <dbl> <dbl>      <dbl>
#> 1         1            10     0   254      114. 
#> 2         1            10     0   280       94.4
#> 3         1            10     0   320      104. 
#> 4         1            10     0   350      106. 
#> 5         1            10     1   254      104. 
#> 6         1            10     1   280       98.9
```

For 2D data, location columns are named with dimension suffixes
(`loc_1`, `loc_2`).

## Irregular Grids

Not all 2D data forms a regular rectangular grid. The package handles
irregular grids gracefully:

``` r
# Create irregular data (different wavelengths sampled at different times)
irregular_data <- tibble(
  sample_id = rep(1, 7),
  time = c(0, 0, 0, 5, 5, 10, 10),
  wavelength = c(254, 280, 320, 254, 280, 254, 350),
  value = rnorm(7),
  outcome = 1
)

irr_rec <- recipe(outcome ~ ., data = irregular_data) |>
  update_role(sample_id, new_role = "id") |>
  step_measure_input_long(value, location = vars(time, wavelength)) |>
  prep()

irr_result <- bake(irr_rec, new_data = NULL)

# Check regularity
measure_is_regular(irr_result$.measures[[1]])
#> [1] FALSE
```

## Dimension Reduction Operations

The package provides several operations for reducing dimensionality of
nD data.

### Unfolding and Folding

[`measure_unfold()`](https://jameshwade.github.io/measure/dev/reference/measure_unfold.md)
converts nD data to 1D for use with modeling techniques that expect
vectors:

``` r
# Unfold 2D to 1D
m1d <- measure_unfold(m1)
m1d
#> <measure_tbl [40 x 2]>
#> # A tibble: 40 × 2
#>    location value
#>       <int> <dbl>
#>  1        1 114. 
#>  2        2 104. 
#>  3        3 120. 
#>  4        4  86.1
#>  5        5  97.2
#>  6        6  96.9
#>  7        7 119. 
#>  8        8 105. 
#>  9        9 110. 
#> 10       10  92.2
#> # ℹ 30 more rows

# The fold metadata is preserved
attr(m1d, "fold_info")$ndim
#> [1] 2
```

[`measure_fold()`](https://jameshwade.github.io/measure/dev/reference/measure_fold.md)
reconstructs the original nD structure:

``` r
# Reconstruct 2D from 1D
m2d_restored <- measure_fold(m1d)
measure_ndim(m2d_restored)
#> [1] 2
```

### Slicing

[`measure_slice()`](https://jameshwade.github.io/measure/dev/reference/measure_slice.md)
extracts subsets at specific coordinates:

``` r
# Extract data at wavelength = 254
slice_254 <- measure_slice(m1, wavelength = 254)
slice_254
#> <measure_tbl [10 x 2]>
#> # A tibble: 10 × 2
#>    location value
#>       <dbl> <dbl>
#>  1        0 114. 
#>  2        1 104. 
#>  3        2 120. 
#>  4        3  86.1
#>  5        4  97.2
#>  6        5  96.9
#>  7        6 119. 
#>  8        7 105. 
#>  9        8 110. 
#> 10        9  92.2

# Extract multiple wavelengths (keeps 2D structure)
slice_uv <- measure_slice(m1, wavelength = c(254, 280), drop = FALSE)
measure_ndim(slice_uv)
#> [1] 2
```

### Projection

[`measure_project()`](https://jameshwade.github.io/measure/dev/reference/measure_project.md)
aggregates across dimensions:

``` r
# Average across wavelengths to get time trace
time_trace <- measure_project(m1, along = "wavelength")
time_trace
#> <measure_tbl [10 x 2]>
#> # A tibble: 10 × 2
#>    location value
#>       <dbl> <dbl>
#>  1        0 105. 
#>  2        1 104. 
#>  3        2 114. 
#>  4        3  97.1
#>  5        4  89.8
#>  6        5  97.4
#>  7        6  98.6
#>  8        7 102. 
#>  9        8  98.0
#> 10        9  90.0

# Sum across time to get total absorbance per wavelength
wl_total <- measure_project(m1, along = "time", fn = sum)
wl_total
#> <measure_tbl [4 x 2]>
#> # A tibble: 4 × 2
#>   location value
#>      <dbl> <dbl>
#> 1      254 1044.
#> 2      280  920.
#> 3      320  987.
#> 4      350 1033.
```

## Multi-Channel Operations

When working with multiple detector channels (e.g., UV + RI in SEC, or
multiple wavelengths in LC-DAD), the package provides steps for
aligning, combining, and computing ratios between channels.

### Channel Alignment

[`step_measure_channel_align()`](https://jameshwade.github.io/measure/dev/reference/step_measure_channel_align.md)
aligns multiple measure columns to a common grid:

``` r
# Align UV and RI detector signals to the same time grid
rec <- recipe(outcome ~ ., data = sec_data) |>
 step_measure_input_wide(starts_with("uv_"), col_name = "uv") |>
 step_measure_input_wide(starts_with("ri_"), col_name = "ri") |>
 step_measure_channel_align(uv, ri, method = "intersection") |>
 prep()
```

Methods include: - `"intersection"`: Keep only locations present in all
channels - `"union"`: Include all locations, interpolating missing
values - `"reference"`: Align all channels to a reference channel’s grid

### Channel Combination

[`step_measure_channel_combine()`](https://jameshwade.github.io/measure/dev/reference/step_measure_channel_combine.md)
merges multiple channels:

``` r
# Stack channels into a single 2D measure (location x channel)
rec <- recipe(outcome ~ ., data = multi_detector_data) |>
 step_measure_input_wide(starts_with("uv_"), col_name = "uv") |>
 step_measure_input_wide(starts_with("ri_"), col_name = "ri") |>
 step_measure_channel_align(uv, ri) |>
 step_measure_channel_combine(uv, ri, strategy = "stack") |>
 prep()
```

Strategies include: - `"stack"`: Create a 2D measure with channel as a
dimension - `"concat"`: Concatenate into a single 1D vector - `"mean"`
or `"weighted_sum"`: Combine into a single channel

### Channel Ratios

[`step_measure_channel_ratio()`](https://jameshwade.github.io/measure/dev/reference/step_measure_channel_ratio.md)
computes ratios between channels:

``` r
# Compute UV/RI ratio for each sample
rec <- recipe(outcome ~ ., data = sec_data) |>
 step_measure_input_wide(starts_with("uv_"), col_name = "uv") |>
 step_measure_input_wide(starts_with("ri_"), col_name = "ri") |>
 step_measure_channel_align(uv, ri) |>
 step_measure_channel_ratio(numerator = uv, denominator = ri) |>
 prep()
```

## Multi-Way Analysis

For extracting interpretable components from 2D or 3D measurement data,
the package provides multi-way decomposition methods.

### PARAFAC Decomposition

[`step_measure_parafac()`](https://jameshwade.github.io/measure/dev/reference/step_measure_parafac.md)
performs Parallel Factor Analysis, extracting trilinear components:

``` r
# Extract 3 PARAFAC components from EEM fluorescence data
rec <- recipe(concentration ~ ., data = eem_data) |>
 step_measure_input_long(
   fluorescence,
   location = vars(excitation, emission)
 ) |>
 step_measure_parafac(n_components = 3) |>
 prep()

# Result contains parafac_1, parafac_2, parafac_3 score columns
baked <- bake(rec, new_data = NULL)
```

PARAFAC is particularly useful for: - EEM fluorescence (excitation x
emission matrices) - Resolving overlapping chromatographic peaks -
Identifying underlying chemical species in mixtures

### Tucker Decomposition

[`step_measure_tucker()`](https://jameshwade.github.io/measure/dev/reference/step_measure_tucker.md)
provides more flexibility with independent ranks per mode:

``` r
# Tucker decomposition with different ranks for each dimension
rec <- recipe(concentration ~ ., data = lc_dad_data) |>
 step_measure_input_long(
   absorbance,
   location = vars(time, wavelength)
 ) |>
 step_measure_tucker(ranks = c(5, 3)) |>  # 5 time, 3 wavelength components
 prep()
```

### MCR-ALS (Experimental)

[`step_measure_mcr_als()`](https://jameshwade.github.io/measure/dev/reference/step_measure_mcr_als.md)
implements Multivariate Curve Resolution with Alternating Least Squares:

``` r
# MCR-ALS with non-negativity constraints
rec <- recipe(concentration ~ ., data = chrom_data) |>
 step_measure_input_long(
   absorbance,
   location = vars(time, wavelength)
 ) |>
 step_measure_mcr_als(
   n_components = 3,
   non_negativity = TRUE
 ) |>
 prep()
```

> **Note:** MCR-ALS is marked as experimental. The implementation uses a
> simple ALS algorithm suitable for exploratory analysis.

## Summary

Key functions for multi-dimensional measurement data:

| Function                                                                                                               | Purpose                                       |
|------------------------------------------------------------------------------------------------------------------------|-----------------------------------------------|
| [`step_measure_input_long()`](https://jameshwade.github.io/measure/dev/reference/step_measure_input_long.md)           | Ingest nD data with multiple location columns |
| [`step_measure_output_long()`](https://jameshwade.github.io/measure/dev/reference/step_measure_output_long.md)         | Convert nD data back to long format           |
| [`measure_ndim()`](https://jameshwade.github.io/measure/dev/reference/measure_ndim.md)                                 | Get number of dimensions                      |
| [`measure_dim_names()`](https://jameshwade.github.io/measure/dev/reference/measure_dim_names.md)                       | Get semantic dimension names                  |
| [`measure_dim_units()`](https://jameshwade.github.io/measure/dev/reference/measure_dim_units.md)                       | Get dimension units                           |
| [`measure_is_regular()`](https://jameshwade.github.io/measure/dev/reference/measure_is_regular.md)                     | Check if grid is regular/rectangular          |
| [`measure_grid_info()`](https://jameshwade.github.io/measure/dev/reference/measure_grid_info.md)                       | Get detailed grid information                 |
| [`measure_apply()`](https://jameshwade.github.io/measure/dev/reference/measure_apply.md)                               | Apply 1D functions along specified dimensions |
| [`measure_unfold()`](https://jameshwade.github.io/measure/dev/reference/measure_unfold.md)                             | Convert nD to 1D with fold metadata           |
| [`measure_fold()`](https://jameshwade.github.io/measure/dev/reference/measure_fold.md)                                 | Reconstruct nD from unfolded 1D               |
| [`measure_slice()`](https://jameshwade.github.io/measure/dev/reference/measure_slice.md)                               | Extract slices at specific coordinates        |
| [`measure_project()`](https://jameshwade.github.io/measure/dev/reference/measure_project.md)                           | Aggregate across dimensions                   |
| [`step_measure_channel_align()`](https://jameshwade.github.io/measure/dev/reference/step_measure_channel_align.md)     | Align channels to common grid                 |
| [`step_measure_channel_combine()`](https://jameshwade.github.io/measure/dev/reference/step_measure_channel_combine.md) | Combine multiple channels                     |
| [`step_measure_channel_ratio()`](https://jameshwade.github.io/measure/dev/reference/step_measure_channel_ratio.md)     | Compute ratios between channels               |
| [`step_measure_parafac()`](https://jameshwade.github.io/measure/dev/reference/step_measure_parafac.md)                 | PARAFAC decomposition                         |
| [`step_measure_tucker()`](https://jameshwade.github.io/measure/dev/reference/step_measure_tucker.md)                   | Tucker decomposition                          |
| [`step_measure_mcr_als()`](https://jameshwade.github.io/measure/dev/reference/step_measure_mcr_als.md)                 | MCR-ALS decomposition (experimental)          |
