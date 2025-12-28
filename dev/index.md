# measure

## Overview

measure extends [tidymodels](https://www.tidymodels.org/) with
preprocessing steps for analytical measurement data such as
spectroscopy, chromatography, and other instrument-generated signals. It
provides a [recipes](https://recipes.tidymodels.org/)-style interface
for common spectral preprocessing techniques.

measure helps you:

- **Convert** measurement data from wide or long formats into an
  internal representation
- **Preprocess** spectra using techniques like smoothing, derivatives,
  and normalization
- **Transform** data back to wide or long format for modeling or
  visualization

## Installation

You can install the development version of measure from
[GitHub](https://github.com/):

``` r
# install.packages("pak")
pak::pak("JamesHWade/measure")
```

## Usage

The measure workflow follows the familiar recipes pattern: define a
recipe, add steps, prep, and bake.

``` r
library(measure)
library(recipes)
library(ggplot2)

# NIR spectroscopy data for predicting meat composition
data(meats_long)
head(meats_long)
#> # A tibble: 6 × 6
#>      id water   fat protein channel transmittance
#>   <int> <dbl> <dbl>   <dbl>   <int>         <dbl>
#> 1     1  60.5  22.5    16.7       1          2.62
#> 2     1  60.5  22.5    16.7       2          2.62
#> 3     1  60.5  22.5    16.7       3          2.62
#> 4     1  60.5  22.5    16.7       4          2.62
#> 5     1  60.5  22.5    16.7       5          2.62
#> 6     1  60.5  22.5    16.7       6          2.62
```

### Building a preprocessing recipe

``` r
rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
  # Assign sample ID role (not used as predictor)
  update_role(id, new_role = "id") |>
  # Convert long-format measurements to internal representation
  step_measure_input_long(transmittance, location = vars(channel)) |>
  # Apply Savitzky-Golay smoothing with first derivative
  step_measure_savitzky_golay(window_side = 5, differentiation_order = 1) |>
  # Standard Normal Variate normalization
  step_measure_snv() |>
  # Convert back to wide format for modeling
  step_measure_output_wide(prefix = "nir_")
```

### Preparing and applying the recipe

``` r
# Prep learns any parameters from training data
prepped <- prep(rec)

# Bake applies the transformations
processed <- bake(prepped, new_data = NULL)

# Result is ready for modeling
processed[1:5, 1:8]
#> # A tibble: 5 × 8
#>      id water   fat protein  nir_01  nir_02  nir_03  nir_04
#>   <int> <dbl> <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
#> 1     1  60.5  22.5    16.7 -0.126  -0.110  -0.0928 -0.0745
#> 2     2  46    40.1    13.5  0.0184  0.0381  0.0601  0.0841
#> 3     3  71     8.4    20.5  0.105   0.114   0.125   0.136 
#> 4     4  72.8   5.9    20.7  0.0716  0.0786  0.0871  0.0974
#> 5     5  58.3  25.5    15.5 -0.132  -0.118  -0.101  -0.0817
```

### Visualizing the preprocessing

``` r
# Get data at intermediate step (before output conversion)
rec_for_viz <- recipe(water + fat + protein ~ ., data = meats_long) |>
update_role(id, new_role = "id") |>
  step_measure_input_long(transmittance, location = vars(channel)) |>
  step_measure_savitzky_golay(window_side = 5, differentiation_order = 1) |>
  step_measure_snv()

processed_long <- bake(prep(rec_for_viz), new_data = NULL)

# Extract and plot a few spectra
library(tidyr)
library(dplyr)

plot_data <- processed_long |>
  slice(1:10) |>
  mutate(sample_id = row_number()) |>
  unnest(.measures)

ggplot(plot_data, aes(x = location, y = value, group = sample_id, color = factor(sample_id))) +
  geom_line(alpha = 0.7) +
  labs(
    x = "Channel",
    y = "Preprocessed Signal",
    title = "NIR Spectra After Preprocessing",
    subtitle = "Savitzky-Golay first derivative + SNV normalization",
    color = "Sample"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
```

![](reference/figures/README-visualization-1.png)

## Available Steps

### Input/Output Steps

| Step                                                                                                           | Description                                                      |
|----------------------------------------------------------------------------------------------------------------|------------------------------------------------------------------|
| [`step_measure_input_wide()`](https://jameshwade.github.io/measure/dev/reference/step_measure_input_wide.md)   | Convert wide format (measurements in columns) to internal format |
| [`step_measure_input_long()`](https://jameshwade.github.io/measure/dev/reference/step_measure_input_long.md)   | Convert long format (measurements in rows) to internal format    |
| [`step_measure_output_wide()`](https://jameshwade.github.io/measure/dev/reference/step_measure_output_wide.md) | Convert back to wide format for modeling                         |
| [`step_measure_output_long()`](https://jameshwade.github.io/measure/dev/reference/step_measure_output_long.md) | Convert back to long format                                      |

### Spectral Math

| Step                                                                                                                 | Description                                      |
|----------------------------------------------------------------------------------------------------------------------|--------------------------------------------------|
| [`step_measure_absorbance()`](https://jameshwade.github.io/measure/dev/reference/step_measure_absorbance.md)         | Convert transmittance to absorbance              |
| [`step_measure_transmittance()`](https://jameshwade.github.io/measure/dev/reference/step_measure_transmittance.md)   | Convert absorbance to transmittance              |
| [`step_measure_log()`](https://jameshwade.github.io/measure/dev/reference/step_measure_log.md)                       | Log transformation with configurable base/offset |
| [`step_measure_kubelka_munk()`](https://jameshwade.github.io/measure/dev/reference/step_measure_kubelka_munk.md)     | Kubelka-Munk transformation for reflectance      |
| [`step_measure_derivative()`](https://jameshwade.github.io/measure/dev/reference/step_measure_derivative.md)         | Simple finite difference derivatives             |
| [`step_measure_derivative_gap()`](https://jameshwade.github.io/measure/dev/reference/step_measure_derivative_gap.md) | Gap (Norris-Williams) derivatives                |

### Filtering & Scatter Correction

| Step                                                                                                                 | Description                           |
|----------------------------------------------------------------------------------------------------------------------|---------------------------------------|
| [`step_measure_savitzky_golay()`](https://jameshwade.github.io/measure/dev/reference/step_measure_savitzky_golay.md) | Smoothing and/or differentiation      |
| [`step_measure_snv()`](https://jameshwade.github.io/measure/dev/reference/step_measure_snv.md)                       | Standard Normal Variate normalization |
| [`step_measure_msc()`](https://jameshwade.github.io/measure/dev/reference/step_measure_msc.md)                       | Multiplicative Scatter Correction     |

### Sample-wise Normalization

| Step                                                                                                                     | Description                        |
|--------------------------------------------------------------------------------------------------------------------------|------------------------------------|
| [`step_measure_normalize_sum()`](https://jameshwade.github.io/measure/dev/reference/step_measure_normalize_sum.md)       | Divide by sum (total intensity)    |
| [`step_measure_normalize_max()`](https://jameshwade.github.io/measure/dev/reference/step_measure_normalize_max.md)       | Divide by maximum value            |
| [`step_measure_normalize_range()`](https://jameshwade.github.io/measure/dev/reference/step_measure_normalize_range.md)   | Scale to 0-1 range                 |
| [`step_measure_normalize_vector()`](https://jameshwade.github.io/measure/dev/reference/step_measure_normalize_vector.md) | L2/Euclidean normalization         |
| [`step_measure_normalize_auc()`](https://jameshwade.github.io/measure/dev/reference/step_measure_normalize_auc.md)       | Divide by area under curve         |
| [`step_measure_normalize_peak()`](https://jameshwade.github.io/measure/dev/reference/step_measure_normalize_peak.md)     | Normalize by peak region (tunable) |

### Variable-wise Scaling

| Step                                                                                                             | Description            |
|------------------------------------------------------------------------------------------------------------------|------------------------|
| [`step_measure_center()`](https://jameshwade.github.io/measure/dev/reference/step_measure_center.md)             | Mean centering         |
| [`step_measure_scale_auto()`](https://jameshwade.github.io/measure/dev/reference/step_measure_scale_auto.md)     | Auto-scaling (z-score) |
| [`step_measure_scale_pareto()`](https://jameshwade.github.io/measure/dev/reference/step_measure_scale_pareto.md) | Pareto scaling         |
| [`step_measure_scale_range()`](https://jameshwade.github.io/measure/dev/reference/step_measure_scale_range.md)   | Range scaling          |
| [`step_measure_scale_vast()`](https://jameshwade.github.io/measure/dev/reference/step_measure_scale_vast.md)     | VAST scaling           |

### Baseline Correction

| Step                                                                                                               | Description                 |
|--------------------------------------------------------------------------------------------------------------------|-----------------------------|
| [`step_measure_baseline_als()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_als.md)   | Asymmetric least squares    |
| [`step_measure_baseline_poly()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_poly.md) | Polynomial baseline fitting |
| [`step_measure_baseline_rf()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_rf.md)     | Rolling ball/LOESS baseline |
| [`step_measure_detrend()`](https://jameshwade.github.io/measure/dev/reference/step_measure_detrend.md)             | Polynomial detrending       |

### Reference Corrections

| Step                                                                                                                         | Description                         |
|------------------------------------------------------------------------------------------------------------------------------|-------------------------------------|
| [`step_measure_subtract_blank()`](https://jameshwade.github.io/measure/dev/reference/step_measure_subtract_blank.md)         | Blank/background subtraction        |
| [`step_measure_subtract_reference()`](https://jameshwade.github.io/measure/dev/reference/step_measure_subtract_reference.md) | Reference spectrum subtraction      |
| [`step_measure_ratio_reference()`](https://jameshwade.github.io/measure/dev/reference/step_measure_ratio_reference.md)       | Reference ratio with optional blank |

## Learning more

- [Getting
  Started](https://jameshwade.github.io/measure/articles/measure.html) -
  A comprehensive introduction to measure
- [Preprocessing
  Techniques](https://jameshwade.github.io/measure/articles/preprocessing.html) -
  Deep dive into available preprocessing methods

## Related packages

measure builds on the [tidymodels](https://www.tidymodels.org/)
ecosystem:

- [recipes](https://recipes.tidymodels.org/) - The foundation for
  preprocessing pipelines
- [parsnip](https://parsnip.tidymodels.org/) - Unified modeling
  interface
- [workflows](https://workflows.tidymodels.org/) - Bundle preprocessing
  and modeling
- [tune](https://tune.tidymodels.org/) - Hyperparameter tuning (works
  with measure’s tunable steps!)

For spectral analysis in R, you might also find these packages useful:

- [prospectr](https://github.com/l-ramirez-lopez/prospectr) - Spectral
  preprocessing functions
- [ChemoSpec](https://bryanhanson.github.io/ChemoSpec/) - Exploratory
  chemometrics
- [mdatools](https://mdatools.com/) - Multivariate data analysis

## Contributing

This package is under active development. Contributions are welcome!
Please see the [contributing
guidelines](https://jameshwade.github.io/measure/CONTRIBUTING.html).

## Code of Conduct

Please note that the measure project is released with a [Contributor
Code of
Conduct](https://jameshwade.github.io/measure/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.
