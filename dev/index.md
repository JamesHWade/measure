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

| Step                                                                                                                 | Description                                       |
|----------------------------------------------------------------------------------------------------------------------|---------------------------------------------------|
| [`step_measure_savitzky_golay()`](https://jameshwade.github.io/measure/dev/reference/step_measure_savitzky_golay.md) | Smoothing and/or differentiation                  |
| [`step_measure_snv()`](https://jameshwade.github.io/measure/dev/reference/step_measure_snv.md)                       | Standard Normal Variate normalization             |
| [`step_measure_msc()`](https://jameshwade.github.io/measure/dev/reference/step_measure_msc.md)                       | Multiplicative Scatter Correction                 |
| [`step_measure_emsc()`](https://jameshwade.github.io/measure/dev/reference/step_measure_emsc.md)                     | Extended MSC with wavelength-dependent correction |
| [`step_measure_osc()`](https://jameshwade.github.io/measure/dev/reference/step_measure_osc.md)                       | Orthogonal Signal Correction                      |

### Smoothing & Noise Reduction

| Step                                                                                                                   | Description                          |
|------------------------------------------------------------------------------------------------------------------------|--------------------------------------|
| [`step_measure_smooth_ma()`](https://jameshwade.github.io/measure/dev/reference/step_measure_smooth_ma.md)             | Moving average smoothing             |
| [`step_measure_smooth_median()`](https://jameshwade.github.io/measure/dev/reference/step_measure_smooth_median.md)     | Median filter (robust to spikes)     |
| [`step_measure_smooth_gaussian()`](https://jameshwade.github.io/measure/dev/reference/step_measure_smooth_gaussian.md) | Gaussian kernel smoothing            |
| [`step_measure_smooth_wavelet()`](https://jameshwade.github.io/measure/dev/reference/step_measure_smooth_wavelet.md)   | Wavelet denoising                    |
| [`step_measure_filter_fourier()`](https://jameshwade.github.io/measure/dev/reference/step_measure_filter_fourier.md)   | Fourier low-pass/high-pass filtering |
| [`step_measure_despike()`](https://jameshwade.github.io/measure/dev/reference/step_measure_despike.md)                 | Spike/outlier detection and removal  |

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

| Step                                                                                                                     | Description                                                    |
|--------------------------------------------------------------------------------------------------------------------------|----------------------------------------------------------------|
| [`step_measure_baseline_als()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_als.md)         | Asymmetric least squares                                       |
| [`step_measure_baseline_poly()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_poly.md)       | Polynomial baseline fitting                                    |
| [`step_measure_baseline_rf()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_rf.md)           | Rolling ball/LOESS baseline                                    |
| [`step_measure_baseline_rolling()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_rolling.md) | Rolling ball algorithm                                         |
| [`step_measure_baseline_airpls()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_airpls.md)   | Adaptive Iteratively Reweighted PLS                            |
| [`step_measure_baseline_arpls()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_arpls.md)     | Asymmetrically Reweighted PLS                                  |
| [`step_measure_baseline_snip()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_snip.md)       | SNIP (Statistics-sensitive Non-linear Iterative Peak-clipping) |
| [`step_measure_baseline_tophat()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_tophat.md)   | Top-hat morphological filter                                   |
| [`step_measure_baseline_morph()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_morph.md)     | Iterative morphological correction                             |
| [`step_measure_baseline_minima()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_minima.md)   | Local minima interpolation                                     |
| [`step_measure_baseline_auto()`](https://jameshwade.github.io/measure/dev/reference/step_measure_baseline_auto.md)       | Automatic method selection                                     |
| [`step_measure_detrend()`](https://jameshwade.github.io/measure/dev/reference/step_measure_detrend.md)                   | Polynomial detrending                                          |

### Reference Corrections

| Step                                                                                                                         | Description                         |
|------------------------------------------------------------------------------------------------------------------------------|-------------------------------------|
| [`step_measure_subtract_blank()`](https://jameshwade.github.io/measure/dev/reference/step_measure_subtract_blank.md)         | Blank/background subtraction        |
| [`step_measure_subtract_reference()`](https://jameshwade.github.io/measure/dev/reference/step_measure_subtract_reference.md) | Reference spectrum subtraction      |
| [`step_measure_ratio_reference()`](https://jameshwade.github.io/measure/dev/reference/step_measure_ratio_reference.md)       | Reference ratio with optional blank |

### Region Operations

| Step                                                                                                     | Description                                   |
|----------------------------------------------------------------------------------------------------------|-----------------------------------------------|
| [`step_measure_trim()`](https://jameshwade.github.io/measure/dev/reference/step_measure_trim.md)         | Keep measurements within specified x-range    |
| [`step_measure_exclude()`](https://jameshwade.github.io/measure/dev/reference/step_measure_exclude.md)   | Remove measurements within specified range(s) |
| [`step_measure_resample()`](https://jameshwade.github.io/measure/dev/reference/step_measure_resample.md) | Interpolate to new regular grid               |

### Alignment & Registration

| Step                                                                                                                   | Description                             |
|------------------------------------------------------------------------------------------------------------------------|-----------------------------------------|
| [`step_measure_align_shift()`](https://jameshwade.github.io/measure/dev/reference/step_measure_align_shift.md)         | Cross-correlation shift alignment       |
| [`step_measure_align_reference()`](https://jameshwade.github.io/measure/dev/reference/step_measure_align_reference.md) | Align to external reference spectrum    |
| [`step_measure_align_dtw()`](https://jameshwade.github.io/measure/dev/reference/step_measure_align_dtw.md)             | Dynamic Time Warping alignment          |
| [`step_measure_align_ptw()`](https://jameshwade.github.io/measure/dev/reference/step_measure_align_ptw.md)             | Parametric Time Warping                 |
| [`step_measure_align_cow()`](https://jameshwade.github.io/measure/dev/reference/step_measure_align_cow.md)             | Correlation Optimized Warping (tunable) |

### Quality Control

| Step                                                                                                             | Description                     |
|------------------------------------------------------------------------------------------------------------------|---------------------------------|
| [`step_measure_qc_snr()`](https://jameshwade.github.io/measure/dev/reference/step_measure_qc_snr.md)             | Calculate signal-to-noise ratio |
| [`step_measure_qc_saturated()`](https://jameshwade.github.io/measure/dev/reference/step_measure_qc_saturated.md) | Detect saturated measurements   |
| [`step_measure_qc_outlier()`](https://jameshwade.github.io/measure/dev/reference/step_measure_qc_outlier.md)     | Detect outlier samples          |
| [`step_measure_impute()`](https://jameshwade.github.io/measure/dev/reference/step_measure_impute.md)             | Interpolate missing values      |

### Peak Operations

| Step                                                                                                                     | Description                                         |
|--------------------------------------------------------------------------------------------------------------------------|-----------------------------------------------------|
| [`step_measure_peaks_detect()`](https://jameshwade.github.io/measure/dev/reference/step_measure_peaks_detect.md)         | Detect peaks using prominence or derivative methods |
| [`step_measure_peaks_integrate()`](https://jameshwade.github.io/measure/dev/reference/step_measure_peaks_integrate.md)   | Calculate peak areas                                |
| [`step_measure_peaks_filter()`](https://jameshwade.github.io/measure/dev/reference/step_measure_peaks_filter.md)         | Filter peaks by height, area, or count              |
| [`step_measure_peaks_deconvolve()`](https://jameshwade.github.io/measure/dev/reference/step_measure_peaks_deconvolve.md) | Deconvolve overlapping peaks                        |
| [`step_measure_peaks_to_table()`](https://jameshwade.github.io/measure/dev/reference/step_measure_peaks_to_table.md)     | Convert peaks to wide format for modeling           |

### SEC/GPC Analysis

| Step                                                                                                                   | Description                                  |
|------------------------------------------------------------------------------------------------------------------------|----------------------------------------------|
| [`step_measure_mw_averages()`](https://jameshwade.github.io/measure/dev/reference/step_measure_mw_averages.md)         | Calculate Mn, Mw, Mz, Mp, and dispersity     |
| [`step_measure_mw_distribution()`](https://jameshwade.github.io/measure/dev/reference/step_measure_mw_distribution.md) | Generate molecular weight distribution curve |
| [`step_measure_mw_fractions()`](https://jameshwade.github.io/measure/dev/reference/step_measure_mw_fractions.md)       | Calculate molecular weight fractions         |

### Feature Engineering

| Step                                                                                                       | Description                                      |
|------------------------------------------------------------------------------------------------------------|--------------------------------------------------|
| [`step_measure_integrals()`](https://jameshwade.github.io/measure/dev/reference/step_measure_integrals.md) | Calculate integrated areas for specified regions |
| [`step_measure_ratios()`](https://jameshwade.github.io/measure/dev/reference/step_measure_ratios.md)       | Calculate ratios between integrated regions      |
| [`step_measure_moments()`](https://jameshwade.github.io/measure/dev/reference/step_measure_moments.md)     | Calculate statistical moments from spectra       |
| [`step_measure_bin()`](https://jameshwade.github.io/measure/dev/reference/step_measure_bin.md)             | Reduce spectrum to fewer points via binning      |

### Data Augmentation

| Step                                                                                                               | Description                                |
|--------------------------------------------------------------------------------------------------------------------|--------------------------------------------|
| [`step_measure_augment_noise()`](https://jameshwade.github.io/measure/dev/reference/step_measure_augment_noise.md) | Add random noise for training augmentation |
| [`step_measure_augment_shift()`](https://jameshwade.github.io/measure/dev/reference/step_measure_augment_shift.md) | Random x-axis shifts for shift invariance  |
| [`step_measure_augment_scale()`](https://jameshwade.github.io/measure/dev/reference/step_measure_augment_scale.md) | Random intensity scaling                   |

### Drift & Batch Correction

| Step/Function                                                                                                          | Description                            |
|------------------------------------------------------------------------------------------------------------------------|----------------------------------------|
| [`step_measure_drift_qc_loess()`](https://jameshwade.github.io/measure/dev/reference/step_measure_drift_qc_loess.md)   | QC-RLSC drift correction using LOESS   |
| [`step_measure_drift_linear()`](https://jameshwade.github.io/measure/dev/reference/step_measure_drift_linear.md)       | Linear drift correction                |
| [`step_measure_drift_spline()`](https://jameshwade.github.io/measure/dev/reference/step_measure_drift_spline.md)       | Spline-based drift correction          |
| [`step_measure_qc_bracket()`](https://jameshwade.github.io/measure/dev/reference/step_measure_qc_bracket.md)           | QC bracketing interpolation            |
| [`step_measure_batch_reference()`](https://jameshwade.github.io/measure/dev/reference/step_measure_batch_reference.md) | Reference-based batch correction       |
| [`measure_detect_drift()`](https://jameshwade.github.io/measure/dev/reference/measure_detect_drift.md)                 | Detect significant drift in QC samples |

## Analytical Validation Functions

measure provides a comprehensive suite of functions for analytical
method validation, designed for compatibility with ICH Q2(R2), ISO
17025, and similar regulatory frameworks.

### Calibration & Quantitation

| Function                                                                                                                                                                    | Description                                        |
|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------|----------------------------------------------------|
| [`measure_calibration_fit()`](https://jameshwade.github.io/measure/dev/reference/measure_calibration_fit.md)                                                                | Fit weighted calibration curves (linear/quadratic) |
| [`measure_calibration_predict()`](https://jameshwade.github.io/measure/dev/reference/measure_calibration_predict.md)                                                        | Predict concentrations with uncertainty            |
| [`measure_calibration_verify()`](https://jameshwade.github.io/measure/dev/reference/measure_calibration_verify.md)                                                          | Continuing calibration verification                |
| [`measure_lod()`](https://jameshwade.github.io/measure/dev/reference/measure_lod.md) / [`measure_loq()`](https://jameshwade.github.io/measure/dev/reference/measure_loq.md) | Detection and quantitation limits                  |

### Precision & Accuracy

| Function                                                                                                                   | Description                                    |
|----------------------------------------------------------------------------------------------------------------------------|------------------------------------------------|
| [`measure_repeatability()`](https://jameshwade.github.io/measure/dev/reference/measure_repeatability.md)                   | Within-run precision                           |
| [`measure_intermediate_precision()`](https://jameshwade.github.io/measure/dev/reference/measure_intermediate_precision.md) | Between-run precision with variance components |
| [`measure_reproducibility()`](https://jameshwade.github.io/measure/dev/reference/measure_reproducibility.md)               | Between-lab precision                          |
| [`measure_gage_rr()`](https://jameshwade.github.io/measure/dev/reference/measure_gage_rr.md)                               | Gage R&R / Measurement System Analysis         |
| [`measure_accuracy()`](https://jameshwade.github.io/measure/dev/reference/measure_accuracy.md)                             | Bias, recovery, and accuracy assessment        |
| [`measure_linearity()`](https://jameshwade.github.io/measure/dev/reference/measure_linearity.md)                           | Linearity with lack-of-fit testing             |
| [`measure_carryover()`](https://jameshwade.github.io/measure/dev/reference/measure_carryover.md)                           | Carryover evaluation                           |

### Uncertainty & Quality Control

| Function                                                                                                           | Description                        |
|--------------------------------------------------------------------------------------------------------------------|------------------------------------|
| [`measure_uncertainty_budget()`](https://jameshwade.github.io/measure/dev/reference/measure_uncertainty_budget.md) | ISO GUM uncertainty budgets        |
| [`measure_uncertainty()`](https://jameshwade.github.io/measure/dev/reference/measure_uncertainty.md)               | Combined and expanded uncertainty  |
| [`measure_control_limits()`](https://jameshwade.github.io/measure/dev/reference/measure_control_limits.md)         | Shewhart, EWMA, or CUSUM limits    |
| [`measure_control_chart()`](https://jameshwade.github.io/measure/dev/reference/measure_control_chart.md)           | Westgard multi-rule control charts |
| [`measure_system_suitability()`](https://jameshwade.github.io/measure/dev/reference/measure_system_suitability.md) | System suitability testing         |

### Criteria System

| Function                                                                                             | Description                    |
|------------------------------------------------------------------------------------------------------|--------------------------------|
| [`measure_criteria()`](https://jameshwade.github.io/measure/dev/reference/measure_criteria.md)       | Define acceptance criteria     |
| [`measure_assess()`](https://jameshwade.github.io/measure/dev/reference/measure_assess.md)           | Evaluate data against criteria |
| [`criteria_bioanalytical()`](https://jameshwade.github.io/measure/dev/reference/criteria_presets.md) | FDA/EMA bioanalytical presets  |
| [`criteria_ich_q2()`](https://jameshwade.github.io/measure/dev/reference/criteria_presets.md)        | ICH Q2 validation presets      |

## Learning more

- [Getting
  Started](https://jameshwade.github.io/measure/articles/measure.html) -
  A comprehensive introduction to measure
- [Preprocessing
  Techniques](https://jameshwade.github.io/measure/articles/preprocessing.html) -
  Deep dive into available preprocessing methods
- [Analytical
  Validation](https://jameshwade.github.io/measure/articles/validation.html) -
  Calibration, uncertainty, and method validation

## Datasets

### Included Datasets

The package includes datasets for examples and testing:

| Dataset              | Technique | Samples | Description                                                |
|----------------------|-----------|---------|------------------------------------------------------------|
| `meats_long`         | NIR       | 215     | NIR transmittance spectra of meat samples (from modeldata) |
| `bioreactors_small`  | Raman     | 210     | Raman spectra from 15 small-scale bioreactors              |
| `bioreactors_large`  | Raman     | 42      | Raman spectra from 3 large-scale bioreactors               |
| `hplc_chromatograms` | HPLC-UV   | 20      | Simulated HPLC chromatograms with 5 compounds              |
| `sec_chromatograms`  | SEC/GPC   | 10      | Simulated SEC chromatograms (5 standards + 5 polymers)     |
| `sec_calibration`    | SEC/GPC   | 5       | Calibration standards for molecular weight curves          |
| `maldi_spectra`      | MALDI-TOF | 16      | Simulated mass spectra (4 groups × 4 replicates)           |

``` r
# Load datasets
data(meats_long)
data(glucose_bioreactors)  # loads bioreactors_small and bioreactors_large
data(hplc_chromatograms)
data(sec_chromatograms)
data(sec_calibration)
data(maldi_spectra)
```

### External Data Sources

For additional test data beyond what’s included with measure, these
sources provide publicly available analytical measurement data:

**R Packages with Spectral Data:**

| Package                                                   | Dataset   | Technique | Description                            |
|-----------------------------------------------------------|-----------|-----------|----------------------------------------|
| [modeldata](https://modeldata.tidymodels.org/)            | `meats`   | NIR       | Meat composition (wide format version) |
| [prospectr](https://github.com/l-ramirez-lopez/prospectr) | `NIRsoil` | NIR       | Soil analysis with 825 samples         |
| [ChemoSpec](https://bryanhanson.github.io/ChemoSpec/)     | Various   | IR, NMR   | Multiple spectroscopy datasets         |
| [hyperSpec](http://hyperspec.r-forge.r-project.org/)      | Various   | Raman, IR | Hyperspectral data examples            |

``` r
# Example: Load NIRsoil from prospectr
# install.packages("prospectr")
data(NIRsoil, package = "prospectr")
```

**Online Repositories:**

- [Mendeley Data](https://data.mendeley.com) - Search “spectroscopy”,
  “chromatography”, or “mass spectrometry”
- [Zenodo](https://zenodo.org) - Open science data repository
- [Kaggle Datasets](https://www.kaggle.com/datasets) -
  Community-contributed datasets
- [NIST Chemistry WebBook](https://webbook.nist.gov/chemistry/) -
  Reference spectra (IR, MS, UV-Vis)
- [SDBS](https://sdbs.db.aist.go.jp/) - Spectral Database for Organic
  Compounds (NMR, IR, MS)

**Domain-Specific Databases:**

| Database                      | Data Type            | URL                                    |
|-------------------------------|----------------------|----------------------------------------|
| MassBank                      | Mass spectra         | <https://massbank.eu/MassBank/>        |
| HMDB                          | NMR, MS metabolomics | <https://hmdb.ca/>                     |
| NMRShiftDB                    | NMR spectra          | <https://nmrshiftdb.nmr.uni-koeln.de/> |
| Crystallography Open Database | XRD patterns         | <https://www.crystallography.net/cod/> |

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
