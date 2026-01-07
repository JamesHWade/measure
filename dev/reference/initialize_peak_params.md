# Smart Parameter Initialization for Peak Deconvolution

Initializes peak model parameters using actual peak properties from the
data rather than naive guesses, improving optimization convergence.

## Usage

``` r
initialize_peak_params(
  x,
  y,
  n_peaks,
  models,
  peak_indices = NULL,
  smooth = TRUE,
  smooth_span = 0.05
)
```

## Arguments

- x:

  Numeric vector of x-axis values.

- y:

  Numeric vector of y-axis values.

- n_peaks:

  Number of peaks to initialize.

- models:

  List of `peak_model` objects (one per peak).

- peak_indices:

  Optional integer vector of peak indices (if already known).

- smooth:

  Logical. If `TRUE`, smooth data before peak detection.

- smooth_span:

  Smoothing span for LOESS (if `smooth = TRUE`).

## Value

List of initialized parameter lists (one per peak).

## See also

Other peak-deconvolution:
[`add_param_jitter()`](https://jameshwade.github.io/measure/dev/reference/add_param_jitter.md),
[`assess_deconv_quality()`](https://jameshwade.github.io/measure/dev/reference/assess_deconv_quality.md),
[`check_quality_gates()`](https://jameshwade.github.io/measure/dev/reference/check_quality_gates.md),
[`optimize_deconvolution()`](https://jameshwade.github.io/measure/dev/reference/optimize_deconvolution.md)

## Examples

``` r
# Create synthetic data with two peaks
x <- seq(0, 20, by = 0.1)
y <- 1.5 * exp(-0.5 * ((x - 8) / 1)^2) +
  0.8 * exp(-0.5 * ((x - 12) / 1.5)^2)

models <- list(gaussian_peak_model(), gaussian_peak_model())
init_params <- initialize_peak_params(x, y, n_peaks = 2, models = models)
```
