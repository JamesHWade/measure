# Assess Deconvolution Quality

Calculates comprehensive quality metrics for a peak deconvolution fit,
including goodness-of-fit statistics, information criteria, per-peak
quality, and residual diagnostics.

## Usage

``` r
assess_deconv_quality(x, y, result, models)
```

## Arguments

- x:

  Numeric vector of x-axis values.

- y:

  Numeric vector of observed y-axis values.

- result:

  Deconvolution result list from
  [`optimize_deconvolution()`](https://jameshwade.github.io/measure/dev/reference/optimize_deconvolution.md).

- models:

  List of `peak_model` objects used in deconvolution.

## Value

A list of class `deconv_quality` containing:

- `goodness_of_fit`: R-squared, RMSE, MAE, chi-squared

- `information_criteria`: AIC, BIC, AICc

- `peak_quality`: Per-peak purity, overlap, area

- `residual_analysis`: Autocorrelation, heteroscedasticity, normality
  tests

- `overall_grade`: Letter grade (A/B/C/D/F)

- `convergence_info`: Optimization convergence details

## See also

Other peak-deconvolution:
[`add_param_jitter()`](https://jameshwade.github.io/measure/dev/reference/add_param_jitter.md),
[`check_quality_gates()`](https://jameshwade.github.io/measure/dev/reference/check_quality_gates.md),
[`initialize_peak_params()`](https://jameshwade.github.io/measure/dev/reference/initialize_peak_params.md),
[`optimize_deconvolution()`](https://jameshwade.github.io/measure/dev/reference/optimize_deconvolution.md)

## Examples

``` r
# Create synthetic data and fit
x <- seq(0, 20, by = 0.1)
true_y <- 1.5 * exp(-0.5 * ((x - 8) / 1)^2) +
  0.8 * exp(-0.5 * ((x - 12) / 1.5)^2)
y <- true_y + rnorm(length(x), sd = 0.05)

models <- list(gaussian_peak_model(), gaussian_peak_model())
init_params <- list(
  list(height = 1.2, center = 7.5, width = 1.2),
  list(height = 0.6, center = 12.5, width = 1.8)
)

result <- optimize_deconvolution(x, y, models, init_params)
quality <- assess_deconv_quality(x, y, result, models)
print(quality)
#> Deconvolution Quality Assessment
#> ================================
#> 
#> Overall Grade: B (good)
#> 
#> Goodness of Fit:
#>   R-squared: 0.9868
#>   RMSE:      0.0510
#>   MAE:       0.0400
#> 
#> Optimization:
#>   Converged:   Yes
#>   Iterations:  20
#>   Optimizer:   lbfgsb
#>   Final SSE:   0.5231
```
