# Optimize Peak Deconvolution

Finds optimal parameters for a set of peak models by minimizing the sum
of squared residuals between the observed and fitted values.

## Usage

``` r
optimize_deconvolution(
  x,
  y,
  models,
  init_params,
  optimizer = "auto",
  max_iter = 1000L,
  tol = 1e-06,
  constrain_positions = TRUE,
  ...
)
```

## Arguments

- x:

  Numeric vector of x-axis values (e.g., retention time, wavelength).

- y:

  Numeric vector of observed y-axis values.

- models:

  List of `peak_model` objects, one per peak.

- init_params:

  List of initial parameter lists, one per peak.

- optimizer:

  Optimization method: `"auto"`, `"lbfgsb"`, `"multistart"`, or
  `"nelder_mead"`.

- max_iter:

  Maximum number of iterations.

- tol:

  Convergence tolerance.

- constrain_positions:

  Logical. If `TRUE`, enforce that peak centers maintain their relative
  ordering.

- ...:

  Additional arguments passed to specific optimizers.

## Value

A list containing:

- `parameters`: List of optimized parameter lists

- `fitted_values`: Numeric vector of fitted y values

- `residuals`: Numeric vector of residuals

- `convergence`: Logical indicating convergence

- `n_iterations`: Number of iterations used

- `final_value`: Final objective function value (SSE)

- `optimizer`: Name of optimizer used

- `elapsed_time`: Optimization time in seconds

## See also

Other peak-deconvolution:
[`add_param_jitter()`](https://jameshwade.github.io/measure/dev/reference/add_param_jitter.md),
[`assess_deconv_quality()`](https://jameshwade.github.io/measure/dev/reference/assess_deconv_quality.md),
[`check_quality_gates()`](https://jameshwade.github.io/measure/dev/reference/check_quality_gates.md),
[`initialize_peak_params()`](https://jameshwade.github.io/measure/dev/reference/initialize_peak_params.md)

## Examples

``` r
# Create synthetic data with two overlapping Gaussian peaks
x <- seq(0, 20, by = 0.1)
true_y <- 1.5 * exp(-0.5 * ((x - 8) / 1)^2) +
  0.8 * exp(-0.5 * ((x - 12) / 1.5)^2)
y <- true_y + rnorm(length(x), sd = 0.05)

# Set up models and initial guesses
models <- list(gaussian_peak_model(), gaussian_peak_model())
init_params <- list(
  list(height = 1.2, center = 7.5, width = 1.2),
  list(height = 0.6, center = 12.5, width = 1.8)
)

# Optimize
result <- optimize_deconvolution(x, y, models, init_params)
print(result$parameters)
#> [[1]]
#> [[1]]$height
#> [1] 1.501288
#> 
#> [[1]]$center
#> [1] 7.999975
#> 
#> [[1]]$width
#> [1] 0.9969704
#> 
#> 
#> [[2]]
#> [[2]]$height
#> [1] 0.7898916
#> 
#> [[2]]$center
#> [1] 12.01145
#> 
#> [[2]]$width
#> [1] 1.526418
#> 
#> 
```
