# Add Jitter to Parameters for Multi-Start Optimization

Perturbs initialized parameters to create diverse starting points for
multi-start optimization strategies.

## Usage

``` r
add_param_jitter(params_list, scale = 0.1, method = c("gaussian", "uniform"))
```

## Arguments

- params_list:

  List of parameter lists (one per peak).

- scale:

  Jitter scale (fraction of parameter value).

- method:

  Jitter method: `"gaussian"` or `"uniform"`.

## Value

List of jittered parameter lists.

## See also

Other peak-deconvolution:
[`assess_deconv_quality()`](https://jameshwade.github.io/measure/dev/reference/assess_deconv_quality.md),
[`check_quality_gates()`](https://jameshwade.github.io/measure/dev/reference/check_quality_gates.md),
[`initialize_peak_params()`](https://jameshwade.github.io/measure/dev/reference/initialize_peak_params.md),
[`optimize_deconvolution()`](https://jameshwade.github.io/measure/dev/reference/optimize_deconvolution.md)
