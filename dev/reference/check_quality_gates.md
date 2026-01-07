# Check if Fit Passes Quality Gates

Evaluates a deconvolution quality assessment against configurable
thresholds to determine if the fit is acceptable.

## Usage

``` r
check_quality_gates(quality, reject_threshold = 0.85, warn_threshold = 0.95)
```

## Arguments

- quality:

  A `deconv_quality` object from
  [`assess_deconv_quality()`](https://jameshwade.github.io/measure/dev/reference/assess_deconv_quality.md).

- reject_threshold:

  Minimum R-squared to accept (default 0.85).

- warn_threshold:

  R-squared threshold for warning (default 0.95).

## Value

A list with:

- `status`: `"pass"`, `"warn"`, or `"reject"`

- `pass`, `warn`, `reject`: Logical flags

- `messages`: Character vector of issues found

- `grade`: Overall quality grade

## See also

Other peak-deconvolution:
[`add_param_jitter()`](https://jameshwade.github.io/measure/dev/reference/add_param_jitter.md),
[`assess_deconv_quality()`](https://jameshwade.github.io/measure/dev/reference/assess_deconv_quality.md),
[`initialize_peak_params()`](https://jameshwade.github.io/measure/dev/reference/initialize_peak_params.md),
[`optimize_deconvolution()`](https://jameshwade.github.io/measure/dev/reference/optimize_deconvolution.md)
