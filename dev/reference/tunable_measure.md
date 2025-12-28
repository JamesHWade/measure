# tunable methods for measure

These functions define what parameters *can* be tuned for specific
steps. They also define the recommended objects from the `dials` package
that can be used to generate new parameter values and other
characteristics.

## Usage

``` r
# S3 method for class 'step_measure_align_cow'
tunable(x, ...)

# S3 method for class 'step_measure_baseline_custom'
tunable(x, ...)

# S3 method for class 'step_measure_baseline_airpls'
tunable(x, ...)

# S3 method for class 'step_measure_baseline_arpls'
tunable(x, ...)

# S3 method for class 'step_measure_baseline_py'
tunable(x, ...)

# S3 method for class 'step_measure_savitzky_golay'
tunable(x, ...)

# S3 method for class 'step_measure_baseline_als'
tunable(x, ...)

# S3 method for class 'step_measure_baseline_poly'
tunable(x, ...)

# S3 method for class 'step_measure_baseline_rf'
tunable(x, ...)

# S3 method for class 'step_measure_detrend'
tunable(x, ...)

# S3 method for class 'step_measure_normalize_peak'
tunable(x, ...)

# S3 method for class 'step_measure_derivative'
tunable(x, ...)

# S3 method for class 'step_measure_derivative_gap'
tunable(x, ...)

# S3 method for class 'step_measure_baseline_airpls'
tunable(x, ...)

# S3 method for class 'step_measure_baseline_arpls'
tunable(x, ...)

# S3 method for class 'step_measure_smooth_ma'
tunable(x, ...)

# S3 method for class 'step_measure_smooth_median'
tunable(x, ...)

# S3 method for class 'step_measure_smooth_gaussian'
tunable(x, ...)

# S3 method for class 'step_measure_filter_fourier'
tunable(x, ...)

# S3 method for class 'step_measure_despike'
tunable(x, ...)

# S3 method for class 'step_measure_align_shift'
tunable(x, ...)

# S3 method for class 'step_measure_qc_outlier'
tunable(x, ...)
```

## Arguments

- x:

  A recipe step object

- ...:

  Not used.

## Value

A tibble object.
