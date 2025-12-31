# Calculate Control Limits

Calculates control limits for quality control monitoring using Shewhart
rules and optionally EWMA or CUSUM statistics.

## Usage

``` r
measure_control_limits(
  data,
  response_col,
  group_col = NULL,
  type = c("shewhart", "ewma", "cusum"),
  n_sigma = 3,
  target = NULL,
  lambda = 0.2,
  k = 0.5,
  h = 5
)
```

## Arguments

- data:

  A data frame containing QC measurements.

- response_col:

  Name of the column containing QC values.

- group_col:

  Optional grouping column (e.g., for different QC levels).

- type:

  Type of control chart: `"shewhart"` (default), `"ewma"`, or `"cusum"`.

- n_sigma:

  Number of standard deviations for control limits. Default is 3.

- target:

  Optional target value. If NULL, calculated from data mean.

- lambda:

  EWMA smoothing parameter (0 \< lambda \<= 1). Default is 0.2.

- k:

  CUSUM slack parameter. Default is 0.5 (in sigma units).

- h:

  CUSUM decision interval. Default is 5 (in sigma units).

## Value

A `measure_control_limits` object containing:

- `center`: Center line (target or mean)

- `lcl`: Lower control limit

- `ucl`: Upper control limit

- `lwl`: Lower warning limit (2 sigma)

- `uwl`: Upper warning limit (2 sigma)

- `sigma`: Estimated standard deviation

- Additional statistics depending on chart type

## Details

### Shewhart Charts

Classic control charts with limits at mean +/- n\*sigma:

- UCL/LCL: Action limits (typically 3 sigma)

- UWL/LWL: Warning limits (typically 2 sigma)

### EWMA Charts

Exponentially weighted moving average, more sensitive to small shifts:

- Control limits narrow as more data is collected

- Lambda parameter controls weight of recent observations

### CUSUM Charts

Cumulative sum chart for detecting persistent shifts:

- Upper and lower CUSUM statistics track cumulative deviations

- Decision interval h determines sensitivity

## See also

[`measure_control_chart()`](https://jameshwade.github.io/measure/dev/reference/measure_control_chart.md),
[`measure_system_suitability()`](https://jameshwade.github.io/measure/dev/reference/measure_system_suitability.md)

Other control-charts:
[`measure_control_chart()`](https://jameshwade.github.io/measure/dev/reference/measure_control_chart.md),
[`measure_system_suitability()`](https://jameshwade.github.io/measure/dev/reference/measure_system_suitability.md)

## Examples

``` r
# Calculate Shewhart control limits
set.seed(123)
qc_data <- data.frame(
  run_order = 1:30,
  qc_value = rnorm(30, mean = 100, sd = 2)
)
limits <- measure_control_limits(qc_data, "qc_value")
print(limits)
#> measure_control_limits: shewhart chart
#> ──────────────────────────────────────────────────────────────────────────────── 
#> 
#>   n = 30 
#>   Center = 99.91 
#>   Sigma = 1.962 
#>   UCL (+3s) = 105.8 
#>   UWL (+2s) = 103.8 
#>   LWL (-2s) = 95.98 
#>   LCL (-3s) = 94.02 
#> 

# EWMA control limits
limits_ewma <- measure_control_limits(qc_data, "qc_value", type = "ewma")
```
