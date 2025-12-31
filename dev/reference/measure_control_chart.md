# Generate Control Chart

Creates a control chart with optional multi-rule (Westgard) violation
detection.

## Usage

``` r
measure_control_chart(
  data,
  response_col,
  order_col,
  limits = NULL,
  rules = c("1_3s", "2_2s", "R_4s", "4_1s", "10x"),
  group_col = NULL
)
```

## Arguments

- data:

  A data frame containing QC measurements.

- response_col:

  Name of the column containing QC values.

- order_col:

  Name of the column containing run order/sequence.

- limits:

  Optional `measure_control_limits` object. If NULL, calculated from the
  data.

- rules:

  Character vector of Westgard rules to apply. Default is
  `c("1_3s", "2_2s", "R_4s", "4_1s", "10x")`.

- group_col:

  Optional grouping column.

## Value

A `measure_control_chart` object containing:

- `data`: The input data with added violation flags

- `limits`: The control limits used

- `violations`: Summary of rule violations

- `rules_applied`: Which rules were checked

## Details

### Westgard Rules

The function supports common Westgard multi-rules:

- **1:3s**: One point beyond 3 sigma (action required)

- **2:2s**: Two consecutive points beyond 2 sigma (warning)

- **R:4s**: Range of two consecutive points \> 4 sigma

- **4:1s**: Four consecutive points beyond 1 sigma (same side)

- **10x**: Ten consecutive points on same side of mean

### Interpretation

- Violations are flagged with the specific rule that was triggered

- Multiple rules can be triggered by the same point

- A run is considered "in control" if no violations are detected

## See also

[`measure_control_limits()`](https://jameshwade.github.io/measure/dev/reference/measure_control_limits.md),
[`autoplot.measure_control_chart()`](https://jameshwade.github.io/measure/dev/reference/autoplot.measure_control_chart.md)

Other control-charts:
[`measure_control_limits()`](https://jameshwade.github.io/measure/dev/reference/measure_control_limits.md),
[`measure_system_suitability()`](https://jameshwade.github.io/measure/dev/reference/measure_system_suitability.md)

## Examples

``` r
# Generate control chart with Westgard rules
set.seed(123)
qc_data <- data.frame(
  run_order = 1:50,
  qc_value = c(rnorm(45, 100, 2), rnorm(5, 106, 2))  # Last 5 shifted
)
chart <- measure_control_chart(qc_data, "qc_value", "run_order")
print(chart)
#> measure_control_chart
#> ──────────────────────────────────────────────────────────────────────────────── 
#> 
#> Observations: 50 
#> Rules applied: 1_3s, 2_2s, R_4s, 4_1s, 10x 
#> Violations detected: 5 
#> 
#> Status: OUT OF CONTROL
#> 
#> Violation summary:
#> # A tibble: 5 × 3
#>   run_order qc_value violation     
#>       <int>    <dbl> <chr>         
#> 1        46     104. 4:1s          
#> 2        47     105. 4:1s 4:1s     
#> 3        48     105. 4:1s 4:1s     
#> 4        49     108. 2:2s 4:1s 4:1s
#> 5        50     106. 2:2s 4:1s     
```
