# Accuracy Assessment

Calculates accuracy metrics including bias, recovery, and confidence
intervals for method validation.

## Usage

``` r
measure_accuracy(
  data,
  measured_col,
  reference_col,
  group_col = NULL,
  conf_level = 0.95
)
```

## Arguments

- data:

  A data frame containing measured and reference values.

- measured_col:

  Name of the column containing measured values.

- reference_col:

  Name of the column containing reference/nominal values.

- group_col:

  Optional grouping column (e.g., concentration level).

- conf_level:

  Confidence level for intervals. Default is 0.95.

## Value

A `measure_accuracy` object containing:

- `n`: Number of observations

- `mean_measured`: Mean of measured values

- `mean_reference`: Mean of reference values

- `bias`: Absolute bias (measured - reference)

- `bias_pct`: Relative bias as percentage

- `recovery`: Recovery percentage (measured/reference \* 100)

- `recovery_ci_lower`, `recovery_ci_upper`: Confidence interval for
  recovery

## Details

Accuracy expresses the closeness of agreement between a measured value
and a reference value. It is typically assessed using:

- **Bias**: Systematic difference from the reference value

- **Recovery**: Percentage of the reference value that is measured

### ICH Q2 Requirements

Accuracy should be assessed at a minimum of 3 concentration levels
covering the specified range (typically 80-120% of the target).

## See also

[`measure_linearity()`](https://jameshwade.github.io/measure/dev/reference/measure_linearity.md),
[`measure_carryover()`](https://jameshwade.github.io/measure/dev/reference/measure_carryover.md)

Other accuracy:
[`measure_carryover()`](https://jameshwade.github.io/measure/dev/reference/measure_carryover.md),
[`measure_linearity()`](https://jameshwade.github.io/measure/dev/reference/measure_linearity.md)

## Examples

``` r
# Accuracy at multiple levels
set.seed(123)
data <- data.frame(
  level = rep(c("low", "mid", "high"), each = 5),
  nominal = rep(c(10, 50, 100), each = 5),
  measured = c(
    rnorm(5, 10.2, 0.3),
    rnorm(5, 49.5, 1.5),
    rnorm(5, 101, 3)
  )
)

result <- measure_accuracy(data, "measured", "nominal", group_col = "level")
print(result)
#> measure_accuracy
#> ──────────────────────────────────────────────────────────────────────────────── 
#> 
#> Group: low 
#>   n = 5 
#>   Mean measured = 10.26 
#>   Mean reference = 10 
#>   Bias = 0.2581 ( 2.6 %)
#>   Recovery = 103 %
#>   Recovery 95% CI: [100%, 106%]
#> 
#> Group: mid 
#>   n = 5 
#>   Mean measured = 49.43 
#>   Mean reference = 50 
#>   Bias = -0.5665 ( -1.1 %)
#>   Recovery = 99 %
#>   Recovery 95% CI: [95%, 103%]
#> 
#> Group: high 
#>   n = 5 
#>   Mean measured = 101.9 
#>   Mean reference = 100 
#>   Bias = 1.924 ( 1.9 %)
#>   Recovery = 102 %
#>   Recovery 95% CI: [100%, 104%]
#> 
```
