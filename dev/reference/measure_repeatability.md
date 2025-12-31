# Repeatability (Within-Run Precision)

Calculates repeatability statistics for replicate measurements performed
under identical conditions (same operator, instrument, short time
interval).

## Usage

``` r
measure_repeatability(data, response_col, group_col = NULL, conf_level = 0.95)
```

## Arguments

- data:

  A data frame containing replicate measurements.

- response_col:

  Name of the column containing the response values.

- group_col:

  Optional name of a grouping column (e.g., concentration level). If
  provided, repeatability is calculated within each group.

- conf_level:

  Confidence level for intervals. Default is 0.95.

## Value

A `measure_precision` object containing:

- `mean`: Mean of the replicates

- `sd`: Standard deviation

- `cv`: Coefficient of variation (%)

- `n`: Number of replicates

- `se`: Standard error

- `ci_lower`, `ci_upper`: Confidence interval for the mean

## Details

Repeatability represents the precision of a method under constant
conditions over a short time interval. It is typically assessed using at
least 6 replicates of a sample at each concentration level of interest.

The coefficient of variation (CV) is reported as a percentage:
`CV = 100 * SD / mean`

## See also

[`measure_intermediate_precision()`](https://jameshwade.github.io/measure/dev/reference/measure_intermediate_precision.md),
[`measure_reproducibility()`](https://jameshwade.github.io/measure/dev/reference/measure_reproducibility.md)

Other precision:
[`measure_gage_rr()`](https://jameshwade.github.io/measure/dev/reference/measure_gage_rr.md),
[`measure_intermediate_precision()`](https://jameshwade.github.io/measure/dev/reference/measure_intermediate_precision.md),
[`measure_reproducibility()`](https://jameshwade.github.io/measure/dev/reference/measure_reproducibility.md)

## Examples

``` r
# Simple repeatability from replicate measurements
data <- data.frame(
  sample_id = rep("QC1", 10),
  concentration = rnorm(10, mean = 100, sd = 2)
)
measure_repeatability(data, "concentration")
#> measure_precision: repeatability 
#> ──────────────────────────────────────────────────────────────────────────────── 
#>   n = 10 
#>   Mean = 99.11 
#>   SD = 2.098 
#>   CV = 2.1 %
#>   95% CI: [97.61, 100.6]

# Repeatability at multiple concentration levels
data <- data.frame(
  level = rep(c("low", "mid", "high"), each = 6),
  concentration = c(
    rnorm(6, 10, 0.5),
    rnorm(6, 50, 2),
    rnorm(6, 100, 4)
  )
)
measure_repeatability(data, "concentration", group_col = "level")
#> measure_precision: repeatability 
#> ──────────────────────────────────────────────────────────────────────────────── 
#> 
#> Group: low 
#>   n = 6 
#>   Mean = 10.04 
#>   SD = 0.1261 
#>   CV = 1.3 %
#>   95% CI: [9.905, 10.17]
#> 
#> Group: mid 
#>   n = 6 
#>   Mean = 50.17 
#>   SD = 2.861 
#>   CV = 5.7 %
#>   95% CI: [47.17, 53.17]
#> 
#> Group: high 
#>   n = 6 
#>   Mean = 101.2 
#>   SD = 4.482 
#>   CV = 4.4 %
#>   95% CI: [96.46, 105.9]
```
