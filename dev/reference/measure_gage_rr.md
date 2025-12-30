# Gage R&R (Measurement System Analysis)

Performs a Gage Repeatability and Reproducibility study to assess
measurement system variation.

## Usage

``` r
measure_gage_rr(
  data,
  response_col,
  part_col,
  operator_col,
  tolerance = NULL,
  conf_level = 0.95,
  k = 5.15
)
```

## Arguments

- data:

  A data frame containing Gage R&R study data.

- response_col:

  Name of the column containing the measurements.

- part_col:

  Name of the column identifying parts/samples.

- operator_col:

  Name of the column identifying operators/analysts.

- tolerance:

  Optional specification tolerance for calculating %Study variation and
  %Tolerance.

- conf_level:

  Confidence level. Default is 0.95.

- k:

  Multiplier for study variation calculation. Default is 5.15 (99%).

## Value

A `measure_gage_rr` object containing:

- Variance components (Repeatability, Reproducibility, Part-to-Part)

- %Contribution of each component

- %Study Variation (using k \* sigma)

- %Tolerance (if tolerance provided)

- Number of distinct categories (ndc)

## Details

Gage R&R decomposes total measurement variation into:

- **Repeatability (EV)**: Equipment variation - variability from
  repeated measurements by the same operator on the same part

- **Reproducibility (AV)**: Appraiser variation - variability between
  operators measuring the same parts

- **Part-to-Part (PV)**: True variation between parts

### Acceptance Criteria (typical guidelines)

- %R&R \< 10%: Measurement system acceptable

- %R&R 10-30%: Measurement system may be acceptable depending on
  application

- %R&R \> 30%: Measurement system needs improvement

The number of distinct categories (ndc) should be \>= 5 for a capable
measurement system.

## See also

[`measure_repeatability()`](https://jameshwade.github.io/measure/dev/reference/measure_repeatability.md),
[`measure_intermediate_precision()`](https://jameshwade.github.io/measure/dev/reference/measure_intermediate_precision.md)

Other precision:
[`measure_intermediate_precision()`](https://jameshwade.github.io/measure/dev/reference/measure_intermediate_precision.md),
[`measure_repeatability()`](https://jameshwade.github.io/measure/dev/reference/measure_repeatability.md),
[`measure_reproducibility()`](https://jameshwade.github.io/measure/dev/reference/measure_reproducibility.md)

## Examples

``` r
# Gage R&R study with 10 parts, 3 operators, 2 replicates each
set.seed(123)
data <- expand.grid(
  part = 1:10,
  operator = c("A", "B", "C"),
  replicate = 1:2
)
data$measurement <- 50 +
  (data$part - 5) * 2 +  # Part-to-part variation
  ifelse(data$operator == "A", 0.5,
         ifelse(data$operator == "B", -0.3, 0)) +  # Operator effect
  rnorm(nrow(data), 0, 0.5)  # Repeatability

result <- measure_gage_rr(
  data,
  response_col = "measurement",
  part_col = "part",
  operator_col = "operator",
  tolerance = 20
)
print(result)
#> measure_gage_rr: Measurement System Analysis
#> ──────────────────────────────────────────────────────────────────────────────── 
#> 
#> Study design:
#>   Parts: 10 
#>   Operators: 3 
#>   Replicates: 2 
#> 
#> Variance Components:
#>   Repeatability: 0.2381 (0.6% contribution)
#>   Reproducibility: 0.1875 (0.5% contribution)
#>   Total R&R: 0.4256 (1% contribution)
#>   Part-to-Part: 36.28 (99% contribution)
#> 
#> % Study Variation:
#>   Repeatability: 8%
#>   Reproducibility: 7%
#>   Total R&R: 11%
#>   Part-to-Part: 99%
#> 
#> % Tolerance:
#>   Repeatability: 13%
#>   Reproducibility: 11%
#>   Total R&R: 17%
#>   Part-to-Part: 155%
#> 
#> Number of Distinct Categories (ndc): 13 
#> 
#> Assessment:
#>   Measurement system MAY BE ACCEPTABLE (%R&R 10-30%)
```
