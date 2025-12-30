# Carryover Assessment

Evaluates carryover by analyzing blank samples run after
high-concentration samples.

## Usage

``` r
measure_carryover(
  data,
  response_col,
  sample_type_col,
  run_order_col,
  blank_type = "blank",
  high_type = "high",
  threshold = 20,
  lloq = NULL
)
```

## Arguments

- data:

  A data frame containing the run sequence with blanks after highs.

- response_col:

  Name of the column containing response values.

- sample_type_col:

  Name of the column identifying sample types.

- run_order_col:

  Name of the column containing run order.

- blank_type:

  Value identifying blank samples. Default is `"blank"`.

- high_type:

  Value identifying high-concentration samples. Default is `"high"`.

- threshold:

  Carryover threshold as percentage of LLOQ or high response. Default is
  20 (meaning 20% of LLOQ).

- lloq:

  Optional LLOQ value for threshold calculation.

## Value

A `measure_carryover` object containing:

- `blank_responses`: Response values in blanks after high samples

- `mean_blank`: Mean blank response

- `max_blank`: Maximum blank response

- `high_responses`: High sample responses

- `carryover_pct`: Carryover as percentage of high or LLOQ

- `pass`: Whether carryover is within acceptable limits

## Details

Carryover is the appearance of analyte in a blank sample due to
contamination from a previous high-concentration sample. It is typically
assessed by analyzing blank samples immediately after the highest
calibration standard or QC sample.

### Acceptance Criteria (ICH M10)

Carryover in the blank sample following the high concentration should
not exceed:

- 20% of the LLOQ (for the analyte)

- 5% of the internal standard response

## See also

[`measure_accuracy()`](https://jameshwade.github.io/measure/dev/reference/measure_accuracy.md),
[`measure_system_suitability()`](https://jameshwade.github.io/measure/dev/reference/measure_system_suitability.md)

Other accuracy:
[`measure_accuracy()`](https://jameshwade.github.io/measure/dev/reference/measure_accuracy.md),
[`measure_linearity()`](https://jameshwade.github.io/measure/dev/reference/measure_linearity.md)

## Examples

``` r
# Carryover assessment
data <- data.frame(
  run_order = 1:10,
  sample_type = c("std", "std", "std", "high", "blank",
                  "qc", "qc", "high", "blank", "std"),
  response = c(100, 500, 1000, 5000, 5, 500, 510, 4900, 8, 100)
)

result <- measure_carryover(
  data,
  response_col = "response",
  sample_type_col = "sample_type",
  run_order_col = "run_order",
  lloq = 50
)
print(result)
#> measure_carryover
#> ──────────────────────────────────────────────────────────────────────────────── 
#> 
#> Evaluation:
#>   High-blank pairs: 2 
#>   Mean high response: 4950 
#>   Mean blank response: 6.5 
#>   Max blank response: 8 
#> 
#> Carryover:
#>   Reference (LLOQ):50
#>   Carryover: 16 % of LLOQ 
#>   Threshold: 20 %
#> 
#> Result: PASS
```
