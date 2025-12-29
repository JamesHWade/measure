# Get Column Summary by Type

Summarizes columns by their detected type, useful for understanding the
structure of analytical datasets.

## Usage

``` r
measure_column_summary(data, patterns = measure_column_patterns)
```

## Arguments

- data:

  A data frame to analyze.

- patterns:

  Named list of regex patterns. Defaults to `measure_column_patterns`.

## Value

A tibble summarizing each detected type:

- type:

  Column type

- n_columns:

  Number of columns of this type

- example_cols:

  First 3 column names of this type

## Examples

``` r
df <- data.frame(
  id = 1:5,
  wn_1000 = rnorm(5), wn_1001 = rnorm(5), wn_1002 = rnorm(5),
  concentration = rnorm(5)
)
measure_column_summary(df)
#> # A tibble: 2 × 3
#>   type       n_columns example_cols             
#>   <chr>          <int> <chr>                    
#> 1 wavenumber         3 wn_1000, wn_1001, wn_1002
#> 2 other              2 id, concentration        
```
