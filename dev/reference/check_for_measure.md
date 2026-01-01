# Check for Measure Columns

Validates that the data contains measure columns (columns with class
`measure_list`). Aborts with an informative error if no measure columns
are found.

## Usage

``` r
check_for_measure(x)
```

## Arguments

- x:

  A data frame or tibble to check.

## Value

Invisibly returns the names of measure columns found.
