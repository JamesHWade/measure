# Check for Measure Columns

Validates that the data contains measure columns (columns with class
`measure_list`). Aborts with an informative error if no measure columns
are found.

## Usage

``` r
check_for_measure(x, call = rlang::caller_env())
```

## Arguments

- x:

  A data frame or tibble to check.

- call:

  The execution environment of a currently running function, e.g.
  [`rlang::caller_env()`](https://rlang.r-lib.org/reference/stack.html).
  Used to report the caller in error messages.

## Value

Invisibly returns the names of measure columns found.
