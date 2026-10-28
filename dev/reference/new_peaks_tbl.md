# Create a new peaks tibble

Creates a `peaks_tbl` with the standard peak columns. Additional columns
can carry technique-specific metadata and are kept with the returned
tibble.

## Usage

``` r
new_peaks_tbl(
  peak_id = integer(),
  location = double(),
  height = double(),
  left_base = double(),
  right_base = double(),
  area = double(),
  ...
)
```

## Arguments

- peak_id:

  Integer vector of peak identifiers.

- location:

  Numeric vector of peak locations (e.g., retention time).

- height:

  Numeric vector of peak heights.

- left_base:

  Numeric vector of left baseline locations.

- right_base:

  Numeric vector of right baseline locations.

- area:

  Numeric vector of peak areas (can be NA if not yet calculated).

- ...:

  Additional named columns to include after the standard columns.

## Value

A tibble with class `peaks_tbl` and the standard peak columns, followed
by any additional columns supplied through `...`.

## Examples

``` r
new_peaks_tbl(
  peak_id = 1L,
  location = 4.2,
  height = 3.1,
  left_base = 3.9,
  right_base = 4.5,
  area = 0.8,
  algorithm_score = 0.97
)
#> # A tibble: 1 × 7
#>   peak_id location height left_base right_base  area algorithm_score
#>     <int>    <dbl>  <dbl>     <dbl>      <dbl> <dbl>           <dbl>
#> 1       1      4.2    3.1       3.9        4.5   0.8            0.97
```
