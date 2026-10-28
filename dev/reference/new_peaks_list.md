# Create a new peaks list

Wraps a list of peak tables as a `peaks_list` for use in a data frame
column. Additional columns on each peak table are preserved.

## Usage

``` r
new_peaks_list(x = list())
```

## Arguments

- x:

  A list of peak tibbles, usually created by
  [`new_peaks_tbl()`](https://jameshwade.github.io/measure/dev/reference/new_peaks_tbl.md).

## Value

A list-like vector with class `peaks_list`.

## Examples

``` r
peaks <- new_peaks_tbl(
  peak_id = 1L,
  location = 4.2,
  height = 3.1,
  left_base = 3.9,
  right_base = 4.5,
  area = 0.8,
  algorithm_score = 0.97
)
new_peaks_list(list(peaks))
#> <peaks_list[1]>
#> [[1]]
#> # A tibble: 1 × 7
#>   peak_id location height left_base right_base  area algorithm_score
#>     <int>    <dbl>  <dbl>     <dbl>      <dbl> <dbl>           <dbl>
#> 1       1      4.2    3.1       3.9        4.5   0.8            0.97
#> 
```
