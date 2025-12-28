# Parameters for alignment steps

`align_max_shift()` controls the maximum shift allowed in alignment.
`align_segment_length()` controls segment size for COW alignment.

## Usage

``` r
align_max_shift(range = c(1L, 50L), trans = NULL)

align_segment_length(range = c(10L, 100L), trans = NULL)
```

## Arguments

- range:

  A two-element vector holding the *defaults* for the smallest and
  largest possible values, respectively. If a transformation is
  specified, these values should be in the *transformed units*.

- trans:

  A `trans` object from the `scales` package, such as
  [`scales::transform_log10()`](https://scales.r-lib.org/reference/transform_log.html)
  or
  [`scales::transform_reciprocal()`](https://scales.r-lib.org/reference/transform_reciprocal.html).
  If not provided, the default is used which matches the units used in
  `range`. If no transformation, `NULL`.

## Value

A function with classes `"quant_param"` and `"param"`.

## Examples

``` r
align_max_shift()
#> Maximum Alignment Shift (quantitative)
#> Range: [1, 50]
align_segment_length()
#> Alignment Segment Length (quantitative)
#> Range: [10, 100]
```
