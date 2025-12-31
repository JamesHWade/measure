# Apply a function to measurement data along dimensions

Central dispatcher that enables 1D preprocessing operations to work on
n-dimensional measurement data. For 1D data, it applies the function
directly. For nD data, it slices along the specified dimensions, applies
the function to each 1D slice, and rebuilds the nD structure.

## Usage

``` r
measure_apply(x, fn, along = 1L, ...)
```

## Arguments

- x:

  A `measure_tbl`, `measure_nd_tbl`, `measure_list`, or
  `measure_nd_list` object.

- fn:

  A function that accepts a `measure_tbl` and returns a `measure_tbl`.
  The function signature should be `fn(x, ...)`.

- along:

  Integer vector specifying which dimensions to apply the function
  along. For 2D data, `along = 1` applies along dimension 1 (e.g., time
  in LC-DAD), treating dimension 2 slices as independent. Default is
  `1L` (apply along the first dimension).

- ...:

  Additional arguments passed to `fn`.

## Value

An object of the same class as the input, with the function applied to
each 1D slice.

## Details

The `measure_apply()` function is the workhorse for making 1D
preprocessing steps work on nD data. It handles:

- **1D data**: Direct function application

- **nD data**: Slice-apply-rebuild pattern

For nD data, the function extracts 1D slices along the specified
dimension(s), applies the transformation function to each slice, and
reassembles the result into the original nD structure.

## Examples

``` r
# Create a simple 2D measurement
m2d <- new_measure_nd_tbl(
  location_1 = rep(1:10, each = 3),
  location_2 = rep(1:3, times = 10),
  value = rnorm(30)
)

# Define a simple smoothing function for 1D data
smooth_1d <- function(x) {
  x$value <- stats::filter(x$value, rep(1/3, 3), sides = 2)
  x[!is.na(x$value), ]
}

# Apply smoothing along dimension 1
result <- measure_apply(m2d, smooth_1d, along = 1)
```
