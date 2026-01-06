# Register a Peak Detection Algorithm

Registers a peak detection algorithm with the measure package. This
function can be called from technique pack packages to add specialized
algorithms.

## Usage

``` r
register_peak_algorithm(
  name,
  algorithm_fn,
  pack_name,
  description = "",
  default_params = list(),
  param_info = list(),
  technique = NULL
)
```

## Arguments

- name:

  Algorithm name (e.g., `"cwt"`, `"finderskeepers"`). Must be unique.

- algorithm_fn:

  The algorithm function. Must accept `location`, `value`, and return a
  `peaks_tbl` object. Additional parameters are passed via `...`.

- pack_name:

  Source package name. Use `pkgname` from `.onLoad()`.

- description:

  Brief description of the algorithm.

- default_params:

  Named list of default parameter values.

- param_info:

  Named list of parameter descriptions (for documentation).

- technique:

  Optional technique name (e.g., `"SEC/GPC"`). If `NULL`, algorithm is
  considered general-purpose.

## Value

Invisible `TRUE`.

## See also

[`peak_algorithms()`](https://jameshwade.github.io/measure/dev/reference/peak_algorithms.md),
[`get_peak_algorithm()`](https://jameshwade.github.io/measure/dev/reference/get_peak_algorithm.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# In a technique pack's R/zzz.R file:
.onLoad <- function(libname, pkgname) {
  if (requireNamespace("measure", quietly = TRUE)) {
    measure::register_peak_algorithm(
      name = "sec_loess_ist",
      algorithm_fn = .detect_peaks_sec_loess_ist,
      pack_name = pkgname,
      description = "LOESS smoothing with iterative soft thresholding",
      default_params = list(loess_span = 0.01, ist_points = 50),
      technique = "SEC/GPC"
    )
  }
}
} # }
```
