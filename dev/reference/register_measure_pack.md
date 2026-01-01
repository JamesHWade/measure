# Register a Technique Pack

Registers an external technique pack with the measure package. This
function should be called from the `.onLoad()` function of technique
pack packages.

## Usage

``` r
register_measure_pack(pack_name, technique, version = NULL, description = NULL)
```

## Arguments

- pack_name:

  Package name (e.g., `"measure.sec"`). Use `pkgname` from `.onLoad()`
  for portability.

- technique:

  Technique name (e.g., `"SEC/GPC"`, `"FTIR"`, `"Raman"`).

- version:

  Package version. If `NULL`, attempts to retrieve from installed
  package.

- description:

  Brief description of the technique pack.

## Value

Invisible `TRUE`.

## See also

[`register_measure_step()`](https://jameshwade.github.io/measure/dev/reference/register_measure_step.md),
[`measure_packs()`](https://jameshwade.github.io/measure/dev/reference/measure_packs.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# In a technique pack's R/zzz.R file:
.onLoad <- function(libname, pkgname) {
  if (requireNamespace("measure", quietly = TRUE)) {
    measure::register_measure_pack(
      pack_name = pkgname,
      technique = "SEC/GPC",
      description = "Size Exclusion Chromatography"
    )
  }
}
} # }
```
