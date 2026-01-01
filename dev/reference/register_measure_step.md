# Register a Step from a Technique Pack

Registers a recipe step with the measure package. This function should
be called from the `.onLoad()` function of technique pack packages after
registering the pack with
[`register_measure_pack()`](https://jameshwade.github.io/measure/dev/reference/register_measure_pack.md).

## Usage

``` r
register_measure_step(
  step_name,
  pack_name,
  category = "processing",
  description = "",
  technique = NULL
)
```

## Arguments

- step_name:

  Full step function name (e.g., `"step_sec_mw_averages"`).

- pack_name:

  Source package name. Use `pkgname` from `.onLoad()`.

- category:

  Step category (e.g., `"preprocessing"`, `"calculation"`).

- description:

  Brief description of what the step does.

- technique:

  Technique name. If `NULL`, inherits from the registered pack.

## Value

Invisible `TRUE`.

## Details

Registration is idempotent: calling this function multiple times with
the same `pack_name` and `step_name` will update rather than duplicate
the entry.

## See also

[`register_measure_pack()`](https://jameshwade.github.io/measure/dev/reference/register_measure_pack.md),
[`measure_steps()`](https://jameshwade.github.io/measure/dev/reference/measure_steps.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# In a technique pack's R/zzz.R file:
measure::register_measure_step(
  step_name = "step_sec_mw_averages",
  pack_name = pkgname,
  category = "calculation",
  description = "Calculate Mn, Mw, Mz, dispersity"
)
} # }
```
