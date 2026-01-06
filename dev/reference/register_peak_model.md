# Register a Peak Model

Registers a peak model constructor with the measure package. Technique
packs can use this to add custom peak shapes.

## Usage

``` r
register_peak_model(
  name,
  constructor,
  pack_name,
  description = "",
  technique = NULL
)
```

## Arguments

- name:

  Model name (e.g., "gaussian", "emg", "fraser_suzuki").

- constructor:

  Function that creates the peak model object.

- pack_name:

  Source package name.

- description:

  Brief description of the model.

- technique:

  Optional technique name (e.g., "SEC/GPC").

## Value

Invisible `TRUE`.

## See also

[`peak_models()`](https://jameshwade.github.io/measure/dev/reference/peak_models.md),
[`create_peak_model()`](https://jameshwade.github.io/measure/dev/reference/create_peak_model.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# In a technique pack's R/zzz.R:
register_peak_model(
  name = "fraser_suzuki",
  constructor = fraser_suzuki_model,
  pack_name = pkgname,
  description = "Fraser-Suzuki asymmetric peak",
  technique = "SEC/GPC"
)
} # }
```
