# Calculate Peak Area

Integrates the peak model over a given range to calculate the area.

## Usage

``` r
peak_model_area(model, params, x_range = NULL)
```

## Arguments

- model:

  A `peak_model` object.

- params:

  Named list of model parameters.

- x_range:

  Numeric vector of length 2 giving the integration range. If `NULL`,
  integrates over the full domain (may require analytical solution).

## Value

Numeric scalar giving the peak area.

## Details

For models with analytical integrals (e.g., Gaussian), this can return
an exact value. Otherwise, numerical integration is used.

## See also

[`peak_model_value()`](https://jameshwade.github.io/measure/dev/reference/peak_model_value.md)

## Examples

``` r
model <- create_peak_model("gaussian")
params <- list(height = 1, center = 5, width = 1)
area <- peak_model_area(model, params, c(0, 10))
area
#> [1] 2.506628
```
