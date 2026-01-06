# Create a Peak Model Object

Creates a new peak model S3 object. This is the base constructor for all
peak shape models used in deconvolution.

## Usage

``` r
new_peak_model(
  name,
  n_params,
  param_names,
  description = "",
  technique = NULL,
  ...
)
```

## Arguments

- name:

  Character name of the model (e.g., "gaussian", "emg").

- n_params:

  Number of parameters in the model.

- param_names:

  Character vector of parameter names.

- description:

  Brief description of the model.

- technique:

  Optional technique name (e.g., "SEC/GPC"). If `NULL`, model is
  general-purpose.

- ...:

  Additional model-specific attributes.

## Value

A `peak_model` S3 object with subclass `{name}_peak_model`.

## See also

[`peak_model_value()`](https://jameshwade.github.io/measure/dev/reference/peak_model_value.md),
[`peak_model_gradient()`](https://jameshwade.github.io/measure/dev/reference/peak_model_gradient.md),
[`peak_model_bounds()`](https://jameshwade.github.io/measure/dev/reference/peak_model_bounds.md)

## Examples

``` r
# Create a simple Gaussian model
model <- new_peak_model(
  name = "gaussian",
  n_params = 3,
  param_names = c("height", "center", "width"),
  description = "Symmetric Gaussian peak"
)
print(model)
#> <peak_model: gaussian >
#>   Parameters (3): height, center, width 
#>   Description: Symmetric Gaussian peak 
```
