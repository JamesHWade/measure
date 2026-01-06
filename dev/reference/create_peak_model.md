# Create a Peak Model by Name

Creates a peak model object from a registered model name.

## Usage

``` r
create_peak_model(name)
```

## Arguments

- name:

  Name of the model (e.g., "gaussian", "emg", "bigaussian").

## Value

A `peak_model` object.

## See also

[`peak_models()`](https://jameshwade.github.io/measure/dev/reference/peak_models.md),
[`register_peak_model()`](https://jameshwade.github.io/measure/dev/reference/register_peak_model.md)

## Examples

``` r
model <- create_peak_model("gaussian")
print(model)
#> <peak_model: gaussian >
#>   Parameters (3): height, center, width 
#>   Description: Symmetric Gaussian peak 
```
