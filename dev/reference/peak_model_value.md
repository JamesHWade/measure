# Evaluate Peak Model

Evaluates the peak model at given x values with specified parameters.

## Usage

``` r
peak_model_value(model, x, params)
```

## Arguments

- model:

  A `peak_model` object.

- x:

  Numeric vector of x values (e.g., retention time, wavelength).

- params:

  Named list of model parameters.

## Value

Numeric vector of y values (same length as `x`).

## See also

[`peak_model_gradient()`](https://jameshwade.github.io/measure/dev/reference/peak_model_gradient.md),
[`peak_model_area()`](https://jameshwade.github.io/measure/dev/reference/peak_model_area.md)

## Examples

``` r
# Using a registered Gaussian model
model <- create_peak_model("gaussian")
x <- seq(0, 10, by = 0.1)
params <- list(height = 1, center = 5, width = 1)
y <- peak_model_value(model, x, params)
plot(x, y, type = "l")

```
