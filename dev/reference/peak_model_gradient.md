# Calculate Peak Model Gradient

Calculates partial derivatives of the model with respect to each
parameter. Used by optimization algorithms for gradient-based fitting.

## Usage

``` r
peak_model_gradient(model, x, params)
```

## Arguments

- model:

  A `peak_model` object.

- x:

  Numeric vector of x values.

- params:

  Named list of model parameters.

## Value

Matrix of partial derivatives with dimensions `(length(x), n_params)`.
Column names correspond to parameter names.

## Details

If no analytical gradient is available, a numerical gradient can be
computed using finite differences. See
[`peak_model_gradient_numerical()`](https://jameshwade.github.io/measure/dev/reference/peak_model_gradient_numerical.md).

## See also

[`peak_model_value()`](https://jameshwade.github.io/measure/dev/reference/peak_model_value.md),
[`peak_model_gradient_numerical()`](https://jameshwade.github.io/measure/dev/reference/peak_model_gradient_numerical.md)
