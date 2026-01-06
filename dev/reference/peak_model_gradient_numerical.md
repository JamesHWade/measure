# Numerical Gradient for Peak Model

Computes the gradient numerically using finite differences. This is used
as a fallback when no analytical gradient is defined.

## Usage

``` r
peak_model_gradient_numerical(model, x, params, eps = 1e-08)
```

## Arguments

- model:

  A `peak_model` object.

- x:

  Numeric vector of x values.

- params:

  Named list of model parameters.

- eps:

  Step size for finite differences. Default is `1e-8`.

## Value

Matrix of partial derivatives with dimensions `(length(x), n_params)`.
