# Generate Initial Parameter Guess

Estimates initial parameter values from the data, providing a starting
point for optimization.

## Usage

``` r
peak_model_initial_guess(model, x, y, peak_idx)
```

## Arguments

- model:

  A `peak_model` object.

- x:

  Numeric vector of x values.

- y:

  Numeric vector of y values (signal intensity).

- peak_idx:

  Integer index of the peak maximum in `x` and `y`.

## Value

Named list of initial parameter values.

## Details

A good initial guess is crucial for successful optimization. The method
should estimate parameters from local features of the data (peak height,
width at half maximum, asymmetry, etc.).

## See also

[`peak_model_bounds()`](https://jameshwade.github.io/measure/dev/reference/peak_model_bounds.md)

## Examples

``` r
model <- create_peak_model("gaussian")
x <- seq(0, 10, by = 0.1)
y <- dnorm(x, mean = 5, sd = 1)
peak_idx <- which.max(y)
initial <- peak_model_initial_guess(model, x, y, peak_idx)
initial
#> $height
#> [1] 0.3989423
#> 
#> $center
#> [1] 5
#> 
#> $width
#> [1] 1.019186
#> 
```
