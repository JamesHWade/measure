# Get Parameter Bounds for Optimization

Returns lower and upper bounds for each parameter, used to constrain
optimization during deconvolution.

## Usage

``` r
peak_model_bounds(model, x_range, y_range)
```

## Arguments

- model:

  A `peak_model` object.

- x_range:

  Numeric vector of length 2 giving the x-axis range (min, max).

- y_range:

  Numeric vector of length 2 giving the y-axis range (min, max).

## Value

A list with two components:

- `lower`: Named numeric vector of lower bounds

- `upper`: Named numeric vector of upper bounds

## See also

[`peak_model_initial_guess()`](https://jameshwade.github.io/measure/dev/reference/peak_model_initial_guess.md)

## Examples

``` r
model <- create_peak_model("gaussian")
bounds <- peak_model_bounds(model, c(0, 20), c(0, 100))
bounds$lower
#> height center  width 
#>  0.000  0.000  0.001 
bounds$upper
#> height center  width 
#>    150     20     10 
```
