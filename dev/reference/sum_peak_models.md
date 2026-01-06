# Sum Multiple Peak Models

Evaluates multiple peak models and sums their contributions.

## Usage

``` r
sum_peak_models(x, models, params_list)
```

## Arguments

- x:

  Numeric vector of x values.

- models:

  List of `peak_model` objects (one per peak).

- params_list:

  List of parameter lists (one per peak).

## Value

Numeric vector of summed peak values.

## Examples

``` r
# Two overlapping Gaussian peaks
model1 <- create_peak_model("gaussian")
model2 <- create_peak_model("gaussian")
x <- seq(0, 20, by = 0.1)
params1 <- list(height = 1, center = 8, width = 1)
params2 <- list(height = 0.8, center = 12, width = 1.5)
y <- sum_peak_models(x, list(model1, model2), list(params1, params2))
plot(x, y, type = "l")

```
