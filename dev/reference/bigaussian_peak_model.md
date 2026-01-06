# Create Bi-Gaussian Peak Model

Creates a Bi-Gaussian peak model with four parameters: height, center,
width_left, and width_right.

## Usage

``` r
bigaussian_peak_model()
```

## Value

A `bigaussian_peak_model` object.

## Details

The Bi-Gaussian function uses different widths on the left and right
sides of the peak, providing flexible asymmetry.

## See also

Other peak-models:
[`emg_peak_model()`](https://jameshwade.github.io/measure/dev/reference/emg_peak_model.md),
[`gaussian_peak_model()`](https://jameshwade.github.io/measure/dev/reference/gaussian_peak_model.md),
[`lorentzian_peak_model()`](https://jameshwade.github.io/measure/dev/reference/lorentzian_peak_model.md)

## Examples

``` r
model <- bigaussian_peak_model()
x <- seq(0, 10, by = 0.1)
params <- list(height = 1, center = 5, width_left = 0.8, width_right = 1.2)
y <- peak_model_value(model, x, params)
plot(x, y, type = "l")

```
