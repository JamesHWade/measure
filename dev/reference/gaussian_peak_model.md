# Create Gaussian Peak Model

Creates a symmetric Gaussian peak model with three parameters: height,
center, and width (sigma).

## Usage

``` r
gaussian_peak_model()
```

## Value

A `gaussian_peak_model` object.

## Details

The Gaussian function is: \$\$f(x) = h \cdot \exp\left(-\frac{(x -
c)^2}{2\sigma^2}\right)\$\$

where `h` is height, `c` is center, and `sigma` is width.

## See also

Other peak-models:
[`bigaussian_peak_model()`](https://jameshwade.github.io/measure/dev/reference/bigaussian_peak_model.md),
[`emg_peak_model()`](https://jameshwade.github.io/measure/dev/reference/emg_peak_model.md),
[`lorentzian_peak_model()`](https://jameshwade.github.io/measure/dev/reference/lorentzian_peak_model.md)

## Examples

``` r
model <- gaussian_peak_model()
x <- seq(0, 10, by = 0.1)
params <- list(height = 1, center = 5, width = 1)
y <- peak_model_value(model, x, params)
plot(x, y, type = "l")

```
