# Create Lorentzian Peak Model

Creates a Lorentzian (Cauchy) peak model with three parameters: height,
center, and gamma (half-width at half-maximum).

## Usage

``` r
lorentzian_peak_model()
```

## Value

A `lorentzian_peak_model` object.

## Details

The Lorentzian function has heavier tails than Gaussian and is commonly
used in spectroscopy.

## See also

Other peak-models:
[`bigaussian_peak_model()`](https://jameshwade.github.io/measure/dev/reference/bigaussian_peak_model.md),
[`emg_peak_model()`](https://jameshwade.github.io/measure/dev/reference/emg_peak_model.md),
[`gaussian_peak_model()`](https://jameshwade.github.io/measure/dev/reference/gaussian_peak_model.md)

## Examples

``` r
model <- lorentzian_peak_model()
x <- seq(0, 10, by = 0.1)
params <- list(height = 1, center = 5, gamma = 0.5)
y <- peak_model_value(model, x, params)
plot(x, y, type = "l")

```
