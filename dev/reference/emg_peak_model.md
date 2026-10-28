# Create EMG Peak Model

Creates an Exponentially Modified Gaussian peak model with four
parameters: height, center, width (sigma), and tau (exponential decay
constant).

## Usage

``` r
emg_peak_model()
```

## Value

An `emg_peak_model` object.

## Details

The EMG function models asymmetric peaks with tailing, common in
chromatography. It is the convolution of a precursor Gaussian with a
one-sided exponential decay density. For positive `tau`, its value is:

\$\$ f(x) = \frac{h \sigma \sqrt{2\pi}}{\tau}
\exp\left(\frac{\sigma^2}{2\tau^2} - \frac{x-c}{\tau}\right)
\Phi\left(\frac{x-c}{\sigma} - \frac{\sigma}{\tau}\right) \$\$

Here, `height` is the peak height of the precursor Gaussian, not the
observed EMG maximum. The integrated area is therefore
`height * width * sqrt(2 * pi)`. A non-positive `tau` returns the
Gaussian limit. Fitting constrains precursor height to be nonnegative
but leaves its upper bound unbounded because a long tail can make the
observed apex much smaller than the precursor height.

## See also

Other peak-models:
[`bigaussian_peak_model()`](https://jameshwade.github.io/measure/dev/reference/bigaussian_peak_model.md),
[`gaussian_peak_model()`](https://jameshwade.github.io/measure/dev/reference/gaussian_peak_model.md),
[`lorentzian_peak_model()`](https://jameshwade.github.io/measure/dev/reference/lorentzian_peak_model.md)

## Examples

``` r
model <- emg_peak_model()
x <- seq(0, 15, by = 0.1)
params <- list(height = 1, center = 5, width = 0.5, tau = 0.3)
y <- peak_model_value(model, x, params)
plot(x, y, type = "l")

```
