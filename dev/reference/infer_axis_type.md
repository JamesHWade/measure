# Infer axis type from location values

Attempts to infer the type of measurement axis based on the range and
characteristics of location values. This is a heuristic that helps guide
appropriate preprocessing choices.

## Usage

``` r
infer_axis_type(location)
```

## Arguments

- location:

  Numeric vector of location values.

## Value

Character string indicating inferred axis type:

- `"wavelength_nm"`: Visible/NIR wavelengths (typically 300-2500 nm)

- `"wavenumber"`: Mid-IR wavenumbers (typically 400-4000 cm^-1)

- `"retention_time"`: Chromatography retention time (typically 0-60 min)

- `"mass_charge"`: Mass spectrometry m/z (typically 50-2000+)

- `"ppm"`: NMR chemical shift (typically -2 to 14 ppm)

- `"two_theta"`: XRD diffraction angle (typically 5-90 degrees)

- `"temperature"`: Thermal analysis (typically 20-1000 C)

- `"unknown"`: Could not determine axis type

## Examples

``` r
# NIR wavelengths
infer_axis_type(seq(1000, 2500, by = 2))
#> [1] "wavelength_nm"

# Mid-IR wavenumbers
infer_axis_type(seq(4000, 400, by = -4))
#> [1] "wavenumber"

# Retention time (minutes)
infer_axis_type(seq(0, 30, by = 0.01))
#> [1] "retention_time"

# NMR chemical shift
infer_axis_type(seq(0, 12, by = 0.001))
#> [1] "ppm"
```
