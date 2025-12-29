# Data Generation Scripts

This directory contains scripts to generate the example datasets included in the `measure` package.

## Datasets Generated

| Dataset | Type | Description | Observations |
|---------|------|-------------|--------------|
| `hplc_chromatograms` | HPLC | Simulated HPLC-UV chromatograms of phenolic compounds | ~30,000 |
| `sec_chromatograms` | GPC/SEC | Simulated SEC chromatograms for polymer MW analysis | ~7,500 |
| `sec_calibration` | GPC/SEC | Calibration standards for SEC analysis | 5 |
| `maldi_spectra` | Mass Spec | Simulated MALDI-TOF mass spectra | ~304,000 |

## Existing Datasets

The package also includes:

| Dataset | Type | Description | Source |
|---------|------|-------------|--------|
| `meats_long` | NIR | NIR transmittance spectra for meat composition | modeldata |
| `bioreactors_small` | Raman | Raman spectra from small-scale bioreactors | FES book |
| `bioreactors_large` | Raman | Raman spectra from large-scale bioreactors | FES book |

## Running the Generation Script

From the package root directory, run:
```r
source("data-raw/generate_datasets.R")

# Generate all datasets
# (The script will prompt for confirmation in interactive mode)
```

Or generate individual datasets:
```r
source("data-raw/generate_datasets.R")

# HPLC data
hplc_chromatograms <- generate_hplc_data()
usethis::use_data(hplc_chromatograms, overwrite = TRUE)

# SEC data
sec_result <- generate_sec_data()
sec_chromatograms <- sec_result$sec_long
sec_calibration <- sec_result$calibration
usethis::use_data(sec_chromatograms, overwrite = TRUE)
usethis::use_data(sec_calibration, overwrite = TRUE)

# MALDI data
maldi_spectra <- generate_maldi_data()
usethis::use_data(maldi_spectra, overwrite = TRUE)
```

## Dependencies

The generation script requires:
- `tibble`
- `dplyr`
- `tidyr`
- `usethis` (for saving data)
- `prospectr` (optional, for NIR soil data)

## Data Format Conventions

All datasets follow these conventions:
- Stored as tibbles in long format
- Location/time variables are numeric
- Sample identifiers are included
- Metadata columns (concentrations, groups, etc.) are preserved

This allows easy conversion to the internal `measure` format using
`step_measure_input_long()`.
