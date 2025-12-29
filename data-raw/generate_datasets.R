# Script to generate example datasets for the measure package
# Run this script from the package root directory

library(tibble)
library(dplyr)
library(tidyr)

# ------------------------------------------------------------------------------
# 1. NIR Soil Spectroscopy Data (from prospectr)
# ------------------------------------------------------------------------------

generate_nir_soil <- function() {
  if (!requireNamespace("prospectr", quietly = TRUE)) {
    stop("prospectr package required to generate NIRsoil data")
  }

  data(NIRsoil, package = "prospectr", envir = environment())

  # Get wavelengths from column names of spc matrix
  wavelengths <- as.numeric(colnames(NIRsoil$spc))

  # Convert to long format similar to meats_long
  nir_soil <- NIRsoil |>
    mutate(
      sample_id = row_number(),
      Nt = Nt,
      Ciso = Ciso,
      CEC = CEC,
      train = train
    ) |>
    select(sample_id, Nt, Ciso, CEC, train) |>
    bind_cols(as_tibble(NIRsoil$spc)) |>
    pivot_longer(
      cols = -c(sample_id, Nt, Ciso, CEC, train),
      names_to = "wavelength",
      values_to = "absorbance"
    ) |>
    mutate(wavelength = as.numeric(wavelength))

  nir_soil
}


# ------------------------------------------------------------------------------
# 2. Simulated HPLC Chromatography Data
# ------------------------------------------------------------------------------

#' Generate simulated HPLC chromatogram
#'
#' Creates realistic HPLC-UV chromatogram data for demonstration purposes.
#' Simulates a separation of 5 compounds with realistic peak shapes.
#'
generate_hplc_data <- function() {
  set.seed(42)

  # Retention times for 5 standard compounds (in minutes)
  peak_times <- c(2.5, 4.2, 6.8, 9.1, 12.3)
  # Peak heights (arbitrary units)
  peak_heights <- c(850, 1200, 650, 980, 420)
  # Peak widths (standard deviation in minutes)
  peak_widths <- c(0.15, 0.18, 0.22, 0.25, 0.30)

  # Compound names
  compound_names <- c("Caffeine", "Theobromine", "Catechin", "Epicatechin", "Quercetin")

  # Create time axis (0 to 15 minutes at 0.01 min resolution)
  time <- seq(0, 15, by = 0.01)
  n_points <- length(time)

  # Function to generate Gaussian peak
  gaussian_peak <- function(t, center, height, width) {
    height * exp(-((t - center)^2) / (2 * width^2))
  }

  # Generate 20 samples with varying concentrations
  n_samples <- 20

  hplc_data <- tibble()

  for (i in 1:n_samples) {
    # Vary concentrations randomly (50-150% of nominal)
    conc_factors <- runif(5, 0.5, 1.5)

    # Generate baseline (slight drift)
    baseline <- 10 + 0.5 * time + rnorm(n_points, 0, 2)

    # Generate signal
    signal <- baseline
    for (j in 1:5) {
      # Add small retention time variation
      rt_shift <- rnorm(1, 0, 0.02)
      signal <- signal + gaussian_peak(
        time,
        peak_times[j] + rt_shift,
        peak_heights[j] * conc_factors[j],
        peak_widths[j]
      )
    }

    # Add noise
    signal <- signal + rnorm(n_points, 0, 3)

    # Create sample data
    sample_data <- tibble(
      sample_id = i,
      time_min = time,
      absorbance_mAU = pmax(signal, 0),  # No negative values
      # Store true concentrations for modeling
      caffeine_conc = 100 * conc_factors[1],
      theobromine_conc = 80 * conc_factors[2],
      catechin_conc = 60 * conc_factors[3],
      epicatechin_conc = 70 * conc_factors[4],
      quercetin_conc = 40 * conc_factors[5]
    )

    hplc_data <- bind_rows(hplc_data, sample_data)
  }

  hplc_data
}

# ------------------------------------------------------------------------------
# 3. Simulated GPC/SEC Data for Molecular Weight Analysis
# ------------------------------------------------------------------------------

#' Generate simulated GPC/SEC chromatogram data
#'
#' Creates realistic SEC chromatograms for polymer analysis demonstration.
#' Includes both narrow standards for calibration and broad distribution samples.
#'
generate_sec_data <- function() {
  set.seed(123)

  # Elution time axis (5 to 20 minutes)
  time <- seq(5, 20, by = 0.02)
  n_points <- length(time)

  # Calibration relationship: log(MW) = a + b * time
  # Typical SEC: higher MW elutes earlier
  a <- 9.5
  b <- -0.35

  # Function to calculate MW from retention time
  time_to_mw <- function(t) 10^(a + b * t)

  # Function to generate SEC peak (log-normal in MW space)
  sec_peak <- function(t, center_mw, height, dispersity) {
    # Convert to log scale
    log_mw <- log10(center_mw)
    log_width <- log10(dispersity) * 0.5

    # Calculate MW at each time point
    mw_at_t <- time_to_mw(t)
    log_mw_at_t <- log10(mw_at_t)

    # Generate peak
    height * exp(-((log_mw_at_t - log_mw)^2) / (2 * log_width^2))
  }

  # Generate standards (narrow distribution)
  standards_mw <- c(1000, 5000, 20000, 100000, 500000)
  standards_names <- c("PS_1k", "PS_5k", "PS_20k", "PS_100k", "PS_500k")

  # Generate 5 polymer samples with different MW distributions
  sample_mw <- c(15000, 45000, 120000, 250000, 80000)  # Weight-average MW
  sample_dispersity <- c(1.8, 2.2, 1.5, 2.5, 1.9)  # Mw/Mn
  sample_names <- c("Polymer_A", "Polymer_B", "Polymer_C", "Polymer_D", "Polymer_E")

  sec_data <- tibble()

  # Generate standard chromatograms
  for (i in seq_along(standards_mw)) {
    baseline <- 2 + rnorm(n_points, 0, 0.5)
    signal <- baseline + sec_peak(time, standards_mw[i], 100, 1.05)  # Narrow dispersity
    signal <- pmax(signal + rnorm(n_points, 0, 0.8), 0)

    sample_data <- tibble(
      sample_id = standards_names[i],
      sample_type = "standard",
      elution_time = time,
      ri_signal = signal,
      known_mw = standards_mw[i],
      known_dispersity = 1.05
    )
    sec_data <- bind_rows(sec_data, sample_data)
  }

  # Generate polymer sample chromatograms
  for (i in seq_along(sample_mw)) {
    baseline <- 2 + 0.1 * (time - 10) + rnorm(n_points, 0, 0.5)
    signal <- baseline + sec_peak(time, sample_mw[i], 80, sample_dispersity[i])
    signal <- pmax(signal + rnorm(n_points, 0, 0.8), 0)

    sample_data <- tibble(
      sample_id = sample_names[i],
      sample_type = "sample",
      elution_time = time,
      ri_signal = signal,
      known_mw = sample_mw[i],
      known_dispersity = sample_dispersity[i]
    )
    sec_data <- bind_rows(sec_data, sample_data)
  }

  # Create wide format version for easier use
  sec_wide <- sec_data |>
    select(sample_id, sample_type, elution_time, ri_signal, known_mw, known_dispersity) |>
    pivot_wider(
      id_cols = c(sample_id, sample_type, known_mw, known_dispersity),
      names_from = elution_time,
      values_from = ri_signal,
      names_prefix = "t_"
    )

  list(
    sec_long = sec_data,
    sec_wide = sec_wide,
    calibration = tibble(
      standard = standards_names,
      mw = standards_mw,
      # Calculate peak retention time for calibration
      # From: log10(MW) = a + b * time => time = (log10(MW) - a) / b
      peak_time = (log10(standards_mw) - a) / b
    )
  )
}


# ------------------------------------------------------------------------------
# 4. Simulated MALDI-TOF Mass Spectrometry Data
# ------------------------------------------------------------------------------

#' Generate simulated MALDI-TOF mass spectrometry data
#'
#' Creates realistic MALDI-TOF spectra for protein/peptide analysis demonstration.
#'
generate_maldi_data <- function() {
  set.seed(456)

  # m/z axis (1000 to 20000 Da)
  mz <- seq(1000, 20000, by = 1)
  n_points <- length(mz)

  # Define typical protein/peptide peaks
  # Each spectrum has different composition
  peak_sets <- list(
    # Sample 1: Typical peptide mixture
    list(mz = c(1200, 1450, 1800, 2200, 3500, 5800, 8400, 12000),
         intensity = c(800, 1200, 950, 600, 1500, 2000, 1100, 400)),
    # Sample 2: Different peptide mixture
    list(mz = c(1100, 1650, 2100, 2800, 4200, 6500, 9200, 14000),
         intensity = c(1000, 900, 1300, 800, 1800, 1600, 900, 350)),
    # Sample 3: Protein-focused
    list(mz = c(2500, 4000, 5500, 8000, 11000, 15000, 18000),
         intensity = c(600, 800, 1200, 2200, 1800, 1000, 500)),
    # Sample 4: Peptide mapping
    list(mz = c(1050, 1280, 1520, 1890, 2340, 2980, 3650, 4500),
         intensity = c(1100, 1400, 1250, 1800, 900, 750, 600, 450))
  )

  # Function to generate Gaussian-like MS peak
  ms_peak <- function(m, center, height, width_ppm = 500) {
    width <- center * width_ppm / 1e6
    height * exp(-((m - center)^2) / (2 * width^2))
  }

  maldi_data <- tibble()

  # Generate 16 spectra (4 groups x 4 replicates)
  group_names <- c("Control", "Treatment_A", "Treatment_B", "Treatment_C")

  for (g in 1:4) {
    for (rep in 1:4) {
      sample_id <- paste0(group_names[g], "_", rep)

      # Base peaks from the group's peak set
      base_peaks <- peak_sets[[g]]

      # Add variation for replicates
      baseline <- 50 + 10 * sin(mz / 2000) + rnorm(n_points, 0, 15)
      signal <- baseline

      for (p in seq_along(base_peaks$mz)) {
        # Add slight m/z shift and intensity variation
        mz_shift <- rnorm(1, 0, base_peaks$mz[p] * 0.0002)
        int_factor <- runif(1, 0.8, 1.2)
        signal <- signal + ms_peak(
          mz,
          base_peaks$mz[p] + mz_shift,
          base_peaks$intensity[p] * int_factor
        )
      }

      # Add noise proportional to baseline
      signal <- signal + rnorm(n_points, 0, sqrt(abs(signal) * 0.5 + 5))
      signal <- pmax(signal, 0)

      sample_data <- tibble(
        sample_id = sample_id,
        group = group_names[g],
        replicate = rep,
        mz = mz,
        intensity = signal
      )

      maldi_data <- bind_rows(maldi_data, sample_data)
    }
  }

  maldi_data
}


# ------------------------------------------------------------------------------
# Main: Generate and save all datasets
# ------------------------------------------------------------------------------

if (interactive()) {
  message("Generating datasets...")

  # 1. NIR Soil data (requires prospectr)
  tryCatch({
    nir_soil <- generate_nir_soil()
    usethis::use_data(nir_soil, overwrite = TRUE)
    message("Created: nir_soil (", nrow(nir_soil), " observations)")
  }, error = function(e) {
    message("Skipping nir_soil: ", e$message)
  })

  # 2. HPLC data
  hplc_chromatograms <- generate_hplc_data()
  usethis::use_data(hplc_chromatograms, overwrite = TRUE)
  message("Created: hplc_chromatograms (", nrow(hplc_chromatograms), " observations)")

  # 3. SEC/GPC data
  sec_result <- generate_sec_data()
  sec_chromatograms <- sec_result$sec_long
  sec_calibration <- sec_result$calibration
  usethis::use_data(sec_chromatograms, overwrite = TRUE)
  usethis::use_data(sec_calibration, overwrite = TRUE)
  message("Created: sec_chromatograms (", nrow(sec_chromatograms), " observations)")
  message("Created: sec_calibration (", nrow(sec_calibration), " standards)")

  # 4. MALDI data
  maldi_spectra <- generate_maldi_data()
  usethis::use_data(maldi_spectra, overwrite = TRUE)
  message("Created: maldi_spectra (", nrow(maldi_spectra), " observations)")

  message("Done! All datasets saved to data/")
}
