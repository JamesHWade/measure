#' Simulated MALDI-TOF Mass Spectrometry Data
#'
#' Simulated MALDI-TOF (Matrix-Assisted Laser Desorption/Ionization Time-of-Flight)
#' mass spectrometry data for demonstration of mass spectral preprocessing. The
#' dataset represents protein/peptide analysis from four experimental groups with
#' four replicates each.
#'
#' MALDI-TOF is a soft ionization technique commonly used for analyzing
#' biomolecules such as proteins, peptides, and polymers. The technique provides
#' mass-to-charge (m/z) ratios that can be used for identification and
#' quantification.
#'
#' The spectra include realistic features such as:
#' - Multiple peptide/protein peaks at different m/z values
#' - Baseline variation
#' - Chemical noise
#' - Peak width proportional to m/z (resolution effects)
#' - Replicate variation
#'
#' This dataset is useful for demonstrating:
#' - Baseline correction methods
#' - Peak detection for mass spectra
#' - Normalization between samples
#' - Differential analysis between groups
#'
#' @format A tibble with 304,016 observations and 5 variables:
#' \describe{
#'   \item{sample_id}{Sample identifier combining group and replicate}
#'   \item{group}{Experimental group ("Control", "Treatment_A", "Treatment_B", "Treatment_C")}
#'   \item{replicate}{Replicate number (1-4)}
#'   \item{mz}{Mass-to-charge ratio (m/z) in Daltons (1000-20000 Da)}
#'   \item{intensity}{Signal intensity (arbitrary units)}
#' }
#'
#' @details
#' Each group has a characteristic peak pattern:
#' - **Control**: Peptides at m/z ~1200, 1450, 1800, 2200, 3500, 5800, 8400, 12000
#' - **Treatment_A**: Peptides at m/z ~1100, 1650, 2100, 2800, 4200, 6500, 9200, 14000
#' - **Treatment_B**: Proteins at m/z ~2500, 4000, 5500, 8000, 11000, 15000, 18000
#' - **Treatment_C**: Peptides at m/z ~1050, 1280, 1520, 1890, 2340, 2980, 3650, 4500
#'
#' The m/z resolution is approximately 500 ppm (parts per million), typical for
#' linear MALDI-TOF instruments. Note that simulated spectra include baseline
#' noise and minor peaks in addition to the characteristic peaks listed above.
#'
#' @source Simulated data generated for the measure package. See
#'   `data-raw/generate_datasets.R` for the generation script.
#'
#' @seealso
#' \code{\link{hplc_chromatograms}} for HPLC chromatography data
#' \code{\link{meats_long}} for NIR spectroscopy data
#'
#' @examples
#' data(maldi_spectra)
#'
#' # View structure
#' str(maldi_spectra)
#'
#' # Get unique samples
#' unique(maldi_spectra$sample_id)
#'
#' # Get one spectrum
#' library(dplyr)
#' spec_1 <- maldi_spectra |> filter(sample_id == "Control_1")
#'
#' # Plot (if ggplot2 available)
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   library(ggplot2)
#'   ggplot(spec_1, aes(x = mz, y = intensity)) +
#'     geom_line() +
#'     labs(x = "m/z (Da)", y = "Intensity",
#'          title = "MALDI-TOF Mass Spectrum")
#' }
#'
#' # Compare groups
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   # Get one replicate per group
#'   comparison <- maldi_spectra |>
#'     filter(replicate == 1)
#'
#'   ggplot(comparison, aes(x = mz, y = intensity, color = group)) +
#'     geom_line(alpha = 0.7) +
#'     facet_wrap(~group, ncol = 1) +
#'     labs(x = "m/z (Da)", y = "Intensity",
#'          title = "MALDI-TOF Spectra by Group")
#' }
#'
#' @name maldi_spectra
#' @aliases maldi_spectra
#' @docType data
#' @keywords datasets
NULL
