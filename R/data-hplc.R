#' Simulated HPLC Chromatography Data
#'
#' Simulated HPLC-UV chromatogram data for demonstration of chromatographic
#' preprocessing and peak analysis. The dataset represents a separation of five
#' phenolic compounds (caffeine, theobromine, catechin, epicatechin, and
#' quercetin) with 20 samples of varying concentrations.
#'
#' The chromatograms include realistic features such as:
#' - Gaussian peak shapes with compound-specific widths
#' - Baseline drift
#' - Instrumental noise
#' - Small retention time variations between runs
#' - Concentration-dependent peak heights
#'
#' This dataset is useful for demonstrating:
#' - Baseline correction methods
#' - Peak detection and integration
#' - Calibration curve construction
#' - Retention time alignment
#'
#' @format A tibble with 30,020 observations and 8 variables:
#' \describe{
#'   \item{sample_id}{Integer sample identifier (1-20)}
#'   \item{time_min}{Retention time in minutes (0-15, 0.01 min resolution)}
#'   \item{absorbance_mAU}{UV absorbance signal in milli-absorbance units}
#'   \item{caffeine_conc}{True caffeine concentration (mg/L) for calibration}
#'   \item{theobromine_conc}{True theobromine concentration (mg/L)}
#'   \item{catechin_conc}{True catechin concentration (mg/L)}
#'   \item{epicatechin_conc}{True epicatechin concentration (mg/L)}
#'   \item{quercetin_conc}{True quercetin concentration (mg/L)}
#' }
#'
#' @details
#' The peaks appear at approximately these retention times:
#' - Caffeine: ~2.5 min
#' - Theobromine: ~4.2 min
#' - Catechin: ~6.8 min
#' - Epicatechin: ~9.1 min
#' - Quercetin: ~12.3 min
#'
#' @source Simulated data generated for the measure package. See
#'   `data-raw/generate_datasets.R` for the generation script.
#'
#' @seealso
#' \code{\link{sec_chromatograms}} for SEC/GPC chromatography data
#'
#' @examples
#' data(hplc_chromatograms)
#'
#' # View structure
#' str(hplc_chromatograms)
#'
#' # Get a single chromatogram
#' library(dplyr)
#' chrom_1 <- hplc_chromatograms |> filter(sample_id == 1)
#'
#' # Plot (if ggplot2 available)
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   library(ggplot2)
#'   ggplot(chrom_1, aes(x = time_min, y = absorbance_mAU)) +
#'     geom_line() +
#'     labs(x = "Retention Time (min)", y = "Absorbance (mAU)",
#'          title = "HPLC Chromatogram")
#' }
#'
#' @name hplc_chromatograms
#' @aliases hplc_chromatograms
#' @docType data
#' @keywords datasets
NULL
