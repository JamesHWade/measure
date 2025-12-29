#' Simulated SEC/GPC Chromatography Data
#'
#' Simulated Size Exclusion Chromatography (SEC) / Gel Permeation Chromatography
#' (GPC) data for demonstration of molecular weight analysis. The dataset
#' includes both narrow polystyrene calibration standards and polymer samples
#' with broad molecular weight distributions.
#'
#' SEC/GPC separates molecules by hydrodynamic size, with larger molecules
#' eluting before smaller ones. This allows determination of molecular weight
#' distributions and averages (Mn, Mw, Mz, dispersity).
#'
#' The dataset is useful for demonstrating:
#' - Baseline correction for chromatography
#' - Calibration curve construction using standards
#' - Molecular weight calculations (step_measure_mw_averages)
#' - Molecular weight distribution analysis
#'
#' @format A tibble with 7,510 observations and 6 variables:
#' \describe{
#'   \item{sample_id}{Sample identifier (standard or polymer name)}
#'   \item{sample_type}{Either "standard" or "sample"}
#'   \item{elution_time}{Elution/retention time in minutes}
#'   \item{ri_signal}{Refractive index detector signal (arbitrary units)}
#'   \item{known_mw}{Known weight-average molecular weight (g/mol)}
#'   \item{known_dispersity}{Known dispersity (Mw/Mn); ~1.05 for standards}
#' }
#'
#' @details
#' The dataset contains:
#'
#' **Calibration Standards** (narrow dispersity polystyrene):
#' - PS_1k: 1,000 g/mol
#' - PS_5k: 5,000 g/mol
#' - PS_20k: 20,000 g/mol
#' - PS_100k: 100,000 g/mol
#' - PS_500k: 500,000 g/mol
#'
#' **Polymer Samples** (broad distribution):
#' - Polymer_A through Polymer_E with varying Mw and dispersity
#'
#' The calibration relationship follows: log10(MW) = 9.5 - 0.35 * time
#'
#' @source Simulated data generated for the measure package. See
#'   `data-raw/generate_datasets.R` for the generation script.
#'
#' @seealso
#' \code{\link{sec_calibration}} for the calibration standards summary
#' \code{\link{hplc_chromatograms}} for HPLC chromatography data
#' \code{\link{step_measure_mw_averages}} for molecular weight calculations
#'
#' @examples
#' data(sec_chromatograms)
#'
#' # View structure
#' str(sec_chromatograms)
#'
#' # Separate standards and samples
#' library(dplyr)
#' standards <- sec_chromatograms |> filter(sample_type == "standard")
#' samples <- sec_chromatograms |> filter(sample_type == "sample")
#'
#' # Plot standards (if ggplot2 available)
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   library(ggplot2)
#'   ggplot(standards, aes(x = elution_time, y = ri_signal, color = sample_id)) +
#'     geom_line() +
#'     labs(x = "Elution Time (min)", y = "RI Signal",
#'          title = "SEC Calibration Standards",
#'          color = "Standard")
#' }
#'
#' @name sec_chromatograms
#' @aliases sec_chromatograms
#' @docType data
#' @keywords datasets
NULL


#' SEC/GPC Calibration Standards Summary
#'
#' Summary information for the polystyrene calibration standards used with
#' \code{\link{sec_chromatograms}}. Contains the known molecular weights and
#' peak retention times needed to construct a calibration curve.
#'
#' @format A tibble with 5 observations and 3 variables:
#' \describe{
#'   \item{standard}{Standard name (e.g., "PS_1k")}
#'   \item{mw}{Known molecular weight in g/mol}
#'   \item{peak_time}{Peak elution time in minutes}
#' }
#'
#' @details
#' The calibration curve for SEC/GPC relates log(MW) to retention time.
#' For this simulated data: log10(MW) = 9.5 - 0.35 * time
#'
#' @source Simulated data generated for the measure package. See
#'   `data-raw/generate_datasets.R` for the generation script.
#'
#' @seealso
#' \code{\link{sec_chromatograms}} for the full chromatogram data
#'
#' @examples
#' data(sec_calibration)
#'
#' # View calibration data
#' sec_calibration
#'
#' # Create calibration curve (if ggplot2 available)
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   library(ggplot2)
#'   ggplot(sec_calibration, aes(x = peak_time, y = log10(mw))) +
#'     geom_point(size = 3) +
#'     geom_smooth(method = "lm", se = FALSE) +
#'     labs(x = "Peak Retention Time (min)", y = "log10(MW)",
#'          title = "SEC Calibration Curve")
#' }
#'
#' @name sec_calibration
#' @aliases sec_calibration
#' @docType data
#' @keywords datasets
NULL
