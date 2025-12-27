#' measure: A Recipes-Style Interface to Tidymodels for Analytical Measurements
#'
#' @description
#' The measure package provides preprocessing steps for analytical measurement
#' data (spectroscopy, chromatography, etc.) within the tidymodels framework.
#'
#' @section Input Steps:
#' Convert raw data to measure's internal format:
#' - [step_measure_input_wide()]: For data with measurements in columns
#' - [step_measure_input_long()]: For data with measurements in rows
#'
#' @section Processing Steps:
#' Apply spectral preprocessing transformations:
#' - [step_measure_savitzky_golay()]: Smoothing and derivatives
#' - [step_measure_snv()]: Standard Normal Variate normalization
#' - [step_measure_msc()]: Multiplicative Scatter Correction
#' - [step_measure_map()]: Custom transformations
#'
#' @section Sample-wise Normalization:
#' Normalize each spectrum independently:
#' - [step_measure_normalize_sum()]: Divide by sum
#' - [step_measure_normalize_max()]: Divide by maximum
#' - [step_measure_normalize_range()]: Scale to 0-1 range
#' - [step_measure_normalize_vector()]: L2 normalization
#' - [step_measure_normalize_auc()]: Divide by area under curve
#' - [step_measure_normalize_peak()]: Normalize by region (tunable)
#'
#' @section Variable-wise Scaling:
#' Scale across samples at each location (learns from training data):
#' - [step_measure_center()]: Mean centering
#' - [step_measure_scale_auto()]: Z-score normalization
#' - [step_measure_scale_pareto()]: Pareto scaling
#' - [step_measure_scale_range()]: Range scaling
#' - [step_measure_scale_vast()]: VAST scaling
#'
#' @section Output Steps:
#' Convert back to modeling-ready format:
#' - [step_measure_output_wide()]: Back to wide format
#' - [step_measure_output_long()]: Back to long format
#'
#' @seealso
#' - `vignette("measure")` for getting started
#' - `vignette("preprocessing")` for detailed technique descriptions
#' - `vignette("recipes")` for tidymodels integration
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import recipes
#' @import rlang
#' @importFrom utils globalVariables
#' @importFrom dplyr select arrange mutate
#' @importFrom tibble tibble
#' @importFrom glue glue
## usethis namespace: end
NULL


utils::globalVariables(
  names = c(
    "baseline",
    ".index",
    ".measures",
    "value",
    "location",
    "..row",
    "temp"
  )
)
