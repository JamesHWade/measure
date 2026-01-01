# ==============================================================================
# Technique Pack Registry
#
# Registration and discovery system for technique packs (external packages).
# ==============================================================================

# Internal registry environment
.measure_registry <- new.env(parent = emptyenv())

#' Reset the measure registry
#'
#' Clears all registered packs and steps. Used internally by `.onLoad()` and
#' in tests.
#'
#' @return Invisible `NULL`.
#' @noRd
.measure_registry_reset <- function() {
  .measure_registry$packs <- list()
  .measure_registry$steps <- data.frame(
    step_name = character(),
    pack_name = character(),

    category = character(),
    description = character(),
    technique = character(),
    stringsAsFactors = FALSE
  )
  invisible(NULL)
}

#' Register Core Steps
#'
#' Registers all built-in measure steps. Called from `.onLoad()`.
#'
#' @return Invisible `NULL`.
#' @noRd
.register_core_steps <- function() {
  # Use static list to avoid introspection issues
  # Format: list(step_name, category, description)
  core_steps <- list(
    # I/O steps
    list(
      "step_measure_input_long",
      "io",
      "Convert long data to measure format"
    ),
    list(
      "step_measure_input_wide",
      "io",
      "Convert wide data to measure format"
    ),
    list("step_measure_output_long", "io", "Convert measure to long output"),
    list("step_measure_output_wide", "io", "Convert measure to wide output"),

    # Smoothing
    list(
      "step_measure_savitzky_golay",
      "smoothing",
      "Savitzky-Golay smoothing"
    ),
    list("step_measure_smooth_ma", "smoothing", "Moving average smoothing"),
    list("step_measure_smooth_median", "smoothing", "Median filter smoothing"),
    list("step_measure_smooth_gaussian", "smoothing", "Gaussian smoothing"),
    list("step_measure_smooth_wavelet", "smoothing", "Wavelet smoothing"),
    list("step_measure_filter_fourier", "smoothing", "Fourier filtering"),

    # Baseline correction
    list("step_measure_baseline_als", "baseline", "Asymmetric Least Squares"),
    list("step_measure_baseline_poly", "baseline", "Polynomial baseline"),
    list("step_measure_baseline_rf", "baseline", "Robust fitting baseline"),
    list(
      "step_measure_baseline_custom",
      "baseline",
      "Custom baseline function"
    ),
    list("step_measure_baseline_py", "baseline", "Python pybaselines methods"),
    list("step_measure_baseline_rolling", "baseline", "Rolling ball baseline"),
    list("step_measure_baseline_airpls", "baseline", "AirPLS baseline"),
    list("step_measure_baseline_snip", "baseline", "SNIP baseline"),
    list("step_measure_baseline_arpls", "baseline", "arPLS baseline"),
    list("step_measure_baseline_tophat", "baseline", "Top-hat morphological"),
    list("step_measure_baseline_morph", "baseline", "Morphological baseline"),
    list("step_measure_baseline_minima", "baseline", "Local minima baseline"),
    list(
      "step_measure_baseline_auto",
      "baseline",
      "Automatic baseline selection"
    ),
    list(
      "step_measure_baseline_gpc",
      "baseline",
      "GPC/SEC baseline correction"
    ),

    # Normalization (sample-wise)
    list("step_measure_normalize_sum", "normalization", "Sum normalization"),
    list("step_measure_normalize_max", "normalization", "Max normalization"),
    list(
      "step_measure_normalize_range",
      "normalization",
      "Range normalization"
    ),
    list(
      "step_measure_normalize_vector",
      "normalization",
      "Vector normalization"
    ),
    list("step_measure_normalize_auc", "normalization", "AUC normalization"),
    list("step_measure_normalize_peak", "normalization", "Peak normalization"),
    list(
      "step_measure_normalize_istd",
      "normalization",
      "Internal standard normalization"
    ),
    list("step_measure_snv", "normalization", "Standard Normal Variate"),
    list(
      "step_measure_msc",
      "normalization",
      "Multiplicative Scatter Correction"
    ),
    list("step_measure_emsc", "normalization", "Extended MSC"),
    list("step_measure_osc", "normalization", "Orthogonal Signal Correction"),

    # Scaling (variable-wise)
    list("step_measure_center", "scaling", "Mean centering"),
    list("step_measure_scale_auto", "scaling", "Autoscaling"),
    list("step_measure_scale_pareto", "scaling", "Pareto scaling"),
    list("step_measure_scale_range", "scaling", "Range scaling"),
    list("step_measure_scale_vast", "scaling", "VAST scaling"),

    # Alignment
    list("step_measure_align_shift", "alignment", "Shift alignment"),
    list("step_measure_align_reference", "alignment", "Reference alignment"),
    list("step_measure_align_dtw", "alignment", "Dynamic time warping"),
    list("step_measure_align_ptw", "alignment", "Parametric time warping"),
    list(
      "step_measure_align_cow",
      "alignment",
      "Correlation optimized warping"
    ),

    # Peak operations
    list("step_measure_peaks_detect", "peaks", "Peak detection"),
    list("step_measure_peaks_integrate", "peaks", "Peak integration"),
    list("step_measure_peaks_filter", "peaks", "Peak filtering"),
    list("step_measure_peaks_to_table", "peaks", "Convert peaks to table"),
    list("step_measure_peaks_deconvolve", "peaks", "Peak deconvolution"),

    # Quality control
    list("step_measure_qc_snr", "qc", "Signal-to-noise ratio"),
    list("step_measure_qc_saturated", "qc", "Detect saturated signals"),
    list("step_measure_impute", "qc", "Impute missing values"),
    list("step_measure_qc_outlier", "qc", "Outlier detection"),
    list("step_measure_qc_bracket", "qc", "QC bracketing"),
    list("step_measure_despike", "qc", "Spike removal"),

    # Drift correction
    list("step_measure_drift_qc_loess", "drift", "LOESS drift correction"),
    list("step_measure_drift_linear", "drift", "Linear drift correction"),
    list("step_measure_drift_spline", "drift", "Spline drift correction"),

    # Region operations
    list("step_measure_trim", "regions", "Trim spectral range"),
    list("step_measure_exclude", "regions", "Exclude spectral regions"),
    list("step_measure_resample", "regions", "Resample to new grid"),
    list("step_measure_interpolate", "regions", "Interpolate to new grid"),

    # Spectral math
    list("step_measure_absorbance", "spectral", "Transmittance to absorbance"),
    list(
      "step_measure_transmittance",
      "spectral",
      "Absorbance to transmittance"
    ),
    list("step_measure_log", "spectral", "Logarithm transformation"),
    list(
      "step_measure_kubelka_munk",
      "spectral",
      "Kubelka-Munk transformation"
    ),
    list("step_measure_derivative", "spectral", "Savitzky-Golay derivative"),
    list("step_measure_derivative_gap", "spectral", "Gap derivative"),
    list("step_measure_detrend", "spectral", "Polynomial detrending"),

    # Multi-way decomposition
    list("step_measure_parafac", "multiway", "PARAFAC decomposition"),
    list("step_measure_tucker", "multiway", "Tucker decomposition"),
    list("step_measure_mcr_als", "multiway", "MCR-ALS decomposition"),

    # Multi-channel
    list(
      "step_measure_channel_combine",
      "channel",
      "Combine multiple channels"
    ),
    list("step_measure_channel_align", "channel", "Align channel grids"),
    list("step_measure_channel_ratio", "channel", "Channel ratio calculation"),

    # Molecular weight (to be migrated to measure.sec)
    list("step_measure_mw_averages", "mw", "Mn, Mw, Mz, dispersity"),
    list("step_measure_mw_fractions", "mw", "MW fraction calculations"),
    list("step_measure_mw_distribution", "mw", "MW distribution curves"),

    # Calibration
    list("step_measure_calibrate_x", "calibration", "X-axis calibration"),
    list("step_measure_calibrate_y", "calibration", "Y-axis calibration"),

    # Feature engineering
    list("step_measure_integrals", "features", "Spectral integrals"),
    list("step_measure_ratios", "features", "Spectral ratios"),
    list("step_measure_moments", "features", "Statistical moments"),
    list("step_measure_bin", "features", "Spectral binning"),

    # Augmentation
    list("step_measure_augment_noise", "augmentation", "Add random noise"),
    list("step_measure_augment_shift", "augmentation", "Random x-axis shift"),
    list(
      "step_measure_augment_scale",
      "augmentation",
      "Random intensity scaling"
    ),

    # Reference operations
    list("step_measure_subtract_blank", "reference", "Subtract blank signal"),
    list("step_measure_subtract_reference", "reference", "Subtract reference"),
    list("step_measure_ratio_reference", "reference", "Ratio to reference"),

    # Corrections
    list("step_measure_dilution_correct", "correction", "Dilution correction"),
    list("step_measure_surrogate_recovery", "correction", "Surrogate recovery"),
    list("step_measure_standard_addition", "correction", "Standard addition"),
    list(
      "step_measure_batch_reference",
      "correction",
      "Batch reference correction"
    ),

    # General
    list("step_measure_map", "general", "Apply custom function")
  )

  for (s in core_steps) {
    register_measure_step(s[[1]], "measure", s[[2]], s[[3]])
  }
  invisible(NULL)
}


# ==============================================================================
# Registration Functions (exported)
# ==============================================================================

#' Register a Technique Pack
#'
#' Registers an external technique pack with the measure package. This function
#' should be called from the `.onLoad()` function of technique pack packages.
#'
#' @param pack_name Package name (e.g., `"measure.sec"`). Use `pkgname` from
#'   `.onLoad()` for portability.
#' @param technique Technique name (e.g., `"SEC/GPC"`, `"FTIR"`, `"Raman"`).
#' @param version Package version. If `NULL`, attempts to retrieve from
#'   installed package.
#' @param description Brief description of the technique pack.
#'
#' @return Invisible `TRUE`.
#'
#' @examples
#' \dontrun{
#' # In a technique pack's R/zzz.R file:
#' .onLoad <- function(libname, pkgname) {
#'   if (requireNamespace("measure", quietly = TRUE)) {
#'     measure::register_measure_pack(
#'       pack_name = pkgname,
#'       technique = "SEC/GPC",
#'       description = "Size Exclusion Chromatography"
#'     )
#'   }
#' }
#' }
#'
#' @seealso [register_measure_step()], [measure_packs()]
#' @export
register_measure_pack <- function(
  pack_name,
  technique,

  version = NULL,
  description = NULL
) {
  version <- if (is.null(version)) {
    tryCatch(
      as.character(utils::packageVersion(pack_name)),
      error = function(e) "0.0.0"
    )
  } else {
    as.character(version)
  }

  .measure_registry$packs[[pack_name]] <- list(
    name = pack_name,
    technique = technique,
    version = version,
    description = if (is.null(description)) "" else description,
    registered_at = Sys.time()
  )
  invisible(TRUE)
}


#' Register a Step from a Technique Pack
#'
#' Registers a recipe step with the measure package. This function should be
#' called from the `.onLoad()` function of technique pack packages after
#' registering the pack with [register_measure_pack()].
#'
#' Registration is idempotent: calling this function multiple times with the
#' same `pack_name` and `step_name` will update rather than duplicate the entry.
#'
#' @param step_name Full step function name (e.g., `"step_sec_mw_averages"`).
#' @param pack_name Source package name. Use `pkgname` from `.onLoad()`.
#' @param category Step category (e.g., `"preprocessing"`, `"calculation"`).
#' @param description Brief description of what the step does.
#' @param technique Technique name. If `NULL`, inherits from the registered pack.
#'
#' @return Invisible `TRUE`.
#'
#' @examples
#' \dontrun{
#' # In a technique pack's R/zzz.R file:
#' measure::register_measure_step(
#'   step_name = "step_sec_mw_averages",
#'   pack_name = pkgname,
#'   category = "calculation",
#'   description = "Calculate Mn, Mw, Mz, dispersity"
#' )
#' }
#'
#' @seealso [register_measure_pack()], [measure_steps()]
#' @export
register_measure_step <- function(
  step_name,
  pack_name,
  category = "processing",
  description = "",
  technique = NULL
) {
  # Get technique from pack if not specified
  tech <- if (is.null(technique)) {
    pack_info <- .measure_registry$packs[[pack_name]]
    if (is.null(pack_info)) "unknown" else pack_info$technique
  } else {
    technique
  }

  new_step <- data.frame(
    step_name = step_name,
    pack_name = pack_name,
    category = category,
    description = description,
    technique = tech,
    stringsAsFactors = FALSE
  )

  # Idempotent: de-duplicate on (pack_name, step_name)
  existing <- .measure_registry$steps
  keep <- !(existing$pack_name == pack_name & existing$step_name == step_name)
  .measure_registry$steps <- rbind(existing[keep, , drop = FALSE], new_step)

  invisible(TRUE)
}


# ==============================================================================
# Discovery Functions (exported)
# ==============================================================================

#' List Registered Technique Packs
#'
#' Returns a tibble of all registered technique packs, including the core
#' `measure` package.
#'
#' @return A tibble with columns:
#'   - `name`: Package name
#'   - `technique`: Technique category (e.g., "general", "SEC/GPC")
#'   - `version`: Package version
#'   - `description`: Brief description
#'
#' @examples
#' measure_packs()
#'
#' @seealso [measure_steps()], [register_measure_pack()]
#' @export
measure_packs <- function() {
  packs <- .measure_registry$packs

  if (length(packs) == 0) {
    return(tibble::tibble(
      name = character(),
      technique = character(),
      version = character(),
      description = character()
    ))
  }

  tibble::tibble(
    name = unname(vapply(packs, `[[`, character(1), "name")),
    technique = unname(vapply(packs, `[[`, character(1), "technique")),
    version = unname(vapply(packs, `[[`, character(1), "version")),
    description = unname(vapply(packs, `[[`, character(1), "description"))
  )
}


#' List Available Steps
#'
#' Returns a tibble of all registered recipe steps from measure and any loaded
#' technique packs. Results can be filtered by pack, category, or technique.
#'
#' @param packs Character vector of pack names to include. If `NULL`, includes
#'   all packs.
#' @param categories Character vector of step categories to include. If `NULL`,
#'   includes all categories.
#' @param techniques Character vector of techniques to include. If `NULL`,
#'   includes all techniques.
#'
#' @return A tibble with columns:
#'   - `step_name`: Function name (e.g., "step_measure_baseline_als")
#'   - `pack_name`: Source package name
#'   - `category`: Step category (e.g., "baseline", "smoothing")
#'   - `description`: Brief description
#'   - `technique`: Technique (e.g., "general", "SEC/GPC")
#'
#' @examples
#' # List all steps
#' measure_steps()
#'
#' # List only baseline correction steps
#' measure_steps(categories = "baseline")
#'
#' # List steps from a specific technique pack
#' measure_steps(techniques = "SEC/GPC")
#'
#' @seealso [measure_packs()], [register_measure_step()]
#' @export
measure_steps <- function(
  packs = NULL,
  categories = NULL,
  techniques = NULL
) {
  steps <- tibble::as_tibble(.measure_registry$steps)

  # Use .data/.env to avoid column/argument name collision (dplyr data masking)
  if (!is.null(packs)) {
    steps <- dplyr::filter(steps, .data$pack_name %in% .env$packs)
  }
  if (!is.null(categories)) {
    steps <- dplyr::filter(steps, .data$category %in% .env$categories)
  }
  if (!is.null(techniques)) {
    steps <- dplyr::filter(steps, .data$technique %in% .env$techniques)
  }

  steps
}
