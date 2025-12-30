# ==============================================================================
# Limit of Detection (LOD) and Limit of Quantitation (LOQ)
#
# This file provides functions for calculating detection and quantitation
# limits using multiple accepted methods with explicit method disclosure.
# ==============================================================================

#' Calculate Limit of Detection (LOD)
#'
#' Calculates the limit of detection using one of several accepted methods.
#' The method used is explicitly documented in the output.
#'
#' @param data A data frame containing the measurement data.
#' @param response_col Name of the response column.
#' @param method Method for LOD calculation:
#'   - `"blank_sd"`: 3 * SD of blank samples (requires `sample_type == "blank"`)
#'   - `"calibration"`: 3.3 * sigma / slope from calibration curve
#'   - `"sn"`: Signal-to-noise ratio method (requires `sn_col` or noise estimate)
#'   - `"precision"`: Based on acceptable precision at low concentrations
#' @param conc_col Name of concentration column (for calibration method).
#' @param sample_type_col Name of sample type column. Default is `"sample_type"`.
#' @param calibration Optional [measure_calibration] object for calibration method.
#' @param k Multiplier for SD. Default is 3 for LOD.
#' @param sn_col Column containing S/N ratios (for `"sn"` method).
#' @param noise Noise estimate for S/N calculation (alternative to `sn_col`).
#' @param sn_threshold S/N threshold for LOD (default 3).
#' @param ... Additional arguments passed to method-specific calculations.
#'
#' @return A `measure_lod` object containing:
#'   - `value`: The LOD value
#'   - `method`: Method used
#'   - `parameters`: Method-specific parameters
#'   - `uncertainty`: Uncertainty estimate (when available)
#'
#' @details
#' ## Blank SD Method
#' LOD = mean(blank) + k * SD(blank)
#'
#' Where k is typically 3. This is a simple but widely accepted approach.
#'
#' ## Calibration Method
#' LOD = k * sigma / slope
#'
#' Where sigma is the residual standard error of the calibration curve
#' and slope is the calibration slope. k is typically 3.3 for LOD.
#'
#' ## Signal-to-Noise Method
#' LOD is the concentration where S/N = threshold (typically 3:1).
#'
#' ## Precision-Based Method
#' LOD is the lowest concentration where precision (CV) meets
#' a specified criterion.
#'
#' @seealso [measure_loq()] for limit of quantitation,
#'   [measure_lod_loq()] for calculating both together.
#'
#' @examples
#' # Create sample data with blanks
#' data <- data.frame(
#'   sample_type = c(rep("blank", 10), rep("standard", 5)),
#'   response = c(rnorm(10, mean = 0.5, sd = 0.1),
#'                c(5, 15, 35, 70, 150)),
#'   nominal_conc = c(rep(0, 10), c(10, 25, 50, 100, 200))
#' )
#'
#' # LOD from blank SD
#' measure_lod(data, "response", method = "blank_sd")
#'
#' # LOD from calibration curve
#' cal <- measure_calibration_fit(
#'   data[data$sample_type == "standard", ],
#'   response ~ nominal_conc
#' )
#' measure_lod(data, "response", method = "calibration", calibration = cal)
#'
#' @export
measure_lod <- function(
    data,
    response_col,
    method = c("blank_sd", "calibration", "sn", "precision"),
    conc_col = "nominal_conc",
    sample_type_col = "sample_type",
    calibration = NULL,
    k = 3,
    sn_col = NULL,
    noise = NULL,
    sn_threshold = 3,
    ...) {

  method <- match.arg(method)

  result <- switch(
    method,
    blank_sd = .lod_blank_sd(data, response_col, sample_type_col, k),
    calibration = .lod_calibration(data, response_col, conc_col, calibration, k = 3.3),
    sn = .lod_signal_noise(data, response_col, conc_col, sn_col, noise, sn_threshold),
    precision = .lod_precision(data, response_col, conc_col, sample_type_col, ...)
  )

  result$method <- method
  result$k <- k
  class(result) <- "measure_lod"
  result
}

#' Calculate Limit of Quantitation (LOQ)
#'
#' Calculates the limit of quantitation using one of several accepted methods.
#' The method used is explicitly documented in the output.
#'
#' @inheritParams measure_lod
#' @param k Multiplier for SD. Default is 10 for LOQ.
#' @param sn_threshold S/N threshold for LOQ (default 10).
#' @param precision_cv Maximum allowable CV for LOQ (default 20%).
#'
#' @return A `measure_loq` object containing:
#'   - `value`: The LOQ value
#'   - `method`: Method used
#'   - `parameters`: Method-specific parameters
#'   - `uncertainty`: Uncertainty estimate (when available)
#'
#' @details
#' ## Blank SD Method
#' LOQ = mean(blank) + k * SD(blank)
#'
#' Where k is typically 10. This is a simple but widely accepted approach.
#'
#' ## Calibration Method
#' LOQ = k * sigma / slope
#'
#' Where sigma is the residual standard error of the calibration curve
#' and slope is the calibration slope. k is typically 10 for LOQ.
#'
#' ## Signal-to-Noise Method
#' LOQ is the concentration where S/N = threshold (typically 10:1).
#'
#' ## Precision-Based Method
#' LOQ is the lowest concentration where precision (CV) is <= the
#' specified criterion (typically 20% for bioanalytical methods).
#'
#' @seealso [measure_lod()] for limit of detection,
#'   [measure_lod_loq()] for calculating both together.
#'
#' @examples
#' # Create sample data with blanks
#' data <- data.frame(
#'   sample_type = c(rep("blank", 10), rep("standard", 5)),
#'   response = c(rnorm(10, mean = 0.5, sd = 0.1),
#'                c(5, 15, 35, 70, 150)),
#'   nominal_conc = c(rep(0, 10), c(10, 25, 50, 100, 200))
#' )
#'
#' # LOQ from blank SD
#' measure_loq(data, "response", method = "blank_sd")
#'
#' @export
measure_loq <- function(
    data,
    response_col,
    method = c("blank_sd", "calibration", "sn", "precision"),
    conc_col = "nominal_conc",
    sample_type_col = "sample_type",
    calibration = NULL,
    k = 10,
    sn_col = NULL,
    noise = NULL,
    sn_threshold = 10,
    precision_cv = 20,
    ...) {

  method <- match.arg(method)

  result <- switch(
    method,
    blank_sd = .lod_blank_sd(data, response_col, sample_type_col, k),
    calibration = .lod_calibration(data, response_col, conc_col, calibration, k = 10),
    sn = .lod_signal_noise(data, response_col, conc_col, sn_col, noise, sn_threshold),
    precision = .loq_precision(data, response_col, conc_col, sample_type_col, precision_cv, ...)
  )

  result$method <- method
  result$k <- k
  class(result) <- "measure_loq"
  result
}

#' Calculate LOD and LOQ Together
#'
#' Convenience function to calculate both LOD and LOQ using the same method.
#'
#' @inheritParams measure_lod
#' @param k_lod Multiplier for LOD (default 3 or 3.3 for calibration).
#' @param k_loq Multiplier for LOQ (default 10).
#'
#' @return A list with components `lod` and `loq`, each being the
#'   respective limit object.
#'
#' @seealso [measure_lod()], [measure_loq()].
#'
#' @examples
#' data <- data.frame(
#'   sample_type = c(rep("blank", 10), rep("standard", 5)),
#'   response = c(rnorm(10, mean = 0.5, sd = 0.1),
#'                c(5, 15, 35, 70, 150)),
#'   nominal_conc = c(rep(0, 10), c(10, 25, 50, 100, 200))
#' )
#'
#' limits <- measure_lod_loq(data, "response", method = "blank_sd")
#' limits$lod
#' limits$loq
#'
#' @export
measure_lod_loq <- function(
    data,
    response_col,
    method = c("blank_sd", "calibration", "sn", "precision"),
    conc_col = "nominal_conc",
    sample_type_col = "sample_type",
    calibration = NULL,
    k_lod = NULL,
    k_loq = 10,
    ...) {

  method <- match.arg(method)

  # Set default k_lod based on method
  if (is.null(k_lod)) {
    k_lod <- if (method == "calibration") 3.3 else 3
  }

  lod <- measure_lod(
    data = data,
    response_col = response_col,
    method = method,
    conc_col = conc_col,
    sample_type_col = sample_type_col,
    calibration = calibration,
    k = k_lod,
    ...
  )

  loq <- measure_loq(
    data = data,
    response_col = response_col,
    method = method,
    conc_col = conc_col,
    sample_type_col = sample_type_col,
    calibration = calibration,
    k = k_loq,
    ...
  )

  structure(
    list(lod = lod, loq = loq, method = method),
    class = "measure_lod_loq"
  )
}

# ==============================================================================
# Print methods
# ==============================================================================

#' @export
print.measure_lod <- function(x, ...) {
  cat("<measure_lod>\n")
  cat("  Value: ", format(x$value, digits = 4), "\n", sep = "")
  cat("  Method: ", x$method, "\n", sep = "")
  cat("  k: ", x$k, "\n", sep = "")

  if (!is.null(x$uncertainty)) {
    cat("  Uncertainty: ", format(x$uncertainty, digits = 4), "\n", sep = "")
  }

  if (!is.null(x$parameters)) {
    cat("  Parameters:\n")
    for (nm in names(x$parameters)) {
      cat("    ", nm, ": ", format(x$parameters[[nm]], digits = 4), "\n", sep = "")
    }
  }

  invisible(x)
}

#' @export
print.measure_loq <- function(x, ...) {
  cat("<measure_loq>\n")
  cat("  Value: ", format(x$value, digits = 4), "\n", sep = "")
  cat("  Method: ", x$method, "\n", sep = "")
  cat("  k: ", x$k, "\n", sep = "")

  if (!is.null(x$uncertainty)) {
    cat("  Uncertainty: ", format(x$uncertainty, digits = 4), "\n", sep = "")
  }

  if (!is.null(x$parameters)) {
    cat("  Parameters:\n")
    for (nm in names(x$parameters)) {
      cat("    ", nm, ": ", format(x$parameters[[nm]], digits = 4), "\n", sep = "")
    }
  }

  invisible(x)
}

#' @export
print.measure_lod_loq <- function(x, ...) {
  cat("<measure_lod_loq>\n")
  cat("  Method: ", x$method, "\n", sep = "")
  cat("  LOD: ", format(x$lod$value, digits = 4), "\n", sep = "")
  cat("  LOQ: ", format(x$loq$value, digits = 4), "\n", sep = "")
  invisible(x)
}

# ==============================================================================
# Tidy methods
# ==============================================================================

#' Tidy LOD/LOQ Results
#'
#' @param x A measure_lod, measure_loq, or measure_lod_loq object.
#' @param ... Additional arguments (unused).
#'
#' @return A tibble with the limit value(s) and method information.
#'
#' @importFrom generics tidy
#' @export
tidy.measure_lod <- function(x, ...) {
  tibble::tibble(
    limit_type = "LOD",
    value = x$value,
    method = x$method,
    k = x$k,
    uncertainty = x$uncertainty %||% NA_real_
  )
}

#' @export
tidy.measure_loq <- function(x, ...) {
  tibble::tibble(
    limit_type = "LOQ",
    value = x$value,
    method = x$method,
    k = x$k,
    uncertainty = x$uncertainty %||% NA_real_
  )
}

#' @export
tidy.measure_lod_loq <- function(x, ...) {
  dplyr::bind_rows(
    tidy(x$lod),
    tidy(x$loq)
  )
}

# ==============================================================================
# Internal calculation methods
# ==============================================================================

.lod_blank_sd <- function(data, response_col, sample_type_col, k) {
  if (!sample_type_col %in% names(data)) {
    cli::cli_abort("Column {.field {sample_type_col}} not found in data.")
  }

  blanks <- data[[response_col]][data[[sample_type_col]] == "blank"]

  if (length(blanks) < 2) {
    cli::cli_abort("At least 2 blank samples are required for blank_sd method.")
  }

  blank_mean <- mean(blanks, na.rm = TRUE)
  blank_sd <- stats::sd(blanks, na.rm = TRUE)
  value <- blank_mean + k * blank_sd

  list(
    value = value,
    parameters = list(
      blank_mean = blank_mean,
      blank_sd = blank_sd,
      n_blanks = sum(!is.na(blanks))
    ),
    uncertainty = k * blank_sd / sqrt(length(blanks))  # Approximate SE
  )
}

.lod_calibration <- function(data, response_col, conc_col, calibration, k) {
  if (is.null(calibration)) {
    cli::cli_abort(
      "A {.cls measure_calibration} object is required for calibration method."
    )
  }

  if (!is_measure_calibration(calibration)) {
    cli::cli_abort("{.arg calibration} must be a {.cls measure_calibration} object.")
  }

  sigma <- calibration$diagnostics$sigma
  coefs <- stats::coef(calibration$model)
  slope <- coefs[calibration$conc_col]

  value <- k * sigma / abs(slope)

  list(
    value = value,
    parameters = list(
      sigma = sigma,
      slope = slope
    ),
    uncertainty = NULL  # Could be calculated via error propagation
  )
}

.lod_signal_noise <- function(data, response_col, conc_col, sn_col, noise, sn_threshold) {
  if (is.null(sn_col) && is.null(noise)) {
    cli::cli_abort("Either {.arg sn_col} or {.arg noise} must be provided for S/N method.")
  }

  if (!is.null(sn_col)) {
    if (!sn_col %in% names(data)) {
      cli::cli_abort("Column {.field {sn_col}} not found in data.")
    }
    sn_values <- data[[sn_col]]
    conc_values <- data[[conc_col]]

    # Find concentration where S/N crosses threshold
    # Simple linear interpolation
    above_threshold <- sn_values >= sn_threshold
    if (all(above_threshold)) {
      # All above threshold - LOD is below lowest concentration
      value <- min(conc_values, na.rm = TRUE)
      cli::cli_warn("All S/N values above threshold. LOD may be below lowest measured concentration.")
    } else if (!any(above_threshold)) {
      value <- NA_real_
      cli::cli_warn("No S/N values above threshold. Cannot determine LOD.")
    } else {
      # Interpolate
      idx_above <- which(above_threshold)
      idx_below <- which(!above_threshold)
      # Find the transition point
      value <- conc_values[min(idx_above)]
    }
  } else {
    # Calculate S/N from noise estimate
    signal <- data[[response_col]]
    sn_calc <- signal / noise
    conc_values <- data[[conc_col]]

    above_threshold <- sn_calc >= sn_threshold
    if (!any(above_threshold)) {
      value <- NA_real_
    } else {
      value <- min(conc_values[above_threshold], na.rm = TRUE)
    }
  }

  list(
    value = value,
    parameters = list(
      sn_threshold = sn_threshold,
      noise = noise
    ),
    uncertainty = NULL
  )
}

.lod_precision <- function(data, response_col, conc_col, sample_type_col, cv_threshold = 33, ...) {
  # Calculate CV at each concentration level
  cv_by_conc <- data |>
    dplyr::group_by(.data[[conc_col]]) |>
    dplyr::summarize(
      mean_response = mean(.data[[response_col]], na.rm = TRUE),
      sd_response = stats::sd(.data[[response_col]], na.rm = TRUE),
      cv = 100 * .data$sd_response / .data$mean_response,
      n = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::filter(!is.na(.data$cv))

  # Find lowest concentration where CV meets criterion
  passing <- cv_by_conc |>
    dplyr::filter(.data$cv <= cv_threshold) |>
    dplyr::arrange(.data[[conc_col]])

  if (nrow(passing) == 0) {
    value <- NA_real_
    cli::cli_warn("No concentration level meets the CV criterion of {cv_threshold}%.")
  } else {
    value <- passing[[conc_col]][1]
  }

  list(
    value = value,
    parameters = list(
      cv_threshold = cv_threshold,
      cv_data = cv_by_conc
    ),
    uncertainty = NULL
  )
}

.loq_precision <- function(data, response_col, conc_col, sample_type_col, precision_cv = 20, ...) {
  .lod_precision(data, response_col, conc_col, sample_type_col, cv_threshold = precision_cv, ...)
}
