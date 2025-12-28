# ==============================================================================
# Molecular Weight Calculations for SEC/GPC
#
# This file contains steps for size exclusion chromatography analysis:
# - step_measure_mw_averages: Calculate Mn, Mw, Mz, dispersity
# ==============================================================================

# ==============================================================================
# step_measure_mw_averages
# ==============================================================================

#' Calculate Molecular Weight Averages for SEC/GPC
#'
#' `step_measure_mw_averages()` creates a *specification* of a recipe step that
#' calculates molecular weight averages from size exclusion chromatography data.
#'
#' @param recipe A recipe object.
#' @param measures An optional character vector of measure column names.
#' @param calibration Calibration method for converting x-axis to log(MW).
#'   Can be:
#'   - `NULL` (default): Assumes x-axis is already log10(MW)
#'   - A numeric vector of length 2: Linear calibration `c(slope, intercept)`
#'     where `log10(MW) = slope * x + intercept`
#'   - `"auto"`: Estimate from data range (assumes typical polymer range)
#' @param integration_range Optional numeric vector `c(min, max)` specifying
#'   the x-axis range for integration. If `NULL`, uses full range.
#' @param output_cols Character vector of metrics to calculate. Default
#'   includes all: `c("mn", "mw", "mz", "mp", "dispersity")`.
#' @param prefix Prefix for output column names. Default is `"mw_"`.
#' @param role Role for generated columns. Default is `"predictor"`.
#' @param trained Logical indicating if the step has been trained.
#' @param skip Logical. Should the step be skipped when baking?
#' @param id Unique step identifier.
#'
#' @return An updated recipe with the new step added.
#'
#' @details
#' This step calculates standard molecular weight averages from SEC/GPC data:
#'
#' | Metric | Formula | Description |
#' |--------|---------|-------------|
#' | Mn | Σwᵢ / Σ(wᵢ/Mᵢ) | Number-average molecular weight |
#' | Mw | Σ(wᵢMᵢ) / Σwᵢ | Weight-average molecular weight |
#' | Mz | Σ(wᵢMᵢ²) / Σ(wᵢMᵢ) | Z-average molecular weight |
#' | Mp | M at peak maximum | Peak molecular weight |
#' | Đ | Mw/Mn | Dispersity (polydispersity index) |
#'
#' The detector signal is assumed to be proportional to weight concentration.
#' For RI detection, this is typically valid. For UV detection, response factors
#' may need to be applied first using [step_measure_calibrate_y()].
#'
#' **Prerequisites:**
#' - Data should be baseline corrected
#' - X-axis should represent retention time/volume or log(MW)
#' - Integration limits should exclude solvent peaks
#'
#' @family measure-chromatography
#' @export
#'
#' @examples
#' library(recipes)
#'
#' # Assuming x-axis is already calibrated to log10(MW)
#' # rec <- recipe(~., data = gpc_data) |>
#' #   step_measure_input_wide(starts_with("signal_")) |>
#' #   step_measure_baseline_als() |>
#' #   step_measure_mw_averages() |>
#' #   prep()
step_measure_mw_averages <- function(
    recipe,
    measures = NULL,
    calibration = NULL,
    integration_range = NULL,
    output_cols = c("mn", "mw", "mz", "mp", "dispersity"),
    prefix = "mw_",
    role = "predictor",
    trained = FALSE,
    skip = FALSE,
    id = recipes::rand_id("measure_mw_averages")) {
  valid_cols <- c("mn", "mw", "mz", "mp", "dispersity")
  if (!all(output_cols %in% valid_cols)) {
    invalid <- setdiff(output_cols, valid_cols)
    cli::cli_abort(
      "Invalid output columns: {.val {invalid}}. Must be one of: {.val {valid_cols}}"
    )
  }

  if (!is.null(integration_range)) {
    if (!is.numeric(integration_range) || length(integration_range) != 2) {
      cli::cli_abort("{.arg integration_range} must be a numeric vector of length 2.")
    }
  }

  recipes::add_step(
    recipe,
    step_measure_mw_averages_new(
      measures = measures,
      calibration = calibration,
      integration_range = integration_range,
      output_cols = output_cols,
      prefix = prefix,
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_measure_mw_averages_new <- function(
    measures, calibration, integration_range, output_cols, prefix,
    role, trained, skip, id) {
  recipes::step(
    subclass = "measure_mw_averages",
    measures = measures,
    calibration = calibration,
    integration_range = integration_range,
    output_cols = output_cols,
    prefix = prefix,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_mw_averages <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  step_measure_mw_averages_new(
    measures = measure_cols,
    calibration = x$calibration,
    integration_range = x$integration_range,
    output_cols = x$output_cols,
    prefix = x$prefix,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' Calculate MW averages from a single chromatogram
#' @noRd
.calc_mw_averages <- function(location, value, calibration, integration_range,
                               output_cols) {
  # Apply integration range
  if (!is.null(integration_range)) {
    idx <- location >= integration_range[1] & location <= integration_range[2]
    location <- location[idx]
    value <- value[idx]
  }

  if (length(location) < 2) {
    result <- stats::setNames(rep(NA_real_, length(output_cols)), output_cols)
    return(result)
  }

  # Convert x-axis to log10(MW) if calibration provided
  if (is.null(calibration)) {
    log_mw <- location
  } else if (is.numeric(calibration) && length(calibration) == 2) {
    # Linear calibration: log10(MW) = slope * x + intercept
    log_mw <- calibration[1] * location + calibration[2]
  } else if (identical(calibration, "auto")) {
    # Auto-calibration: assume typical polymer range (1e2 to 1e7)
    log_mw <- seq(7, 2, length.out = length(location))
  } else {
    log_mw <- location
  }

  # Convert to MW
mw <- 10^log_mw

  # Weight is proportional to signal (assuming RI detector)
  # Ensure non-negative
  w <- pmax(value, 0)

  # Remove zero weights
  valid <- w > 0
  if (sum(valid) < 2) {
    result <- stats::setNames(rep(NA_real_, length(output_cols)), output_cols)
    return(result)
  }

  mw <- mw[valid]
  w <- w[valid]

  result <- numeric(length(output_cols))
  names(result) <- output_cols

  # Calculate moments
  sum_w <- sum(w)

  if ("mn" %in% output_cols) {
    # Mn = Σwᵢ / Σ(wᵢ/Mᵢ)
    result["mn"] <- sum_w / sum(w / mw)
  }

  if ("mw" %in% output_cols) {
    # Mw = Σ(wᵢMᵢ) / Σwᵢ
    result["mw"] <- sum(w * mw) / sum_w
  }

  if ("mz" %in% output_cols) {
    # Mz = Σ(wᵢMᵢ²) / Σ(wᵢMᵢ)
    result["mz"] <- sum(w * mw^2) / sum(w * mw)
  }

  if ("mp" %in% output_cols) {
    # Mp = MW at peak maximum
    peak_idx <- which.max(w)
    result["mp"] <- mw[peak_idx]
  }

  if ("dispersity" %in% output_cols) {
    # Đ = Mw / Mn
    if ("mw" %in% names(result) && "mn" %in% names(result)) {
      result["dispersity"] <- result["mw"] / result["mn"]
    } else {
      mw_val <- sum(w * mw) / sum_w
      mn_val <- sum_w / sum(w / mw)
      result["dispersity"] <- mw_val / mn_val
    }
  }

  result
}

#' @export
bake.step_measure_mw_averages <- function(object, new_data, ...) {
  calibration <- object$calibration
  integration_range <- object$integration_range
  output_cols <- object$output_cols
  prefix <- object$prefix

  # Calculate MW averages for each sample
  all_results <- purrr::map(new_data[[object$measures[1]]], function(m) {
    .calc_mw_averages(m$location, m$value, calibration, integration_range,
                       output_cols)
  })

  # Convert to data frame
  result_df <- do.call(rbind, all_results)
  result_df <- tibble::as_tibble(result_df)

  # Add prefix to column names
  names(result_df) <- paste0(prefix, names(result_df))

  # Bind to original data
  new_data <- dplyr::bind_cols(new_data, result_df)

  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_mw_averages <- function(x, width = max(20, options()$width - 30), ...) {
  cols <- paste(x$output_cols, collapse = ", ")
  title <- paste0("MW averages (", cols, ")")
  if (x$trained) {
    cat(title, " on <internal measurements>", sep = "")
  } else {
    cat(title)
  }
  cat("\n")
  invisible(x)
}

#' @rdname tidy.recipe
#' @export
#' @keywords internal
tidy.step_measure_mw_averages <- function(x, ...) {
  tibble::tibble(
    output_cols = list(x$output_cols),
    prefix = x$prefix,
    id = x$id
  )
}

#' @rdname required_pkgs.recipe
#' @export
#' @keywords internal
required_pkgs.step_measure_mw_averages <- function(x, ...) {
  c("measure")
}
