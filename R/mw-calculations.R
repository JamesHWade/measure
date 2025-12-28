# ==============================================================================
# Molecular Weight Calculations for SEC/GPC
#
# This file contains steps for size exclusion chromatography analysis:
# - step_measure_mw_averages: Calculate Mn, Mw, Mz, dispersity
# - step_measure_mw_fractions: Calculate weight fractions above/below MW cutoffs
# - step_measure_mw_distribution: Generate MW distribution curves
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

# ==============================================================================
# step_measure_mw_fractions
# ==============================================================================

#' Calculate Molecular Weight Fractions for SEC/GPC
#'
#' `step_measure_mw_fractions()` creates a *specification* of a recipe step that
#' calculates weight fractions above and below specified molecular weight cutoffs.
#'
#' @param recipe A recipe object.
#' @param measures An optional character vector of measure column names.
#' @param cutoffs Numeric vector of MW cutoff values. For each cutoff, the step
#'   calculates the weight fraction below and above that value.
#' @param calibration Calibration method for converting x-axis to log(MW).
#'   See [step_measure_mw_averages()] for details.
#' @param integration_range Optional numeric vector `c(min, max)` specifying
#'   the x-axis range for integration. If `NULL`, uses full range.
#' @param prefix Prefix for output column names. Default is `"frac_"`.
#' @param role Role for generated columns. Default is `"predictor"`.
#' @param trained Logical indicating if the step has been trained.
#' @param skip Logical. Should the step be skipped when baking?
#' @param id Unique step identifier.
#'
#' @return An updated recipe with the new step added.
#'
#' @details
#' For each cutoff value `C`, this step calculates:
#' - `frac_below_C`: Weight fraction with MW < C
#' - `frac_above_C`: Weight fraction with MW >= C
#'
#' These fractions sum to 1.0 and are useful for characterizing polymer
#' distributions. Common cutoffs include:
#' - 1000 Da for oligomer content
#' - 10000 Da for low MW fraction
#' - 100000 Da for high MW fraction
#'
#' @family measure-chromatography
#' @export
#'
#' @examples
#' library(recipes)
#'
#' # Calculate fractions at multiple cutoffs
#' # rec <- recipe(~., data = gpc_data) |>
#' #   step_measure_input_wide(starts_with("signal_")) |>
#' #   step_measure_baseline_als() |>
#' #   step_measure_mw_fractions(cutoffs = c(1000, 10000, 100000)) |>
#' #   prep()
step_measure_mw_fractions <- function(
    recipe,
    measures = NULL,
    cutoffs = c(1000, 10000, 100000),
    calibration = NULL,
    integration_range = NULL,
    prefix = "frac_",
    role = "predictor",
    trained = FALSE,
    skip = FALSE,
    id = recipes::rand_id("measure_mw_fractions")) {
  if (!is.numeric(cutoffs) || length(cutoffs) < 1) {
    cli::cli_abort("{.arg cutoffs} must be a numeric vector with at least one value.")
  }

  if (any(cutoffs <= 0)) {
    cli::cli_abort("All {.arg cutoffs} must be positive values.")
  }

  if (!is.null(integration_range)) {
    if (!is.numeric(integration_range) || length(integration_range) != 2) {
      cli::cli_abort("{.arg integration_range} must be a numeric vector of length 2.")
    }
  }

  recipes::add_step(
    recipe,
    step_measure_mw_fractions_new(
      measures = measures,
      cutoffs = sort(cutoffs),
      calibration = calibration,
      integration_range = integration_range,
      prefix = prefix,
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_measure_mw_fractions_new <- function(
    measures, cutoffs, calibration, integration_range, prefix,
    role, trained, skip, id) {
  recipes::step(
    subclass = "measure_mw_fractions",
    measures = measures,
    cutoffs = cutoffs,
    calibration = calibration,
    integration_range = integration_range,
    prefix = prefix,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_mw_fractions <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  step_measure_mw_fractions_new(
    measures = measure_cols,
    cutoffs = x$cutoffs,
    calibration = x$calibration,
    integration_range = x$integration_range,
    prefix = x$prefix,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' Calculate MW fractions from a single chromatogram
#' @noRd
.calc_mw_fractions <- function(location, value, cutoffs, calibration,
                                integration_range) {
  # Apply integration range
  if (!is.null(integration_range)) {
    idx <- location >= integration_range[1] & location <= integration_range[2]
    location <- location[idx]
    value <- value[idx]
  }

  n_cutoffs <- length(cutoffs)
  result <- numeric(n_cutoffs * 2)
  names(result) <- c(
    paste0("below_", cutoffs),
    paste0("above_", cutoffs)
  )

  if (length(location) < 2) {
    return(rep(NA_real_, length(result)))
  }

  # Convert x-axis to log10(MW) if calibration provided
  if (is.null(calibration)) {
    log_mw <- location
  } else if (is.numeric(calibration) && length(calibration) == 2) {
    log_mw <- calibration[1] * location + calibration[2]
  } else if (identical(calibration, "auto")) {
    log_mw <- seq(7, 2, length.out = length(location))
  } else {
    log_mw <- location
  }

  # Convert to MW
  mw <- 10^log_mw

  # Weight is proportional to signal
  w <- pmax(value, 0)
  total_w <- sum(w)

  if (total_w <= 0) {
    return(rep(NA_real_, length(result)))
  }

  # Calculate fractions for each cutoff
  for (i in seq_along(cutoffs)) {
    cutoff <- cutoffs[i]
    below_idx <- mw < cutoff
    frac_below <- sum(w[below_idx]) / total_w
    result[paste0("below_", cutoff)] <- frac_below
    result[paste0("above_", cutoff)] <- 1 - frac_below
  }

  result
}

#' @export
bake.step_measure_mw_fractions <- function(object, new_data, ...) {
  calibration <- object$calibration
  integration_range <- object$integration_range
  cutoffs <- object$cutoffs
  prefix <- object$prefix

  # Calculate MW fractions for each sample
  all_results <- purrr::map(new_data[[object$measures[1]]], function(m) {
    .calc_mw_fractions(m$location, m$value, cutoffs, calibration,
                        integration_range)
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
print.step_measure_mw_fractions <- function(x, width = max(20, options()$width - 30), ...) {
  cutoffs_str <- paste(format(x$cutoffs, scientific = FALSE), collapse = ", ")
  title <- paste0("MW fractions at cutoffs: ", cutoffs_str)
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
tidy.step_measure_mw_fractions <- function(x, ...) {
  tibble::tibble(
    cutoffs = list(x$cutoffs),
    prefix = x$prefix,
    id = x$id
  )
}

#' @rdname required_pkgs.recipe
#' @export
#' @keywords internal
required_pkgs.step_measure_mw_fractions <- function(x, ...) {
  c("measure")
}

# ==============================================================================
# step_measure_mw_distribution
# ==============================================================================

#' Generate Molecular Weight Distribution Curve
#'
#' `step_measure_mw_distribution()` creates a *specification* of a recipe step
#' that generates molecular weight distribution curves from SEC/GPC data.
#'
#' @param recipe A recipe object.
#' @param measures An optional character vector of measure column names.
#' @param type Type of distribution to generate:
#'   - `"differential"` (default): dW/d(log M) differential distribution
#'   - `"cumulative"`: Cumulative weight fraction distribution
#'   - `"both"`: Generate both distributions
#' @param calibration Calibration method for converting x-axis to log(MW).
#'   See [step_measure_mw_averages()] for details.
#' @param n_points Number of points in the output distribution. Default is 100.
#'   If `NULL`, uses the original data resolution.
#' @param mw_range Optional numeric vector `c(min, max)` specifying the MW range
#'   for the output distribution. If `NULL`, uses the range from data.
#' @param normalize Logical. Should the differential distribution be normalized
#'   to integrate to 1? Default is `TRUE`.
#' @param role Role for generated columns. Default is `"predictor"`.
#' @param trained Logical indicating if the step has been trained.
#' @param skip Logical. Should the step be skipped when baking?
#' @param id Unique step identifier.
#'
#' @return An updated recipe with the new step added.
#'
#' @details
#' This step transforms the raw chromatogram into standard MW distribution
#' representations:
#'
#' **Differential Distribution (dW/d(log M)):**
#' The weight fraction per unit log(MW). This representation is preferred

#' because the area under the curve represents the weight fraction in that
#' MW range.
#'
#' **Cumulative Distribution:**
#' The cumulative weight fraction from low to high MW. Values range from 0 to 1.
#'
#' The output replaces the `.measures` column with the distribution data,
#' where `location` contains log10(MW) values and `value` contains the
#' distribution values.
#'
#' @family measure-chromatography
#' @export
#'
#' @examples
#' library(recipes)
#'
#' # Generate differential MW distribution
#' # rec <- recipe(~., data = gpc_data) |>
#' #   step_measure_input_wide(starts_with("signal_")) |>
#' #   step_measure_baseline_als() |>
#' #   step_measure_mw_distribution(type = "differential") |>
#' #   prep()
step_measure_mw_distribution <- function(
    recipe,
    measures = NULL,
    type = c("differential", "cumulative", "both"),
    calibration = NULL,
    n_points = 100L,
    mw_range = NULL,
    normalize = TRUE,
    role = NA,
    trained = FALSE,
    skip = FALSE,
    id = recipes::rand_id("measure_mw_distribution")) {
  type <- match.arg(type)

  if (!is.null(n_points)) {
    if (!is.numeric(n_points) || length(n_points) != 1 || n_points < 10) {
      cli::cli_abort("{.arg n_points} must be a positive integer >= 10 or NULL.")
    }
    n_points <- as.integer(n_points)
  }

  if (!is.null(mw_range)) {
    if (!is.numeric(mw_range) || length(mw_range) != 2) {
      cli::cli_abort("{.arg mw_range} must be a numeric vector of length 2.")
    }
    if (mw_range[1] >= mw_range[2]) {
      cli::cli_abort("{.arg mw_range} must have min < max.")
    }
  }

  recipes::add_step(
    recipe,
    step_measure_mw_distribution_new(
      measures = measures,
      type = type,
      calibration = calibration,
      n_points = n_points,
      mw_range = mw_range,
      normalize = normalize,
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_measure_mw_distribution_new <- function(
    measures, type, calibration, n_points, mw_range, normalize,
    role, trained, skip, id) {
  recipes::step(
    subclass = "measure_mw_distribution",
    measures = measures,
    type = type,
    calibration = calibration,
    n_points = n_points,
    mw_range = mw_range,
    normalize = normalize,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_mw_distribution <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  step_measure_mw_distribution_new(
    measures = measure_cols,
    type = x$type,
    calibration = x$calibration,
    n_points = x$n_points,
    mw_range = x$mw_range,
    normalize = x$normalize,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' Generate MW distribution from a single chromatogram
#' @noRd
.generate_mw_distribution <- function(location, value, type, calibration,
                                       n_points, mw_range, normalize) {
  if (length(location) < 2) {
    return(list(
      differential = tibble::tibble(location = numeric(0), value = numeric(0)),
      cumulative = tibble::tibble(location = numeric(0), value = numeric(0))
    ))
  }

  # Convert x-axis to log10(MW) if calibration provided
  if (is.null(calibration)) {
    log_mw <- location
  } else if (is.numeric(calibration) && length(calibration) == 2) {
    log_mw <- calibration[1] * location + calibration[2]
  } else if (identical(calibration, "auto")) {
    log_mw <- seq(7, 2, length.out = length(location))
  } else {
    log_mw <- location
  }

  # Weight is proportional to signal
  w <- pmax(value, 0)

  # Sort by log_mw (ascending order for cumulative)
  ord <- order(log_mw)
  log_mw <- log_mw[ord]
  w <- w[ord]

  # Determine MW range for output
  if (is.null(mw_range)) {
    out_range <- range(log_mw)
  } else {
    out_range <- log10(mw_range)
  }

  # Create output grid
  if (is.null(n_points)) {
    out_log_mw <- log_mw
  } else {
    out_log_mw <- seq(out_range[1], out_range[2], length.out = n_points)
  }

  result <- list()

  # Calculate differential distribution: dW/d(log M)
  if (type %in% c("differential", "both")) {
    # Interpolate weights to output grid
    if (length(log_mw) >= 2) {
      interp_w <- stats::approx(log_mw, w, xout = out_log_mw, rule = 2)$y
    } else {
      interp_w <- rep(w[1], length(out_log_mw))
    }

    # For differential distribution, we want dW/d(log M)
    # The detector signal is already proportional to dW/dV (volume)
    # We need to convert to dW/d(log M)

    # If calibration is linear: log M = a*V + b
    # Then d(log M)/dV = a (constant)
    # So dW/d(log M) = (1/a) * dW/dV

    # For now, we use the interpolated weights directly as the differential
    # This assumes uniform spacing in log M space after interpolation
    diff_dist <- interp_w

    if (normalize && sum(diff_dist) > 0) {
      # Normalize so integral = 1
      d_log_mw <- if (length(out_log_mw) > 1) {
        mean(diff(out_log_mw))
      } else {
        1
      }
      diff_dist <- diff_dist / (sum(diff_dist) * d_log_mw)
    }

    result$differential <- new_measure_tbl(out_log_mw, diff_dist)
  }

  # Calculate cumulative distribution
  if (type %in% c("cumulative", "both")) {
    # Interpolate weights to output grid
    if (length(log_mw) >= 2) {
      interp_w <- stats::approx(log_mw, w, xout = out_log_mw, rule = 2)$y
    } else {
      interp_w <- rep(w[1], length(out_log_mw))
    }

    # Cumulative sum normalized to [0, 1]
    cum_dist <- cumsum(interp_w)
    if (max(cum_dist) > 0) {
      cum_dist <- cum_dist / max(cum_dist)
    }

    result$cumulative <- new_measure_tbl(out_log_mw, cum_dist)
  }

  result
}

#' @export
bake.step_measure_mw_distribution <- function(object, new_data, ...) {
  type <- object$type
  calibration <- object$calibration
  n_points <- object$n_points
  mw_range <- object$mw_range
  normalize <- object$normalize
  measure_col <- object$measures[1]

  # Generate distributions for each sample
  all_results <- purrr::map(new_data[[measure_col]], function(m) {
    .generate_mw_distribution(m$location, m$value, type, calibration,
                               n_points, mw_range, normalize)
  })

  # Replace measure column with distribution data
  if (type == "differential") {
    new_data[[measure_col]] <- new_measure_list(
      purrr::map(all_results, ~ .x$differential)
    )
  } else if (type == "cumulative") {
    new_data[[measure_col]] <- new_measure_list(
      purrr::map(all_results, ~ .x$cumulative)
    )
  } else {
    # Both - create two columns
    new_data[[paste0(measure_col, "_differential")]] <- new_measure_list(
      purrr::map(all_results, ~ .x$differential)
    )
    new_data[[paste0(measure_col, "_cumulative")]] <- new_measure_list(
      purrr::map(all_results, ~ .x$cumulative)
    )
    # Keep original column as differential
    new_data[[measure_col]] <- new_measure_list(
      purrr::map(all_results, ~ .x$differential)
    )
  }

  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_mw_distribution <- function(x, width = max(20, options()$width - 30), ...) {
  title <- paste0("MW distribution (", x$type, ")")
  if (!is.null(x$n_points)) {
    title <- paste0(title, ", ", x$n_points, " points")
  }
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
tidy.step_measure_mw_distribution <- function(x, ...) {
  tibble::tibble(
    type = x$type,
    n_points = x$n_points %||% NA_integer_,
    normalize = x$normalize,
    id = x$id
  )
}

#' @rdname required_pkgs.recipe
#' @export
#' @keywords internal
required_pkgs.step_measure_mw_distribution <- function(x, ...) {
  c("measure")
}
