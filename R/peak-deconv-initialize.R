# ==============================================================================
# Smart Initialization for Peak Deconvolution
#
# Intelligent parameter initialization that improves optimization convergence
# by using actual peak properties from the data rather than naive guesses.
# ==============================================================================

#' Smart Parameter Initialization for Peak Deconvolution
#'
#' Initializes peak model parameters using actual peak properties from the data
#' rather than naive guesses, improving optimization convergence.
#'
#' @param x Numeric vector of x-axis values.
#' @param y Numeric vector of y-axis values.
#' @param n_peaks Number of peaks to initialize.
#' @param models List of `peak_model` objects (one per peak).
#' @param peak_indices Optional integer vector of peak indices (if already known).
#' @param smooth Logical. If `TRUE`, smooth data before peak detection.
#' @param smooth_span Smoothing span for LOESS (if `smooth = TRUE`).
#'
#' @return List of initialized parameter lists (one per peak).
#'
#' @examples
#' # Create synthetic data with two peaks
#' x <- seq(0, 20, by = 0.1)
#' y <- 1.5 * exp(-0.5 * ((x - 8) / 1)^2) +
#'   0.8 * exp(-0.5 * ((x - 12) / 1.5)^2)
#'
#' models <- list(gaussian_peak_model(), gaussian_peak_model())
#' init_params <- initialize_peak_params(x, y, n_peaks = 2, models = models)
#'
#' @family peak-deconvolution
#' @export
initialize_peak_params <- function(
  x,
  y,
  n_peaks,
  models,
  peak_indices = NULL,
  smooth = TRUE,
  smooth_span = 0.05
) {
  # Validate inputs
  if (!is.numeric(x) || !is.numeric(y)) {
    cli::cli_abort("{.arg x} and {.arg y} must be numeric vectors.")
  }
  if (length(x) != length(y)) {
    cli::cli_abort("{.arg x} and {.arg y} must have the same length.")
  }
  if (n_peaks < 1L) {
    cli::cli_abort("{.arg n_peaks} must be at least 1.")
  }
  if (length(models) != n_peaks) {
    cli::cli_abort(
      "Number of models ({length(models)}) must match n_peaks ({n_peaks})."
    )
  }

  # Step 1: Smooth data to reduce noise
  if (smooth && length(x) >= 4) {
    y_smooth <- .smooth_for_init(x, y, span = smooth_span)
  } else {
    y_smooth <- y
  }

  # Step 2: Find actual peak maxima
  if (is.null(peak_indices) || length(peak_indices) != n_peaks) {
    peak_indices <- .find_local_maxima(x, y_smooth, n = n_peaks)
  }

  # Step 3: Initialize each peak using model-specific logic

  params_list <- vector("list", n_peaks)

  for (i in seq_len(n_peaks)) {
    peak_idx <- peak_indices[i]

    # Use model's initial_guess method
    params_list[[i]] <- peak_model_initial_guess(
      models[[i]],
      x = x,
      y = y_smooth,
      peak_idx = peak_idx
    )

    # Step 4: Detect and incorporate asymmetry for EMG/Bi-Gaussian
    if (models[[i]]$name == "emg") {
      asymmetry <- .detect_peak_asymmetry(x, y_smooth, peak_idx)
      params_list[[i]]$tau <- asymmetry$tau
    } else if (models[[i]]$name == "bigaussian") {
      asymmetry <- .detect_peak_asymmetry(x, y_smooth, peak_idx)
      params_list[[i]]$width_left <- asymmetry$width_left
      params_list[[i]]$width_right <- asymmetry$width_right
    }
  }

  params_list
}


#' Add Jitter to Parameters for Multi-Start Optimization
#'
#' Perturbs initialized parameters to create diverse starting points for
#' multi-start optimization strategies.
#'
#' @param params_list List of parameter lists (one per peak).
#' @param scale Jitter scale (fraction of parameter value).
#' @param method Jitter method: `"gaussian"` or `"uniform"`.
#'
#' @return List of jittered parameter lists.
#'
#' @family peak-deconvolution
#' @export
add_param_jitter <- function(
  params_list,
  scale = 0.1,
  method = c("gaussian", "uniform")
) {
  method <- match.arg(method)

  lapply(params_list, function(params) {
    lapply(params, function(p) {
      if (method == "gaussian") {
        p * (1 + stats::rnorm(1, mean = 0, sd = scale))
      } else {
        p * (1 + stats::runif(1, min = -scale, max = scale))
      }
    })
  })
}


# ==============================================================================
# Internal Helper Functions
# ==============================================================================

#' Smooth Data for Initialization
#'
#' @noRd
.smooth_for_init <- function(x, y, span = 0.05) {
  loess_fit <- tryCatch(
    suppressWarnings(stats::loess(y ~ x, span = span, degree = 2)),
    error = function(e) NULL
  )

  if (is.null(loess_fit)) {
    return(y)
  }

  stats::predict(loess_fit, x)
}


#' Find Local Maxima in Data
#'
#' @param x Numeric vector of x values
#' @param y Numeric vector of y values
#' @param n Number of maxima to find
#' @param min_distance Minimum distance between peaks (in x units)
#'
#' @return Integer vector of indices of local maxima
#' @noRd
.find_local_maxima <- function(x, y, n, min_distance = NULL) {
  if (is.null(min_distance)) {
    # Default: peaks should be at least 5% of range apart
    min_distance <- diff(range(x)) * 0.05
  }

  # Convert min_distance to index units
  avg_dx <- mean(diff(x))
  min_distance_idx <- max(1L, floor(min_distance / avg_dx))

  # Find all local maxima using pracma
  peaks_info <- pracma::findpeaks(
    abs(y), # Use absolute value to find both positive and negative peaks
    npeaks = n * 3L, # Find more than needed
    minpeakdistance = min_distance_idx,
    sortstr = TRUE # Sort by height
  )

  if (is.null(peaks_info) || nrow(peaks_info) == 0) {
    # No peaks found, use equally-spaced fallback
    cli::cli_warn("No peaks detected, using equally-spaced initialization")
    return(round(seq(
      from = length(y) * 0.2,
      to = length(y) * 0.8,
      length.out = n
    )))
  }

  # Extract peak indices (column 2) and return top n
  head(peaks_info[, 2], n)
}


#' Estimate Peak Width at Given Index
#'
#' @noRd
.estimate_width_at_peak <- function(x, y, peak_idx) {
  peak_height <- abs(y[peak_idx])
  half_height <- peak_height / 2

  # Find left half-height
  left_idx <- peak_idx
  while (
    left_idx > 1 && !is.na(y[left_idx]) && abs(y[left_idx]) > half_height
  ) {
    left_idx <- left_idx - 1
  }

  # Find right half-height
  right_idx <- peak_idx
  while (
    right_idx < length(y) &&
      !is.na(y[right_idx]) &&
      abs(y[right_idx]) > half_height
  ) {
    right_idx <- right_idx + 1
  }

  # Calculate FWHM
  fwhm <- abs(x[right_idx] - x[left_idx])

  # Convert FWHM to standard deviation
  # For Gaussian: FWHM = 2 * sqrt(2 * ln(2)) * sigma

  width <- fwhm / (2 * sqrt(2 * log(2)))

  # Ensure reasonable minimum
  min_width <- diff(range(x)) / 100
  max(width, min_width)
}


#' Detect Peak Asymmetry
#'
#' @noRd
.detect_peak_asymmetry <- function(x, y, peak_idx) {
  peak_height <- abs(y[peak_idx])
  peak_center <- x[peak_idx]

  # Measure widths on each side at multiple heights
  height_fractions <- c(0.9, 0.7, 0.5, 0.3, 0.1)
  left_widths <- numeric(length(height_fractions))
  right_widths <- numeric(length(height_fractions))

  for (i in seq_along(height_fractions)) {
    target_height <- peak_height * height_fractions[i]

    # Left side
    left_idx <- peak_idx
    while (
      left_idx > 1 && !is.na(y[left_idx]) && abs(y[left_idx]) > target_height
    ) {
      left_idx <- left_idx - 1
    }
    left_widths[i] <- abs(peak_center - x[left_idx])

    # Right side
    right_idx <- peak_idx
    while (
      right_idx < length(y) &&
        !is.na(y[right_idx]) &&
        abs(y[right_idx]) > target_height
    ) {
      right_idx <- right_idx + 1
    }
    right_widths[i] <- abs(x[right_idx] - peak_center)
  }

  # Calculate asymmetry metrics
  asymmetry_ratio <- mean(right_widths, na.rm = TRUE) /
    mean(left_widths, na.rm = TRUE)

  # Estimate tau for EMG model
  if (is.na(asymmetry_ratio) || is.infinite(asymmetry_ratio)) {
    tau <- 0.01
  } else if (asymmetry_ratio > 1.1) {
    # Right tailing
    tau <- mean(right_widths - left_widths, na.rm = TRUE)
  } else if (asymmetry_ratio < 0.9) {
    # Left tailing (fronting)
    tau <- -mean(left_widths - right_widths, na.rm = TRUE)
  } else {
    # Symmetric
    tau <- 0.01
  }

  # Ensure tau is positive and reasonable
  if (is.na(tau)) {
    tau <- 0.01
  }
  tau <- abs(tau)
  tau <- max(tau, 0.001)
  tau <- min(tau, diff(range(x)) / 5)

  # Estimate widths for bi-Gaussian
  width_left <- mean(left_widths, na.rm = TRUE) / (2 * sqrt(2 * log(2)))
  width_right <- mean(right_widths, na.rm = TRUE) / (2 * sqrt(2 * log(2)))

  # Ensure minimum widths

  min_width <- diff(range(x)) / 100
  if (is.na(width_left)) {
    width_left <- min_width
  }
  if (is.na(width_right)) {
    width_right <- min_width
  }
  width_left <- max(width_left, min_width)
  width_right <- max(width_right, min_width)

  list(
    asymmetry_ratio = asymmetry_ratio,
    tau = tau,
    width_left = width_left,
    width_right = width_right,
    is_symmetric = abs(asymmetry_ratio - 1) < 0.1
  )
}


#' Validate Initialized Parameters
#'
#' @noRd
.validate_initialization <- function(params_list, models, x_range, y_range) {
  for (i in seq_along(params_list)) {
    bounds <- peak_model_bounds(models[[i]], x_range, y_range)

    for (pname in names(params_list[[i]])) {
      val <- params_list[[i]][[pname]]

      # Check bounds
      lower_idx <- which(names(bounds$lower) == pname)
      upper_idx <- which(names(bounds$upper) == pname)

      if (length(lower_idx) > 0 && length(upper_idx) > 0) {
        if (val < bounds$lower[lower_idx] || val > bounds$upper[upper_idx]) {
          return(FALSE)
        }
      }

      # Check for NaN or Inf
      if (is.na(val) || is.infinite(val)) {
        return(FALSE)
      }
    }
  }

  TRUE
}
