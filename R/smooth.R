# ==============================================================================
# Smoothing & Noise Reduction Steps
#
# This file contains smoothing preprocessing steps:
# - step_measure_smooth_ma: Moving average smoothing
# - step_measure_smooth_median: Median filter smoothing
# - step_measure_smooth_gaussian: Gaussian kernel smoothing
# - step_measure_filter_fourier: Fourier low-pass filtering
# ==============================================================================

# ==============================================================================
# step_measure_smooth_ma
# ==============================================================================

#' Moving Average Smoothing
#'
#' `step_measure_smooth_ma()` creates a *specification* of a recipe step that
#' applies moving average smoothing to measurement data. This is a simple and
#' fast method for reducing high-frequency noise.
#'
#' @param recipe A recipe object.
#' @param measures An optional character vector of measure column names.
#' @param window The window size for the moving average. Must be an odd integer
#'   of at least 3. Default is 5. Larger values produce more smoothing. Tunable
#'   via [smooth_window()].
#' @param edge_method How to handle edges where the full window doesn't fit.
#'   One of `"reflect"` (default, reflects values at boundaries), `"constant"`
#'   (pads with edge values), or `"NA"` (returns NA for edge values).
#' @param role Not used.
#' @param trained Logical indicating if the step has been trained.
#' @param skip Logical. Should the step be skipped when baking?
#' @param id Unique step identifier.
#'
#' @return An updated recipe with the new step added.
#'
#' @details
#' Moving average smoothing replaces each point with the mean of its neighbors
#' within a sliding window. This is equivalent to convolution with a uniform
#' kernel.
#'
#' For a window size of `w`, the smoothed value at position `i` is:
#' \deqn{y_i = \frac{1}{w} \sum_{j=-k}^{k} x_{i+j}}{y[i] = mean(x[(i-k):(i+k)])}
#'
#' where `k = (w-1)/2` is the half-window size.
#'
#' @family measure-smoothing
#' @export
#'
#' @examples
#' library(recipes)
#'
#' rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_smooth_ma(window = 5) |>
#'   prep()
#'
#' bake(rec, new_data = NULL)
step_measure_smooth_ma <- function(
    recipe,
    measures = NULL,
    window = 5L,
    edge_method = c("reflect", "constant", "NA"),
    role = NA,
    trained = FALSE,
    skip = FALSE,
    id = recipes::rand_id("measure_smooth_ma")) {
  edge_method <- match.arg(edge_method)

  recipes::add_step(
    recipe,
    step_measure_smooth_ma_new(
      measures = measures,
      window = as.integer(window),
      edge_method = edge_method,
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_measure_smooth_ma_new <- function(
    measures, window, edge_method, role, trained, skip, id) {
  recipes::step(
    subclass = "measure_smooth_ma",
    measures = measures,
    window = window,
    edge_method = edge_method,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_smooth_ma <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  # Validate window
  if (!is.numeric(x$window) || length(x$window) != 1 || x$window < 3) {
    cli::cli_abort("{.arg window} must be an integer >= 3.")
  }

  # Ensure window is odd
  window <- as.integer(x$window)
  if (window %% 2 == 0) {
    cli::cli_abort("{.arg window} must be an odd number.")
  }

  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  step_measure_smooth_ma_new(
    measures = measure_cols,
    window = window,
    edge_method = x$edge_method,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_smooth_ma <- function(object, new_data, ...) {
  for (col in object$measures) {
    result <- purrr::map(
      new_data[[col]],
      .smooth_ma_single,
      window = object$window,
      edge_method = object$edge_method
    )
    new_data[[col]] <- new_measure_list(result)
  }
  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_smooth_ma <- function(x, width = max(20, options()$width - 30), ...) {
 title <- paste0("Moving average smoothing (window = ", x$window, ") on ")
  if (x$trained) {
    cat(title, "<internal measurements>", sep = "")
  } else {
    cat(title)
  }
  cat("\n")
  invisible(x)
}

#' @rdname tidy.recipe
#' @export
#' @keywords internal
tidy.step_measure_smooth_ma <- function(x, ...) {
  tibble::tibble(
    terms = if (is_trained(x)) x$measures else "<all measure columns>",
    window = x$window,
    id = x$id
  )
}

.smooth_ma_single <- function(x, window, edge_method) {
  values <- x$value
  n <- length(values)

  if (n < window) {
    cli::cli_warn("Spectrum length ({n}) is less than window ({window}). Returning unchanged.")
    return(x)
  }

  # Pad values based on edge method
  half <- (window - 1) / 2
  if (edge_method == "reflect") {
    padded <- c(rev(values[2:(half + 1)]), values, rev(values[(n - half):(n - 1)]))
  } else if (edge_method == "constant") {
    padded <- c(rep(values[1], half), values, rep(values[n], half))
  } else {
    padded <- c(rep(NA_real_, half), values, rep(NA_real_, half))
  }

  # Apply moving average using stats::filter
  kernel <- rep(1 / window, window)
  smoothed <- stats::filter(padded, kernel, sides = 2)

  # Extract the valid portion
  x$value <- as.numeric(smoothed[(half + 1):(half + n)])
  x
}


# ==============================================================================
# step_measure_smooth_median
# ==============================================================================

#' Median Filter Smoothing
#'
#' `step_measure_smooth_median()` creates a *specification* of a recipe step
#' that applies median filter smoothing. This is a robust method that is
#' particularly effective at removing spike noise while preserving edges.
#'
#' @inheritParams step_measure_smooth_ma
#'
#' @return An updated recipe with the new step added.
#'
#' @details
#' Median filtering replaces each point with the median of its neighbors
#' within a sliding window. Unlike moving average, median filtering is
#' robust to outliers and spikes, making it ideal for:
#'
#' - Removing cosmic ray spikes in Raman spectroscopy
#' - Cleaning detector artifacts
#' - Preserving sharp edges while removing noise
#'
#' @family measure-smoothing
#' @export
#'
#' @examples
#' library(recipes)
#'
#' rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_smooth_median(window = 5) |>
#'   prep()
#'
#' bake(rec, new_data = NULL)
step_measure_smooth_median <- function(
    recipe,
    measures = NULL,
    window = 5L,
    edge_method = c("reflect", "constant", "NA"),
    role = NA,
    trained = FALSE,
    skip = FALSE,
    id = recipes::rand_id("measure_smooth_median")) {
  edge_method <- match.arg(edge_method)

  recipes::add_step(
    recipe,
    step_measure_smooth_median_new(
      measures = measures,
      window = as.integer(window),
      edge_method = edge_method,
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_measure_smooth_median_new <- function(
    measures, window, edge_method, role, trained, skip, id) {
  recipes::step(
    subclass = "measure_smooth_median",
    measures = measures,
    window = window,
    edge_method = edge_method,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_smooth_median <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  if (!is.numeric(x$window) || length(x$window) != 1 || x$window < 3) {
    cli::cli_abort("{.arg window} must be an integer >= 3.")
  }

  window <- as.integer(x$window)
  if (window %% 2 == 0) {
    cli::cli_abort("{.arg window} must be an odd number.")
  }

  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  step_measure_smooth_median_new(
    measures = measure_cols,
    window = window,
    edge_method = x$edge_method,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_smooth_median <- function(object, new_data, ...) {
  for (col in object$measures) {
    result <- purrr::map(
      new_data[[col]],
      .smooth_median_single,
      window = object$window,
      edge_method = object$edge_method
    )
    new_data[[col]] <- new_measure_list(result)
  }
  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_smooth_median <- function(x, width = max(20, options()$width - 30), ...) {
  title <- paste0("Median filter smoothing (window = ", x$window, ") on ")
  if (x$trained) {
    cat(title, "<internal measurements>", sep = "")
  } else {
    cat(title)
  }
  cat("\n")
  invisible(x)
}

#' @rdname tidy.recipe
#' @export
#' @keywords internal
tidy.step_measure_smooth_median <- function(x, ...) {
  tibble::tibble(
    terms = if (is_trained(x)) x$measures else "<all measure columns>",
    window = x$window,
    id = x$id
  )
}

.smooth_median_single <- function(x, window, edge_method) {
  values <- x$value
  n <- length(values)

  if (n < window) {
    cli::cli_warn("Spectrum length ({n}) is less than window ({window}). Returning unchanged.")
    return(x)
  }

  half <- (window - 1) / 2

  # Pad values
  if (edge_method == "reflect") {
    padded <- c(rev(values[2:(half + 1)]), values, rev(values[(n - half):(n - 1)]))
  } else if (edge_method == "constant") {
    padded <- c(rep(values[1], half), values, rep(values[n], half))
  } else {
    padded <- c(rep(NA_real_, half), values, rep(NA_real_, half))
  }

  # Apply running median
  result <- numeric(n)
  for (i in seq_len(n)) {
    idx <- i:(i + window - 1)
    result[i] <- stats::median(padded[idx], na.rm = TRUE)
  }

  x$value <- result
  x
}


# ==============================================================================
# step_measure_smooth_gaussian
# ==============================================================================

#' Gaussian Kernel Smoothing
#'
#' `step_measure_smooth_gaussian()` creates a *specification* of a recipe step
#' that applies Gaussian kernel smoothing. This produces smooth results while
#' preserving the general shape of peaks.
#'
#' @param recipe A recipe object.
#' @param measures An optional character vector of measure column names.
#' @param sigma The standard deviation of the Gaussian kernel. Default is 1.
#'   Larger values produce more smoothing. Tunable via [smooth_sigma()].
#' @param window The window size. If `NULL` (default), automatically set to
#'   `ceiling(6 * sigma) | 1` (6 sigma rule, ensuring odd).
#' @param edge_method How to handle edges. One of `"reflect"` (default),
#'   `"constant"`, or `"NA"`.
#' @param role Not used.
#' @param trained Logical indicating if the step has been trained.
#' @param skip Logical. Should the step be skipped when baking?
#' @param id Unique step identifier.
#'
#' @return An updated recipe with the new step added.
#'
#' @details
#' Gaussian smoothing convolves the spectrum with a Gaussian kernel:
#' \deqn{G(x) = \exp(-x^2 / 2\sigma^2)}
#'
#' The kernel is normalized to sum to 1. This provides smooth, natural-looking
#' results that preserve peak shapes better than moving average.
#'
#' @family measure-smoothing
#' @export
#'
#' @examples
#' library(recipes)
#'
#' rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_smooth_gaussian(sigma = 2) |>
#'   prep()
#'
#' bake(rec, new_data = NULL)
step_measure_smooth_gaussian <- function(
    recipe,
    measures = NULL,
    sigma = 1,
    window = NULL,
    edge_method = c("reflect", "constant", "NA"),
    role = NA,
    trained = FALSE,
    skip = FALSE,
    id = recipes::rand_id("measure_smooth_gaussian")) {
  edge_method <- match.arg(edge_method)

  recipes::add_step(
    recipe,
    step_measure_smooth_gaussian_new(
      measures = measures,
      sigma = sigma,
      window = window,
      edge_method = edge_method,
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_measure_smooth_gaussian_new <- function(
    measures, sigma, window, edge_method, role, trained, skip, id) {
  recipes::step(
    subclass = "measure_smooth_gaussian",
    measures = measures,
    sigma = sigma,
    window = window,
    edge_method = edge_method,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_smooth_gaussian <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  if (!is.numeric(x$sigma) || length(x$sigma) != 1 || x$sigma <= 0) {
    cli::cli_abort("{.arg sigma} must be a positive number.")
  }

  # Calculate window from sigma if not provided
  window <- x$window
  if (is.null(window)) {
    window <- ceiling(6 * x$sigma)
    if (window %% 2 == 0) window <- window + 1L
  } else {
    window <- as.integer(window)
    if (window %% 2 == 0) {
      cli::cli_abort("{.arg window} must be an odd number.")
    }
  }

  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  step_measure_smooth_gaussian_new(
    measures = measure_cols,
    sigma = x$sigma,
    window = window,
    edge_method = x$edge_method,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_smooth_gaussian <- function(object, new_data, ...) {
  for (col in object$measures) {
    result <- purrr::map(
      new_data[[col]],
      .smooth_gaussian_single,
      sigma = object$sigma,
      window = object$window,
      edge_method = object$edge_method
    )
    new_data[[col]] <- new_measure_list(result)
  }
  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_smooth_gaussian <- function(x, width = max(20, options()$width - 30), ...) {
  title <- paste0("Gaussian smoothing (sigma = ", x$sigma, ") on ")
  if (x$trained) {
    cat(title, "<internal measurements>", sep = "")
  } else {
    cat(title)
  }
  cat("\n")
  invisible(x)
}

#' @rdname tidy.recipe
#' @export
#' @keywords internal
tidy.step_measure_smooth_gaussian <- function(x, ...) {
  tibble::tibble(
    terms = if (is_trained(x)) x$measures else "<all measure columns>",
    sigma = x$sigma,
    window = x$window,
    id = x$id
  )
}

.smooth_gaussian_single <- function(x, sigma, window, edge_method) {
  values <- x$value
  n <- length(values)

  if (n < window) {
    cli::cli_warn("Spectrum length ({n}) is less than window ({window}). Returning unchanged.")
    return(x)
  }

  half <- (window - 1) / 2

  # Build Gaussian kernel
  k <- seq(-half, half)
  kernel <- exp(-k^2 / (2 * sigma^2))
  kernel <- kernel / sum(kernel)  # Normalize

  # Pad values
  if (edge_method == "reflect") {
    padded <- c(rev(values[2:(half + 1)]), values, rev(values[(n - half):(n - 1)]))
  } else if (edge_method == "constant") {
    padded <- c(rep(values[1], half), values, rep(values[n], half))
  } else {
    padded <- c(rep(NA_real_, half), values, rep(NA_real_, half))
  }

  # Apply convolution
  smoothed <- stats::filter(padded, kernel, sides = 2)

  x$value <- as.numeric(smoothed[(half + 1):(half + n)])
  x
}


# ==============================================================================
# step_measure_filter_fourier
# ==============================================================================

#' Fourier Low-Pass Filtering
#'
#' `step_measure_filter_fourier()` creates a *specification* of a recipe step
#' that applies Fourier-domain low-pass filtering to remove high-frequency
#' noise.
#'
#' @param recipe A recipe object.
#' @param measures An optional character vector of measure column names.
#' @param cutoff The cutoff frequency as a fraction of the Nyquist frequency
#'   (0 to 0.5). Default is 0.1. Frequencies above this are attenuated.
#'   Tunable via [fourier_cutoff()].
#' @param type Type of filter: `"lowpass"` (default) keeps low frequencies,
#'   `"highpass"` keeps high frequencies.
#' @param role Not used.
#' @param trained Logical indicating if the step has been trained.
#' @param skip Logical. Should the step be skipped when baking?
#' @param id Unique step identifier.
#'
#' @return An updated recipe with the new step added.
#'
#' @details
#' Fourier filtering transforms the spectrum to the frequency domain using FFT,
#' applies a frequency mask, and transforms back. This is effective for:
#'
#' - Removing periodic noise
#' - Smoothing with precise frequency control
#' - Removing high-frequency detector noise
#'
#' The cutoff is specified as a fraction of the Nyquist frequency. A cutoff
#' of 0.1 keeps only the lowest 10% of frequencies.
#'
#' @family measure-smoothing
#' @export
#'
#' @examples
#' library(recipes)
#'
#' rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_filter_fourier(cutoff = 0.1) |>
#'   prep()
#'
#' bake(rec, new_data = NULL)
step_measure_filter_fourier <- function(
    recipe,
    measures = NULL,
    cutoff = 0.1,
    type = c("lowpass", "highpass"),
    role = NA,
    trained = FALSE,
    skip = FALSE,
    id = recipes::rand_id("measure_filter_fourier")) {
  type <- match.arg(type)

  recipes::add_step(
    recipe,
    step_measure_filter_fourier_new(
      measures = measures,
      cutoff = cutoff,
      type = type,
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_measure_filter_fourier_new <- function(
    measures, cutoff, type, role, trained, skip, id) {
  recipes::step(
    subclass = "measure_filter_fourier",
    measures = measures,
    cutoff = cutoff,
    type = type,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_filter_fourier <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  if (!is.numeric(x$cutoff) || length(x$cutoff) != 1 ||
      x$cutoff <= 0 || x$cutoff >= 0.5) {
    cli::cli_abort("{.arg cutoff} must be a number between 0 and 0.5.")
  }

  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  step_measure_filter_fourier_new(
    measures = measure_cols,
    cutoff = x$cutoff,
    type = x$type,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_filter_fourier <- function(object, new_data, ...) {
  for (col in object$measures) {
    result <- purrr::map(
      new_data[[col]],
      .filter_fourier_single,
      cutoff = object$cutoff,
      type = object$type
    )
    new_data[[col]] <- new_measure_list(result)
  }
  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_filter_fourier <- function(x, width = max(20, options()$width - 30), ...) {
  title <- paste0("Fourier ", x$type, " filter (cutoff = ", x$cutoff, ") on ")
  if (x$trained) {
    cat(title, "<internal measurements>", sep = "")
  } else {
    cat(title)
  }
  cat("\n")
  invisible(x)
}

#' @rdname tidy.recipe
#' @export
#' @keywords internal
tidy.step_measure_filter_fourier <- function(x, ...) {
  tibble::tibble(
    terms = if (is_trained(x)) x$measures else "<all measure columns>",
    cutoff = x$cutoff,
    type = x$type,
    id = x$id
  )
}

.filter_fourier_single <- function(x, cutoff, type) {
  values <- x$value
  n <- length(values)

  if (n < 4) {
    cli::cli_warn("Spectrum too short for Fourier filtering. Returning unchanged.")
    return(x)
  }

  # Handle NA values by interpolation first
  if (anyNA(values)) {
    non_na <- !is.na(values)
    if (sum(non_na) < 2) {
      return(x)
    }
    values <- stats::approx(which(non_na), values[non_na], seq_len(n), rule = 2)$y
  }

  # FFT
  fft_vals <- stats::fft(values)

  # Frequency indices (0 to n-1, normalized)
  freqs <- (seq_len(n) - 1) / n

  # Create frequency mask
  if (type == "lowpass") {
    # Keep low frequencies (near 0 and near 1, which is same as 0 due to symmetry)
    mask <- (freqs <= cutoff) | (freqs >= (1 - cutoff))
  } else {
    # Keep high frequencies
    mask <- (freqs > cutoff) & (freqs < (1 - cutoff))
  }

  # Apply mask
  fft_vals[!mask] <- 0

  # Inverse FFT
  x$value <- Re(stats::fft(fft_vals, inverse = TRUE)) / n
  x
}
