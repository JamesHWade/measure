# ==============================================================================
# Morphological Baseline Correction Methods
#
# This file contains morphological baseline correction algorithms:
# - step_measure_baseline_tophat: Top-hat filter
# - step_measure_baseline_morphological: Erosion/dilation
# - step_measure_baseline_minima: Connect local minima
# ==============================================================================

# ==============================================================================
# step_measure_baseline_tophat
# ==============================================================================

#' Top-Hat Morphological Baseline Correction
#'
#' `step_measure_baseline_tophat()` creates a *specification* of a recipe step
#' that applies morphological top-hat filtering for baseline correction.
#'
#' @param recipe A recipe object.
#' @param measures An optional character vector of measure column names.
#' @param window_size Size of the structuring element in number of points.
#'   Default is 50.
#' @param smoothing Additional smoothing window applied to baseline. Default is 11.
#' @param role Not used.
#' @param trained Logical indicating if the step has been trained.
#' @param skip Logical. Should the step be skipped when baking?
#' @param id Unique step identifier.
#'
#' @return An updated recipe with the new step added.
#'
#' @details
#' The top-hat filter is a morphological operation that uses opening
#' (erosion followed by dilation) to estimate the baseline. The baseline
#' is then subtracted from the original signal.
#'
#' This method is effective for:
#' - Chromatographic data with narrow peaks
#' - Spectra where peaks are narrower than baseline variations
#' - Data with gradual drift
#'
#' @family measure-baseline
#' @export
#'
#' @examples
#' library(recipes)
#'
#' \donttest{
#' rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_baseline_tophat(window_size = 50) |>
#'   prep()
#'
#' bake(rec, new_data = NULL)
#' }
step_measure_baseline_tophat <- function(
  recipe,
  measures = NULL,

  window_size = 50L,
  smoothing = 11L,
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_baseline_tophat")
) {
  if (!is.numeric(window_size) || window_size < 3) {
    cli::cli_abort("{.arg window_size} must be a number >= 3.")
  }

  recipes::add_step(
    recipe,
    step_measure_baseline_tophat_new(
      measures = measures,
      window_size = as.integer(window_size),
      smoothing = as.integer(smoothing),
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_measure_baseline_tophat_new <- function(
  measures,
  window_size,
  smoothing,
  role,
  trained,
  skip,
  id
) {
  recipes::step(
    subclass = "measure_baseline_tophat",
    measures = measures,
    window_size = window_size,
    smoothing = smoothing,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_baseline_tophat <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  step_measure_baseline_tophat_new(
    measures = measure_cols,
    window_size = x$window_size,
    smoothing = x$smoothing,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' Top-hat baseline algorithm
#' @noRd
.tophat_baseline <- function(y, window_size, smoothing) {
  n <- length(y)
  half_window <- window_size %/% 2

  # Opening operation: erosion followed by dilation
  # Step 1: Erosion - local minimum
  eroded <- numeric(n)
  for (i in seq_len(n)) {
    start <- max(1, i - half_window)
    end <- min(n, i + half_window)
    eroded[i] <- min(y[start:end])
  }

  # Step 2: Dilation - local maximum of eroded signal
  baseline <- numeric(n)
  for (i in seq_len(n)) {
    start <- max(1, i - half_window)
    end <- min(n, i + half_window)
    baseline[i] <- max(eroded[start:end])
  }

  # Optional smoothing
  if (smoothing > 1) {
    half_smooth <- smoothing %/% 2
    smoothed <- numeric(n)
    for (i in seq_len(n)) {
      start <- max(1, i - half_smooth)
      end <- min(n, i + half_smooth)
      smoothed[i] <- mean(baseline[start:end])
    }
    baseline <- smoothed
  }

  baseline
}

#' @export
bake.step_measure_baseline_tophat <- function(object, new_data, ...) {
  window_size <- object$window_size
  smoothing <- object$smoothing

  for (col in object$measures) {
    result <- purrr::map(new_data[[col]], function(m) {
      baseline <- .tophat_baseline(m$value, window_size, smoothing)
      new_measure_tbl(
        location = m$location,
        value = m$value - baseline
      )
    })
    new_data[[col]] <- new_measure_list(result)
  }

  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_baseline_tophat <- function(
  x,
  width = max(20, options()$width - 30),
  ...
) {
  title <- paste0("Top-hat baseline (window=", x$window_size, ")")
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
tidy.step_measure_baseline_tophat <- function(x, ...) {
  tibble::tibble(
    terms = if (recipes::is_trained(x)) x$measures else "<all measure columns>",
    window_size = x$window_size,
    smoothing = x$smoothing,
    id = x$id
  )
}

# ==============================================================================
# step_measure_baseline_morphological
# ==============================================================================

#' Morphological Baseline Correction (Erosion/Dilation)
#'
#' `step_measure_baseline_morphological()` creates a *specification* of a recipe
#' step that applies morphological erosion followed by dilation for baseline
#' estimation.
#'
#' @param recipe A recipe object.
#' @param measures An optional character vector of measure column names.
#' @param window_size Size of the structuring element. Default is 50.
#' @param iterations Number of erosion iterations. Default is 1.
#' @param role Not used.
#' @param trained Logical indicating if the step has been trained.
#' @param skip Logical. Should the step be skipped when baking?
#' @param id Unique step identifier.
#'
#' @return An updated recipe with the new step added.
#'
#' @details
#' This morphological approach uses erosion (local minimum) to push the
#' baseline down below peaks, followed by dilation (local maximum) to
#' smooth the result.
#'
#' Multiple erosion iterations can be used for signals with tall peaks
#' that require more aggressive baseline estimation.
#'
#' @family measure-baseline
#' @export
#'
#' @examples
#' library(recipes)
#'
#' \donttest{
#' rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_baseline_morphological(window_size = 50) |>
#'   prep()
#'
#' bake(rec, new_data = NULL)
#' }
step_measure_baseline_morphological <- function(
  recipe,
  measures = NULL,
  window_size = 50L,
  iterations = 1L,
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_baseline_morphological")
) {
  if (!is.numeric(window_size) || window_size < 3) {
    cli::cli_abort("{.arg window_size} must be a number >= 3.")
  }
  if (!is.numeric(iterations) || iterations < 1) {
    cli::cli_abort("{.arg iterations} must be a positive integer.")
  }

  recipes::add_step(
    recipe,
    step_measure_baseline_morphological_new(
      measures = measures,
      window_size = as.integer(window_size),
      iterations = as.integer(iterations),
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_measure_baseline_morphological_new <- function(
  measures,
  window_size,
  iterations,
  role,
  trained,
  skip,
  id
) {
  recipes::step(
    subclass = "measure_baseline_morphological",
    measures = measures,
    window_size = window_size,
    iterations = iterations,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_baseline_morphological <- function(
  x,
  training,
  info = NULL,
  ...
) {
  check_for_measure(training)

  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  step_measure_baseline_morphological_new(
    measures = measure_cols,
    window_size = x$window_size,
    iterations = x$iterations,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' Morphological baseline algorithm
#' @noRd
.morphological_baseline <- function(y, window_size, iterations) {
  n <- length(y)
  baseline <- y
  half_window <- window_size %/% 2

  # Multiple erosion iterations
  for (iter in seq_len(iterations)) {
    eroded <- numeric(n)
    for (i in seq_len(n)) {
      start <- max(1, i - half_window)
      end <- min(n, i + half_window)
      eroded[i] <- min(baseline[start:end])
    }
    baseline <- eroded
  }

  # Single dilation to smooth
  dilated <- numeric(n)
  for (i in seq_len(n)) {
    start <- max(1, i - half_window)
    end <- min(n, i + half_window)
    dilated[i] <- max(baseline[start:end])
  }

  dilated
}

#' @export
bake.step_measure_baseline_morphological <- function(object, new_data, ...) {
  window_size <- object$window_size
  iterations <- object$iterations

  for (col in object$measures) {
    result <- purrr::map(new_data[[col]], function(m) {
      baseline <- .morphological_baseline(m$value, window_size, iterations)
      new_measure_tbl(
        location = m$location,
        value = m$value - baseline
      )
    })
    new_data[[col]] <- new_measure_list(result)
  }

  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_baseline_morphological <- function(
  x,
  width = max(20, options()$width - 30),
  ...
) {
  title <- paste0(
    "Morphological baseline (window=",
    x$window_size,
    ", iter=",
    x$iterations,
    ")"
  )
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
tidy.step_measure_baseline_morphological <- function(x, ...) {
  tibble::tibble(
    terms = if (recipes::is_trained(x)) x$measures else "<all measure columns>",
    window_size = x$window_size,
    iterations = x$iterations,
    id = x$id
  )
}

# ==============================================================================
# step_measure_baseline_minima
# ==============================================================================

#' Local Minima Baseline Correction
#'
#' `step_measure_baseline_minima()` creates a *specification* of a recipe step
#' that estimates baseline by connecting local minima with spline interpolation.
#'
#' @param recipe A recipe object.
#' @param measures An optional character vector of measure column names.
#' @param window_size Window for finding local minima. Default is 30.
#' @param smooth_window Smoothing window size for final baseline. Default is 50.
#' @param role Not used.
#' @param trained Logical indicating if the step has been trained.
#' @param skip Logical. Should the step be skipped when baking?
#' @param id Unique step identifier.
#'
#' @return An updated recipe with the new step added.
#'
#' @details
#' This algorithm finds local minima in the signal and connects them using
#' spline interpolation to form a baseline estimate. The result is then
#' smoothed to reduce noise.
#'
#' This method is particularly effective for:
#' - Signals with clearly defined baseline regions between peaks
#' - Chromatographic data with resolved peaks
#' - Data where the baseline varies slowly
#'
#' @family measure-baseline
#' @export
#'
#' @examples
#' library(recipes)
#'
#' \donttest{
#' rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_baseline_minima(window_size = 30) |>
#'   prep()
#'
#' bake(rec, new_data = NULL)
#' }
step_measure_baseline_minima <- function(
  recipe,
  measures = NULL,
  window_size = 30L,
  smooth_window = 50L,
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_baseline_minima")
) {
  if (!is.numeric(window_size) || window_size < 3) {
    cli::cli_abort("{.arg window_size} must be a number >= 3.")
  }
  if (!is.numeric(smooth_window) || smooth_window < 1) {
    cli::cli_abort("{.arg smooth_window} must be a positive number.")
  }

  recipes::add_step(
    recipe,
    step_measure_baseline_minima_new(
      measures = measures,
      window_size = as.integer(window_size),
      smooth_window = as.integer(smooth_window),
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_measure_baseline_minima_new <- function(
  measures,
  window_size,
  smooth_window,
  role,
  trained,
  skip,
  id
) {
  recipes::step(
    subclass = "measure_baseline_minima",
    measures = measures,
    window_size = window_size,
    smooth_window = smooth_window,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_baseline_minima <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  step_measure_baseline_minima_new(
    measures = measure_cols,
    window_size = x$window_size,
    smooth_window = x$smooth_window,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' Local minima baseline algorithm
#' @noRd
.minima_baseline <- function(y, window_size, smooth_window) {
  n <- length(y)
  half_window <- window_size %/% 2

  # Find local minima
  is_local_min <- rep(FALSE, n)
  for (i in (half_window + 1):(n - half_window)) {
    window_vals <- y[(i - half_window):(i + half_window)]
    if (y[i] <= min(window_vals)) {
      is_local_min[i] <- TRUE
    }
  }

  # Add endpoints
  is_local_min[1:min(half_window, n)] <- TRUE
  is_local_min[max(1, n - half_window + 1):n] <- TRUE

  # If too few local minima, add percentile points
  if (sum(is_local_min) < n / 10) {
    threshold <- stats::quantile(y, 0.2)
    is_local_min[y < threshold] <- TRUE
  }

  # Interpolate between local minima
  x_minima <- which(is_local_min)
  y_minima <- y[is_local_min]

  # Use spline interpolation
  if (length(x_minima) > 3) {
    baseline <- stats::spline(x_minima, y_minima, n = n)$y
  } else {
    baseline <- rep(min(y), n)
  }

  # Ensure baseline doesn't exceed signal
  baseline <- pmin(baseline, y)

  # Smooth the baseline
  if (smooth_window > 1) {
    half_smooth <- smooth_window %/% 2
    smoothed <- numeric(n)
    for (i in seq_len(n)) {
      start <- max(1, i - half_smooth)
      end <- min(n, i + half_smooth)
      smoothed[i] <- mean(baseline[start:end])
    }
    baseline <- smoothed
  }

  baseline
}

#' @export
bake.step_measure_baseline_minima <- function(object, new_data, ...) {
  window_size <- object$window_size
  smooth_window <- object$smooth_window

  for (col in object$measures) {
    result <- purrr::map(new_data[[col]], function(m) {
      baseline <- .minima_baseline(m$value, window_size, smooth_window)
      new_measure_tbl(
        location = m$location,
        value = m$value - baseline
      )
    })
    new_data[[col]] <- new_measure_list(result)
  }

  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_baseline_minima <- function(
  x,
  width = max(20, options()$width - 30),
  ...
) {
  title <- paste0("Local minima baseline (window=", x$window_size, ")")
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
tidy.step_measure_baseline_minima <- function(x, ...) {
  tibble::tibble(
    terms = if (recipes::is_trained(x)) x$measures else "<all measure columns>",
    window_size = x$window_size,
    smooth_window = x$smooth_window,
    id = x$id
  )
}
