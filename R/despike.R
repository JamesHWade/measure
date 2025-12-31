# ==============================================================================
# Spike Removal Step
#
# This file contains the despike preprocessing step:
# - step_measure_despike: Detect and remove spikes/outliers
# ==============================================================================

#' Remove Spikes and Outliers from Measurements
#'
#' `step_measure_despike()` creates a *specification* of a recipe step that
#' detects and removes spikes (sudden, brief outliers) from measurement data.
#' Spikes are common artifacts in spectroscopy (cosmic rays in Raman, detector
#' glitches) and chromatography (electrical noise).
#'
#' @param recipe A recipe object.
#' @param measures An optional character vector of measure column names.
#' @param window The window size for local statistics. Must be an odd integer
#'   of at least 3. Default is 5. Tunable via [smooth_window()].
#' @param threshold The threshold multiplier for spike detection. Points
#'   deviating more than `threshold * MAD` from the local median are flagged.
#'   Default is 5. Tunable via [despike_threshold()].
#' @param method How to replace detected spikes. One of
#'   `"interpolate"` (default, linear interpolation from neighbors),
#'   `"median"` (replace with local median), or
#'   `"mean"` (replace with local mean).
#' @param max_width Maximum width (in points) of a spike. Consecutive outliers
#'   wider than this are not considered spikes. Default is 3.
#' @param role Not used.
#' @param trained Logical indicating if the step has been trained.
#' @param skip Logical. Should the step be skipped when baking?
#' @param id Unique step identifier.
#'
#' @return An updated recipe with the new step added.
#'
#' @details
#' Spike detection uses a robust local statistic approach:
#'
#' 1. For each point, calculate the local median and MAD (Median Absolute
#'    Deviation) within a sliding window
#' 2. Flag points where `|value - local_median| > threshold * MAD`
#' 3. Group consecutive flagged points into spike regions
#' 4. If a spike region is narrower than `max_width`, replace with the
#'    specified method
#'
#' MAD is scaled by 1.4826 to be consistent with standard deviation for

#' normally distributed data.
#'
#' This approach is robust because:
#' - Median and MAD are not affected by the spikes themselves
#' - The threshold adapts to local noise levels
#' - The max_width parameter prevents removing genuine peaks
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
#'   step_measure_despike(threshold = 5) |>
#'   prep()
#'
#' bake(rec, new_data = NULL)
step_measure_despike <- function(
    recipe,
    measures = NULL,
    window = 5L,
    threshold = 5,
    method = c("interpolate", "median", "mean"),
    max_width = 3L,
    role = NA,
    trained = FALSE,
    skip = FALSE,
    id = recipes::rand_id("measure_despike")) {
  method <- match.arg(method)

  recipes::add_step(
    recipe,
    step_measure_despike_new(
      measures = measures,
      window = as.integer(window),
      threshold = threshold,
      method = method,
      max_width = as.integer(max_width),
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_measure_despike_new <- function(
    measures, window, threshold, method, max_width, role, trained, skip, id) {
  recipes::step(
    subclass = "measure_despike",
    measures = measures,
    window = window,
    threshold = threshold,
    method = method,
    max_width = max_width,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_despike <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  # Validate window
  if (!is.numeric(x$window) || length(x$window) != 1 || x$window < 3) {
    cli::cli_abort("{.arg window} must be an integer >= 3.")
  }

  window <- as.integer(x$window)
  if (window %% 2 == 0) {
    cli::cli_abort("{.arg window} must be an odd number.")
  }

  # Validate threshold
  if (!is.numeric(x$threshold) || length(x$threshold) != 1 || x$threshold <= 0) {
    cli::cli_abort("{.arg threshold} must be a positive number.")
  }

  # Validate max_width
  if (!is.numeric(x$max_width) || length(x$max_width) != 1 || x$max_width < 1) {
    cli::cli_abort("{.arg max_width} must be an integer >= 1.")
  }

  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  step_measure_despike_new(
    measures = measure_cols,
    window = window,
    threshold = x$threshold,
    method = x$method,
    max_width = as.integer(x$max_width),
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_despike <- function(object, new_data, ...) {
  for (col in object$measures) {
    result <- purrr::map(
      new_data[[col]],
      .despike_single,
      window = object$window,
      threshold = object$threshold,
      method = object$method,
      max_width = object$max_width
    )
    new_data[[col]] <- new_measure_list(result)
  }

  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_despike <- function(x, width = max(20, options()$width - 30), ...) {
  title <- paste0(
    "Spike removal (threshold = ", x$threshold,
    ", window = ", x$window, ") on "
  )
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
tidy.step_measure_despike <- function(x, ...) {
  tibble::tibble(
    terms = if (is_trained(x)) x$measures else "<all measure columns>",
    window = x$window,
    threshold = x$threshold,
    method = x$method,
    id = x$id
  )
}

.despike_single <- function(x, window, threshold, method, max_width) {
  values <- x$value
  n <- length(values)

  if (n < window) {
    cli::cli_warn("Spectrum length ({n}) is less than window ({window}). Returning unchanged.")
    return(x)
  }

  half <- (window - 1) / 2

  # Calculate local median and MAD for each point
  local_median <- numeric(n)
  local_mad <- numeric(n)

  for (i in seq_len(n)) {
    # Window indices with boundary handling
    start_idx <- max(1, i - half)
    end_idx <- min(n, i + half)

    local_vals <- values[start_idx:end_idx]
    local_median[i] <- stats::median(local_vals, na.rm = TRUE)
    # MAD scaled to be comparable to SD for normal data
    local_mad[i] <- stats::mad(local_vals, constant = 1.4826, na.rm = TRUE)
  }

  # Avoid division by zero - use minimum non-zero MAD
  positive_mads <- local_mad[local_mad > 0]
  if (length(positive_mads) > 0) {
    min_mad <- min(positive_mads, na.rm = TRUE)
  } else {
    min_mad <- 1e-10
  }
  local_mad[local_mad == 0] <- min_mad

  # Detect spikes: points deviating more than threshold * MAD
  deviation <- abs(values - local_median)
  is_spike <- deviation > (threshold * local_mad)

  # Find consecutive spike regions
  if (!any(is_spike, na.rm = TRUE)) {
    return(x)  # No spikes found
  }

  # Identify spike regions using run-length encoding
  spike_rle <- rle(is_spike)
  spike_ends <- cumsum(spike_rle$lengths)
  spike_starts <- c(1, spike_ends[-length(spike_ends)] + 1)

  # Process each spike region
  result_values <- values
  for (i in seq_along(spike_rle$lengths)) {
    if (spike_rle$values[i] && spike_rle$lengths[i] <= max_width) {
      start <- spike_starts[i]
      end <- spike_ends[i]

      # Replace spike based on method
      if (method == "interpolate") {
        # Find valid neighbors for interpolation
        left_idx <- start - 1
        right_idx <- end + 1

        if (left_idx >= 1 && right_idx <= n) {
          left_val <- result_values[left_idx]
          right_val <- result_values[right_idx]
          # Linear interpolation
          for (j in start:end) {
            t <- (j - left_idx) / (right_idx - left_idx)
            result_values[j] <- left_val + t * (right_val - left_val)
          }
        } else if (left_idx >= 1) {
          result_values[start:end] <- result_values[left_idx]
        } else if (right_idx <= n) {
          result_values[start:end] <- result_values[right_idx]
        }
        # If neither neighbor exists, leave unchanged

      } else if (method == "median") {
        # Replace with local median (from non-spike neighbors)
        result_values[start:end] <- local_median[start:end]

      } else if (method == "mean") {
        # Use mean of neighbors
        left_idx <- max(1, start - half)
        right_idx <- min(n, end + half)
        neighbor_vals <- values[c(left_idx:(start - 1), (end + 1):right_idx)]
        neighbor_vals <- neighbor_vals[!is.na(neighbor_vals)]
        if (length(neighbor_vals) > 0) {
          result_values[start:end] <- mean(neighbor_vals)
        }
      }
    }
  }

  x$value <- result_values
  x
}
