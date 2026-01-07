# ==============================================================================
# Peak Operations
#
# This file contains steps for peak detection and analysis:
# - step_measure_peaks_detect: Detect peaks in measurements
# - step_measure_peaks_integrate: Calculate peak areas
# - step_measure_peaks_properties: Calculate peak metrics
# - step_measure_peaks_filter: Filter peaks by criteria
# - step_measure_peaks_to_table: Convert peaks to tidy table
# ==============================================================================

# ==============================================================================
# Peak data structures
# ==============================================================================

#' Create a new peaks tibble
#'
#' @param peak_id Integer vector of peak identifiers.
#' @param location Numeric vector of peak locations (e.g., retention time).
#' @param height Numeric vector of peak heights.
#' @param left_base Numeric vector of left baseline locations.
#' @param right_base Numeric vector of right baseline locations.
#' @param area Numeric vector of peak areas (can be NA if not yet calculated).
#'
#' @return A tibble with class `peaks_tbl`.
#' @noRd
new_peaks_tbl <- function(
  peak_id = integer(),
  location = double(),
  height = double(),
  left_base = double(),
  right_base = double(),
  area = double()
) {
  x <- tibble::tibble(
    peak_id = peak_id,
    location = location,
    height = height,
    left_base = left_base,
    right_base = right_base,
    area = area
  )
  class(x) <- c("peaks_tbl", class(x))
  x
}

#' Create a new peaks list
#'
#' @param x A list of peaks tibbles.
#' @return A list with class `peaks_list`.
#' @noRd
new_peaks_list <- function(x = list()) {
  if (!is.list(x)) {
    cli::cli_abort("{.arg x} must be a list.")
  }
  # Use vctrs for proper list-column behavior
  x <- vctrs::new_list_of(
    x,
    ptype = new_peaks_tbl(),
    class = "peaks_list"
  )
  x
}

#' Test if object is a peaks list
#' @param x Object to test.
#' @return Logical.
#' @export
is_peaks_list <- function(x) {
  inherits(x, "peaks_list")
}

#' @export
format.peaks_list <- function(x, ...) {
  if (length(x) == 0) {
    return(character())
  }
  n_peaks <- vapply(x, nrow, integer(1))
  paste0("<peaks [", n_peaks, "]>")
}

#' Find peaks columns in a data frame
#' @param data A data frame.
#' @return Character vector of column names.
#' @export
find_peaks_cols <- function(data) {
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame.")
  }
  is_peaks <- vapply(data, is_peaks_list, logical(1))
  names(data)[is_peaks]
}

# ==============================================================================
# Peak detection algorithms
# ==============================================================================

#' Detect peaks using derivative method
#' @noRd
.detect_peaks_derivative <- function(
  location,
  value,
  min_height = 0,
  min_distance = 0
) {
  n <- length(value)
  if (n < 3) {
    return(new_peaks_tbl())
  }

  # First derivative (forward difference)
  d1 <- diff(value)

  # Find zero crossings (positive to negative = peak)
  peaks_idx <- which(d1[-length(d1)] > 0 & d1[-1] <= 0) + 1

  if (length(peaks_idx) == 0) {
    return(new_peaks_tbl())
  }

  # Filter by minimum height
  if (min_height > 0) {
    peaks_idx <- peaks_idx[value[peaks_idx] >= min_height]
  }

  if (length(peaks_idx) == 0) {
    return(new_peaks_tbl())
  }

  # Filter by minimum distance (O(n) incremental approach)
  if (min_distance > 0 && length(peaks_idx) > 1) {
    # Sort by height descending to keep highest peaks
    height_order <- order(value[peaks_idx], decreasing = TRUE)
    sorted_idx <- peaks_idx[height_order]
    n_peaks <- length(sorted_idx)

    keep <- logical(n_peaks)
    keep[1] <- TRUE # Always keep the highest peak
    kept_locs <- numeric(n_peaks)
    kept_locs[1] <- location[sorted_idx[1]]
    n_kept <- 1L

    for (i in seq_len(n_peaks)[-1]) {
      current_loc <- location[sorted_idx[i]]
      if (all(abs(current_loc - kept_locs[seq_len(n_kept)]) >= min_distance)) {
        keep[i] <- TRUE
        n_kept <- n_kept + 1L
        kept_locs[n_kept] <- current_loc
      }
    }

    # Restore original order (by location)
    kept_idx <- sorted_idx[keep]
    peaks_idx <- kept_idx[order(location[kept_idx])]
  }

  if (length(peaks_idx) == 0) {
    return(new_peaks_tbl())
  }

  # Find peak bases (local minima on each side)
  left_bases <- integer(length(peaks_idx))
  right_bases <- integer(length(peaks_idx))

  for (i in seq_along(peaks_idx)) {
    pk <- peaks_idx[i]

    # Find left base (search backwards for local minimum)
    left_idx <- pk - 1
    while (left_idx > 1 && value[left_idx] >= value[left_idx - 1]) {
      left_idx <- left_idx - 1
    }
    left_bases[i] <- left_idx

    # Find right base (search forwards for local minimum)
    right_idx <- pk + 1
    while (right_idx < n && value[right_idx] >= value[right_idx + 1]) {
      right_idx <- right_idx + 1
    }
    right_bases[i] <- right_idx
  }

  new_peaks_tbl(
    peak_id = seq_along(peaks_idx),
    location = location[peaks_idx],
    height = value[peaks_idx],
    left_base = location[left_bases],
    right_base = location[right_bases],
    area = rep(NA_real_, length(peaks_idx))
  )
}

#' Detect peaks using prominence method
#' @noRd
.detect_peaks_prominence <- function(
  location,
  value,
  min_prominence = 0,
  min_height = 0,
  min_distance = 0
) {
  n <- length(value)
  if (n < 3) {
    return(new_peaks_tbl())
  }

  # Find all local maxima
  is_peak <- c(
    FALSE,
    value[-c(1, n)] > value[-c(n - 1, n)] &
      value[-c(1, n)] > value[-c(1, 2)],
    FALSE
  )
  peaks_idx <- which(is_peak)

  if (length(peaks_idx) == 0) {
    return(new_peaks_tbl())
  }

  # Calculate prominence for each peak
  prominences <- numeric(length(peaks_idx))
  left_bases <- integer(length(peaks_idx))
  right_bases <- integer(length(peaks_idx))

  for (i in seq_along(peaks_idx)) {
    pk <- peaks_idx[i]
    pk_val <- value[pk]

    # Find left contour base (with boundary check)
    left_idx <- pk
    left_min <- pk_val
    if (pk > 1) {
      for (j in (pk - 1):1) {
        if (value[j] > pk_val) {
          break
        }
        if (value[j] < left_min) {
          left_min <- value[j]
          left_idx <- j
        }
      }
    }
    left_bases[i] <- left_idx

    # Find right contour base (with boundary check)
    right_idx <- pk
    right_min <- pk_val
    if (pk < n) {
      for (j in (pk + 1):n) {
        if (value[j] > pk_val) {
          break
        }
        if (value[j] < right_min) {
          right_min <- value[j]
          right_idx <- j
        }
      }
    }
    right_bases[i] <- right_idx

    # Prominence is height above the higher of the two bases
    base_height <- max(value[left_idx], value[right_idx])
    prominences[i] <- pk_val - base_height
  }

  # Filter by prominence
  if (min_prominence > 0) {
    keep <- prominences >= min_prominence
    peaks_idx <- peaks_idx[keep]
    prominences <- prominences[keep]
    left_bases <- left_bases[keep]
    right_bases <- right_bases[keep]
  }

  if (length(peaks_idx) == 0) {
    return(new_peaks_tbl())
  }

  # Filter by minimum height
  if (min_height > 0) {
    keep <- value[peaks_idx] >= min_height
    peaks_idx <- peaks_idx[keep]
    prominences <- prominences[keep]
    left_bases <- left_bases[keep]
    right_bases <- right_bases[keep]
  }

  if (length(peaks_idx) == 0) {
    return(new_peaks_tbl())
  }

  # Filter by minimum distance (O(n) incremental approach)
  if (min_distance > 0 && length(peaks_idx) > 1) {
    # Sort by prominence descending to keep most prominent peaks
    prom_order <- order(prominences, decreasing = TRUE)
    n_peaks <- length(prom_order)

    keep <- logical(n_peaks)
    keep[1] <- TRUE # Always keep the most prominent peak
    kept_locs <- numeric(n_peaks)
    kept_locs[1] <- location[peaks_idx[prom_order[1]]]
    n_kept <- 1L

    for (i in seq_len(n_peaks)[-1]) {
      current_loc <- location[peaks_idx[prom_order[i]]]
      if (all(abs(current_loc - kept_locs[seq_len(n_kept)]) >= min_distance)) {
        keep[i] <- TRUE
        n_kept <- n_kept + 1L
        kept_locs[n_kept] <- current_loc
      }
    }

    # Get indices of kept peaks in original order
    kept_order <- prom_order[keep]
    kept_order <- kept_order[order(location[peaks_idx[kept_order]])]

    peaks_idx <- peaks_idx[kept_order]
    left_bases <- left_bases[kept_order]
    right_bases <- right_bases[kept_order]
  }

  if (length(peaks_idx) == 0) {
    return(new_peaks_tbl())
  }

  new_peaks_tbl(
    peak_id = seq_along(peaks_idx),
    location = location[peaks_idx],
    height = value[peaks_idx],
    left_base = location[left_bases],
    right_base = location[right_bases],
    area = rep(NA_real_, length(peaks_idx))
  )
}

# ==============================================================================
# step_measure_peaks_detect
# ==============================================================================

#' Detect Peaks in Measurements
#'
#' `step_measure_peaks_detect()` creates a *specification* of a recipe step that
#' detects peaks in measurement data and stores them in a new `.peaks` column.
#'
#' @param recipe A recipe object.
#' @param algorithm Peak detection algorithm. One of `"prominence"` (default),
#'   `"derivative"`, `"local_maxima"`, or any algorithm registered via
#'
#'   [register_peak_algorithm()]. Use [peak_algorithms()] to see available
#'   algorithms.
#' @param min_height Minimum peak height. If `snr_threshold = TRUE`, this is
#'   interpreted as a signal-to-noise ratio threshold.
#' @param min_distance Minimum distance between peaks in x-axis units.
#' @param min_prominence Minimum peak prominence (only for `algorithm = "prominence"`).
#' @param snr_threshold Logical. If `TRUE`, `min_height` is interpreted as a
#'   signal-to-noise ratio. Noise is estimated as the MAD of the signal.
#' @param algorithm_params Named list of additional algorithm-specific parameters.
#'   These are passed to the algorithm function along with the standard parameters.
#' @param measures Optional character vector of measure column names.
#' @param role Not used.
#' @param trained Logical indicating if the step has been trained.
#' @param skip Logical. Should the step be skipped when baking?
#' @param id Unique step identifier.
#'
#' @return An updated recipe with the new step added.
#'
#' @details
#' This step detects peaks in measurement data and creates a new `.peaks`
#' column containing the detected peaks for each sample. The original
#' `.measures` column is preserved.
#'
#' **Detection algorithms:**
#'
#' - `"prominence"` (default): Finds local maxima and calculates their prominence
#'   (how much a peak stands out from surrounding signal). More robust to noise.
#'
#' - `"derivative"`: Finds peaks by detecting zero-crossings in the first
#'   derivative. Faster but more sensitive to noise.
#'
#' - `"local_maxima"`: Finds all local maxima above a threshold. Simple and fast
#'   but may detect many spurious peaks.
#'
#' Additional algorithms can be registered by technique packs using
#' [register_peak_algorithm()].
#'
#' **Peak properties stored:**
#'
#' - `peak_id`: Integer identifier
#' - `location`: X-axis position of peak apex
#' - `height`: Y-value at peak apex
#' - `left_base`, `right_base`: X-axis positions of peak boundaries
#' - `area`: Initially NA; use `step_measure_peaks_integrate()` to calculate
#'
#' @seealso [peak_algorithms()], [register_peak_algorithm()]
#' @family peak-operations
#' @export
#'
#' @examples
#' library(recipes)
#'
#' rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_peaks_detect(min_height = 0.5, min_distance = 5) |>
#'   prep()
#'
#' result <- bake(rec, new_data = NULL)
#' # Result now has .peaks column alongside .measures
#'
#' # Use a different algorithm
#' rec2 <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_peaks_detect(algorithm = "derivative", min_height = 0.5) |>
#'   prep()
step_measure_peaks_detect <- function(
  recipe,
  algorithm = "prominence",
  min_height = 0,

  min_distance = 0,
  min_prominence = 0,
  snr_threshold = FALSE,
  algorithm_params = list(),
  measures = NULL,
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_peaks_detect")
) {
  # Validate algorithm
  if (!is.character(algorithm) || length(algorithm) != 1) {
    cli::cli_abort("{.arg algorithm} must be a single character string.")
  }
  if (!has_peak_algorithm(algorithm)) {
    available <- peak_algorithms()$name
    cli::cli_abort(
      c(
        "Unknown peak detection algorithm {.val {algorithm}}.",
        "i" = "Available algorithms: {.val {available}}"
      )
    )
  }

  if (!is.numeric(min_height) || length(min_height) != 1 || min_height < 0) {
    cli::cli_abort("{.arg min_height} must be a non-negative number.")
  }
  if (
    !is.numeric(min_distance) || length(min_distance) != 1 || min_distance < 0
  ) {
    cli::cli_abort("{.arg min_distance} must be a non-negative number.")
  }
  if (
    !is.numeric(min_prominence) ||
      length(min_prominence) != 1 ||
      min_prominence < 0
  ) {
    cli::cli_abort("{.arg min_prominence} must be a non-negative number.")
  }
  if (!is.list(algorithm_params)) {
    cli::cli_abort("{.arg algorithm_params} must be a list.")
  }

  recipes::add_step(
    recipe,
    step_measure_peaks_detect_new(
      algorithm = algorithm,
      min_height = min_height,
      min_distance = min_distance,
      min_prominence = min_prominence,
      snr_threshold = snr_threshold,
      algorithm_params = algorithm_params,
      measures = measures,
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_measure_peaks_detect_new <- function(
  algorithm,
  min_height,
  min_distance,
  min_prominence,
  snr_threshold,
  algorithm_params,
  measures,
  role,
  trained,
  skip,
  id
) {
  recipes::step(
    subclass = "measure_peaks_detect",
    algorithm = algorithm,
    min_height = min_height,
    min_distance = min_distance,
    min_prominence = min_prominence,
    snr_threshold = snr_threshold,
    algorithm_params = algorithm_params,
    measures = measures,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_peaks_detect <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  step_measure_peaks_detect_new(
    algorithm = x$algorithm,
    min_height = x$min_height,
    min_distance = x$min_distance,
    min_prominence = x$min_prominence,
    snr_threshold = x$snr_threshold,
    algorithm_params = x$algorithm_params,
    measures = measure_cols,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_peaks_detect <- function(object, new_data, ...) {
  algorithm <- object$algorithm
  min_height <- object$min_height
  min_distance <- object$min_distance
  min_prominence <- object$min_prominence
  snr_threshold <- object$snr_threshold
  algorithm_params <- object$algorithm_params %||% list()

  # Process each measure column
  for (col in object$measures) {
    peaks_col_name <- sub("^\\.?measures?", ".peaks", col)
    if (peaks_col_name == col) {
      peaks_col_name <- ".peaks"
    }

    peaks_list <- purrr::map(new_data[[col]], function(m) {
      loc <- m$location
      val <- m$value

      # Calculate height threshold
      height_thresh <- min_height
      if (snr_threshold && min_height > 0) {
        noise <- stats::mad(val, constant = 1)
        if (noise > 0) {
          height_thresh <- min_height * noise
        }
      }

      # Get algorithm's default params to know what it accepts
      algo_info <- get_peak_algorithm(algorithm)
      algo_param_names <- names(algo_info$default_params)

      # Validate algorithm_params keys
      if (length(algorithm_params) > 0) {
        invalid_params <- setdiff(names(algorithm_params), algo_param_names)
        if (length(invalid_params) > 0) {
          cli::cli_abort(
            c(
              "Invalid algorithm parameters: {.val {invalid_params}}",
              "i" = "Algorithm {.val {algorithm}} accepts: {.val {algo_param_names}}"
            )
          )
        }
      }

      # Build params for algorithm - only include supported params
      all_params <- list(
        min_height = height_thresh,
        min_distance = min_distance,
        min_prominence = min_prominence
      )
      # Filter to only params the algorithm supports
      params <- all_params[names(all_params) %in% algo_param_names]
      # Add validated algorithm-specific params
      params <- utils::modifyList(params, algorithm_params)

      # Use registry-based algorithm dispatch
      do.call(
        .run_peak_algorithm,
        c(list(name = algorithm, location = loc, value = val), params)
      )
    })

    new_data[[peaks_col_name]] <- new_peaks_list(peaks_list)
  }

  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_peaks_detect <- function(
  x,
  width = max(20, options()$width - 30),
  ...
) {
  title <- paste0("Peak detection (", x$algorithm, ")")
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
tidy.step_measure_peaks_detect <- function(x, ...) {
  tibble::tibble(
    terms = if (is_trained(x)) x$measures else "<all measure columns>",
    algorithm = x$algorithm,
    min_height = x$min_height,
    min_distance = x$min_distance,
    min_prominence = x$min_prominence,
    id = x$id
  )
}

# ==============================================================================
# step_measure_peaks_integrate
# ==============================================================================

#' Integrate Peak Areas
#'
#' `step_measure_peaks_integrate()` creates a *specification* of a recipe step
#' that calculates the area under each detected peak.
#'
#' @param recipe A recipe object.
#' @param method Integration method. One of `"trapezoid"` (default) or
#'   `"simpson"`.
#' @param baseline Baseline handling. One of `"local"` (linear interpolation
#'   between peak bases), `"none"` (integrate to zero), or `"global"` (use
#'   minimum value as baseline).
#' @param measures Optional character vector of measure column names.
#' @param role Not used.
#' @param trained Logical indicating if the step has been trained.
#' @param skip Logical. Should the step be skipped when baking?
#' @param id Unique step identifier.
#'
#' @return An updated recipe with the new step added.
#'
#' @details
#' This step calculates the area under each peak detected by
#' `step_measure_peaks_detect()`. The areas are stored in the `area` column
#' of the `.peaks` tibble.
#'
#' **Integration methods:**
#'
#' - `"trapezoid"`: Trapezoidal rule integration. Fast and accurate for
#'   well-resolved peaks.
#'
#' - `"simpson"`: Simpson's rule integration. More accurate for smooth curves
#'   but requires odd number of points.
#'
#' **Baseline handling:**
#'
#' - `"local"`: Subtracts a linear baseline connecting the left and right
#'   peak bases before integration.
#'
#' - `"none"`: Integrates directly to y=0.
#'
#' - `"global"`: Subtracts the minimum value in the peak region.
#'
#' @family peak-operations
#' @export
#'
#' @examples
#' library(recipes)
#'
#' rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_peaks_detect(min_height = 0.5) |>
#'   step_measure_peaks_integrate() |>
#'   prep()
#'
#' result <- bake(rec, new_data = NULL)
step_measure_peaks_integrate <- function(
  recipe,
  method = c("trapezoid", "simpson"),
  baseline = c("local", "none", "global"),
  measures = NULL,
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_peaks_integrate")
) {
  method <- rlang::arg_match(method)
  baseline <- rlang::arg_match(baseline)

  recipes::add_step(
    recipe,
    step_measure_peaks_integrate_new(
      method = method,
      baseline = baseline,
      measures = measures,
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_measure_peaks_integrate_new <- function(
  method,
  baseline,
  measures,
  role,
  trained,
  skip,
  id
) {
  recipes::step(
    subclass = "measure_peaks_integrate",
    method = method,
    baseline = baseline,
    measures = measures,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_peaks_integrate <- function(x, training, info = NULL, ...) {
  # Check for peaks column
  peaks_cols <- find_peaks_cols(training)
  if (length(peaks_cols) == 0) {
    cli::cli_abort(
      c(
        "No peaks column found.",
        "i" = "Use {.fn step_measure_peaks_detect} before integration."
      )
    )
  }

  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  step_measure_peaks_integrate_new(
    method = x$method,
    baseline = x$baseline,
    measures = measure_cols,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' Integrate a single peak
#' @noRd
.integrate_peak <- function(
  location,
  value,
  left_base,
  right_base,
  method = "trapezoid",
  baseline_type = "local"
) {
  # Find indices within peak range
  idx <- which(location >= left_base & location <= right_base)
  if (length(idx) < 2) {
    return(NA_real_)
  }

  x <- location[idx]
  y <- value[idx]

  # Handle baseline
  if (baseline_type == "local") {
    # Linear baseline from left to right base
    y_left <- value[which.min(abs(location - left_base))]
    y_right <- value[which.min(abs(location - right_base))]
    baseline_y <- y_left +
      (y_right - y_left) * (x - left_base) / (right_base - left_base)
    y <- y - baseline_y
  } else if (baseline_type == "global") {
    y <- y - min(y)
  }
  # "none" - integrate as-is

  # Ensure non-negative
  y <- pmax(y, 0)

  if (method == "trapezoid") {
    # Trapezoidal rule
    area <- sum(diff(x) * (y[-1] + y[-length(y)]) / 2)
  } else {
    # Simpson's rule (fallback to trapezoid if even number of intervals)
    n <- length(x)
    if (n < 3) {
      area <- sum(diff(x) * (y[-1] + y[-length(y)]) / 2)
    } else if ((n - 1) %% 2 == 0) {
      # Even number of intervals - standard Simpson's
      h <- (x[n] - x[1]) / (n - 1)
      area <- h /
        3 *
        (y[1] +
          4 * sum(y[seq(2, n - 1, 2)]) +
          2 * sum(y[seq(3, n - 2, 2)]) +
          y[n])
    } else {
      # Odd number of intervals - use Simpson's 3/8 for last segment
      h <- (x[n - 1] - x[1]) / (n - 2)
      area <- h /
        3 *
        (y[1] +
          4 * sum(y[seq(2, n - 2, 2)]) +
          2 * sum(y[seq(3, n - 3, 2)]) +
          y[n - 1])
      # Add last segment with trapezoidal
      area <- area + (x[n] - x[n - 1]) * (y[n] + y[n - 1]) / 2
    }
  }

  area
}

#' @export
bake.step_measure_peaks_integrate <- function(object, new_data, ...) {
  method <- object$method
  baseline <- object$baseline

  peaks_cols <- find_peaks_cols(new_data)
  measure_cols <- object$measures

  for (i in seq_along(peaks_cols)) {
    peaks_col <- peaks_cols[i]
    measure_col <- measure_cols[min(i, length(measure_cols))]

    new_peaks <- purrr::map2(
      new_data[[peaks_col]],
      new_data[[measure_col]],
      function(peaks, measures) {
        if (nrow(peaks) == 0) {
          return(peaks)
        }

        areas <- numeric(nrow(peaks))
        for (j in seq_len(nrow(peaks))) {
          areas[j] <- .integrate_peak(
            measures$location,
            measures$value,
            peaks$left_base[j],
            peaks$right_base[j],
            method,
            baseline
          )
        }
        peaks$area <- areas
        peaks
      }
    )

    new_data[[peaks_col]] <- new_peaks_list(new_peaks)
  }

  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_peaks_integrate <- function(
  x,
  width = max(20, options()$width - 30),
  ...
) {
  title <- paste0(
    "Peak integration (",
    x$method,
    ", ",
    x$baseline,
    " baseline)"
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
tidy.step_measure_peaks_integrate <- function(x, ...) {
  tibble::tibble(
    method = x$method,
    baseline = x$baseline,
    id = x$id
  )
}

# ==============================================================================
# step_measure_peaks_filter
# ==============================================================================

#' Filter Peaks by Criteria
#'
#' `step_measure_peaks_filter()` creates a *specification* of a recipe step
#' that filters detected peaks based on various criteria.
#'
#' @param recipe A recipe object.
#' @param min_height Minimum peak height. Peaks below this are removed.
#' @param min_area Minimum peak area. Requires prior integration.
#' @param min_area_pct Minimum area as percentage of total. Peaks with area
#'   less than this percentage of total peak area are removed.
#' @param min_prominence Minimum peak prominence.
#' @param max_peaks Maximum number of peaks to keep (keeps largest by area
#'   or height).
#' @param role Not used.
#' @param trained Logical indicating if the step has been trained.
#' @param skip Logical. Should the step be skipped when baking?
#' @param id Unique step identifier.
#'
#' @return An updated recipe with the new step added.
#'
#' @details
#' This step removes peaks that don't meet specified criteria. Multiple
#' criteria can be combined - peaks must pass ALL specified filters.
#'
#' @family peak-operations
#' @export
#'
#' @examples
#' library(recipes)
#'
#' rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_peaks_detect(min_height = 0.3) |>
#'   step_measure_peaks_integrate() |>
#'   step_measure_peaks_filter(min_area_pct = 1) |>
#'   prep()
#'
#' result <- bake(rec, new_data = NULL)
step_measure_peaks_filter <- function(
  recipe,
  min_height = NULL,
  min_area = NULL,
  min_area_pct = NULL,
  min_prominence = NULL,
  max_peaks = NULL,
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_peaks_filter")
) {
  recipes::add_step(
    recipe,
    step_measure_peaks_filter_new(
      min_height = min_height,
      min_area = min_area,
      min_area_pct = min_area_pct,
      min_prominence = min_prominence,
      max_peaks = max_peaks,
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_measure_peaks_filter_new <- function(
  min_height,
  min_area,
  min_area_pct,
  min_prominence,
  max_peaks,
  role,
  trained,
  skip,
  id
) {
  recipes::step(
    subclass = "measure_peaks_filter",
    min_height = min_height,
    min_area = min_area,
    min_area_pct = min_area_pct,
    min_prominence = min_prominence,
    max_peaks = max_peaks,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_peaks_filter <- function(x, training, info = NULL, ...) {
  peaks_cols <- find_peaks_cols(training)
  if (length(peaks_cols) == 0) {
    cli::cli_abort(
      c(
        "No peaks column found.",
        "i" = "Use {.fn step_measure_peaks_detect} before filtering."
      )
    )
  }

  step_measure_peaks_filter_new(
    min_height = x$min_height,
    min_area = x$min_area,
    min_area_pct = x$min_area_pct,
    min_prominence = x$min_prominence,
    max_peaks = x$max_peaks,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_peaks_filter <- function(object, new_data, ...) {
  peaks_cols <- find_peaks_cols(new_data)

  for (col in peaks_cols) {
    new_peaks <- purrr::map(new_data[[col]], function(peaks) {
      if (nrow(peaks) == 0) {
        return(peaks)
      }

      keep <- rep(TRUE, nrow(peaks))

      # Filter by height
      if (!is.null(object$min_height)) {
        keep <- keep & peaks$height >= object$min_height
      }

      # Filter by area
      if (!is.null(object$min_area)) {
        keep <- keep & !is.na(peaks$area) & peaks$area >= object$min_area
      }

      # Filter by area percentage
      if (!is.null(object$min_area_pct)) {
        total_area <- sum(peaks$area, na.rm = TRUE)
        if (total_area > 0) {
          pct <- peaks$area / total_area * 100
          keep <- keep & !is.na(pct) & pct >= object$min_area_pct
        }
      }

      peaks <- peaks[keep, , drop = FALSE]

      # Limit number of peaks (keep largest)
      if (!is.null(object$max_peaks) && nrow(peaks) > object$max_peaks) {
        # Sort by area if available, otherwise height
        if (!anyNA(peaks$area)) {
          ord <- order(peaks$area, decreasing = TRUE)
        } else {
          ord <- order(peaks$height, decreasing = TRUE)
        }
        peaks <- peaks[ord[seq_len(object$max_peaks)], , drop = FALSE]
        # Re-sort by location
        peaks <- peaks[order(peaks$location), , drop = FALSE]
        peaks$peak_id <- seq_len(nrow(peaks))
      }

      class(peaks) <- c("peaks_tbl", class(tibble::tibble()))
      peaks
    })

    new_data[[col]] <- new_peaks_list(new_peaks)
  }

  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_peaks_filter <- function(
  x,
  width = max(20, options()$width - 30),
  ...
) {
  filters <- character()
  if (!is.null(x$min_height)) {
    filters <- c(filters, paste0("height>=", x$min_height))
  }
  if (!is.null(x$min_area)) {
    filters <- c(filters, paste0("area>=", x$min_area))
  }
  if (!is.null(x$min_area_pct)) {
    filters <- c(filters, paste0("area>=", x$min_area_pct, "%"))
  }
  if (!is.null(x$max_peaks)) {
    filters <- c(filters, paste0("max ", x$max_peaks))
  }

  title <- paste0("Peak filtering (", paste(filters, collapse = ", "), ")")
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
tidy.step_measure_peaks_filter <- function(x, ...) {
  tibble::tibble(
    min_height = x$min_height %||% NA_real_,
    min_area = x$min_area %||% NA_real_,
    min_area_pct = x$min_area_pct %||% NA_real_,
    max_peaks = x$max_peaks %||% NA_integer_,
    id = x$id
  )
}

# ==============================================================================
# step_measure_peaks_to_table
# ==============================================================================

#' Convert Peaks to Tidy Table
#'
#' `step_measure_peaks_to_table()` creates a *specification* of a recipe step
#' that converts the peaks list-column to a wide format with one column per
#' peak property.
#'
#' @param recipe A recipe object.
#' @param prefix Prefix for generated column names. Default is `"peak_"`.
#' @param properties Which peak properties to include. Default includes
#'   location, height, and area for each peak.
#' @param max_peaks Maximum number of peaks to include in output. If a sample
#'   has more peaks, only the first `max_peaks` are included.
#' @param role Not used.
#' @param trained Logical indicating if the step has been trained.
#' @param skip Logical. Should the step be skipped when baking?
#' @param id Unique step identifier.
#'
#' @return An updated recipe with the new step added.
#'
#' @details
#' This step converts peak data to a wide format suitable for modeling.
#' For each peak, it creates columns like `peak_1_location`, `peak_1_height`,
#' `peak_1_area`, etc.
#'
#' The `.peaks` and `.measures` columns are removed after conversion.
#'
#' @family peak-operations
#' @export
#'
#' @examples
#' library(recipes)
#'
#' rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_peaks_detect(min_height = 0.5) |>
#'   step_measure_peaks_integrate() |>
#'   step_measure_peaks_to_table(max_peaks = 5) |>
#'   prep()
#'
#' result <- bake(rec, new_data = NULL)
step_measure_peaks_to_table <- function(
  recipe,
  prefix = "peak_",
  properties = c("location", "height", "area"),
  max_peaks = 10,
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_peaks_to_table")
) {
  if (!is.character(prefix) || length(prefix) != 1) {
    cli::cli_abort("{.arg prefix} must be a single string.")
  }
  if (!is.numeric(max_peaks) || length(max_peaks) != 1 || max_peaks < 1) {
    cli::cli_abort("{.arg max_peaks} must be a positive integer.")
  }

  recipes::add_step(
    recipe,
    step_measure_peaks_to_table_new(
      prefix = prefix,
      properties = properties,
      max_peaks = as.integer(max_peaks),
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_measure_peaks_to_table_new <- function(
  prefix,
  properties,
  max_peaks,
  role,
  trained,
  skip,
  id
) {
  recipes::step(
    subclass = "measure_peaks_to_table",
    prefix = prefix,
    properties = properties,
    max_peaks = max_peaks,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_peaks_to_table <- function(x, training, info = NULL, ...) {
  peaks_cols <- find_peaks_cols(training)
  if (length(peaks_cols) == 0) {
    cli::cli_abort(
      c(
        "No peaks column found.",
        "i" = "Use {.fn step_measure_peaks_detect} before converting to table."
      )
    )
  }

  step_measure_peaks_to_table_new(
    prefix = x$prefix,
    properties = x$properties,
    max_peaks = x$max_peaks,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_peaks_to_table <- function(object, new_data, ...) {
  peaks_cols <- find_peaks_cols(new_data)
  measure_cols <- find_measure_cols(new_data)

  prefix <- object$prefix
  properties <- object$properties
  max_peaks <- object$max_peaks

  # Generate column names
  col_names <- character()
  for (i in seq_len(max_peaks)) {
    for (prop in properties) {
      col_names <- c(col_names, paste0(prefix, i, "_", prop))
    }
  }

  # Create matrix of values
  n_rows <- nrow(new_data)
  n_cols <- length(col_names)
  result_matrix <- matrix(NA_real_, nrow = n_rows, ncol = n_cols)
  colnames(result_matrix) <- col_names

  for (col in peaks_cols) {
    for (row_idx in seq_len(n_rows)) {
      peaks <- new_data[[col]][[row_idx]]
      n_peaks_available <- min(nrow(peaks), max_peaks)

      if (n_peaks_available > 0) {
        for (i in seq_len(n_peaks_available)) {
          for (prop in properties) {
            col_name <- paste0(prefix, i, "_", prop)
            if (prop %in% names(peaks)) {
              result_matrix[row_idx, col_name] <- peaks[[prop]][i]
            }
          }
        }
      }
    }
  }

  # Add new columns
  result_df <- tibble::as_tibble(result_matrix)
  new_data <- dplyr::bind_cols(new_data, result_df)

  # Remove peaks and measures columns
  new_data <- new_data[,
    !(names(new_data) %in% c(peaks_cols, measure_cols)),
    drop = FALSE
  ]

  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_peaks_to_table <- function(
  x,
  width = max(20, options()$width - 30),
  ...
) {
  title <- paste0("Convert peaks to table (max ", x$max_peaks, " peaks)")
  if (x$trained) {
    cat(title, sep = "")
  } else {
    cat(title)
  }
  cat("\n")
  invisible(x)
}

#' @rdname tidy.recipe
#' @export
#' @keywords internal
tidy.step_measure_peaks_to_table <- function(x, ...) {
  tibble::tibble(
    prefix = x$prefix,
    max_peaks = x$max_peaks,
    properties = list(x$properties),
    id = x$id
  )
}

# ==============================================================================
# step_measure_peaks_deconvolve
# ==============================================================================

#' Deconvolve Overlapping Peaks
#'
#' `step_measure_peaks_deconvolve()` creates a *specification* of a recipe step
#' that resolves overlapping peaks using curve fitting. This step requires
#' peaks to have been detected first using [step_measure_peaks_detect()].
#'
#' @param recipe A recipe object.
#' @param model Peak model to use. Either a character string naming a registered
#'   model (`"gaussian"`, `"emg"`, `"bigaussian"`, `"lorentzian"`), or a
#'   `peak_model` object created directly. Use [peak_models()] to see
#'   all registered models. Default is `"gaussian"`.
#' @param optimizer Optimization method: `"auto"` (default), `"lbfgsb"`,
#'   `"multistart"`, or `"nelder_mead"`. Auto-selection chooses based on problem
#'   complexity and signal-to-noise ratio.
#' @param max_iter Maximum iterations for optimization. Default is 500.
#' @param tol Convergence tolerance. Default is 1e-6.
#' @param n_starts Number of random starts for `optimizer = "multistart"`.
#'   Default is 5.
#' @param constrain_positions Logical. If `TRUE`, enforce that peak centers
#'   maintain their relative ordering. Default is `TRUE`.
#' @param quality_threshold Minimum R-squared to accept fit. Fits below this
#'   threshold trigger a warning. Default is 0.8.
#' @param store_components Logical. If `TRUE`, store individual fitted peak
#'   curves in the output. Default is `FALSE`.
#' @param smart_init Logical. If `TRUE`, use smart initialization based on
#'   peak properties. Default is `TRUE`.
#' @param peaks_col Name of the peaks column. Default is ".peaks".
#' @param measures_col Name of the measures column. Default is ".measures".
#' @param role Not used.
#' @param trained Logical indicating if the step has been trained.
#' @param skip Logical. Should the step be skipped when baking?
#' @param id Unique step identifier.
#'
#' @return An updated recipe with the new step added. The `.peaks` column
#'   will be updated with deconvolved peak parameters, fitted areas, and
#'   quality metrics.
#'
#' @details
#' Peak deconvolution fits mathematical models to overlapping peaks to
#' determine their individual contributions. This is essential for
#' quantitative analysis when peaks are not baseline-resolved.
#'
#' **Peak Models:**
#'
#' Built-in models (use [peak_models()] to see all):
#' - `"gaussian"`: Symmetric Gaussian (3 params: height, center, width)
#' - `"emg"`: Exponentially Modified Gaussian (4 params, handles tailing)
#' - `"bigaussian"`: Bi-Gaussian (4 params, flexible asymmetry)
#' - `"lorentzian"`: Lorentzian/Cauchy peak (3 params, heavier tails)
#'
#' Technique packs may register additional models.
#'
#' **Optimizers:**
#'
#' - `"auto"`: Selects based on problem complexity and SNR
#' - `"lbfgsb"`: L-BFGS-B (fast, local optimization)
#' - `"multistart"`: Multiple L-BFGS-B runs from perturbed starts (robust)
#' - `"nelder_mead"`: Derivative-free Nelder-Mead simplex
#'
#' **Quality Assessment:**
#'
#' Each fit is assessed for quality. The `.peaks` tibble gains columns:
#' - `fit_r_squared`: R-squared of the overall fit
#' - `fit_quality`: Quality grade (A/B/C/D/F)
#' - `purity`: How much of signal at peak max comes from this peak
#'
#' @seealso [optimize_deconvolution()], [assess_deconv_quality()],
#'   [peak_models()], [gaussian_peak_model()]
#' @family peak-operations
#' @export
#'
#' @examples
#' library(recipes)
#'
#' # Create synthetic data with overlapping peaks
#' set.seed(42)
#' x <- seq(0, 20, by = 0.1)
#' y <- 1.5 * exp(-0.5 * ((x - 8) / 1)^2) +
#'   0.8 * exp(-0.5 * ((x - 12) / 1.5)^2) +
#'   rnorm(length(x), sd = 0.02)
#' df <- data.frame(id = "sample1", location = x, value = y)
#'
#' \donttest{
#' # Deconvolve overlapping peaks
#' rec <- recipe(~., data = df) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(value, location = vars(location)) |>
#'   step_measure_peaks_detect(min_height = 0.5, min_prominence = 0.3) |>
#'   step_measure_peaks_deconvolve(model = "gaussian") |>
#'   prep()
#'
#' result <- bake(rec, new_data = NULL)
#' # Check fitted peaks
#' result$.peaks[[1]]
#' }
step_measure_peaks_deconvolve <- function(
  recipe,
  model = "gaussian",
  optimizer = "auto",
  max_iter = 500L,
  tol = 1e-6,
  n_starts = 5L,
  constrain_positions = TRUE,
  quality_threshold = 0.8,
  store_components = FALSE,
  smart_init = TRUE,
  peaks_col = ".peaks",
  measures_col = ".measures",
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_peaks_deconvolve")
) {
  # Validate model - can be character or peak_model object
  if (is.character(model)) {
    if (length(model) != 1) {
      cli::cli_abort(
        "{.arg model} must be a single string or peak_model object."
      )
    }
    if (!has_peak_model(model)) {
      available <- peak_models()$name
      cli::cli_abort(
        c(
          "Unknown peak model {.val {model}}.",
          "i" = "Available models: {.val {available}}"
        )
      )
    }
  } else if (!is_peak_model(model)) {
    cli::cli_abort(
      "{.arg model} must be a character string or {.cls peak_model} object."
    )
  }

  # Validate optimizer
  valid_optimizers <- c("auto", "lbfgsb", "multistart", "nelder_mead")
  if (!optimizer %in% valid_optimizers) {
    cli::cli_abort(
      c(
        "Unknown optimizer {.val {optimizer}}.",
        "i" = "Valid options: {.val {valid_optimizers}}"
      )
    )
  }

  if (!is.numeric(max_iter) || max_iter < 1) {
    cli::cli_abort("{.arg max_iter} must be a positive integer.")
  }
  if (
    !is.numeric(quality_threshold) ||
      quality_threshold < 0 ||
      quality_threshold > 1
  ) {
    cli::cli_abort("{.arg quality_threshold} must be between 0 and 1.")
  }

  recipes::add_step(
    recipe,
    step_measure_peaks_deconvolve_new(
      model = model,
      optimizer = optimizer,
      max_iter = as.integer(max_iter),
      tol = tol,
      n_starts = as.integer(n_starts),
      constrain_positions = constrain_positions,
      quality_threshold = quality_threshold,
      store_components = store_components,
      smart_init = smart_init,
      peaks_col = peaks_col,
      measures_col = measures_col,
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_measure_peaks_deconvolve_new <- function(
  model,
  optimizer,
  max_iter,
  tol,
  n_starts,
  constrain_positions,
  quality_threshold,
  store_components,
  smart_init,
  peaks_col,
  measures_col,
  role,
  trained,
  skip,
  id
) {
  recipes::step(
    subclass = "measure_peaks_deconvolve",
    model = model,
    optimizer = optimizer,
    max_iter = max_iter,
    tol = tol,
    n_starts = n_starts,
    constrain_positions = constrain_positions,
    quality_threshold = quality_threshold,
    store_components = store_components,
    smart_init = smart_init,
    peaks_col = peaks_col,
    measures_col = measures_col,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_peaks_deconvolve <- function(x, training, info = NULL, ...) {
  # Verify peaks column exists
  if (!x$peaks_col %in% names(training)) {
    cli::cli_abort(
      "Column {.val {x$peaks_col}} not found. Run step_measure_peaks_detect() first."
    )
  }
  if (!x$measures_col %in% names(training)) {
    cli::cli_abort("Column {.val {x$measures_col}} not found.")
  }

  step_measure_peaks_deconvolve_new(
    model = x$model,
    optimizer = x$optimizer,
    max_iter = x$max_iter,
    tol = x$tol,
    n_starts = x$n_starts,
    constrain_positions = x$constrain_positions,
    quality_threshold = x$quality_threshold,
    store_components = x$store_components,
    smart_init = x$smart_init,
    peaks_col = x$peaks_col,
    measures_col = x$measures_col,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_peaks_deconvolve <- function(object, new_data, ...) {
  model_spec <- object$model
  optimizer <- object$optimizer
  max_iter <- object$max_iter
  tol <- object$tol
  n_starts <- object$n_starts
  constrain_positions <- object$constrain_positions
  quality_threshold <- object$quality_threshold
  store_components <- object$store_components
  smart_init <- object$smart_init
  peaks_col <- object$peaks_col
  measures_col <- object$measures_col

  # Convert to regular list to allow schema changes during modification
  # (list_of enforces strict type matching on assignment)
  peaks_list <- as.list(new_data[[peaks_col]])

  # Process each row
  for (row_idx in seq_len(nrow(new_data))) {
    peaks <- peaks_list[[row_idx]]
    measures <- new_data[[measures_col]][[row_idx]]

    if (nrow(peaks) > 0) {
      peaks <- .deconvolve_peaks_with_framework(
        x = measures$location,
        y = measures$value,
        peaks_tbl = peaks,
        model_spec = model_spec,
        optimizer = optimizer,
        max_iter = max_iter,
        tol = tol,
        n_starts = n_starts,
        constrain_positions = constrain_positions,
        quality_threshold = quality_threshold,
        store_components = store_components,
        smart_init = smart_init,
        row_idx = row_idx
      )

      peaks_list[[row_idx]] <- peaks
    }
  }

  # Wrap back as peaks_list with updated schema
  new_data[[peaks_col]] <- new_peaks_list(peaks_list)

  tibble::as_tibble(new_data)
}

#' Deconvolve peaks using the new framework
#' @noRd
.deconvolve_peaks_with_framework <- function(
  x,
  y,
  peaks_tbl,
  model_spec,
  optimizer,
  max_iter,
  tol,
  n_starts,
  constrain_positions,
  quality_threshold,
  store_components,
  smart_init,
  row_idx
) {
  n_peaks <- nrow(peaks_tbl)
  if (n_peaks == 0) {
    return(peaks_tbl)
  }

  # Create peak model objects
  if (is.character(model_spec)) {
    models <- lapply(seq_len(n_peaks), function(i) {
      create_peak_model(model_spec)
    })
  } else {
    # Use provided peak_model object for all peaks
    models <- lapply(seq_len(n_peaks), function(i) model_spec)
  }

  # Initialize parameters
  if (smart_init) {
    # Get peak indices from detected peaks
    peak_indices <- vapply(
      peaks_tbl$location,
      function(loc) which.min(abs(x - loc)),
      integer(1)
    )

    init_params <- initialize_peak_params(
      x,
      y,
      n_peaks = n_peaks,
      models = models,
      peak_indices = peak_indices,
      smooth = TRUE
    )
  } else {
    # Basic initialization from peak properties
    init_params <- lapply(seq_len(n_peaks), function(i) {
      width <- (peaks_tbl$right_base[i] - peaks_tbl$left_base[i]) / 4
      width <- max(width, diff(range(x)) / 100)

      params <- list(
        height = peaks_tbl$height[i],
        center = peaks_tbl$location[i],
        width = width
      )

      # Add extra params for EMG/bigaussian
      if (models[[i]]$name == "emg") {
        params$tau <- width * 0.5
      } else if (models[[i]]$name == "bigaussian") {
        params$width_left <- width
        params$width_right <- width
        params$width <- NULL
      }

      params
    })
  }

  # Run optimization
  opt_result <- tryCatch(
    optimize_deconvolution(
      x = x,
      y = y,
      models = models,
      init_params = init_params,
      optimizer = optimizer,
      max_iter = max_iter,
      tol = tol,
      constrain_positions = constrain_positions,
      n_starts = n_starts
    ),
    error = function(e) {
      cli::cli_warn(
        "Deconvolution failed for row {row_idx}: {e$message}"
      )
      NULL
    }
  )

  if (is.null(opt_result)) {
    # Return original peaks if optimization failed
    return(peaks_tbl)
  }

  # Assess quality
  quality <- assess_deconv_quality(x, y, opt_result, models)

  # Check quality threshold
  r_squared <- quality$goodness_of_fit$r_squared
  if (!is.na(r_squared) && r_squared < quality_threshold) {
    cli::cli_warn(
      c(
        "Deconvolution quality below threshold for row {row_idx}.",
        "i" = "R-squared = {round(r_squared, 3)}, threshold = {quality_threshold}"
      )
    )
  }

  # Update peaks_tbl with fitted parameters
  for (i in seq_len(n_peaks)) {
    params <- opt_result$parameters[[i]]

    # Update location and height from fitted params
    peaks_tbl$location[i] <- params$center
    peaks_tbl$height[i] <- params$height

    # Update width-based bounds
    width <- params$width %||%
      ((params$width_left %||% 0) + (params$width_right %||% 0)) /
      2
    peaks_tbl$left_base[i] <- params$center - 3 * width
    peaks_tbl$right_base[i] <- params$center + 3 * width

    # Calculate area from peak model
    peaks_tbl$area[i] <- peak_model_area(models[[i]], params, range(x))
  }

  # Add quality columns
  peaks_tbl$fit_r_squared <- rep(r_squared, n_peaks)
  peaks_tbl$fit_quality <- rep(quality$overall_grade, n_peaks)

  # Add per-peak quality metrics
  if (!is.null(quality$peak_quality) && nrow(quality$peak_quality) == n_peaks) {
    peaks_tbl$purity <- quality$peak_quality$purity
    peaks_tbl$overlap_degree <- quality$peak_quality$overlap_degree
  }

  # Store components if requested
  if (store_components) {
    components <- lapply(seq_len(n_peaks), function(i) {
      peak_model_value(models[[i]], x, opt_result$parameters[[i]])
    })
    attr(peaks_tbl, "fitted_components") <- components
    attr(peaks_tbl, "fitted_total") <- opt_result$fitted_values
    attr(peaks_tbl, "residuals") <- opt_result$residuals
  }

  class(peaks_tbl) <- c("peaks_tbl", class(tibble::tibble()))
  peaks_tbl
}

#' @export
print.step_measure_peaks_deconvolve <- function(
  x,
  width = max(20, options()$width - 30),
  ...
) {
  model_name <- if (is.character(x$model)) x$model else x$model$name
  title <- paste0("Deconvolve peaks (", model_name, ", ", x$optimizer, ")")
  if (x$trained) {
    cat(title, sep = "")
  } else {
    cat(title)
  }
  cat("\n")
  invisible(x)
}

#' @rdname tidy.recipe
#' @export
#' @keywords internal
tidy.step_measure_peaks_deconvolve <- function(x, ...) {
  model_name <- if (is.character(x$model)) x$model else x$model$name
  tibble::tibble(
    model = model_name,
    optimizer = x$optimizer,
    max_iter = x$max_iter,
    quality_threshold = x$quality_threshold,
    id = x$id
  )
}
