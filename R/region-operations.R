# ==============================================================================
# Region Operations
#
# This file contains steps for manipulating measurement regions:
# - step_measure_trim: Keep specified x-range(s)
# - step_measure_exclude: Exclude specified x-range(s)
# - step_measure_resample: Interpolate to new x-grid
# ==============================================================================

# ==============================================================================
# step_measure_trim
# ==============================================================================

#' Trim Measurements to Specified Range
#'
#' `step_measure_trim()` creates a *specification* of a recipe step that
#' keeps only the measurement points within the specified x-axis range(s).
#'
#' @param recipe A recipe object. The step will be added to the sequence of
#'   operations for this recipe.
#' @param range A numeric vector of length 2 specifying the range to keep as
#'   `c(min, max)`. Points with location >= min and <= max are retained.
#' @param measures An optional character vector of measure column names to
#'   process. If `NULL` (the default), all measure columns will be processed.
#' @param role Not used by this step since no new variables are created.
#' @param trained A logical to indicate if the step has been trained.
#' @param skip A logical. Should the step be skipped when baking?
#' @param id A character string that is unique to this step.
#'
#' @return An updated version of `recipe` with the new step added.
#'
#' @details
#' This step filters measurements to keep only points within the specified
#' range. This is useful for:
#'
#' - Defining integration windows (e.g., keep only 8-18 mL elution range)
#' - Removing noisy regions at start/end of measurement
#' - Focusing analysis on a region of interest
#'
#' Points with location values outside the range are removed. The order of
#' remaining points is preserved.
#'
#' @family region-operations
#' @seealso [step_measure_exclude()] for removing specific ranges,
#'   [step_measure_resample()] for interpolating to a new grid
#' @export
#'
#' @examples
#' library(recipes)
#'
#' # Keep only a specific wavelength range
#' rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_trim(range = c(10, 90)) |>
#'   prep()
#'
#' bake(rec, new_data = NULL)
step_measure_trim <- function(
    recipe,
    range,
    measures = NULL,
    role = NA,
    trained = FALSE,
    skip = FALSE,
    id = recipes::rand_id("measure_trim")) {
  if (missing(range)) {
    cli::cli_abort("{.arg range} must be provided.")
  }
  if (!is.numeric(range) || length(range) != 2) {
    cli::cli_abort("{.arg range} must be a numeric vector of length 2.")
  }
  if (range[1] >= range[2]) {
    cli::cli_abort(
      "{.arg range} must have the minimum value first: c(min, max)."
    )
  }

  recipes::add_step(
    recipe,
    step_measure_trim_new(
      range = range,
      measures = measures,
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_measure_trim_new <- function(range, measures, role, trained, skip, id) {
  recipes::step(
    subclass = "measure_trim",
    range = range,
    measures = measures,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_trim <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  step_measure_trim_new(
    range = x$range,
    measures = measure_cols,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_trim <- function(object, new_data, ...) {
  range <- object$range

  for (col in object$measures) {
    result <- purrr::map(new_data[[col]], function(m) {
      idx <- m$location >= range[1] & m$location <= range[2]
      new_measure_tbl(
        location = m$location[idx],
        value = m$value[idx]
      )
    })
    new_data[[col]] <- new_measure_list(result)
  }

  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_trim <- function(
    x,
    width = max(20, options()$width - 30),
    ...) {
  title <- paste0("Trim measurements to [", x$range[1], ", ", x$range[2], "]")

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
tidy.step_measure_trim <- function(x, ...) {
  if (is_trained(x)) {
    terms <- x$measures
  } else {
    terms <- "<all measure columns>"
  }

  tibble::tibble(
    terms = terms,
    range_min = x$range[1],
    range_max = x$range[2],
    id = x$id
  )
}

# ==============================================================================
# step_measure_exclude
# ==============================================================================

#' Exclude Measurement Ranges
#'
#' `step_measure_exclude()` creates a *specification* of a recipe step that
#' removes measurement points within the specified x-axis range(s).
#'
#' @param recipe A recipe object. The step will be added to the sequence of
#'   operations for this recipe.
#' @param ranges A list of numeric vectors, each of length 2 specifying ranges
#'   to exclude as `c(min, max)`. Points with location >= min and <= max in
#'   any range are removed.
#' @param measures An optional character vector of measure column names to
#'   process. If `NULL` (the default), all measure columns will be processed.
#' @param role Not used by this step since no new variables are created.
#' @param trained A logical to indicate if the step has been trained.
#' @param skip A logical. Should the step be skipped when baking?
#' @param id A character string that is unique to this step.
#'
#' @return An updated version of `recipe` with the new step added.
#'
#' @details
#' This step removes measurements falling within specified ranges. This is
#' useful for:
#'
#' - Removing solvent peaks in chromatography
#' - Excluding system peaks or artifacts
#' - Removing detector saturation regions
#' - Removing known interference regions in spectroscopy
#'
#' Multiple ranges can be excluded by providing a list of ranges. Points
#' falling within any of the specified ranges are removed.
#'
#' @family region-operations
#' @seealso [step_measure_trim()] for keeping specific ranges,
#'   [step_measure_resample()] for interpolating to a new grid
#' @export
#'
#' @examples
#' library(recipes)
#'
#' # Exclude specific regions (e.g., solvent peaks)
#' rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_exclude(ranges = list(c(1, 5), c(95, 100))) |>
#'   prep()
#'
#' bake(rec, new_data = NULL)
step_measure_exclude <- function(
    recipe,
    ranges,
    measures = NULL,
    role = NA,
    trained = FALSE,
    skip = FALSE,
    id = recipes::rand_id("measure_exclude")) {
  if (missing(ranges)) {
    cli::cli_abort("{.arg ranges} must be provided.")
  }

  # Accept a single range as c(min, max) for convenience

if (!is.list(ranges)) {
    if (is.numeric(ranges) && length(ranges) == 2) {
      ranges <- list(ranges)
    } else {
      cli::cli_abort(
        "{.arg ranges} must be a list of numeric vectors or a single \\
        numeric vector of length 2."
      )
    }
  }

  # Validate each range
  for (i in seq_along(ranges)) {
    r <- ranges[[i]]
    if (!is.numeric(r) || length(r) != 2) {
      cli::cli_abort(
        "Each range must be a numeric vector of length 2. \\
        Range {i} has {length(r)} element{?s}."
      )
    }
    if (r[1] >= r[2]) {
      cli::cli_abort(
        "Each range must have the minimum value first. \\
        Range {i}: c({r[1]}, {r[2]})."
      )
    }
  }

  recipes::add_step(
    recipe,
    step_measure_exclude_new(
      ranges = ranges,
      measures = measures,
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_measure_exclude_new <- function(ranges, measures, role, trained, skip, id) {
  recipes::step(
    subclass = "measure_exclude",
    ranges = ranges,
    measures = measures,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_exclude <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  step_measure_exclude_new(
    ranges = x$ranges,
    measures = measure_cols,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_exclude <- function(object, new_data, ...) {
  ranges <- object$ranges

  for (col in object$measures) {
    result <- purrr::map(new_data[[col]], function(m) {
      # Start with all points included
      keep <- rep(TRUE, length(m$location))

      # Mark points in any exclusion range as FALSE
      for (r in ranges) {
        in_range <- m$location >= r[1] & m$location <= r[2]
        keep[in_range] <- FALSE
      }

      new_measure_tbl(
        location = m$location[keep],
        value = m$value[keep]
      )
    })
    new_data[[col]] <- new_measure_list(result)
  }

  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_exclude <- function(
    x,
    width = max(20, options()$width - 30),
    ...) {
  n_ranges <- length(x$ranges)
  ranges_str <- paste(
    purrr::map_chr(x$ranges, ~ paste0("[", .x[1], ", ", .x[2], "]")),
    collapse = ", "
  )
  title <- paste0("Exclude ", n_ranges, " range", if (n_ranges > 1) "s", ": ", ranges_str)

  if (nchar(title) > width) {
    title <- paste0("Exclude ", n_ranges, " range", if (n_ranges > 1) "s")
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
tidy.step_measure_exclude <- function(x, ...) {
  if (is_trained(x)) {
    terms <- x$measures
  } else {
    terms <- "<all measure columns>"
  }

  # Create a row for each exclusion range
  ranges_df <- purrr::map_dfr(x$ranges, function(r) {
    tibble::tibble(range_min = r[1], range_max = r[2])
  })

  tibble::tibble(
    terms = rep(terms, nrow(ranges_df)),
    range_min = ranges_df$range_min,
    range_max = ranges_df$range_max,
    id = x$id
  )
}

# ==============================================================================
# step_measure_resample
# ==============================================================================

#' Resample Measurements to New Grid
#'
#' `step_measure_resample()` creates a *specification* of a recipe step that
#' interpolates measurements to a new regular x-axis grid.
#'
#' @param recipe A recipe object. The step will be added to the sequence of
#'   operations for this recipe.
#' @param n A positive integer specifying the number of points in the new grid.
#'   Mutually exclusive with `spacing`.
#' @param spacing A positive numeric value specifying the spacing between points
#'   in the new grid. Mutually exclusive with `n`.
#' @param range Optional numeric vector of length 2 specifying the range for the
#'   new grid as `c(min, max)`. If `NULL` (default), uses the range of the
#'   existing measurements.
#' @param method The interpolation method. One of:
#'   - `"linear"`: Linear interpolation (default)
#'   - `"spline"`: Cubic spline interpolation (smoother)
#' @param measures An optional character vector of measure column names to
#'   process. If `NULL` (the default), all measure columns will be processed.
#' @param role Not used by this step since no new variables are created.
#' @param trained A logical to indicate if the step has been trained.
#' @param new_locations The computed new grid locations (after training).
#' @param skip A logical. Should the step be skipped when baking?
#' @param id A character string that is unique to this step.
#'
#' @return An updated version of `recipe` with the new step added.
#'
#' @details
#' This step interpolates measurements to a new regular grid of x-axis values.
#' This is useful for:
#'
#' - Aligning data from different instruments with different sampling rates
#' - Reducing data density for faster processing
#' - Ensuring uniform spacing for methods that require it
#' - Matching measurements to a reference grid
#'
#' The new grid is determined during `prep()` based on the training data. If
#' `range` is not specified, the grid spans from the minimum to maximum
#' location values in the training data.
#'
#' **Interpolation methods:**
#'
#' - `"linear"`: Fast and simple, may introduce slight distortion at peaks

#' - `"spline"`: Smoother interpolation that preserves peak shape better
#'
#' @family region-operations
#' @seealso [step_measure_trim()] for keeping specific ranges,
#'   [step_measure_exclude()] for removing specific ranges
#' @export
#'
#' @examples
#' library(recipes)
#'
#' # Resample to 50 evenly spaced points
#' rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_resample(n = 50) |>
#'   prep()
#'
#' bake(rec, new_data = NULL)
#'
#' # Resample with specific spacing
#' rec2 <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_resample(spacing = 2, method = "spline") |>
#'   prep()
#'
#' bake(rec2, new_data = NULL)
step_measure_resample <- function(
    recipe,
    n = NULL,
    spacing = NULL,
    range = NULL,
    method = c("linear", "spline"),
    measures = NULL,
    role = NA,
    trained = FALSE,
    new_locations = NULL,
    skip = FALSE,
    id = recipes::rand_id("measure_resample")) {
  method <- rlang::arg_match(method)

  # Must specify exactly one of n or spacing
  has_n <- !is.null(n)
  has_spacing <- !is.null(spacing)

  if (!has_n && !has_spacing) {
    cli::cli_abort("Either {.arg n} or {.arg spacing} must be specified.")
  }
  if (has_n && has_spacing) {
    cli::cli_abort("{.arg n} and {.arg spacing} are mutually exclusive.")
  }

  if (has_n) {
    if (!is.numeric(n) || length(n) != 1 || n < 2 || n != round(n)) {
      cli::cli_abort("{.arg n} must be a positive integer >= 2.")
    }
  }

  if (has_spacing) {
    if (!is.numeric(spacing) || length(spacing) != 1 || spacing <= 0) {
      cli::cli_abort("{.arg spacing} must be a positive number.")
    }
  }

  if (!is.null(range)) {
    if (!is.numeric(range) || length(range) != 2) {
      cli::cli_abort("{.arg range} must be a numeric vector of length 2.")
    }
    if (range[1] >= range[2]) {
      cli::cli_abort(
        "{.arg range} must have the minimum value first: c(min, max)."
      )
    }
  }

  recipes::add_step(
    recipe,
    step_measure_resample_new(
      n = n,
      spacing = spacing,
      range = range,
      method = method,
      measures = measures,
      role = role,
      trained = trained,
      new_locations = new_locations,
      skip = skip,
      id = id
    )
  )
}

step_measure_resample_new <- function(
    n, spacing, range, method, measures, role, trained, new_locations, skip, id) {
  recipes::step(
    subclass = "measure_resample",
    n = n,
    spacing = spacing,
    range = range,
    method = method,
    measures = measures,
    role = role,
    trained = trained,
    new_locations = new_locations,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_resample <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  # Determine the grid range from training data
  if (is.null(x$range)) {
    # Get all locations from training data
    all_locs <- unlist(purrr::map(training[[measure_cols[1]]], ~ .x$location))
    grid_min <- min(all_locs, na.rm = TRUE)
    grid_max <- max(all_locs, na.rm = TRUE)
  } else {
    grid_min <- x$range[1]
    grid_max <- x$range[2]
  }

  # Compute new locations
  if (!is.null(x$n)) {
    new_locations <- seq(grid_min, grid_max, length.out = x$n)
  } else {
    new_locations <- seq(grid_min, grid_max, by = x$spacing)
    # Ensure we include the endpoint if spacing doesn't land exactly on it
    if (utils::tail(new_locations, 1) < grid_max) {
      new_locations <- c(new_locations, grid_max)
    }
  }

  step_measure_resample_new(
    n = x$n,
    spacing = x$spacing,
    range = x$range,
    method = x$method,
    measures = measure_cols,
    role = x$role,
    trained = TRUE,
    new_locations = new_locations,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_resample <- function(object, new_data, ...) {
  new_locs <- object$new_locations
  method <- object$method

  for (col in object$measures) {
    result <- purrr::map(new_data[[col]], function(m) {
      if (method == "linear") {
        new_vals <- stats::approx(
          x = m$location,
          y = m$value,
          xout = new_locs,
          rule = 2  # Use boundary values for extrapolation
        )$y
      } else if (method == "spline") {
        # spline requires at least 4 points
        if (length(m$location) >= 4) {
          new_vals <- stats::spline(
            x = m$location,
            y = m$value,
            xout = new_locs,
            method = "natural"
          )$y
        } else {
          # Fall back to linear for short spectra
          new_vals <- stats::approx(
            x = m$location,
            y = m$value,
            xout = new_locs,
            rule = 2
          )$y
        }
      }

      new_measure_tbl(
        location = new_locs,
        value = new_vals
      )
    })
    new_data[[col]] <- new_measure_list(result)
  }

  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_resample <- function(
    x,
    width = max(20, options()$width - 30),
    ...) {
  if (x$trained) {
    n_pts <- length(x$new_locations)
    title <- paste0("Resample to ", n_pts, " points (", x$method, ")")
    cat(title, " on <internal measurements>", sep = "")
  } else {
    if (!is.null(x$n)) {
      title <- paste0("Resample to ", x$n, " points (", x$method, ")")
    } else {
      title <- paste0("Resample with spacing ", x$spacing, " (", x$method, ")")
    }
    cat(title)
  }
  cat("\n")

  invisible(x)
}

#' @rdname tidy.recipe
#' @export
#' @keywords internal
tidy.step_measure_resample <- function(x, ...) {
  if (is_trained(x)) {
    terms <- x$measures
    n_points <- length(x$new_locations)
    range_min <- min(x$new_locations)
    range_max <- max(x$new_locations)
  } else {
    terms <- "<all measure columns>"
    n_points <- x$n %||% NA_integer_
    range_min <- x$range[1] %||% NA_real_
    range_max <- x$range[2] %||% NA_real_
  }

  tibble::tibble(
    terms = terms,
    n_points = n_points,
    method = x$method,
    range_min = range_min,
    range_max = range_max,
    id = x$id
  )
}

#' @rdname required_pkgs.recipe
#' @export
#' @keywords internal
required_pkgs.step_measure_resample <- function(x, ...) {
  c("measure", "stats")
}

# ==============================================================================
# step_measure_interpolate
# ==============================================================================

#' Interpolate Gaps in Measurement Data
#'
#' `step_measure_interpolate()` creates a *specification* of a recipe step that
#' fills gaps or missing values in measurement data using interpolation.
#'
#' @param recipe A recipe object.
#' @param ranges A list of numeric vectors specifying ranges to interpolate.
#'   Each element should be a vector of length 2: `c(min, max)`.
#' @param method Interpolation method: "linear" or "spline". Default is "linear".
#' @param measures An optional character vector of measure column names.
#' @param role Not used.
#' @param trained Logical indicating if the step has been trained.
#' @param skip Logical. Should the step be skipped when baking?
#' @param id Unique step identifier.
#'
#' @return An updated recipe with the new step added.
#'
#' @details
#' This step is useful for:
#' - Filling gaps left by excluded regions that need restoration
#' - Handling missing or invalid data points
#' - Smoothing over detector saturation regions
#'
#' The interpolation uses data points immediately outside the specified
#' ranges to estimate values within the ranges.
#'
#' @family measure-region
#' @export
#'
#' @examples
#' library(recipes)
#'
#' # Interpolate over a problematic region
#' rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_interpolate(ranges = list(c(40, 50)), method = "spline") |>
#'   prep()
step_measure_interpolate <- function(
    recipe,
    ranges,
    method = c("linear", "spline"),
    measures = NULL,
    role = NA,
    trained = FALSE,
    skip = FALSE,
    id = recipes::rand_id("measure_interpolate")) {

  method <- rlang::arg_match(method)

  if (!is.list(ranges)) {
    if (is.numeric(ranges) && length(ranges) == 2) {
      ranges <- list(ranges)
    } else {
      cli::cli_abort("{.arg ranges} must be a list of numeric vectors of length 2.")
    }
  }

  for (r in ranges) {
    if (!is.numeric(r) || length(r) != 2) {
      cli::cli_abort("Each range must be a numeric vector of length 2.")
    }
    if (r[1] >= r[2]) {
      cli::cli_abort("Range minimum must be less than maximum.")
    }
  }

  recipes::add_step(
    recipe,
    step_measure_interpolate_new(
      ranges = ranges,
      method = method,
      measures = measures,
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_measure_interpolate_new <- function(
    ranges, method, measures, role, trained, skip, id) {
  recipes::step(
    subclass = "measure_interpolate",
    ranges = ranges,
    method = method,
    measures = measures,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_interpolate <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  step_measure_interpolate_new(
    ranges = x$ranges,
    method = x$method,
    measures = measure_cols,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' Interpolate values within specified ranges
#' @noRd
.interpolate_ranges <- function(location, value, ranges, method) {
  result <- value

  for (r in ranges) {
    # Find indices within the range
    in_range <- location >= r[1] & location <= r[2]

    if (!any(in_range)) next

    # Find boundary points (just outside the range)
    below_range <- location < r[1]
    above_range <- location > r[2]

    if (!any(below_range) || !any(above_range)) {
      # Can't interpolate without points on both sides
      next
    }

    # Get the closest points outside the range
    anchor_idx <- c(
      which(below_range),
      which(above_range)
    )
    anchor_x <- location[anchor_idx]
    anchor_y <- value[anchor_idx]

    # Interpolate
    if (method == "spline" && length(anchor_idx) >= 4) {
      interp <- stats::spline(anchor_x, anchor_y, xout = location[in_range])
      result[in_range] <- interp$y
    } else {
      interp <- stats::approx(anchor_x, anchor_y, xout = location[in_range])
      result[in_range] <- interp$y
    }
  }

  result
}

#' @export
bake.step_measure_interpolate <- function(object, new_data, ...) {
  ranges <- object$ranges
  method <- object$method

  for (col in object$measures) {
    new_data[[col]] <- purrr::map(new_data[[col]], function(m) {
      m$value <- .interpolate_ranges(m$location, m$value, ranges, method)
      m
    })
    new_data[[col]] <- new_measure_list(new_data[[col]])
  }

  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_interpolate <- function(x, width = max(20, options()$width - 30), ...) {
  n_ranges <- length(x$ranges)
  title <- paste0("Interpolate ", n_ranges, " region(s) (", x$method, ")")
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
tidy.step_measure_interpolate <- function(x, ...) {
  tibble::tibble(
    terms = if (is_trained(x)) x$measures else "<all measure columns>",
    n_ranges = length(x$ranges),
    method = x$method,
    id = x$id
  )
}
