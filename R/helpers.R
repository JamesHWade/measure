# ------------------------------------------------------------------------------
# Move between lists of tibbles and matrices (and back)
# Callers that compare points by location must also check their axes.

require_trained_grid <- function(locations, call = rlang::caller_env()) {
  if (is.null(locations)) {
    cli::cli_abort(
      c(
        "The trained step has no recorded coordinate grid.",
        "i" = "Prep the recipe again with its training data before baking."
      ),
      call = call
    )
  }
  locations
}

check_measure_grid <- function(
  x,
  locations = NULL,
  call = rlang::caller_env()
) {
  if (length(x) == 0L) {
    return(invisible(locations))
  }
  if (is.null(locations)) {
    locations <- x[[1L]][["location"]]
  }
  for (i in seq_along(x)) {
    axis <- x[[i]][["location"]]
    if (!is.numeric(axis) || !all(is.finite(axis))) {
      cli::cli_abort(
        "Sample {i} must have finite, one-dimensional measurement locations.",
        call = call
      )
    }
    if (length(axis) != length(locations)) {
      cli::cli_abort(
        "Cannot match the reference grid for sample {i}: different point counts.",
        call = call
      )
    }
    if (!isTRUE(all(axis == locations))) {
      cli::cli_abort(
        "Sample {i} must have the same locations in the same order as the reference grid.",
        call = call
      )
    }
  }
  invisible(locations)
}

measure_to_matrix <- function(x, call = rlang::caller_env()) {
  values <- purrr::map(x, "value")
  if (length(unique(lengths(values))) > 1L) {
    cli::cli_abort(
      "Cannot form a measurement matrix from samples with different point counts.",
      call = call
    )
  }
  do.call(rbind, values)
}

matrix_to_measure <- function(x, loc, call = rlang::caller_env()) {
  # x is {num_samples} x {num_features}
  # We need to convert this to a list of length {num_samples}.
  # Each list element is a measure_tbl that is {num_features} x 2
  if (!is.matrix(x)) {
    cli::cli_abort("Input should be a matrix.", call = call)
  }
  if (length(loc) != ncol(x)) {
    cli::cli_abort(
      "# locations should be the same as the number of columns in the source matrix.",
      call = call
    )
  }

  x <- t(x)
  x <- tibble::as_tibble(x, .name_repair = "minimal")

  # Create measure_tbl objects for each column
  res <- purrr::map(x, ~ new_measure_tbl(location = loc, value = .x))
  unname(res)
}

measure_to_tibble <- function(x) {
  x <-
    tibble::tibble(x = x, sample_num = seq_along(x)) |>
    tidyr::unnest(cols = x)
  x
}

# ------------------------------------------------------------------------------

#' Check for Measure Columns
#'
#' Validates that the data contains measure columns (columns with class
#' `measure_list`). Aborts with an informative error if no measure columns
#' are found.
#'
#' @param x A data frame or tibble to check.
#' @param call The execution environment of a currently running function, e.g.
#'   `rlang::caller_env()`. Used to report the caller in error messages.
#' @return Invisibly returns the names of measure columns found.
#' @export
#' @keywords internal
check_for_measure <- function(x, call = rlang::caller_env()) {
  # First try class-based detection (preferred)
  meas_cols <- find_measure_cols(x)
  if (length(meas_cols) > 0) {
    return(invisible(meas_cols))
  }

  # Fallback to name-based detection for backwards compatibility
  if (!any(names(x) == ".measures")) {
    cli::cli_abort(
      c(
        "A column called {.code .measures} should be in the data.",
        "i" = "See {.fn step_measure_input_wide} and {.fn step_measure_input_long}."
      ),
      call = call
    )
  }
  invisible(".measures")
}

# ------------------------------------------------------------------------------
# Make sure that data are in the correct format.

check_has_measure <- function(x, cl, call = rlang::caller_env()) {
  # First try class-based detection (preferred)
  meas_cols <- find_measure_cols(x)
  if (length(meas_cols) > 0) {
    return(invisible(meas_cols))
  }

  # Fallback to name-based detection for backwards compatibility
  if (".measures" %in% names(x)) {
    return(invisible(".measures"))
  }

  # Build informative error message
  step_fn <- as.character(cl[[1]])
  step_fn <- gsub("prep\\.", "", step_fn)

  cli::cli_abort(
    c(
      "Measurements have not been converted to internal format.",
      "i" = "Use {.fn step_measure_input_long} or {.fn step_measure_input_wide} \\
          before {.fn {step_fn}}."
    ),
    call = call
  )
}

.tidy_step_parameter <- function(x) {
  if (rlang::is_call(x, "tune")) {
    list(x)
  } else {
    x
  }
}

.format_step_parameter <- function(x, ...) {
  if (rlang::is_call(x, "tune")) rlang::expr_deparse(x) else format(x, ...)
}

# A tunable step stores the call until tune::finalize_recipe() substitutes a
# candidate. Recheck at prep because finalization updates the stored field.
.check_tunable_integer <- function(
  x,
  arg,
  lower = 1L,
  upper = .Machine$integer.max,
  odd = FALSE,
  message = NULL,
  allow_tune = TRUE,
  call = rlang::caller_env()
) {
  if (rlang::is_call(x, "tune")) {
    if (allow_tune) {
      return(x)
    }
    cli::cli_abort(
      "Finalize {.arg {arg}} before preparing the recipe.",
      call = call
    )
  }
  if (
    !is.numeric(x) ||
      length(x) != 1L ||
      !is.finite(x) ||
      x != trunc(x) ||
      x < lower ||
      x > upper ||
      (odd && x %% 2 != 1)
  ) {
    if (is.null(message)) {
      type <- if (odd) "odd integer" else "integer"
      message <- paste("a single finite", type, ">=", lower)
    }
    cli::cli_abort("{.arg {arg}} must be {message}.", call = call)
  }
  as.integer(x)
}

.check_tunable_positive <- function(
  x,
  arg,
  allow_tune = TRUE,
  call = rlang::caller_env()
) {
  if (rlang::is_call(x, "tune")) {
    if (allow_tune) {
      return(x)
    }
    cli::cli_abort(
      "Finalize {.arg {arg}} before preparing the recipe.",
      call = call
    )
  }
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x <= 0) {
    cli::cli_abort("{.arg {arg}} must be a positive number.", call = call)
  }
  x
}
