# ------------------------------------------------------------------------------
# Add location columns

add_location_col <- function(x, loc) {
  x$location <- loc
  x
}

add_location <- function(.data, loc) {
  dplyr::mutate(
    .data,
    .measures = purrr::map(.measures, add_location_col, loc = loc)
  )
}

# ------------------------------------------------------------------------------
# Move between lists of tibbles and matrices (and back)
# Assumes identical locations. We have code to check that the per-sample
# dimensions are equal but nothing yet for identical locations.

measure_to_matrix <- function(x) {
  res <- do.call("rbind", purrr::map(x, ~ .x[["value"]]))
  res
}

matrix_to_measure <- function(x, loc) {
  # x is {num_samples} x {num_features}
  # We need to convert this to a list of length {num_samples}.
  # Each list element is a measure_tbl that is {num_features} x 2
  if (!is.matrix(x)) {
    cli::cli_abort("Input should be a matrix.")
  }
  if (length(loc) != ncol(x)) {
    cli::cli_abort(
      "# locations should be the same as the number of columns in the source matrix."
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
    tibble::tibble(x = x, sample_num = seq_along(x)) %>%
    tidyr::unnest(cols = x)
  x
}

# ------------------------------------------------------------------------------

check_for_measure <- function(x) {
  # First try class-based detection (preferred)
  meas_cols <- find_measure_cols(x)
  if (length(meas_cols) > 0) {
    return(invisible(meas_cols))
  }

  # Fallback to name-based detection for backwards compatibility
  if (!any(names(x) == ".measures")) {
    cli::cli_abort(c(
      "A column called {.code .measures} should be in the data.",
      "i" = "See {.fn step_measure_input_wide} and {.fn step_measure_input_long}."
    ))
  }
  invisible(".measures")
}

# ------------------------------------------------------------------------------
# Make sure that data are in the correct format.

check_has_measure <- function(x, cl) {
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

  cli::cli_abort(c(
    "Measurements have not been converted to internal format.",
    "i" = "Use {.fn step_measure_input_long} or {.fn step_measure_input_wide} \\
          before {.fn {step_fn}}."
  ))
}
