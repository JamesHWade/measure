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
  # Each list element is a tibble that is {num_features} x 2
  if (!is.matrix(x)) {
    cli::cli_abort("Input should be a matrix.")
  }
  if (length(loc) != ncol(x)) {
    cli::cli_abort("# locations should be the same at the number of columns in the source matrix.")
  }

  x <- t(x)
  x <-  tibble::as_tibble(x, .name_repair = "minimal")

  res <- purrr::map(x, ~ tibble::new_tibble(list(location = loc, value = .x)))
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
  if (!any(names(x) == ".measures")) {
    cli::cli_abort("A column called {.code .measures} should be in the data. See
                    {.fn step_measure_input_wide} and
                    {.fn step_measure_input_long}.")
  }
  invisible(NULL)
}