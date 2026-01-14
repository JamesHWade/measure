# ==============================================================================
# Morphological Baseline Correction Methods
#
# This file contains the morphological baseline correction algorithm:
# - step_measure_baseline_morphological: Erosion/dilation
#
# Note: step_measure_baseline_tophat and step_measure_baseline_minima
# are defined in baseline-extended.R
# ==============================================================================

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
