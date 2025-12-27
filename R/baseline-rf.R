#' Robust Fitting Baseline Correction
#'
#' `step_measure_baseline_rf()` creates a *specification* of a recipe step
#' that applies robust fitting baseline correction to measurement data. This
#' method uses local regression with iterative reweighting to fit a baseline
#' that is resistant to peaks.
#'
#' @param recipe A recipe object. The step will be added to the sequence of
#'   operations for this recipe.
#' @param measures An optional character vector of measure column names to
#'   process. If `NULL` (the default), all measure columns (columns with class
#'   `measure_list`) will be processed.
#' @param span Controls the amount of smoothing. This is the fraction of data
#'   used in computing each fitted value. Default is `2/3`. Smaller values
#'   produce less smooth baselines that follow local features more closely.
#' @param maxit A length-2 integer vector specifying the number of iterations
#'   for the robust fit. The first value is for the asymmetric weighting
#'   function, the second for symmetric weighting. Default is `c(5, 5)`.
#' @param role Not used by this step since no new variables are created.
#' @param trained A logical to indicate if the quantities for preprocessing
#'   have been estimated.
#' @param skip A logical. Should the step be skipped when the recipe is baked?
#' @param id A character string that is unique to this step to identify it.
#'
#' @return An updated version of `recipe` with the new step added to the
#'   sequence of any existing operations.
#'
#' @details
#' Robust fitting baseline correction uses local polynomial regression
#' (LOESS/LOWESS) with iterative reweighting to estimate the baseline. The
#' algorithm uses asymmetric weights in initial iterations to down-weight
#' peaks, then symmetric weights for final smoothing.
#'
#' This method is particularly effective for:
#' - Spectra with peaks of varying widths
#' - Data where the baseline shape is not well-described by a polynomial
#' - Situations where peaks should not influence the baseline estimate
#'
#' The `span` parameter controls the trade-off between smoothness and local
#' adaptation:
#' - Larger span (e.g., 0.8): Smoother baseline, may miss local variations
#' - Smaller span (e.g., 0.3): More local adaptation, may overfit
#'
#' **No selectors should be supplied to this step function**. The data should be
#' in the internal format produced by [step_measure_input_wide()] or
#' [step_measure_input_long()].
#'
#' # Tidying
#'
#' When you [`tidy()`][recipes::tidy.recipe()] this step, a tibble with columns
#' `terms`, `span`, and `id` is returned.
#'
#' @seealso [subtract_rf_baseline()] for the standalone function this step wraps.
#'
#' @family measure-baseline
#' @export
#'
#' @examples
#' library(recipes)
#'
#' rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_baseline_rf(span = 0.5) |>
#'   prep()
#'
#' bake(rec, new_data = NULL)
step_measure_baseline_rf <- function(
    recipe,
    measures = NULL,
    span = 2 / 3,
    maxit = c(5L, 5L),
    role = NA,
    trained = FALSE,
    skip = FALSE,
    id = recipes::rand_id("measure_baseline_rf")) {
  recipes::add_step(
    recipe,
    step_measure_baseline_rf_new(
      measures = measures,
      span = span,
      maxit = maxit,
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_measure_baseline_rf_new <- function(
    measures, span, maxit, role, trained, skip, id) {
  recipes::step(
    subclass = "measure_baseline_rf",
    measures = measures,
    span = span,
    maxit = maxit,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_baseline_rf <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  # Validate parameters
  if (!is.numeric(x$span) || length(x$span) != 1 ||
      x$span <= 0 || x$span > 1) {
    cli::cli_abort(
      "{.arg span} must be a number between 0 and 1, not {.val {x$span}}."
    )
  }
  if (!is.numeric(x$maxit) || length(x$maxit) != 2 ||
      any(x$maxit < 1)) {
    cli::cli_abort(
      "{.arg maxit} must be a length-2 vector of positive integers."
    )
  }

  # Resolve which columns to process
  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  step_measure_baseline_rf_new(
    measures = measure_cols,
    span = x$span,
    maxit = as.integer(x$maxit),
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_baseline_rf <- function(object, new_data, ...) {
  for (col in object$measures) {
    result <- .compute_baseline_rf(
      new_data[[col]],
      span = object$span,
      maxit = object$maxit
    )
    new_data[[col]] <- new_measure_list(result)
  }
  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_baseline_rf <- function(
    x,
    width = max(20, options()$width - 30),
    ...) {
  title <- "Robust fitting baseline correction on "

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
tidy.step_measure_baseline_rf <- function(x, ...) {
  if (recipes::is_trained(x)) {
    terms_val <- x$measures
  } else {
    terms_val <- "<all measure columns>"
  }
  tibble::tibble(
    terms = terms_val,
    span = x$span,
    id = x$id
  )
}

#' @export
required_pkgs.step_measure_baseline_rf <- function(x, ...) {
  c("measure", "IDPmisc")
}

# ------------------------------------------------------------------------------
# Internal computation

#' Compute robust fitting baseline correction for a measure_list
#'
#' @param dat A measure_list (list of measure_tbl objects).
#' @param span Smoothing parameter.
#' @param maxit Iteration counts.
#' @return A list of measure_tbl objects with baseline-corrected values.
#' @noRd
.compute_baseline_rf <- function(dat, span, maxit) {
  purrr::map(dat, .rf_baseline_single, span = span, maxit = maxit)
}

#' Apply robust fitting baseline correction to a single spectrum
#'
#' @param x A measure_tbl with location and value columns.
#' @param span Smoothing parameter.
#' @param maxit Iteration counts.
#' @return A measure_tbl with baseline-corrected values.
#' @noRd
.rf_baseline_single <- function(x, span, maxit) {
  y <- x$value
  n <- length(y)

  # Handle edge cases
  if (n < 4) {
    cli::cli_warn(
      "Spectrum has fewer than 4 points; returning unchanged."
    )
    return(x)
  }

  if (all(is.na(y))) {
    cli::cli_warn("Spectrum is all NA; returning unchanged.")
    return(x)
  }

  # Use location as x-axis, or sequential indices if locations are problematic
  loc <- x$location
  if (length(unique(loc)) < 4) {
    loc <- seq_along(y)
  }

  # Apply robust baseline fitting using IDPmisc
  baseline_result <- tryCatch(
    IDPmisc::rfbaseline(
      x = loc,
      y = y,
      span = span,
      maxit = maxit
    ),
    error = function(e) {
      cli::cli_warn(
        "Robust baseline fitting failed: {e$message}; returning unchanged."
      )
      return(NULL)
    }
  )

  if (is.null(baseline_result)) {
    return(x)
  }

  # Subtract baseline
  x$value <- y - baseline_result$fit
  x
}
