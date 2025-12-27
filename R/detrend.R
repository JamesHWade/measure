#' Remove Trend from Measurements
#'
#' `step_measure_detrend()` creates a *specification* of a recipe step that
#' removes a polynomial trend from measurement data. This is useful for
#' removing drift, offset, or slowly varying background effects.
#'
#' @param recipe A recipe object. The step will be added to the sequence of
#'   operations for this recipe.
#' @param measures An optional character vector of measure column names to
#'   process. If `NULL` (the default), all measure columns (columns with class
#'   `measure_list`) will be processed.
#' @param degree Polynomial degree for trend fitting. Default is `1` (linear
#'   detrending). Use `0` to remove only the mean (centering).
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
#' Detrending removes a polynomial trend from each spectrum. This is simpler
#' than baseline correction methods like ALS or robust fitting, but effective
#' for:
#'
#' - **Linear drift** (`degree = 1`): Instrumental drift, temperature effects
#' - **Offset removal** (`degree = 0`): Centers each spectrum at zero mean
#' - **Curved trends** (`degree = 2+`): Gradual curvature from scattering
#'
#' Unlike [step_measure_baseline_poly()], detrending fits the polynomial to
#' ALL points without iterative peak exclusion. This makes it faster and
#' appropriate when:
#' - The trend is the dominant feature (not peaks)
#' - You want to preserve peak structure while removing background
#' - Processing time-series or process data with drift
#'
#' **No selectors should be supplied to this step function**. The data should be
#' in the internal format produced by [step_measure_input_wide()] or
#' [step_measure_input_long()].
#'
#' # Tidying
#'
#' When you [`tidy()`][recipes::tidy.recipe()] this step, a tibble with columns
#' `terms`, `degree`, and `id` is returned.
#'
#' @seealso [step_measure_baseline_poly()] for baseline correction with peak
#'   exclusion.
#'
#' @family measure-baseline
#' @export
#'
#' @examples
#' library(recipes)
#'
#' # Linear detrending
#' rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_detrend(degree = 1) |>
#'   prep()
#'
#' bake(rec, new_data = NULL)
#'
#' # Mean centering only (degree = 0)
#' rec2 <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_detrend(degree = 0) |>
#'   prep()
step_measure_detrend <- function(
    recipe,
    measures = NULL,
    degree = 1L,
    role = NA,
    trained = FALSE,
    skip = FALSE,
    id = recipes::rand_id("measure_detrend")) {
  recipes::add_step(
    recipe,
    step_measure_detrend_new(
      measures = measures,
      degree = degree,
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_measure_detrend_new <- function(
    measures, degree, role, trained, skip, id) {
  recipes::step(
    subclass = "measure_detrend",
    measures = measures,
    degree = degree,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_detrend <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  # Validate parameters
  if (!is.numeric(x$degree) || length(x$degree) != 1 || x$degree < 0) {
    cli::cli_abort(
      "{.arg degree} must be a non-negative integer, not {.val {x$degree}}."
    )
  }

  # Resolve which columns to process
  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  step_measure_detrend_new(
    measures = measure_cols,
    degree = as.integer(x$degree),
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_detrend <- function(object, new_data, ...) {
  for (col in object$measures) {
    result <- .compute_detrend(new_data[[col]], degree = object$degree)
    new_data[[col]] <- new_measure_list(result)
  }
  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_detrend <- function(
    x,
    width = max(20, options()$width - 30),
    ...) {
  if (x$degree == 0) {
    title <- "Mean centering on "
  } else if (x$degree == 1) {
    title <- "Linear detrending on "
  } else {
    title <- paste0("Polynomial (degree ", x$degree, ") detrending on ")
  }

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
tidy.step_measure_detrend <- function(x, ...) {
  if (recipes::is_trained(x)) {
    terms_val <- x$measures
  } else {
    terms_val <- "<all measure columns>"
  }
  tibble::tibble(
    terms = terms_val,
    degree = x$degree,
    id = x$id
  )
}

#' @export
required_pkgs.step_measure_detrend <- function(x, ...) {
  "measure"
}

# ------------------------------------------------------------------------------
# Internal computation

#' Compute detrending for a measure_list
#'
#' @param dat A measure_list (list of measure_tbl objects).
#' @param degree Polynomial degree for trend.
#' @return A list of measure_tbl objects with trend removed.
#' @noRd
.compute_detrend <- function(dat, degree) {
  purrr::map(dat, .detrend_single, degree = degree)
}

#' Remove trend from a single spectrum
#'
#' @param x A measure_tbl with location and value columns.
#' @param degree Polynomial degree.
#' @return A measure_tbl with trend removed.
#' @noRd
.detrend_single <- function(x, degree) {
  y <- x$value
  n <- length(y)

  # Handle edge cases
  if (n <= degree) {
    cli::cli_warn(
      "Spectrum has {n} points, not enough for degree {degree}; returning unchanged."
    )
    return(x)
  }

  if (all(is.na(y))) {
    cli::cli_warn("Spectrum is all NA; returning unchanged.")
    return(x)
  }

  # Degree 0: just remove mean
  if (degree == 0) {
    x$value <- y - mean(y, na.rm = TRUE)
    return(x)
  }

  # For degree >= 1, fit polynomial
  loc <- x$location

  # Scale location for numerical stability
  loc_mean <- mean(loc, na.rm = TRUE)
  loc_sd <- stats::sd(loc, na.rm = TRUE)

  if (is.na(loc_sd) || loc_sd == 0) {
    loc_scaled <- loc - loc_mean
  } else {
    loc_scaled <- (loc - loc_mean) / loc_sd
  }

  # Fit polynomial trend
  fit_data <- data.frame(y = y, x = loc_scaled)

  fit <- tryCatch(
    stats::lm(y ~ poly(x, degree, raw = TRUE), data = fit_data),
    error = function(e) {
      cli::cli_warn("Polynomial fit failed: {e$message}; using linear fit.")
      stats::lm(y ~ x, data = fit_data)
    }
  )

  # Predict trend and subtract
  trend <- stats::predict(fit, newdata = data.frame(x = loc_scaled))
  x$value <- y - trend
  x
}
