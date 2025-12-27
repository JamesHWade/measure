#' Polynomial Baseline Correction
#'
#' `step_measure_baseline_poly()` creates a *specification* of a recipe step
#' that applies polynomial baseline correction to measurement data. The method
#' fits a polynomial to the spectrum, optionally with iterative peak exclusion.
#'
#' @param recipe A recipe object. The step will be added to the sequence of
#'   operations for this recipe.
#' @param measures An optional character vector of measure column names to
#'   process. If `NULL` (the default), all measure columns (columns with class
#'   `measure_list`) will be processed.
#' @param degree Polynomial degree for baseline fitting. Default is `2`
#'   (quadratic). Higher degrees fit more complex baselines but risk overfitting.
#'   Tunable via [baseline_degree()].
#' @param max_iter Maximum number of iterations for peak exclusion. Default is
#'   `0` (no iteration, fit polynomial to all points). Set to a positive integer
#'   to iteratively exclude points above the fitted baseline.
#' @param threshold Number of standard deviations above baseline for a point to
#'   be excluded in iterative fitting. Default is `1.5`. Only used when
#'   `max_iter > 0`.
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
#' Polynomial baseline correction fits a polynomial function to the spectrum
#' and subtracts it. This is effective for removing smooth, curved baselines
#' caused by instrumental drift, scattering, or other slowly varying effects.
#'
#' When `max_iter > 0`, the algorithm uses iterative peak exclusion:
#' 1. Fit polynomial to all points
#' 2. Calculate residuals (spectrum - baseline)
#' 3. Exclude points where residual > threshold * SD(residuals)
#' 4. Refit polynomial to remaining points
#' 5. Repeat until convergence or max_iter reached
#'
#' This iterative approach prevents peaks from pulling up the baseline estimate.
#'
#' **Degree selection:**
#' - `degree = 1`: Linear baseline (for simple drift)
#' - `degree = 2`: Quadratic (most common, handles gentle curvature)
#' - `degree = 3-5`: Higher-order (for complex baselines, use cautiously)
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
#' @family measure-baseline
#' @export
#'
#' @examples
#' library(recipes)
#'
#' # Simple polynomial baseline (no iteration)
#' rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_baseline_poly(degree = 2) |>
#'   prep()
#'
#' bake(rec, new_data = NULL)
#'
#' # With iterative peak exclusion
#' rec2 <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_baseline_poly(degree = 3, max_iter = 5, threshold = 2) |>
#'   prep()
step_measure_baseline_poly <- function(
  recipe,
  measures = NULL,
  degree = 2L,
  max_iter = 0L,
  threshold = 1.5,
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_baseline_poly")
) {
  recipes::add_step(
    recipe,
    step_measure_baseline_poly_new(
      measures = measures,
      degree = degree,
      max_iter = max_iter,
      threshold = threshold,
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_measure_baseline_poly_new <- function(
  measures,
  degree,
  max_iter,
  threshold,
  role,
  trained,
  skip,
  id
) {
  recipes::step(
    subclass = "measure_baseline_poly",
    measures = measures,
    degree = degree,
    max_iter = max_iter,
    threshold = threshold,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_baseline_poly <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  # Validate parameters
  if (!is.numeric(x$degree) || length(x$degree) != 1 || x$degree < 1) {
    cli::cli_abort(
      "{.arg degree} must be a positive integer, not {.val {x$degree}}."
    )
  }
  if (!is.numeric(x$max_iter) || length(x$max_iter) != 1 || x$max_iter < 0) {
    cli::cli_abort(
      "{.arg max_iter} must be a non-negative integer, not {.val {x$max_iter}}."
    )
  }
  if (
    !is.numeric(x$threshold) || length(x$threshold) != 1 || x$threshold <= 0
  ) {
    cli::cli_abort(
      "{.arg threshold} must be a positive number, not {.val {x$threshold}}."
    )
  }

  # Resolve which columns to process
  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  step_measure_baseline_poly_new(
    measures = measure_cols,
    degree = as.integer(x$degree),
    max_iter = as.integer(x$max_iter),
    threshold = x$threshold,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_baseline_poly <- function(object, new_data, ...) {
  for (col in object$measures) {
    result <- .compute_baseline_poly(
      new_data[[col]],
      degree = object$degree,
      max_iter = object$max_iter,
      threshold = object$threshold
    )
    new_data[[col]] <- new_measure_list(result)
  }
  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_baseline_poly <- function(
  x,
  width = max(20, options()$width - 30),
  ...
) {
  title <- "Polynomial baseline correction on "

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
tidy.step_measure_baseline_poly <- function(x, ...) {
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
required_pkgs.step_measure_baseline_poly <- function(x, ...) {
  "measure"
}

# ------------------------------------------------------------------------------
# Internal computation

#' Compute polynomial baseline correction for a measure_list
#'
#' @param dat A measure_list (list of measure_tbl objects).
#' @param degree Polynomial degree.
#' @param max_iter Maximum iterations for peak exclusion.
#' @param threshold SD threshold for peak exclusion.
#' @return A list of measure_tbl objects with baseline-corrected values.
#' @noRd
.compute_baseline_poly <- function(dat, degree, max_iter, threshold) {
  purrr::map(
    dat,
    .poly_baseline_single,
    degree = degree,
    max_iter = max_iter,
    threshold = threshold
  )
}

#' Apply polynomial baseline correction to a single spectrum
#'
#' @param x A measure_tbl with location and value columns.
#' @param degree Polynomial degree.
#' @param max_iter Maximum iterations.
#' @param threshold SD threshold.
#' @return A measure_tbl with baseline-corrected values.
#' @noRd
.poly_baseline_single <- function(x, degree, max_iter, threshold) {
  y <- x$value
  loc <- x$location
  n <- length(y)

  # Handle edge cases
  if (n <= degree) {
    cli::cli_warn(
      "Spectrum has {n} points, fewer than degree + 1 ({degree + 1});
       returning unchanged."
    )
    return(x)
  }

  if (all(is.na(y))) {
    cli::cli_warn("Spectrum is all NA; returning unchanged.")
    return(x)
  }

  # Scale location for numerical stability
  loc_scaled <- (loc - mean(loc, na.rm = TRUE)) / stats::sd(loc, na.rm = TRUE)

  # Handle case where all locations are the same
  if (
    is.na(stats::sd(loc, na.rm = TRUE)) || stats::sd(loc, na.rm = TRUE) == 0
  ) {
    loc_scaled <- loc - mean(loc, na.rm = TRUE)
  }

  # Initialize: all points included
  include <- rep(TRUE, n)

  # Fit polynomial (iteratively if max_iter > 0)
  for (iter in seq_len(max(1, max_iter + 1))) {
    # Create data frame for fitting
    fit_data <- data.frame(y = y[include], x = loc_scaled[include])

    # Fit polynomial
    fit <- tryCatch(
      stats::lm(y ~ poly(x, degree, raw = TRUE), data = fit_data),
      error = function(e) {
        cli::cli_warn("Polynomial fit failed: {e$message}; using linear fit.")
        stats::lm(y ~ x, data = fit_data)
      }
    )

    # Predict baseline for all points
    baseline <- stats::predict(fit, newdata = data.frame(x = loc_scaled))

    # If no iteration requested, exit after first fit
    if (max_iter == 0) {
      break
    }

    # Calculate residuals and update inclusion mask
    residuals <- y - baseline
    res_sd <- stats::sd(residuals[include], na.rm = TRUE)

    if (is.na(res_sd) || res_sd == 0) {
      break
    }

    # Exclude points above threshold
    new_include <- residuals <= threshold * res_sd

    # Check convergence
    if (all(new_include == include)) {
      break
    }

    include <- new_include

    # Safety: ensure we have enough points to fit
    if (sum(include) <= degree + 1) {
      cli::cli_warn(
        "Too few points remaining after peak exclusion; using last valid fit."
      )
      break
    }
  }

  # Subtract baseline
  x$value <- y - baseline
  x
}
