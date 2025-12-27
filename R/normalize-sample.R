# Sample-wise normalization steps for measure package
# Each spectrum is normalized independently - no learning from training data

# ==============================================================================
# step_measure_normalize_sum
# ==============================================================================

#' Normalize by Sum (Total Intensity)
#'
#' `step_measure_normalize_sum()` creates a *specification* of a recipe step that
#' divides each spectrum by its sum (total intensity). This is useful for
#' comparing relative abundances across samples with different total signals.
#'
#' @inheritParams step_measure_snv
#'
#' @return An updated version of `recipe` with the new step added to the
#'   sequence of any existing operations.
#'
#' @details
#' For each spectrum \eqn{x}, the transformation is:
#'
#' \deqn{x_{norm} = \frac{x}{\sum x}}{x_norm = x / sum(x)}
#'
#' After transformation, the sum of each spectrum will equal 1.
#'
#' If the sum is zero or NA, a warning is issued and the original values are
#' returned unchanged.
#'
#' **No selectors should be supplied to this step function**. The data should be
#' in the internal format produced by [step_measure_input_wide()] or
#' [step_measure_input_long()].
#'
#' @family measure-normalization
#' @seealso [step_measure_normalize_max()], [step_measure_normalize_auc()]
#' @export
#'
#' @examples
#' library(recipes)
#'
#' rec <-
#'   recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_normalize_sum() |>
#'   prep()
#'
#' bake(rec, new_data = NULL)
step_measure_normalize_sum <- function(
  recipe,
  measures = NULL,
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_normalize_sum")
) {
  recipes::add_step(
    recipe,
    step_measure_normalize_sum_new(
      measures = measures,
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_measure_normalize_sum_new <- function(measures, role, trained, skip, id) {
  recipes::step(
    subclass = "measure_normalize_sum",
    measures = measures,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_normalize_sum <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  step_measure_normalize_sum_new(
    measures = measure_cols,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_normalize_sum <- function(object, new_data, ...) {
  for (col in object$measures) {
    result <- .compute_normalize_sum(new_data[[col]])
    new_data[[col]] <- new_measure_list(result)
  }
  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_normalize_sum <- function(
  x,
  width = max(20, options()$width - 30),
  ...
) {
  title <- "Sum normalization on "
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
tidy.step_measure_normalize_sum <- function(x, ...) {
  if (is_trained(x)) {
    terms <- x$measures
  } else {
    terms <- "<all measure columns>"
  }

  tibble::tibble(terms = terms, id = x$id)
}

.compute_normalize_sum <- function(dat) {
  purrr::map(dat, .normalize_sum_single)
}

.normalize_sum_single <- function(x) {
  total <- sum(x$value, na.rm = TRUE)
  if (is.na(total) || abs(total) < .Machine$double.eps) {
    cli::cli_warn(
      "Sum is zero or NA for a spectrum; returning original values."
    )
    return(x)
  }
  x$value <- x$value / total
  x
}

# ==============================================================================
# step_measure_normalize_max
# ==============================================================================

#' Normalize by Maximum Value
#'
#' `step_measure_normalize_max()` creates a *specification* of a recipe step that
#' divides each spectrum by its maximum value. This is useful for peak-focused
#' analysis where you want the highest peak to equal 1.
#'
#' @inheritParams step_measure_snv
#'
#' @return An updated version of `recipe` with the new step added to the
#'   sequence of any existing operations.
#'
#' @details
#' For each spectrum \eqn{x}, the transformation is:
#'
#' \deqn{x_{norm} = \frac{x}{\max(x)}}{x_norm = x / max(x)}
#'
#' After transformation, the maximum value of each spectrum will equal 1.
#'
#' If the maximum is zero or NA, a warning is issued and the original values are
#' returned unchanged.
#'
#' **No selectors should be supplied to this step function**. The data should be
#' in the internal format produced by [step_measure_input_wide()] or
#' [step_measure_input_long()].
#'
#' @family measure-normalization
#' @seealso [step_measure_normalize_sum()], [step_measure_normalize_range()]
#' @export
#'
#' @examples
#' library(recipes)
#'
#' rec <-
#'   recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_normalize_max() |>
#'   prep()
#'
#' bake(rec, new_data = NULL)
step_measure_normalize_max <- function(
  recipe,
  measures = NULL,
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_normalize_max")
) {
  recipes::add_step(
    recipe,
    step_measure_normalize_max_new(
      measures = measures,
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_measure_normalize_max_new <- function(measures, role, trained, skip, id) {
  recipes::step(
    subclass = "measure_normalize_max",
    measures = measures,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_normalize_max <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  step_measure_normalize_max_new(
    measures = measure_cols,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_normalize_max <- function(object, new_data, ...) {
  for (col in object$measures) {
    result <- .compute_normalize_max(new_data[[col]])
    new_data[[col]] <- new_measure_list(result)
  }
  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_normalize_max <- function(
  x,
  width = max(20, options()$width - 30),
  ...
) {
  title <- "Max normalization on "
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
tidy.step_measure_normalize_max <- function(x, ...) {
  if (is_trained(x)) {
    terms <- x$measures
  } else {
    terms <- "<all measure columns>"
  }
  tibble::tibble(terms = terms, id = x$id)
}

.compute_normalize_max <- function(dat) {
  purrr::map(dat, .normalize_max_single)
}

.normalize_max_single <- function(x) {
  max_val <- max(x$value, na.rm = TRUE)
  if (is.na(max_val) || abs(max_val) < .Machine$double.eps) {
    cli::cli_warn(
      "Maximum is zero or NA for a spectrum; returning original values."
    )
    return(x)
  }
  x$value <- x$value / max_val
  x
}

# ==============================================================================
# step_measure_normalize_range
# ==============================================================================

#' Normalize to Range 0-1
#'
#' `step_measure_normalize_range()` creates a *specification* of a recipe step that
#' applies min-max normalization to scale each spectrum to the range 0 to 1.
#'
#' @inheritParams step_measure_snv
#'
#' @return An updated version of `recipe` with the new step added to the
#'   sequence of any existing operations.
#'
#' @details
#' For each spectrum \eqn{x}, the transformation is:
#'
#' \deqn{x_{norm} = \frac{x - \min(x)}{\max(x) - \min(x)}}{x_norm = (x - min(x)) / (max(x) - min(x))}
#'
#' After transformation, the minimum value of each spectrum will be 0 and the
#' maximum will be 1.
#'
#' If the range is zero (constant spectrum), a warning is issued and centered
#' values are returned (minimum subtracted but no scaling).
#'
#' **No selectors should be supplied to this step function**. The data should be
#' in the internal format produced by [step_measure_input_wide()] or
#' [step_measure_input_long()].
#'
#' @family measure-normalization
#' @seealso [step_measure_normalize_max()], [step_measure_snv()]
#' @export
#'
#' @examples
#' library(recipes)
#'
#' rec <-
#'   recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_normalize_range() |>
#'   prep()
#'
#' bake(rec, new_data = NULL)
step_measure_normalize_range <- function(
  recipe,
  measures = NULL,
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_normalize_range")
) {
  recipes::add_step(
    recipe,
    step_measure_normalize_range_new(
      measures = measures,
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_measure_normalize_range_new <- function(
  measures,
  role,
  trained,
  skip,
  id
) {
  recipes::step(
    subclass = "measure_normalize_range",
    measures = measures,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_normalize_range <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  step_measure_normalize_range_new(
    measures = measure_cols,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_normalize_range <- function(object, new_data, ...) {
  for (col in object$measures) {
    result <- .compute_normalize_range(new_data[[col]])
    new_data[[col]] <- new_measure_list(result)
  }
  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_normalize_range <- function(
  x,
  width = max(20, options()$width - 30),
  ...
) {
  title <- "Range [0,1] normalization on "
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
tidy.step_measure_normalize_range <- function(x, ...) {
  if (is_trained(x)) {
    terms <- x$measures
  } else {
    terms <- "<all measure columns>"
  }
  tibble::tibble(terms = terms, id = x$id)
}

.compute_normalize_range <- function(dat) {
  purrr::map(dat, .normalize_range_single)
}

.normalize_range_single <- function(x) {
  min_val <- min(x$value, na.rm = TRUE)
  max_val <- max(x$value, na.rm = TRUE)
  range_val <- max_val - min_val

  if (is.na(range_val) || abs(range_val) < .Machine$double.eps) {
    cli::cli_warn(
      "Range is zero or NA for a spectrum; returning centered values."
    )
    x$value <- x$value - min_val
    return(x)
  }
  x$value <- (x$value - min_val) / range_val
  x
}

# ==============================================================================
# step_measure_normalize_vector
# ==============================================================================

#' Normalize by L2 (Euclidean) Norm
#'
#' `step_measure_normalize_vector()` creates a *specification* of a recipe step that
#' divides each spectrum by its L2 (Euclidean) norm. After transformation, each
#' spectrum will have unit length in Euclidean space.
#'
#' @inheritParams step_measure_snv
#'
#' @return An updated version of `recipe` with the new step added to the
#'   sequence of any existing operations.
#'
#' @details
#' For each spectrum \eqn{x}, the transformation is:
#'
#' \deqn{x_{norm} = \frac{x}{\|x\|_2} = \frac{x}{\sqrt{\sum x^2}}}{x_norm = x / ||x||_2 = x / sqrt(sum(x^2))}
#'
#' After transformation, the L2 norm of each spectrum will equal 1.
#'
#' If the L2 norm is zero or NA, a warning is issued and the original values are
#' returned unchanged.
#'
#' **No selectors should be supplied to this step function**. The data should be
#' in the internal format produced by [step_measure_input_wide()] or
#' [step_measure_input_long()].
#'
#' @family measure-normalization
#' @seealso [step_measure_normalize_sum()], [step_measure_snv()]
#' @export
#'
#' @examples
#' library(recipes)
#'
#' rec <-
#'   recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_normalize_vector() |>
#'   prep()
#'
#' bake(rec, new_data = NULL)
step_measure_normalize_vector <- function(
  recipe,
  measures = NULL,
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_normalize_vector")
) {
  recipes::add_step(
    recipe,
    step_measure_normalize_vector_new(
      measures = measures,
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_measure_normalize_vector_new <- function(
  measures,
  role,
  trained,
  skip,
  id
) {
  recipes::step(
    subclass = "measure_normalize_vector",
    measures = measures,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_normalize_vector <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  step_measure_normalize_vector_new(
    measures = measure_cols,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_normalize_vector <- function(object, new_data, ...) {
  for (col in object$measures) {
    result <- .compute_normalize_vector(new_data[[col]])
    new_data[[col]] <- new_measure_list(result)
  }
  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_normalize_vector <- function(
  x,
  width = max(20, options()$width - 30),
  ...
) {
  title <- "L2 (vector) normalization on "
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
tidy.step_measure_normalize_vector <- function(x, ...) {
  if (is_trained(x)) {
    terms <- x$measures
  } else {
    terms <- "<all measure columns>"
  }
  tibble::tibble(terms = terms, id = x$id)
}

.compute_normalize_vector <- function(dat) {
  purrr::map(dat, .normalize_vector_single)
}

.normalize_vector_single <- function(x) {
  l2_norm <- sqrt(sum(x$value^2, na.rm = TRUE))
  if (is.na(l2_norm) || abs(l2_norm) < .Machine$double.eps) {
    cli::cli_warn(
      "L2 norm is zero or NA for a spectrum; returning original values."
    )
    return(x)
  }
  x$value <- x$value / l2_norm
  x
}

# ==============================================================================
# step_measure_normalize_auc
# ==============================================================================

#' Normalize by Area Under Curve
#'
#' `step_measure_normalize_auc()` creates a *specification* of a recipe step that
#' divides each spectrum by its area under the curve (computed using trapezoidal
#' integration). This is useful for chromatography where peak areas are meaningful.
#'
#' @inheritParams step_measure_snv
#'
#' @return An updated version of `recipe` with the new step added to the
#'   sequence of any existing operations.
#'
#' @details
#' The area under the curve is computed using trapezoidal integration:
#'
#' \deqn{AUC = \sum_{i=1}^{n-1} \frac{(y_i + y_{i+1})}{2} \cdot (x_{i+1} - x_i)}{AUC = sum((y[i] + y[i+1]) / 2 * (x[i+1] - x[i]))}
#'
#' where \eqn{y} are the values and \eqn{x} are the locations.
#'
#' After transformation, the AUC of each spectrum will equal 1.
#'
#' If the AUC is zero or NA, a warning is issued and the original values are
#' returned unchanged. At least 2 points are required for integration.
#'
#' **No selectors should be supplied to this step function**. The data should be
#' in the internal format produced by [step_measure_input_wide()] or
#' [step_measure_input_long()].
#'
#' @family measure-normalization
#' @seealso [step_measure_normalize_sum()], [step_measure_normalize_peak()]
#' @export
#'
#' @examples
#' library(recipes)
#'
#' rec <-
#'   recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_normalize_auc() |>
#'   prep()
#'
#' bake(rec, new_data = NULL)
step_measure_normalize_auc <- function(
  recipe,
  measures = NULL,
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_normalize_auc")
) {
  recipes::add_step(
    recipe,
    step_measure_normalize_auc_new(
      measures = measures,
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_measure_normalize_auc_new <- function(measures, role, trained, skip, id) {
  recipes::step(
    subclass = "measure_normalize_auc",
    measures = measures,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_normalize_auc <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  step_measure_normalize_auc_new(
    measures = measure_cols,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_normalize_auc <- function(object, new_data, ...) {
  for (col in object$measures) {
    result <- .compute_normalize_auc(new_data[[col]])
    new_data[[col]] <- new_measure_list(result)
  }
  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_normalize_auc <- function(
  x,
  width = max(20, options()$width - 30),
  ...
) {
  title <- "AUC normalization on "
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
tidy.step_measure_normalize_auc <- function(x, ...) {
  if (is_trained(x)) {
    terms <- x$measures
  } else {
    terms <- "<all measure columns>"
  }
  tibble::tibble(terms = terms, id = x$id)
}

.compute_normalize_auc <- function(dat) {
  purrr::map(dat, .normalize_auc_single)
}

.normalize_auc_single <- function(x) {
  n <- nrow(x)
  if (n < 2) {
    cli::cli_warn("Need at least 2 points for AUC; returning original values.")
    return(x)
  }

  # Trapezoidal integration
  dx <- diff(x$location)
  y_avg <- (x$value[-n] + x$value[-1]) / 2
  auc <- sum(dx * y_avg, na.rm = TRUE)

  if (is.na(auc) || abs(auc) < .Machine$double.eps) {
    cli::cli_warn(
      "AUC is zero or NA for a spectrum; returning original values."
    )
    return(x)
  }
  x$value <- x$value / auc
  x
}

# ==============================================================================
# step_measure_normalize_peak
# ==============================================================================

#' Normalize to a Specific Peak Region
#'
#' `step_measure_normalize_peak()` creates a *specification* of a recipe step that
#' divides each spectrum by a summary statistic computed from a specified region.
#' This is commonly used for internal standard normalization.
#'
#' @inheritParams step_measure_snv
#' @param location_min Numeric. The lower bound of the region to use for
#'   normalization. This parameter is tunable with [peak_location_min()].
#' @param location_max Numeric. The upper bound of the region to use for
#'   normalization. This parameter is tunable with [peak_location_max()].
#' @param method Character. The summary statistic to compute from the region.
#'   One of `"mean"` (default), `"max"`, or `"integral"`.
#'
#' @return An updated version of `recipe` with the new step added to the
#'   sequence of any existing operations.
#'
#' @details
#' For each spectrum, this step:
#' 1. Selects values in the region `[location_min, location_max]`
#' 2. Computes a summary statistic (mean, max, or integral) from that region
#' 3. Divides the entire spectrum by this value
#'
#' This is useful when you have an internal standard peak at a known location
#' and want to normalize all spectra to that peak.
#'
#' The `location_min` and `location_max` parameters are tunable with
#' [peak_location_min()] and [peak_location_max()] for hyperparameter
#' optimization.
#'
#' If no values fall within the specified region, an error is raised. If the
#' computed normalizer is zero or NA, a warning is issued and the original
#' values are returned unchanged.
#'
#' **No selectors should be supplied to this step function**. The data should be
#' in the internal format produced by [step_measure_input_wide()] or
#' [step_measure_input_long()].
#'
#' @family measure-normalization
#' @seealso [step_measure_normalize_max()], [step_measure_normalize_auc()],
#'   [peak_location_min()], [peak_location_max()]
#' @export
#'
#' @examples
#' library(recipes)
#'
#' # Normalize to mean of region 40-60
#' rec <-
#'   recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_normalize_peak(location_min = 40, location_max = 60) |>
#'   prep()
#'
#' bake(rec, new_data = NULL)
step_measure_normalize_peak <- function(
  recipe,
  measures = NULL,
  role = NA,
  trained = FALSE,
  location_min = NULL,
  location_max = NULL,
  method = "mean",
  skip = FALSE,
  id = recipes::rand_id("measure_normalize_peak")
) {
  method <- rlang::arg_match(method, c("mean", "max", "integral"))

  recipes::add_step(
    recipe,
    step_measure_normalize_peak_new(
      measures = measures,
      role = role,
      trained = trained,
      location_min = location_min,
      location_max = location_max,
      method = method,
      skip = skip,
      id = id
    )
  )
}

step_measure_normalize_peak_new <- function(
  measures,
  role,
  trained,
  location_min,
  location_max,
  method,
  skip,
  id
) {
  recipes::step(
    subclass = "measure_normalize_peak",
    measures = measures,
    role = role,
    trained = trained,
    location_min = location_min,
    location_max = location_max,
    method = method,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_normalize_peak <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  # Validate that location bounds are provided
  if (is.null(x$location_min) || is.null(x$location_max)) {
    cli::cli_abort(
      c(
        "Both {.arg location_min} and {.arg location_max} must be specified.",
        "i" = "These define the region to use for normalization."
      )
    )
  }

  if (x$location_min >= x$location_max) {
    cli::cli_abort(
      "{.arg location_min} ({x$location_min}) must be less than {.arg location_max} ({x$location_max})."
    )
  }

  # Validate that region contains at least one point
  for (col in measure_cols) {
    locs <- training[[col]][[1]]$location
    if (!any(locs >= x$location_min & locs <= x$location_max)) {
      cli::cli_abort(
        c(
          "Peak region [{x$location_min}, {x$location_max}] contains no measurement locations.",
          "i" = "Location range in data: [{min(locs)}, {max(locs)}]."
        )
      )
    }
  }

  step_measure_normalize_peak_new(
    measures = measure_cols,
    role = x$role,
    trained = TRUE,
    location_min = x$location_min,
    location_max = x$location_max,
    method = x$method,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_normalize_peak <- function(object, new_data, ...) {
  for (col in object$measures) {
    result <- .compute_normalize_peak(
      new_data[[col]],
      location_min = object$location_min,
      location_max = object$location_max,
      method = object$method
    )
    new_data[[col]] <- new_measure_list(result)
  }
  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_normalize_peak <- function(
  x,
  width = max(20, options()$width - 30),
  ...
) {
  title <- "Peak region normalization on "
  if (x$trained) {
    cat(title, "<internal measurements>", sep = "")
    cat(sprintf(
      " (region: [%s, %s], method: %s)",
      x$location_min,
      x$location_max,
      x$method
    ))
  } else {
    cat(title)
  }
  cat("\n")
  invisible(x)
}

#' @rdname tidy.recipe
#' @export
#' @keywords internal
tidy.step_measure_normalize_peak <- function(x, ...) {
  if (is_trained(x)) {
    tibble::tibble(
      terms = x$measures,
      location_min = x$location_min,
      location_max = x$location_max,
      method = x$method,
      id = x$id
    )
  } else {
    tibble::tibble(
      terms = "<all measure columns>",
      location_min = x$location_min,
      location_max = x$location_max,
      method = x$method,
      id = x$id
    )
  }
}

.compute_normalize_peak <- function(dat, location_min, location_max, method) {
  purrr::map(
    dat,
    .normalize_peak_single,
    location_min = location_min,
    location_max = location_max,
    method = method
  )
}

.normalize_peak_single <- function(x, location_min, location_max, method) {
  # Select values in the region
  in_region <- x$location >= location_min & x$location <= location_max
  region_locs <- x$location[in_region]
  region_values <- x$value[in_region]

  if (length(region_values) == 0) {
    cli::cli_warn(
      "No values in region [{location_min}, {location_max}]; returning original values."
    )
    return(x)
  }

  normalizer <- switch(
    method,
    "mean" = mean(region_values, na.rm = TRUE),
    "max" = max(region_values, na.rm = TRUE),
    "integral" = {
      n <- length(region_values)
      if (n < 2) {
        mean(region_values, na.rm = TRUE)
      } else {
        dx <- diff(region_locs)
        y_avg <- (region_values[-n] + region_values[-1]) / 2
        sum(dx * y_avg, na.rm = TRUE)
      }
    }
  )

  if (is.na(normalizer) || abs(normalizer) < .Machine$double.eps) {
    cli::cli_warn("Normalizer is zero or NA; returning original values.")
    return(x)
  }
  x$value <- x$value / normalizer
  x
}

#' @export
required_pkgs.step_measure_normalize_sum <- function(x, ...) {
  c("measure", "purrr")
}

#' @export
required_pkgs.step_measure_normalize_max <- function(x, ...) {
  c("measure", "purrr")
}

#' @export
required_pkgs.step_measure_normalize_range <- function(x, ...) {
  c("measure", "purrr")
}

#' @export
required_pkgs.step_measure_normalize_vector <- function(x, ...) {
  c("measure", "purrr")
}

#' @export
required_pkgs.step_measure_normalize_auc <- function(x, ...) {
  c("measure", "purrr")
}

#' @export
required_pkgs.step_measure_normalize_peak <- function(x, ...) {
  c("measure", "purrr")
}
