# Variable-wise scaling steps for measure package
# These steps learn statistics from training data and apply them during baking.
# Operations are column-wise (across samples at each measurement location).

# ==============================================================================
# step_measure_center
# ==============================================================================

#' Mean Centering
#'
#' `step_measure_center()` creates a *specification* of a recipe step that
#' subtracts the mean at each measurement location (column-wise centering).
#' The means are computed from the training data and applied to new data.
#'
#' @inheritParams step_measure_msc
#' @param learned_params A named list containing learned means and locations
#'   for each measure column. This is `NULL` until the step is trained.
#'
#' @return An updated version of `recipe` with the new step added to the
#'   sequence of any existing operations.
#'
#' @details
#' Mean centering is a fundamental preprocessing step for multivariate analysis
#' methods like PCA and PLS. It removes the average signal at each measurement
#' location.
#'
#' For a data matrix \eqn{X} with samples as rows and measurement locations as
#' columns, the transformation is:
#'
#' \deqn{X_{centered} = X - \bar{X}}{X_centered = X - mean(X)}
#'
#' where \eqn{\bar{X}}{mean(X)} is the column-wise mean computed from the
#' training data.
#'
#' The means are learned during `prep()` from the training data and stored for
#' use when applying the transformation to new data during `bake()`.
#'
#' **No selectors should be supplied to this step function**. The data should be
#' in the internal format produced by [step_measure_input_wide()] or
#' [step_measure_input_long()].
#'
#' # Tidying
#'
#' When you [`tidy()`][recipes::tidy.recipe()] this step after training, a tibble
#' with the learned means at each location is returned.
#'
#' @family measure-scaling
#' @seealso [step_measure_scale_auto()], [step_measure_scale_pareto()]
#' @export
#'
#' @examples
#' library(recipes)
#'
#' rec <-
#'   recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_center() |>
#'   prep()
#'
#' bake(rec, new_data = NULL)
step_measure_center <- function(
    recipe,
    measures = NULL,
    role = NA,
    trained = FALSE,
    learned_params = NULL,
    skip = FALSE,
    id = recipes::rand_id("measure_center")
) {
  recipes::add_step(
    recipe,
    step_measure_center_new(
      measures = measures,
      role = role,
      trained = trained,
      learned_params = learned_params,
      skip = skip,
      id = id
    )
  )
}

step_measure_center_new <- function(
    measures, role, trained, learned_params, skip, id
) {
  recipes::step(
    subclass = "measure_center",
    measures = measures,
    role = role,
    trained = trained,
    learned_params = learned_params,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_center <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  learned_params <- list()
  for (col in measure_cols) {
    mat <- measure_to_matrix(training[[col]])
    learned_params[[col]] <- list(
      means = colMeans(mat, na.rm = TRUE),
      locations = training[[col]][[1]]$location
    )
  }

  step_measure_center_new(
    measures = measure_cols,
    role = x$role,
    trained = TRUE,
    learned_params = learned_params,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_center <- function(object, new_data, ...) {
  for (col in object$measures) {
    params <- object$learned_params[[col]]
    mat <- measure_to_matrix(new_data[[col]])

    mat <- sweep(mat, 2, params$means, "-")

    new_data[[col]] <- new_measure_list(matrix_to_measure(mat, params$locations))
  }
  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_center <- function(x, width = max(20, options()$width - 30), ...) {
  title <- "Mean centering on "
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
tidy.step_measure_center <- function(x, ...) {
  if (is_trained(x)) {
    res <- purrr::map_dfr(names(x$learned_params), function(col) {
      params <- x$learned_params[[col]]
      tibble::tibble(
        terms = col,
        location = params$locations,
        mean = params$means
      )
    })
    res$id <- x$id
    res
  } else {
    tibble::tibble(
      terms = "<all measure columns>",
      location = NA_real_,
      mean = NA_real_,
      id = x$id
    )
  }
}

#' @export
required_pkgs.step_measure_center <- function(x, ...) {
  c("measure")
}

# ==============================================================================
# step_measure_scale_auto
# ==============================================================================

#' Auto-Scaling (Z-Score Normalization)
#'
#' `step_measure_scale_auto()` creates a *specification* of a recipe step that
#' applies auto-scaling (also known as z-score normalization or standardization)
#' at each measurement location. This centers and scales to unit variance.
#'
#' @inheritParams step_measure_center
#'
#' @return An updated version of `recipe` with the new step added to the
#'   sequence of any existing operations.
#'
#' @details
#' Auto-scaling (standardization) transforms each variable to have zero mean
#' and unit variance. This gives equal importance to all measurement locations
#' regardless of their original scale.
#'
#' For a data matrix \eqn{X}, the transformation is:
#'
#' \deqn{X_{scaled} = \frac{X - \bar{X}}{s_X}}{X_scaled = (X - mean(X)) / sd(X)}
#'
#' where \eqn{\bar{X}}{mean(X)} and \eqn{s_X}{sd(X)} are the column-wise mean
#' and standard deviation computed from the training data.
#'
#' If a column has zero standard deviation (constant values), that column is
#' only centered, not scaled (the divisor is set to 1).
#'
#' The means and standard deviations are learned during `prep()` from the
#' training data and stored for use when applying the transformation to new
#' data during `bake()`.
#'
#' **No selectors should be supplied to this step function**. The data should be
#' in the internal format produced by [step_measure_input_wide()] or
#' [step_measure_input_long()].
#'
#' @family measure-scaling
#' @seealso [step_measure_center()], [step_measure_scale_pareto()]
#' @export
#'
#' @examples
#' library(recipes)
#'
#' rec <-
#'   recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_scale_auto() |>
#'   prep()
#'
#' bake(rec, new_data = NULL)
step_measure_scale_auto <- function(
    recipe,
    measures = NULL,
    role = NA,
    trained = FALSE,
    learned_params = NULL,
    skip = FALSE,
    id = recipes::rand_id("measure_scale_auto")
) {
  recipes::add_step(
    recipe,
    step_measure_scale_auto_new(
      measures = measures,
      role = role,
      trained = trained,
      learned_params = learned_params,
      skip = skip,
      id = id
    )
  )
}

step_measure_scale_auto_new <- function(
    measures, role, trained, learned_params, skip, id
) {
  recipes::step(
    subclass = "measure_scale_auto",
    measures = measures,
    role = role,
    trained = trained,
    learned_params = learned_params,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_scale_auto <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  learned_params <- list()
  for (col in measure_cols) {
    mat <- measure_to_matrix(training[[col]])
    learned_params[[col]] <- list(
      means = colMeans(mat, na.rm = TRUE),
      sds = apply(mat, 2, stats::sd, na.rm = TRUE),
      locations = training[[col]][[1]]$location
    )
  }

  step_measure_scale_auto_new(
    measures = measure_cols,
    role = x$role,
    trained = TRUE,
    learned_params = learned_params,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_scale_auto <- function(object, new_data, ...) {
  for (col in object$measures) {
    params <- object$learned_params[[col]]
    mat <- measure_to_matrix(new_data[[col]])

    # Center
    mat <- sweep(mat, 2, params$means, "-")

    # Scale (handle zero SDs)
    safe_sds <- params$sds
    safe_sds[is.na(safe_sds) | safe_sds < .Machine$double.eps] <- 1
    mat <- sweep(mat, 2, safe_sds, "/")

    new_data[[col]] <- new_measure_list(matrix_to_measure(mat, params$locations))
  }
  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_scale_auto <- function(x, width = max(20, options()$width - 30), ...) {
  title <- "Auto-scaling (z-score) on "
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
tidy.step_measure_scale_auto <- function(x, ...) {
  if (is_trained(x)) {
    res <- purrr::map_dfr(names(x$learned_params), function(col) {
      params <- x$learned_params[[col]]
      tibble::tibble(
        terms = col,
        location = params$locations,
        mean = params$means,
        sd = params$sds
      )
    })
    res$id <- x$id
    res
  } else {
    tibble::tibble(
      terms = "<all measure columns>",
      location = NA_real_,
      mean = NA_real_,
      sd = NA_real_,
      id = x$id
    )
  }
}

#' @export
required_pkgs.step_measure_scale_auto <- function(x, ...) {
  c("measure")
}

# ==============================================================================
# step_measure_scale_pareto
# ==============================================================================

#' Pareto Scaling
#'
#' `step_measure_scale_pareto()` creates a *specification* of a recipe step that
#' applies Pareto scaling at each measurement location. This is a compromise
#' between no scaling and auto-scaling, commonly used in metabolomics.
#'
#' @inheritParams step_measure_center
#'
#' @return An updated version of `recipe` with the new step added to the
#'   sequence of any existing operations.
#'
#' @details
#' Pareto scaling divides by the square root of the standard deviation rather
#' than the standard deviation itself. This reduces the relative importance of
#' large values while still giving more weight to larger fold changes.
#'
#' For a data matrix \eqn{X}, the transformation is:
#'
#' \deqn{X_{scaled} = \frac{X - \bar{X}}{\sqrt{s_X}}}{X_scaled = (X - mean(X)) / sqrt(sd(X))}
#'
#' where \eqn{\bar{X}}{mean(X)} and \eqn{s_X}{sd(X)} are the column-wise mean
#' and standard deviation computed from the training data.
#'
#' If a column has zero standard deviation (constant values), that column is
#' only centered, not scaled.
#'
#' The means and standard deviations are learned during `prep()` from the
#' training data and stored for use when applying the transformation to new
#' data during `bake()`.
#'
#' **No selectors should be supplied to this step function**. The data should be
#' in the internal format produced by [step_measure_input_wide()] or
#' [step_measure_input_long()].
#'
#' @references
#' van den Berg, R.A., Hoefsloot, H.C., Westerhuis, J.A., Smilde, A.K., and
#' van der Werf, M.J. 2006. Centering, scaling, and transformations: improving
#' the biological information content of metabolomics data. BMC Genomics, 7:142.
#'
#' @family measure-scaling
#' @seealso [step_measure_scale_auto()], [step_measure_center()]
#' @export
#'
#' @examples
#' library(recipes)
#'
#' rec <-
#'   recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_scale_pareto() |>
#'   prep()
#'
#' bake(rec, new_data = NULL)
step_measure_scale_pareto <- function(
    recipe,
    measures = NULL,
    role = NA,
    trained = FALSE,
    learned_params = NULL,
    skip = FALSE,
    id = recipes::rand_id("measure_scale_pareto")
) {
  recipes::add_step(
    recipe,
    step_measure_scale_pareto_new(
      measures = measures,
      role = role,
      trained = trained,
      learned_params = learned_params,
      skip = skip,
      id = id
    )
  )
}

step_measure_scale_pareto_new <- function(
    measures, role, trained, learned_params, skip, id
) {
  recipes::step(
    subclass = "measure_scale_pareto",
    measures = measures,
    role = role,
    trained = trained,
    learned_params = learned_params,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_scale_pareto <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  learned_params <- list()
  for (col in measure_cols) {
    mat <- measure_to_matrix(training[[col]])
    learned_params[[col]] <- list(
      means = colMeans(mat, na.rm = TRUE),
      sds = apply(mat, 2, stats::sd, na.rm = TRUE),
      locations = training[[col]][[1]]$location
    )
  }

  step_measure_scale_pareto_new(
    measures = measure_cols,
    role = x$role,
    trained = TRUE,
    learned_params = learned_params,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_scale_pareto <- function(object, new_data, ...) {
  for (col in object$measures) {
    params <- object$learned_params[[col]]
    mat <- measure_to_matrix(new_data[[col]])

    # Center
    mat <- sweep(mat, 2, params$means, "-")

    # Scale by sqrt(SD)
    safe_sds <- params$sds
    safe_sds[is.na(safe_sds) | safe_sds < .Machine$double.eps] <- 1
    sqrt_sds <- sqrt(safe_sds)
    mat <- sweep(mat, 2, sqrt_sds, "/")

    new_data[[col]] <- new_measure_list(matrix_to_measure(mat, params$locations))
  }
  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_scale_pareto <- function(x, width = max(20, options()$width - 30), ...) {
  title <- "Pareto scaling on "
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
tidy.step_measure_scale_pareto <- function(x, ...) {
  if (is_trained(x)) {
    res <- purrr::map_dfr(names(x$learned_params), function(col) {
      params <- x$learned_params[[col]]
      tibble::tibble(
        terms = col,
        location = params$locations,
        mean = params$means,
        sd = params$sds
      )
    })
    res$id <- x$id
    res
  } else {
    tibble::tibble(
      terms = "<all measure columns>",
      location = NA_real_,
      mean = NA_real_,
      sd = NA_real_,
      id = x$id
    )
  }
}

#' @export
required_pkgs.step_measure_scale_pareto <- function(x, ...) {
  c("measure")
}

# ==============================================================================
# step_measure_scale_range
# ==============================================================================

#' Range Scaling
#'
#' `step_measure_scale_range()` creates a *specification* of a recipe step that
#' applies range scaling at each measurement location. This centers and divides
#' by the range (max - min) of each variable.
#'
#' @inheritParams step_measure_center
#'
#' @return An updated version of `recipe` with the new step added to the
#'   sequence of any existing operations.
#'
#' @details
#' Range scaling centers the data and divides by the range, giving bounded
#' values suitable for methods sensitive to variable scale.
#'
#' For a data matrix \eqn{X}, the transformation is:
#'
#' \deqn{X_{scaled} = \frac{X - \bar{X}}{\max(X) - \min(X)}}{X_scaled = (X - mean(X)) / (max(X) - min(X))}
#'
#' where \eqn{\bar{X}}{mean(X)} is the column-wise mean and the range is
#' computed from the training data.
#'
#' If a column has zero range (constant values), that column is only centered,
#' not scaled.
#'
#' The means and ranges are learned during `prep()` from the training data and
#' stored for use when applying the transformation to new data during `bake()`.
#'
#' **No selectors should be supplied to this step function**. The data should be
#' in the internal format produced by [step_measure_input_wide()] or
#' [step_measure_input_long()].
#'
#' @family measure-scaling
#' @seealso [step_measure_scale_auto()], [step_measure_center()]
#' @export
#'
#' @examples
#' library(recipes)
#'
#' rec <-
#'   recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_scale_range() |>
#'   prep()
#'
#' bake(rec, new_data = NULL)
step_measure_scale_range <- function(
    recipe,
    measures = NULL,
    role = NA,
    trained = FALSE,
    learned_params = NULL,
    skip = FALSE,
    id = recipes::rand_id("measure_scale_range")
) {
  recipes::add_step(
    recipe,
    step_measure_scale_range_new(
      measures = measures,
      role = role,
      trained = trained,
      learned_params = learned_params,
      skip = skip,
      id = id
    )
  )
}

step_measure_scale_range_new <- function(
    measures, role, trained, learned_params, skip, id
) {
  recipes::step(
    subclass = "measure_scale_range",
    measures = measures,
    role = role,
    trained = trained,
    learned_params = learned_params,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_scale_range <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  learned_params <- list()
  for (col in measure_cols) {
    mat <- measure_to_matrix(training[[col]])
    learned_params[[col]] <- list(
      means = colMeans(mat, na.rm = TRUE),
      ranges = apply(mat, 2, function(x) diff(range(x, na.rm = TRUE))),
      locations = training[[col]][[1]]$location
    )
  }

  step_measure_scale_range_new(
    measures = measure_cols,
    role = x$role,
    trained = TRUE,
    learned_params = learned_params,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_scale_range <- function(object, new_data, ...) {
  for (col in object$measures) {
    params <- object$learned_params[[col]]
    mat <- measure_to_matrix(new_data[[col]])

    # Center
    mat <- sweep(mat, 2, params$means, "-")

    # Scale by range
    safe_ranges <- params$ranges
    safe_ranges[is.na(safe_ranges) | safe_ranges < .Machine$double.eps] <- 1
    mat <- sweep(mat, 2, safe_ranges, "/")

    new_data[[col]] <- new_measure_list(matrix_to_measure(mat, params$locations))
  }
  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_scale_range <- function(x, width = max(20, options()$width - 30), ...) {
  title <- "Range scaling on "
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
tidy.step_measure_scale_range <- function(x, ...) {
  if (is_trained(x)) {
    res <- purrr::map_dfr(names(x$learned_params), function(col) {
      params <- x$learned_params[[col]]
      tibble::tibble(
        terms = col,
        location = params$locations,
        mean = params$means,
        range = params$ranges
      )
    })
    res$id <- x$id
    res
  } else {
    tibble::tibble(
      terms = "<all measure columns>",
      location = NA_real_,
      mean = NA_real_,
      range = NA_real_,
      id = x$id
    )
  }
}

#' @export
required_pkgs.step_measure_scale_range <- function(x, ...) {
  c("measure")
}

# ==============================================================================
# step_measure_scale_vast
# ==============================================================================

#' VAST Scaling (Variable Stability Scaling)
#'
#' `step_measure_scale_vast()` creates a *specification* of a recipe step that
#' applies VAST (Variable Stability) scaling at each measurement location.
#' This focuses on variables with high stability (low coefficient of variation).
#'
#' @inheritParams step_measure_center
#'
#' @return An updated version of `recipe` with the new step added to the
#'   sequence of any existing operations.
#'
#' @details
#' VAST scaling divides by the product of the standard deviation and the
#' coefficient of variation (CV = SD/mean). This gives more weight to variables
#' that are stable across samples (low CV).
#'
#' For a data matrix \eqn{X}, the transformation is:
#'
#' \deqn{X_{scaled} = \frac{X - \bar{X}}{s_X \cdot CV_X}}{X_scaled = (X - mean(X)) / (sd(X) * CV(X))}
#'
#' where \eqn{\bar{X}}{mean(X)}, \eqn{s_X}{sd(X)}, and \eqn{CV_X = s_X / |\bar{X}|}{CV(X) = sd(X) / |mean(X)|}
#' are computed from the training data.
#'
#' If a column has zero divisor (constant values or zero mean), that column is
#' only centered, not scaled.
#'
#' The means, standard deviations, and CVs are learned during `prep()` from the
#' training data and stored for use when applying the transformation to new
#' data during `bake()`.
#'
#' **No selectors should be supplied to this step function**. The data should be
#' in the internal format produced by [step_measure_input_wide()] or
#' [step_measure_input_long()].
#'
#' @references
#' van den Berg, R.A., Hoefsloot, H.C., Westerhuis, J.A., Smilde, A.K., and
#' van der Werf, M.J. 2006. Centering, scaling, and transformations: improving
#' the biological information content of metabolomics data. BMC Genomics, 7:142.
#'
#' @family measure-scaling
#' @seealso [step_measure_scale_auto()], [step_measure_scale_pareto()]
#' @export
#'
#' @examples
#' library(recipes)
#'
#' rec <-
#'   recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_scale_vast() |>
#'   prep()
#'
#' bake(rec, new_data = NULL)
step_measure_scale_vast <- function(
    recipe,
    measures = NULL,
    role = NA,
    trained = FALSE,
    learned_params = NULL,
    skip = FALSE,
    id = recipes::rand_id("measure_scale_vast")
) {
  recipes::add_step(
    recipe,
    step_measure_scale_vast_new(
      measures = measures,
      role = role,
      trained = trained,
      learned_params = learned_params,
      skip = skip,
      id = id
    )
  )
}

step_measure_scale_vast_new <- function(
    measures, role, trained, learned_params, skip, id
) {
  recipes::step(
    subclass = "measure_scale_vast",
    measures = measures,
    role = role,
    trained = trained,
    learned_params = learned_params,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_scale_vast <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  learned_params <- list()
  for (col in measure_cols) {
    mat <- measure_to_matrix(training[[col]])
    means <- colMeans(mat, na.rm = TRUE)
    sds <- apply(mat, 2, stats::sd, na.rm = TRUE)

    # CV = SD / |mean| (use absolute value for negative means)
    safe_means <- abs(means)
    safe_means[is.na(safe_means) | safe_means < .Machine$double.eps] <- 1
    cvs <- sds / safe_means

    learned_params[[col]] <- list(
      means = means,
      sds = sds,
      cvs = cvs,
      locations = training[[col]][[1]]$location
    )
  }

  step_measure_scale_vast_new(
    measures = measure_cols,
    role = x$role,
    trained = TRUE,
    learned_params = learned_params,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_scale_vast <- function(object, new_data, ...) {
  for (col in object$measures) {
    params <- object$learned_params[[col]]
    mat <- measure_to_matrix(new_data[[col]])

    # Center
    mat <- sweep(mat, 2, params$means, "-")

    # Scale by SD * CV
    divisor <- params$sds * params$cvs
    safe_divisor <- divisor
    safe_divisor[is.na(safe_divisor) | safe_divisor < .Machine$double.eps] <- 1
    mat <- sweep(mat, 2, safe_divisor, "/")

    new_data[[col]] <- new_measure_list(matrix_to_measure(mat, params$locations))
  }
  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_scale_vast <- function(x, width = max(20, options()$width - 30), ...) {
  title <- "VAST scaling on "
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
tidy.step_measure_scale_vast <- function(x, ...) {
  if (is_trained(x)) {
    res <- purrr::map_dfr(names(x$learned_params), function(col) {
      params <- x$learned_params[[col]]
      tibble::tibble(
        terms = col,
        location = params$locations,
        mean = params$means,
        sd = params$sds,
        cv = params$cvs
      )
    })
    res$id <- x$id
    res
  } else {
    tibble::tibble(
      terms = "<all measure columns>",
      location = NA_real_,
      mean = NA_real_,
      sd = NA_real_,
      cv = NA_real_,
      id = x$id
    )
  }
}

#' @export
required_pkgs.step_measure_scale_vast <- function(x, ...) {
  c("measure")
}
