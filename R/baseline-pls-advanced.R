# ==============================================================================
# Advanced Penalized Least Squares Baseline Correction Methods
#
# This file contains advanced PLS-based baseline correction algorithms:
# - step_measure_baseline_aspls: Adaptive Smoothness PLS
# - step_measure_baseline_iarpls: Improved arPLS (two-stage)
# - step_measure_baseline_fastchrom: Fast chromatography baseline
# ==============================================================================

# ==============================================================================
# step_measure_baseline_aspls
# ==============================================================================

#' Adaptive Smoothness Penalized Least Squares Baseline
#'
#' `step_measure_baseline_aspls()` creates a *specification* of a recipe step
#' that applies Adaptive Smoothness Penalized Least Squares baseline correction.
#'
#' @param recipe A recipe object.
#' @param measures An optional character vector of measure column names.
#' @param lambda Base smoothness parameter. Default is 1e6.
#' @param alpha Adaptive weight parameter controlling smoothness adaptation
#'   (0 = no adaptation, 1 = maximum adaptation). Higher values cause regions
#'   with larger residuals to receive higher smoothness penalties. Note that
#'   adaptation is applied globally via an averaged lambda. Default is 0.5.
#'   Tunable via [baseline_alpha()].
#' @param max_iter Maximum number of iterations. Default is 50.
#' @param tol Convergence tolerance. Default is 1e-5.
#' @param role Not used.
#' @param trained Logical indicating if the step has been trained.
#' @param skip Logical. Should the step be skipped when baking?
#' @param id Unique step identifier.
#'
#' @return An updated recipe with the new step added.
#'
#' @details
#' aspls adapts the smoothness parameter based on the signal properties.
#' The algorithm computes a local smoothness weight based on residual magnitude,
#' then uses the global average as the effective lambda. This provides some
#' adaptation to peak intensity while maintaining computational efficiency.
#'
#' This method is particularly effective for:
#' - Signals with varying peak widths
#' - Data with both sharp peaks and gradual baseline changes
#' - Chromatography with complex baselines
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
#'   step_measure_baseline_aspls(lambda = 1e6, alpha = 0.5) |>
#'   prep()
#'
#' bake(rec, new_data = NULL)
#' }
step_measure_baseline_aspls <- function(
  recipe,
  measures = NULL,
  lambda = 1e6,
  alpha = 0.5,
  max_iter = 50L,
  tol = 1e-5,
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_baseline_aspls")
) {
  recipes::add_step(
    recipe,
    step_measure_baseline_aspls_new(
      measures = measures,
      lambda = lambda,
      alpha = alpha,
      max_iter = as.integer(max_iter),
      tol = tol,
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_measure_baseline_aspls_new <- function(
  measures,
  lambda,
  alpha,
  max_iter,
  tol,
  role,
  trained,
  skip,
  id
) {
  recipes::step(
    subclass = "measure_baseline_aspls",
    measures = measures,
    lambda = lambda,
    alpha = alpha,
    max_iter = max_iter,
    tol = tol,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_baseline_aspls <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  # Validate parameters
  if (!is.numeric(x$lambda) || length(x$lambda) != 1 || x$lambda <= 0) {
    cli::cli_abort(
      "{.arg lambda} must be a single positive number, not {.val {x$lambda}}."
    )
  }
  if (
    !is.numeric(x$alpha) || length(x$alpha) != 1 || x$alpha < 0 || x$alpha > 1
  ) {
    cli::cli_abort(
      "{.arg alpha} must be a single number between 0 and 1, not {.val {x$alpha}}."
    )
  }
  if (!is.numeric(x$max_iter) || length(x$max_iter) != 1 || x$max_iter < 1) {
    cli::cli_abort(
      "{.arg max_iter} must be a positive integer, not {.val {x$max_iter}}."
    )
  }
  if (!is.numeric(x$tol) || length(x$tol) != 1 || x$tol <= 0) {
    cli::cli_abort(
      "{.arg tol} must be a single positive number, not {.val {x$tol}}."
    )
  }

  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  step_measure_baseline_aspls_new(
    measures = measure_cols,
    lambda = x$lambda,
    alpha = x$alpha,
    max_iter = x$max_iter,
    tol = x$tol,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' Create difference matrix for smoothing
#' @noRd
.create_diff_matrix <- function(n, d = 2) {
  if (n <= d) {
    return(matrix(0, nrow = 1, ncol = n))
  }

  if (d == 2) {
    D <- matrix(0, n - 2, n)
    for (i in seq_len(n - 2)) {
      D[i, i] <- 1
      D[i, i + 1] <- -2
      D[i, i + 2] <- 1
    }
  } else {
    D <- diag(n)
    for (i in seq_len(d)) {
      D <- diff(D)
      if (!is.matrix(D)) {
        D <- matrix(D, nrow = 1)
      }
    }
  }

  D
}

#' aspls baseline algorithm
#' @noRd
.aspls_baseline <- function(y, lambda, alpha, max_iter, tol) {
  n <- length(y)

  # Validate input
  if (n < 3) {
    cli::cli_warn("Input vector has fewer than 3 points, returning original.")
    return(y)
  }
  if (anyNA(y)) {
    cli::cli_warn(
      "Input contains {sum(is.na(y))} NA value{?s}. NA values will propagate."
    )
  }
  if (any(!is.finite(y) & !is.na(y))) {
    cli::cli_abort("Input contains Inf/-Inf values. Cannot compute baseline.")
  }

  # Create difference matrix
  D <- .create_diff_matrix(n, d = 2)
  DTD <- crossprod(D)

  # Initialize
  baseline <- y
  w <- rep(1, n)
  converged <- FALSE

  for (iter in seq_len(max_iter)) {
    baseline_old <- baseline

    # Calculate residuals
    residuals <- y - baseline

    # Adaptive smoothness: adjust lambda based on local signal properties
    max_res <- max(abs(residuals), na.rm = TRUE)
    if (max_res > 0) {
      local_lambda <- lambda * (1 + alpha * abs(residuals) / max_res)
    } else {
      local_lambda <- rep(lambda, n)
    }

    # Update weights based on residuals
    mad_val <- stats::mad(residuals, na.rm = TRUE)
    if (mad_val == 0) {
      mad_val <- stats::sd(residuals, na.rm = TRUE)
    }
    if (mad_val == 0) {
      mad_val <- 1
    }

    w <- ifelse(
      residuals > 0,
      exp(-abs(residuals) / mad_val),
      1
    )

    # Weighted least squares solution with adaptive smoothness
    # Use average local_lambda for global smoothing matrix
    avg_lambda <- mean(local_lambda)
    W <- diag(w)
    system_matrix <- W + avg_lambda * DTD
    diag(system_matrix) <- diag(system_matrix) + 1e-6

    baseline <- tryCatch(
      as.vector(solve(system_matrix, w * y)),
      error = function(e) {
        cli::cli_warn(c(
          "Matrix solve failed in aspls iteration {iter}.",
          "i" = "Using previous iteration baseline as fallback.",
          "x" = "Error: {conditionMessage(e)}"
        ))
        baseline_old
      }
    )

    # Check convergence
    if (!anyNA(baseline) && !anyNA(baseline_old)) {
      if (max(abs(baseline - baseline_old)) < tol) {
        converged <- TRUE
        break
      }
    }
  }

  if (!converged) {
    final_change <- if (!anyNA(baseline) && !anyNA(baseline_old)) {
      max(abs(baseline - baseline_old))
    } else {
      NA
    }
    cli::cli_warn(c(
      "aspls did not converge after {max_iter} iterations.",
      "i" = "Final change: {format(final_change, digits = 3)} (tol: {tol})"
    ))
  }

  baseline
}

#' @export
bake.step_measure_baseline_aspls <- function(object, new_data, ...) {
  lambda <- object$lambda
  alpha <- object$alpha
  max_iter <- object$max_iter
  tol <- object$tol

  for (col in object$measures) {
    result <- purrr::map(new_data[[col]], function(m) {
      baseline <- .aspls_baseline(m$value, lambda, alpha, max_iter, tol)
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
print.step_measure_baseline_aspls <- function(
  x,
  width = max(20, options()$width - 30),
  ...
) {
  title <- paste0(
    "aspls baseline (lambda=",
    format(x$lambda, scientific = TRUE),
    ", alpha=",
    x$alpha,
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
tidy.step_measure_baseline_aspls <- function(x, ...) {
  tibble::tibble(
    terms = if (recipes::is_trained(x)) x$measures else "<all measure columns>",
    lambda = x$lambda,
    alpha = x$alpha,
    id = x$id
  )
}

#' @rdname tunable_measure
#' @export
tunable.step_measure_baseline_aspls <- function(x, ...) {
  tibble::tibble(
    name = c("lambda", "alpha"),
    call_info = list(
      list(pkg = "measure", fun = "baseline_lambda"),
      list(pkg = "measure", fun = "baseline_alpha")
    ),
    source = "recipe",
    component = "step_measure_baseline_aspls",
    component_id = x$id
  )
}

# ==============================================================================
# step_measure_baseline_iarpls
# ==============================================================================

#' Improved arPLS Baseline Correction (Two-Stage)
#'
#' `step_measure_baseline_iarpls()` creates a *specification* of a recipe step
#' that applies Improved arPLS baseline correction using a two-stage approach.
#'
#' @param recipe A recipe object.
#' @param measures An optional character vector of measure column names.
#' @param lambda Final smoothness parameter. Default is 1e6.
#' @param lambda_1 First stage (coarse) smoothness parameter. Default is 1e4.
#' @param max_iter Maximum number of iterations. Default is 10.
#' @param tol Convergence tolerance. Default is 1e-5.
#' @param role Not used.
#' @param trained Logical indicating if the step has been trained.
#' @param skip Logical. Should the step be skipped when baking?
#' @param id Unique step identifier.
#'
#' @return An updated recipe with the new step added.
#'
#' @details
#' iarpls uses a two-stage approach:
#' 1. First stage with smaller lambda for coarse baseline estimation
#' 2. Second stage with larger lambda for refined baseline using weights
#'    derived from the first stage
#'
#' This approach often provides better results than single-stage arPLS for
#' signals with complex baseline patterns.
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
#'   step_measure_baseline_iarpls(lambda = 1e6, lambda_1 = 1e4) |>
#'   prep()
#'
#' bake(rec, new_data = NULL)
#' }
step_measure_baseline_iarpls <- function(
  recipe,
  measures = NULL,
  lambda = 1e6,
  lambda_1 = 1e4,
  max_iter = 10L,
  tol = 1e-5,
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_baseline_iarpls")
) {
  recipes::add_step(
    recipe,
    step_measure_baseline_iarpls_new(
      measures = measures,
      lambda = lambda,
      lambda_1 = lambda_1,
      max_iter = as.integer(max_iter),
      tol = tol,
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_measure_baseline_iarpls_new <- function(
  measures,
  lambda,
  lambda_1,
  max_iter,
  tol,
  role,
  trained,
  skip,
  id
) {
  recipes::step(
    subclass = "measure_baseline_iarpls",
    measures = measures,
    lambda = lambda,
    lambda_1 = lambda_1,
    max_iter = max_iter,
    tol = tol,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_baseline_iarpls <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  # Validate parameters
  if (!is.numeric(x$lambda) || length(x$lambda) != 1 || x$lambda <= 0) {
    cli::cli_abort(
      "{.arg lambda} must be a single positive number, not {.val {x$lambda}}."
    )
  }
  if (!is.numeric(x$lambda_1) || length(x$lambda_1) != 1 || x$lambda_1 <= 0) {
    cli::cli_abort(
      "{.arg lambda_1} must be a single positive number, not {.val {x$lambda_1}}."
    )
  }
  if (!is.numeric(x$max_iter) || length(x$max_iter) != 1 || x$max_iter < 1) {
    cli::cli_abort(
      "{.arg max_iter} must be a positive integer, not {.val {x$max_iter}}."
    )
  }
  if (!is.numeric(x$tol) || length(x$tol) != 1 || x$tol <= 0) {
    cli::cli_abort(
      "{.arg tol} must be a single positive number, not {.val {x$tol}}."
    )
  }

  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  step_measure_baseline_iarpls_new(
    measures = measure_cols,
    lambda = x$lambda,
    lambda_1 = x$lambda_1,
    max_iter = as.integer(x$max_iter),
    tol = x$tol,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' Simple arPLS for first stage
#' @noRd
.arpls_simple <- function(y, lambda, max_iter = 5) {
  n <- length(y)
  D <- .create_diff_matrix(n, d = 2)
  DTD <- crossprod(D)

  w <- rep(1, n)
  baseline <- y

  for (iter in seq_len(max_iter)) {
    W <- diag(w)
    system_matrix <- W + lambda * DTD
    diag(system_matrix) <- diag(system_matrix) + 1e-6

    baseline_old <- baseline
    baseline <- tryCatch(
      as.vector(solve(system_matrix, w * y)),
      error = function(e) {
        cli::cli_warn(c(
          "Matrix solve failed in arpls_simple iteration {iter}.",
          "i" = "Using previous iteration baseline as fallback.",
          "x" = "Error: {conditionMessage(e)}"
        ))
        baseline_old
      }
    )

    # Update weights for next iteration
    residuals <- y - baseline
    w <- ifelse(residuals > 0, 0.001, 1)
  }

  baseline
}

#' iarpls baseline algorithm
#' @noRd
.iarpls_baseline <- function(y, lambda, lambda_1, max_iter, tol) {
  n <- length(y)

  # Validate input
  if (n < 3) {
    cli::cli_warn("Input vector has fewer than 3 points, returning original.")
    return(y)
  }
  if (anyNA(y)) {
    cli::cli_warn(
      "Input contains {sum(is.na(y))} NA value{?s}. NA values will propagate."
    )
  }
  if (any(!is.finite(y) & !is.na(y))) {
    cli::cli_abort("Input contains Inf/-Inf values. Cannot compute baseline.")
  }

  # First stage: coarse baseline with smaller lambda
  baseline_coarse <- .arpls_simple(y, lambda_1, max_iter = 5)

  # Initialize refined baseline
  baseline <- baseline_coarse
  converged <- FALSE

  # Create difference matrix
  D <- .create_diff_matrix(n, d = 2)
  DTD <- crossprod(D)

  for (iter in seq_len(max_iter)) {
    baseline_old <- baseline

    # Update residuals and mask
    residuals <- y - baseline
    neg_mask <- residuals < 0

    # Improved weight function
    mad_val <- stats::mad(residuals[neg_mask], na.rm = TRUE)
    if (is.na(mad_val) || mad_val == 0) {
      mad_val <- stats::mad(residuals, na.rm = TRUE)
    }
    if (mad_val == 0) {
      mad_val <- 1
    }

    w <- rep(1, n)
    w[neg_mask] <- exp(-abs(residuals[neg_mask]) / mad_val)

    # Additional weight adjustment
    very_neg <- residuals < -2 * mad_val
    w[very_neg] <- w[very_neg] * 0.01

    # Weighted least squares solution
    W <- diag(w)
    system_matrix <- W + lambda * DTD
    diag(system_matrix) <- diag(system_matrix) + 1e-6

    baseline <- tryCatch(
      as.vector(solve(system_matrix, w * y)),
      error = function(e) {
        cli::cli_warn(c(
          "Matrix solve failed in iarpls iteration {iter}.",
          "i" = "Using previous iteration baseline as fallback.",
          "x" = "Error: {conditionMessage(e)}"
        ))
        baseline_old
      }
    )

    # Check convergence
    if (!anyNA(baseline) && !anyNA(baseline_old)) {
      if (max(abs(baseline - baseline_old)) < tol) {
        converged <- TRUE
        break
      }
    }
  }

  if (!converged) {
    final_change <- if (!anyNA(baseline) && !anyNA(baseline_old)) {
      max(abs(baseline - baseline_old))
    } else {
      NA
    }
    cli::cli_warn(c(
      "iarpls did not converge after {max_iter} iterations.",
      "i" = "Final change: {format(final_change, digits = 3)} (tol: {tol})"
    ))
  }

  baseline
}

#' @export
bake.step_measure_baseline_iarpls <- function(object, new_data, ...) {
  lambda <- object$lambda
  lambda_1 <- object$lambda_1
  max_iter <- object$max_iter
  tol <- object$tol

  for (col in object$measures) {
    result <- purrr::map(new_data[[col]], function(m) {
      baseline <- .iarpls_baseline(m$value, lambda, lambda_1, max_iter, tol)
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
print.step_measure_baseline_iarpls <- function(
  x,
  width = max(20, options()$width - 30),
  ...
) {
  title <- paste0(
    "iarpls baseline (lambda=",
    format(x$lambda, scientific = TRUE),
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
tidy.step_measure_baseline_iarpls <- function(x, ...) {
  tibble::tibble(
    terms = if (recipes::is_trained(x)) x$measures else "<all measure columns>",
    lambda = x$lambda,
    lambda_1 = x$lambda_1,
    id = x$id
  )
}

#' @rdname tunable_measure
#' @export
tunable.step_measure_baseline_iarpls <- function(x, ...) {
  tibble::tibble(
    name = c("lambda", "lambda_1"),
    call_info = list(
      list(pkg = "measure", fun = "baseline_lambda"),
      list(pkg = "measure", fun = "baseline_lambda")
    ),
    source = "recipe",
    component = "step_measure_baseline_iarpls",
    component_id = x$id
  )
}

# ==============================================================================
# step_measure_baseline_fastchrom
# ==============================================================================

#' Fast Chromatography Baseline Correction
#'
#' `step_measure_baseline_fastchrom()` creates a *specification* of a recipe
#' step that applies fast baseline correction optimized for chromatography data.
#'
#' @param recipe A recipe object.
#' @param measures An optional character vector of measure column names.
#' @param lambda Smoothness parameter. Default is 1e6.
#' @param window Window size for local minima detection. Default is 50.
#' @param max_iter Maximum number of refinement iterations. Default is 10.
#' @param role Not used.
#' @param trained Logical indicating if the step has been trained.
#' @param skip Logical. Should the step be skipped when baking?
#' @param id Unique step identifier.
#'
#' @return An updated recipe with the new step added.
#'
#' @details
#' This algorithm combines morphological operations with penalized least
#' squares for fast and robust baseline estimation:
#'
#' 1. Finds local minima using a rolling window
#' 2. Smooths the minima to get initial baseline estimate
#' 3. Iteratively refines using weighted PLS
#'
#' Particularly effective for SEC/GPC chromatography and other analytical
#' techniques with well-defined peaks.
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
#'   step_measure_baseline_fastchrom(lambda = 1e6, window = 50) |>
#'   prep()
#'
#' bake(rec, new_data = NULL)
#' }
step_measure_baseline_fastchrom <- function(
  recipe,
  measures = NULL,
  lambda = 1e6,
  window = 50L,
  max_iter = 10L,
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_baseline_fastchrom")
) {
  recipes::add_step(
    recipe,
    step_measure_baseline_fastchrom_new(
      measures = measures,
      lambda = lambda,
      window = as.integer(window),
      max_iter = as.integer(max_iter),
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_measure_baseline_fastchrom_new <- function(
  measures,
  lambda,
  window,
  max_iter,
  role,
  trained,
  skip,
  id
) {
  recipes::step(
    subclass = "measure_baseline_fastchrom",
    measures = measures,
    lambda = lambda,
    window = window,
    max_iter = max_iter,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_baseline_fastchrom <- function(
  x,
  training,
  info = NULL,
  ...
) {
  check_for_measure(training)

  # Validate parameters
  if (!is.numeric(x$lambda) || length(x$lambda) != 1 || x$lambda <= 0) {
    cli::cli_abort(
      "{.arg lambda} must be a single positive number, not {.val {x$lambda}}."
    )
  }
  if (!is.numeric(x$window) || length(x$window) != 1 || x$window < 3) {
    cli::cli_abort(
      "{.arg window} must be a single integer >= 3, not {.val {x$window}}."
    )
  }
  if (!is.numeric(x$max_iter) || length(x$max_iter) != 1 || x$max_iter < 1) {
    cli::cli_abort(
      "{.arg max_iter} must be a positive integer, not {.val {x$max_iter}}."
    )
  }

  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  step_measure_baseline_fastchrom_new(
    measures = measure_cols,
    lambda = x$lambda,
    window = as.integer(x$window),
    max_iter = as.integer(x$max_iter),
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' fastchrom baseline algorithm
#' @noRd
.fastchrom_baseline <- function(y, lambda, window, max_iter) {
  n <- length(y)

  # Validate input
  if (n < 3) {
    cli::cli_warn("Input vector has fewer than 3 points, returning original.")
    return(y)
  }
  if (anyNA(y)) {
    cli::cli_warn(
      "Input contains {sum(is.na(y))} NA value{?s}. NA values will propagate."
    )
  }
  if (any(!is.finite(y) & !is.na(y))) {
    cli::cli_abort("Input contains Inf/-Inf values. Cannot compute baseline.")
  }

  half_window <- window %/% 2

  # Step 1: Find local minima using rolling minimum
  local_min <- numeric(n)
  for (i in seq_len(n)) {
    start <- max(1, i - half_window)
    end <- min(n, i + half_window)
    local_min[i] <- min(y[start:end])
  }

  # Smooth local minima for initial baseline
  smooth_win <- min(21, n %/% 5)
  if (smooth_win %% 2 == 0) {
    smooth_win <- smooth_win + 1
  }
  half_smooth <- smooth_win %/% 2

  baseline <- numeric(n)
  for (i in seq_len(n)) {
    start <- max(1, i - half_smooth)
    end <- min(n, i + half_smooth)
    baseline[i] <- mean(local_min[start:end])
  }

  # Step 2: Iterative refinement with PLS
  D <- .create_diff_matrix(n, d = 2)
  DTD <- crossprod(D)

  for (iter in seq_len(max_iter)) {
    baseline_old <- baseline
    residuals <- y - baseline

    # Weight points based on whether they're below baseline
    w <- ifelse(residuals < 0, 1, 0.001)

    # Smooth weights
    w_smooth <- numeric(n)
    for (i in seq_len(n)) {
      start <- max(1, i - 5)
      end <- min(n, i + 5)
      w_smooth[i] <- mean(w[start:end])
    }
    w <- (w + w_smooth) / 2
    w[w < 1e-6] <- 1e-6

    # Solve weighted least squares
    W <- diag(w)
    C <- W + lambda * DTD

    # Add regularization if needed
    if (rcond(C) < 1e-10) {
      diag(C) <- diag(C) + 1e-6
    }

    baseline <- tryCatch(
      as.vector(solve(C, w * y)),
      error = function(e) {
        cli::cli_warn(c(
          "Matrix solve failed in fastchrom iteration {iter}.",
          "i" = "Using previous iteration baseline as fallback.",
          "x" = "Error: {conditionMessage(e)}"
        ))
        baseline_old
      }
    )
  }

  baseline
}

#' @export
bake.step_measure_baseline_fastchrom <- function(object, new_data, ...) {
  lambda <- object$lambda
  window <- object$window
  max_iter <- object$max_iter

  for (col in object$measures) {
    result <- purrr::map(new_data[[col]], function(m) {
      baseline <- .fastchrom_baseline(m$value, lambda, window, max_iter)
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
print.step_measure_baseline_fastchrom <- function(
  x,
  width = max(20, options()$width - 30),
  ...
) {
  title <- paste0(
    "FastChrom baseline (lambda=",
    format(x$lambda, scientific = TRUE),
    ", window=",
    x$window,
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
tidy.step_measure_baseline_fastchrom <- function(x, ...) {
  tibble::tibble(
    terms = if (recipes::is_trained(x)) x$measures else "<all measure columns>",
    lambda = x$lambda,
    window = x$window,
    id = x$id
  )
}

#' @rdname tunable_measure
#' @export
tunable.step_measure_baseline_fastchrom <- function(x, ...) {
  tibble::tibble(
    name = c("lambda", "window"),
    call_info = list(
      list(pkg = "measure", fun = "baseline_lambda"),
      list(pkg = "measure", fun = "baseline_window")
    ),
    source = "recipe",
    component = "step_measure_baseline_fastchrom",
    component_id = x$id
  )
}
