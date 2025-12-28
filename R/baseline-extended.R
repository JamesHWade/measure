# ==============================================================================
# Extended Baseline Correction Methods
#
# This file contains additional baseline correction algorithms:
# - step_measure_baseline_rolling: Rolling ball baseline
# - step_measure_baseline_airpls: Adaptive Iteratively Reweighted PLS
# - step_measure_baseline_snip: SNIP algorithm
# ==============================================================================

# ==============================================================================
# step_measure_baseline_rolling
# ==============================================================================

#' Rolling Ball Baseline Correction
#'
#' `step_measure_baseline_rolling()` creates a *specification* of a recipe step
#' that applies rolling ball baseline correction. This morphological approach
#' "rolls" a ball of specified radius along the underside of the spectrum.
#'
#' @param recipe A recipe object.
#' @param measures An optional character vector of measure column names.
#' @param window_size The diameter of the rolling ball in number of points.
#'   Default is 100.
#' @param smoothing Additional smoothing window applied to the baseline.
#'   Default is 50.
#' @param role Not used.
#' @param trained Logical indicating if the step has been trained.
#' @param skip Logical. Should the step be skipped when baking?
#' @param id Unique step identifier.
#'
#' @return An updated recipe with the new step added.
#'
#' @details
#' The rolling ball algorithm simulates rolling a ball of specified radius
#' along the underside of the spectrum. Points where the ball touches become
#' the baseline. This is effective for:
#'
#' - Chromatographic baselines
#' - Spectra with gradual drift
#' - Data where peaks are narrower than baseline features
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
#'   step_measure_baseline_rolling(window_size = 50) |>
#'   prep()
#'
#' bake(rec, new_data = NULL)
step_measure_baseline_rolling <- function(
    recipe,
    measures = NULL,
    window_size = 100,
    smoothing = 50,
    role = NA,
    trained = FALSE,
    skip = FALSE,
    id = recipes::rand_id("measure_baseline_rolling")) {
  if (!is.numeric(window_size) || window_size < 3) {
    cli::cli_abort("{.arg window_size} must be a number >= 3.")
  }
  if (!is.numeric(smoothing) || smoothing < 1) {
    cli::cli_abort("{.arg smoothing} must be a positive number.")
  }

  recipes::add_step(
    recipe,
    step_measure_baseline_rolling_new(
      measures = measures,
      window_size = as.integer(window_size),
      smoothing = as.integer(smoothing),
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_measure_baseline_rolling_new <- function(
    measures, window_size, smoothing, role, trained, skip, id) {
  recipes::step(
    subclass = "measure_baseline_rolling",
    measures = measures,
    window_size = window_size,
    smoothing = smoothing,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_baseline_rolling <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  step_measure_baseline_rolling_new(
    measures = measure_cols,
    window_size = x$window_size,
    smoothing = x$smoothing,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' Rolling ball baseline algorithm
#' @noRd
.rolling_ball_baseline <- function(y, window_size, smoothing) {
  n <- length(y)
  half_win <- window_size %/% 2

  # Compute local minima within rolling window
  baseline <- numeric(n)
  for (i in seq_len(n)) {
    start_idx <- max(1, i - half_win)
    end_idx <- min(n, i + half_win)
    baseline[i] <- min(y[start_idx:end_idx])
  }

  # Apply smoothing using moving average
  if (smoothing > 1) {
    half_smooth <- smoothing %/% 2
    smoothed <- numeric(n)
    for (i in seq_len(n)) {
      start_idx <- max(1, i - half_smooth)
      end_idx <- min(n, i + half_smooth)
      smoothed[i] <- mean(baseline[start_idx:end_idx])
    }
    baseline <- smoothed
  }

  baseline
}

#' @export
bake.step_measure_baseline_rolling <- function(object, new_data, ...) {
  window_size <- object$window_size
  smoothing <- object$smoothing

  for (col in object$measures) {
    result <- purrr::map(new_data[[col]], function(m) {
      baseline <- .rolling_ball_baseline(m$value, window_size, smoothing)
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
print.step_measure_baseline_rolling <- function(x, width = max(20, options()$width - 30), ...) {
  title <- paste0("Rolling ball baseline (window=", x$window_size, ")")
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
tidy.step_measure_baseline_rolling <- function(x, ...) {
  tibble::tibble(
    terms = if (is_trained(x)) x$measures else "<all measure columns>",
    window_size = x$window_size,
    smoothing = x$smoothing,
    id = x$id
  )
}

# ==============================================================================
# step_measure_baseline_airpls
# ==============================================================================

#' Adaptive Iteratively Reweighted Penalized Least Squares Baseline
#'
#' `step_measure_baseline_airpls()` creates a *specification* of a recipe step
#' that applies airPLS baseline correction. This method automatically adjusts
#' weights based on the difference between the signal and fitted baseline.
#'
#' @param recipe A recipe object.
#' @param measures An optional character vector of measure column names.
#' @param lambda Smoothness parameter. Higher values produce smoother baselines.
#'   Default is 1e5. Tunable via [baseline_lambda()].
#' @param max_iter Maximum number of iterations. Default is 50.
#' @param tol Convergence tolerance for weight changes. Default is 1e-3.
#' @param role Not used.
#' @param trained Logical indicating if the step has been trained.
#' @param skip Logical. Should the step be skipped when baking?
#' @param id Unique step identifier.
#'
#' @return An updated recipe with the new step added.
#'
#' @details
#' airPLS (Adaptive Iteratively Reweighted Penalized Least Squares) is an
#' improvement over standard ALS that automatically adapts the asymmetry
#' parameter based on the residuals. Key features:
#'
#' - No need to manually set asymmetry parameter
#' - Good for signals with varying baseline curvature
#' - Robust to different peak heights
#'
#' @references
#' Zhang, Z.M., Chen, S., & Liang, Y.Z. (2010). Baseline correction using
#' adaptive iteratively reweighted penalized least squares. Analyst, 135,
#' 1138-1146.
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
#'   step_measure_baseline_airpls(lambda = 1e5) |>
#'   prep()
#'
#' bake(rec, new_data = NULL)
step_measure_baseline_airpls <- function(
    recipe,
    measures = NULL,
    lambda = 1e5,
    max_iter = 50L,
    tol = 1e-3,
    role = NA,
    trained = FALSE,
    skip = FALSE,
    id = recipes::rand_id("measure_baseline_airpls")) {
  if (!is.numeric(lambda) || lambda <= 0) {
    cli::cli_abort("{.arg lambda} must be a positive number.")
  }

  recipes::add_step(
    recipe,
    step_measure_baseline_airpls_new(
      measures = measures,
      lambda = lambda,
      max_iter = as.integer(max_iter),
      tol = tol,
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_measure_baseline_airpls_new <- function(
    measures, lambda, max_iter, tol, role, trained, skip, id) {
  recipes::step(
    subclass = "measure_baseline_airpls",
    measures = measures,
    lambda = lambda,
    max_iter = max_iter,
    tol = tol,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_baseline_airpls <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  step_measure_baseline_airpls_new(
    measures = measure_cols,
    lambda = x$lambda,
    max_iter = x$max_iter,
    tol = x$tol,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' Whittaker smoother (2nd derivative penalty)
#' @noRd
.whittaker_smooth <- function(y, w, lambda) {
  n <- length(y)

  # Build difference matrix D (2nd derivative)
  # D is (n-2) x n with pattern [1, -2, 1]
  D <- matrix(0, nrow = n - 2, ncol = n)
  for (i in seq_len(n - 2)) {
    D[i, i] <- 1
    D[i, i + 1] <- -2
    D[i, i + 2] <- 1
  }

  # Solve (W + lambda * D'D) z = W y
  W <- diag(w)
  DtD <- t(D) %*% D
  A <- W + lambda * DtD
  b <- w * y

  # Use Cholesky decomposition for efficiency
  z <- tryCatch(
    solve(A, b),
    error = function(e) {
      # Fallback if matrix is singular
      MASS::ginv(A) %*% b
    }
  )

  as.vector(z)
}

#' airPLS baseline algorithm
#' @noRd
.airpls_baseline <- function(y, lambda, max_iter, tol) {
  n <- length(y)
  w <- rep(1, n)

  for (iter in seq_len(max_iter)) {
    z <- .whittaker_smooth(y, w, lambda)

    # Calculate residuals
    d <- y - z

    # Update weights using airPLS formula
    # Points below baseline get weight 0, above get exponentially decaying weight
    d_neg <- d[d < 0]
    if (length(d_neg) == 0) break

    m <- mean(abs(d_neg))
    w_new <- numeric(n)
    w_new[d >= 0] <- 0
    w_new[d < 0] <- exp(iter * d[d < 0] / (2 * m))

    # Check convergence
    if (sum(abs(w_new - w)) / sum(w) < tol) break

    w <- w_new
    w[w < 1e-6] <- 1e-6  # Prevent zero weights
  }

  z
}

#' @export
bake.step_measure_baseline_airpls <- function(object, new_data, ...) {
  lambda <- object$lambda
  max_iter <- object$max_iter
  tol <- object$tol

  for (col in object$measures) {
    result <- purrr::map(new_data[[col]], function(m) {
      baseline <- .airpls_baseline(m$value, lambda, max_iter, tol)
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
print.step_measure_baseline_airpls <- function(x, width = max(20, options()$width - 30), ...) {
  title <- paste0("airPLS baseline (lambda=", format(x$lambda, scientific = TRUE), ")")
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
tidy.step_measure_baseline_airpls <- function(x, ...) {
  tibble::tibble(
    terms = if (is_trained(x)) x$measures else "<all measure columns>",
    lambda = x$lambda,
    max_iter = x$max_iter,
    id = x$id
  )
}

#' @rdname tunable
#' @export
tunable.step_measure_baseline_airpls <- function(x, ...) {
  tibble::tibble(
    name = "lambda",
    call_info = list(list(pkg = "measure", fun = "baseline_lambda")),
    source = "recipe",
    component = "step_measure_baseline_airpls",
    component_id = x$id
  )
}

# ==============================================================================
# step_measure_baseline_snip
# ==============================================================================

#' SNIP Baseline Correction
#'
#' `step_measure_baseline_snip()` creates a *specification* of a recipe step
#' that applies SNIP (Statistics-sensitive Non-linear Iterative Peak-clipping)
#' baseline correction.
#'
#' @param recipe A recipe object.
#' @param measures An optional character vector of measure column names.
#' @param iterations Number of clipping iterations. More iterations produce
#'   lower baselines. Default is 40.
#' @param decreasing Logical. If `TRUE` (default), iterations decrease from
#'   `iterations` to 1. If `FALSE`, uses fixed window size.
#' @param role Not used.
#' @param trained Logical indicating if the step has been trained.
#' @param skip Logical. Should the step be skipped when baking?
#' @param id Unique step identifier.
#'
#' @return An updated recipe with the new step added.
#'
#' @details
#' SNIP is a robust baseline estimation algorithm originally developed for
#' gamma-ray spectroscopy. It works by iteratively replacing each point with
#' the minimum of itself and the average of its neighbors at increasing
#' distances.
#'
#' The algorithm is particularly effective for:
#' - Spectra with sharp peaks on slowly varying baseline
#' - X-ray fluorescence and diffraction
#' - Mass spectrometry
#'
#' @references
#' Ryan, C.G., et al. (1988). SNIP, a statistics-sensitive background treatment
#' for the quantitative analysis of PIXE spectra in geoscience applications.
#' Nuclear Instruments and Methods in Physics Research B, 34, 396-402.
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
#'   step_measure_baseline_snip(iterations = 30) |>
#'   prep()
#'
#' bake(rec, new_data = NULL)
step_measure_baseline_snip <- function(
    recipe,
    measures = NULL,
    iterations = 40L,
    decreasing = TRUE,
    role = NA,
    trained = FALSE,
    skip = FALSE,
    id = recipes::rand_id("measure_baseline_snip")) {
  if (!is.numeric(iterations) || iterations < 1) {
    cli::cli_abort("{.arg iterations} must be a positive integer.")
  }

  recipes::add_step(
    recipe,
    step_measure_baseline_snip_new(
      measures = measures,
      iterations = as.integer(iterations),
      decreasing = decreasing,
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_measure_baseline_snip_new <- function(
    measures, iterations, decreasing, role, trained, skip, id) {
  recipes::step(
    subclass = "measure_baseline_snip",
    measures = measures,
    iterations = iterations,
    decreasing = decreasing,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_baseline_snip <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  step_measure_baseline_snip_new(
    measures = measure_cols,
    iterations = x$iterations,
    decreasing = x$decreasing,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' SNIP baseline algorithm
#' @noRd
.snip_baseline <- function(y, iterations, decreasing) {
  n <- length(y)
  baseline <- y

  # Generate iteration sequence
  if (decreasing) {
    iter_seq <- seq(iterations, 1, by = -1)
  } else {
    iter_seq <- rep(iterations, iterations)
  }

  for (p in iter_seq) {
    for (i in (p + 1):(n - p)) {
      # SNIP clipping: replace with min of self and average of neighbors
      avg_neighbors <- (baseline[i - p] + baseline[i + p]) / 2
      baseline[i] <- min(baseline[i], avg_neighbors)
    }
  }

  baseline
}

#' @export
bake.step_measure_baseline_snip <- function(object, new_data, ...) {
  iterations <- object$iterations
  decreasing <- object$decreasing

  for (col in object$measures) {
    result <- purrr::map(new_data[[col]], function(m) {
      baseline <- .snip_baseline(m$value, iterations, decreasing)
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
print.step_measure_baseline_snip <- function(x, width = max(20, options()$width - 30), ...) {
  title <- paste0("SNIP baseline (iterations=", x$iterations, ")")
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
tidy.step_measure_baseline_snip <- function(x, ...) {
  tibble::tibble(
    terms = if (is_trained(x)) x$measures else "<all measure columns>",
    iterations = x$iterations,
    decreasing = x$decreasing,
    id = x$id
  )
}
