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

#' @rdname tunable_measure
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

# ==============================================================================
# step_measure_baseline_arpls
# ==============================================================================

#' Asymmetrically Reweighted Penalized Least Squares Baseline Correction
#'
#' `step_measure_baseline_arpls()` creates a *specification* of a recipe step
#' that applies arPLS baseline correction using asymmetric weighting.
#'
#' @param recipe A recipe object.
#' @param measures An optional character vector of measure column names.
#' @param lambda Smoothing parameter. Larger values produce smoother baselines.
#'   Default is 1e5.
#' @param ratio Asymmetric weighting ratio. Default is 0.001.
#' @param max_iter Maximum number of iterations. Default is 50.
#' @param tol Convergence tolerance. Default is 1e-3.
#' @param role Not used.
#' @param trained Logical indicating if the step has been trained.
#' @param skip Logical. Should the step be skipped when baking?
#' @param id Unique step identifier.
#'
#' @return An updated recipe with the new step added.
#'
#' @details
#' The arPLS algorithm uses asymmetric least squares with a ratio-based
#' weighting scheme. It is robust to peak interference and works well for
#' signals with varying baseline curvature.
#'
#' Reference: Baek et al. (2015), Analyst 140, 250-257
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
#'   step_measure_baseline_arpls(lambda = 1e5) |>
#'   prep()
step_measure_baseline_arpls <- function(
    recipe,
    measures = NULL,
    lambda = 1e5,
    ratio = 0.001,
    max_iter = 50L,
    tol = 1e-3,
    role = NA,
    trained = FALSE,
    skip = FALSE,
    id = recipes::rand_id("measure_baseline_arpls")) {

  if (!is.numeric(lambda) || length(lambda) != 1 || lambda <= 0) {
    cli::cli_abort("{.arg lambda} must be a positive number.")
  }
  if (!is.numeric(ratio) || length(ratio) != 1 || ratio <= 0 || ratio >= 1) {
    cli::cli_abort("{.arg ratio} must be a number between 0 and 1.")
  }

  recipes::add_step(
    recipe,
    step_measure_baseline_arpls_new(
      measures = measures,
      lambda = lambda,
      ratio = ratio,
      max_iter = as.integer(max_iter),
      tol = tol,
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_measure_baseline_arpls_new <- function(
    measures, lambda, ratio, max_iter, tol, role, trained, skip, id) {
  recipes::step(
    subclass = "measure_baseline_arpls",
    measures = measures,
    lambda = lambda,
    ratio = ratio,
    max_iter = max_iter,
    tol = tol,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_baseline_arpls <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  step_measure_baseline_arpls_new(
    measures = measure_cols,
    lambda = x$lambda,
    ratio = x$ratio,
    max_iter = x$max_iter,
    tol = x$tol,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' arPLS baseline estimation
#' @noRd
.arpls_baseline <- function(y, lambda, ratio, max_iter, tol) {
  n <- length(y)

  # Create difference matrix for penalized least squares
  D <- diff(diag(n), differences = 2)
  H <- lambda * crossprod(D)

  w <- rep(1, n)
  z <- y

  for (iter in seq_len(max_iter)) {
    W <- diag(w)
    A <- W + H
    b <- w * y

    z_new <- tryCatch(
      as.vector(solve(A, b)),
      error = function(e) as.vector(MASS::ginv(A) %*% b)
    )

    # Check convergence
    if (sum(abs(z_new - z)) / sum(abs(z)) < tol) {
      z <- z_new
      break
    }

    z <- z_new

    # Update weights using ratio
    d <- y - z
    dn <- d[d < 0]
    m <- mean(dn)
    s <- stats::sd(dn)
    if (is.na(s) || s == 0) s <- 1

    w <- ifelse(d < 0,
                ratio,
                ratio * exp(-(d - m)^2 / (2 * s^2)))
  }

  z
}

#' @export
bake.step_measure_baseline_arpls <- function(object, new_data, ...) {
  lambda <- object$lambda
  ratio <- object$ratio
  max_iter <- object$max_iter
  tol <- object$tol

  for (col in object$measures) {
    new_data[[col]] <- purrr::map(new_data[[col]], function(m) {
      baseline <- .arpls_baseline(m$value, lambda, ratio, max_iter, tol)
      m$value <- m$value - baseline
      m
    })
    new_data[[col]] <- new_measure_list(new_data[[col]])
  }

  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_baseline_arpls <- function(x, width = max(20, options()$width - 30), ...) {
  title <- paste0("arPLS baseline (lambda=", format(x$lambda, scientific = TRUE), ")")
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
tidy.step_measure_baseline_arpls <- function(x, ...) {
  tibble::tibble(
    terms = if (is_trained(x)) x$measures else "<all measure columns>",
    lambda = x$lambda,
    ratio = x$ratio,
    id = x$id
  )
}

#' @rdname tunable_measure
#' @export
tunable.step_measure_baseline_arpls <- function(x, ...) {
  tibble::tibble(
    name = "lambda",
    call_info = list(list(pkg = "measure", fun = "baseline_lambda")),
    source = "recipe",
    component = "step_measure_baseline_arpls",
    component_id = x$id
  )
}

# ==============================================================================
# step_measure_baseline_tophat
# ==============================================================================

#' Top-Hat Morphological Baseline Correction
#'
#' `step_measure_baseline_tophat()` creates a *specification* of a recipe step
#' that applies top-hat morphological baseline correction.
#'
#' @param recipe A recipe object.
#' @param measures An optional character vector of measure column names.
#' @param half_window Half-window size for the structuring element. Default is 50.
#' @param role Not used.
#' @param trained Logical indicating if the step has been trained.
#' @param skip Logical. Should the step be skipped when baking?
#' @param id Unique step identifier.
#'
#' @return An updated recipe with the new step added.
#'
#' @details
#' The top-hat transform is a morphological operation that extracts bright
#' features (peaks) from a dark background. It is computed as the difference
#' between the original signal and its morphological opening.
#'
#' This is effective for chromatography with sharp, well-defined peaks
#' on a smooth baseline.
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
#'   step_measure_baseline_tophat(half_window = 30) |>
#'   prep()
step_measure_baseline_tophat <- function(
    recipe,
    measures = NULL,
    half_window = 50L,
    role = NA,
    trained = FALSE,
    skip = FALSE,
    id = recipes::rand_id("measure_baseline_tophat")) {

  if (!is.numeric(half_window) || length(half_window) != 1 || half_window < 1) {
    cli::cli_abort("{.arg half_window} must be a positive integer.")
  }

  recipes::add_step(
    recipe,
    step_measure_baseline_tophat_new(
      measures = measures,
      half_window = as.integer(half_window),
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_measure_baseline_tophat_new <- function(
    measures, half_window, role, trained, skip, id) {
  recipes::step(
    subclass = "measure_baseline_tophat",
    measures = measures,
    half_window = half_window,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_baseline_tophat <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  step_measure_baseline_tophat_new(
    measures = measure_cols,
    half_window = x$half_window,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' Morphological erosion
#' @noRd
.morph_erode <- function(y, half_window) {
  n <- length(y)
  result <- numeric(n)
  for (i in seq_len(n)) {
    start <- max(1, i - half_window)
    end <- min(n, i + half_window)
    result[i] <- min(y[start:end])
  }
  result
}

#' Morphological dilation
#' @noRd
.morph_dilate <- function(y, half_window) {
  n <- length(y)
  result <- numeric(n)
  for (i in seq_len(n)) {
    start <- max(1, i - half_window)
    end <- min(n, i + half_window)
    result[i] <- max(y[start:end])
  }
  result
}

#' Morphological opening (erosion then dilation)
#' @noRd
.morph_open <- function(y, half_window) {
  .morph_dilate(.morph_erode(y, half_window), half_window)
}

#' @export
bake.step_measure_baseline_tophat <- function(object, new_data, ...) {
  half_window <- object$half_window

  for (col in object$measures) {
    new_data[[col]] <- purrr::map(new_data[[col]], function(m) {
      # Top-hat = original - opening
      baseline <- .morph_open(m$value, half_window)
      m$value <- m$value - baseline
      m
    })
    new_data[[col]] <- new_measure_list(new_data[[col]])
  }

  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_baseline_tophat <- function(x, width = max(20, options()$width - 30), ...) {
  title <- paste0("Top-hat baseline (half_window=", x$half_window, ")")
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
tidy.step_measure_baseline_tophat <- function(x, ...) {
  tibble::tibble(
    terms = if (is_trained(x)) x$measures else "<all measure columns>",
    half_window = x$half_window,
    id = x$id
  )
}

# ==============================================================================
# step_measure_baseline_morph
# ==============================================================================

#' Iterative Morphological Baseline Correction
#'
#' `step_measure_baseline_morph()` creates a *specification* of a recipe step
#' that applies iterative morphological baseline correction using erosion
#' and dilation operations.
#'
#' @param recipe A recipe object.
#' @param measures An optional character vector of measure column names.
#' @param half_window Half-window size for the structuring element. Default is 50.
#' @param iterations Number of erosion-dilation iterations. Default is 10.
#' @param role Not used.
#' @param trained Logical indicating if the step has been trained.
#' @param skip Logical. Should the step be skipped when baking?
#' @param id Unique step identifier.
#'
#' @return An updated recipe with the new step added.
#'
#' @details
#' This method applies iterative morphological operations (erosion followed
#' by dilation) to estimate the baseline. Multiple iterations can help
#' refine the baseline estimate for complex signals.
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
#'   step_measure_baseline_morph(half_window = 30, iterations = 5) |>
#'   prep()
step_measure_baseline_morph <- function(
    recipe,
    measures = NULL,
    half_window = 50L,
    iterations = 10L,
    role = NA,
    trained = FALSE,
    skip = FALSE,
    id = recipes::rand_id("measure_baseline_morph")) {

  if (!is.numeric(half_window) || length(half_window) != 1 || half_window < 1) {
    cli::cli_abort("{.arg half_window} must be a positive integer.")
  }
  if (!is.numeric(iterations) || length(iterations) != 1 || iterations < 1) {
    cli::cli_abort("{.arg iterations} must be a positive integer.")
  }

  recipes::add_step(
    recipe,
    step_measure_baseline_morph_new(
      measures = measures,
      half_window = as.integer(half_window),
      iterations = as.integer(iterations),
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_measure_baseline_morph_new <- function(
    measures, half_window, iterations, role, trained, skip, id) {
  recipes::step(
    subclass = "measure_baseline_morph",
    measures = measures,
    half_window = half_window,
    iterations = iterations,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_baseline_morph <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  step_measure_baseline_morph_new(
    measures = measure_cols,
    half_window = x$half_window,
    iterations = x$iterations,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_baseline_morph <- function(object, new_data, ...) {
  half_window <- object$half_window
  iterations <- object$iterations

  for (col in object$measures) {
    new_data[[col]] <- purrr::map(new_data[[col]], function(m) {
      baseline <- m$value
      for (i in seq_len(iterations)) {
        baseline <- .morph_open(baseline, half_window)
      }
      m$value <- m$value - baseline
      m
    })
    new_data[[col]] <- new_measure_list(new_data[[col]])
  }

  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_baseline_morph <- function(x, width = max(20, options()$width - 30), ...) {
  title <- paste0("Morphological baseline (half_window=", x$half_window,
                  ", iterations=", x$iterations, ")")
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
tidy.step_measure_baseline_morph <- function(x, ...) {
  tibble::tibble(
    terms = if (is_trained(x)) x$measures else "<all measure columns>",
    half_window = x$half_window,
    iterations = x$iterations,
    id = x$id
  )
}

# ==============================================================================
# step_measure_baseline_minima
# ==============================================================================

#' Local Minima Interpolation Baseline Correction
#'
#' `step_measure_baseline_minima()` creates a *specification* of a recipe step
#' that estimates baseline by interpolating between local minima.
#'
#' @param recipe A recipe object.
#' @param measures An optional character vector of measure column names.
#' @param window_size Window size for finding local minima. Default is 50.
#' @param method Interpolation method: "linear" or "spline". Default is "spline".
#' @param role Not used.
#' @param trained Logical indicating if the step has been trained.
#' @param skip Logical. Should the step be skipped when baking?
#' @param id Unique step identifier.
#'
#' @return An updated recipe with the new step added.
#'
#' @details
#' This method finds local minima within specified windows, then interpolates
#' between them to create a baseline estimate. This is intuitive and works
#' well when baseline points are clearly identifiable as local minima.
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
#'   step_measure_baseline_minima(window_size = 30, method = "spline") |>
#'   prep()
step_measure_baseline_minima <- function(
    recipe,
    measures = NULL,
    window_size = 50L,
    method = c("spline", "linear"),
    role = NA,
    trained = FALSE,
    skip = FALSE,
    id = recipes::rand_id("measure_baseline_minima")) {

  method <- rlang::arg_match(method)

  if (!is.numeric(window_size) || length(window_size) != 1 || window_size < 3) {
    cli::cli_abort("{.arg window_size} must be a number >= 3.")
  }

  recipes::add_step(
    recipe,
    step_measure_baseline_minima_new(
      measures = measures,
      window_size = as.integer(window_size),
      method = method,
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_measure_baseline_minima_new <- function(
    measures, window_size, method, role, trained, skip, id) {
  recipes::step(
    subclass = "measure_baseline_minima",
    measures = measures,
    window_size = window_size,
    method = method,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_baseline_minima <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  step_measure_baseline_minima_new(
    measures = measure_cols,
    window_size = x$window_size,
    method = x$method,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' Find local minima in windows
#' @noRd
.find_local_minima <- function(y, window_size) {
  n <- length(y)
  n_windows <- ceiling(n / window_size)
  minima_idx <- integer(n_windows)

  for (i in seq_len(n_windows)) {
    start <- (i - 1) * window_size + 1
    end <- min(i * window_size, n)
    window_min <- which.min(y[start:end])
    minima_idx[i] <- start + window_min - 1
  }

  # Ensure endpoints are included
  unique(c(1, minima_idx, n))
}

#' @export
bake.step_measure_baseline_minima <- function(object, new_data, ...) {
  window_size <- object$window_size
  method <- object$method

  for (col in object$measures) {
    new_data[[col]] <- purrr::map(new_data[[col]], function(m) {
      # Find local minima
      minima_idx <- .find_local_minima(m$value, window_size)
      minima_x <- m$location[minima_idx]
      minima_y <- m$value[minima_idx]

      # Interpolate baseline
      if (method == "spline" && length(minima_idx) >= 4) {
        spline_fit <- stats::spline(minima_x, minima_y, xout = m$location)
        baseline <- spline_fit$y
      } else {
        baseline <- stats::approx(minima_x, minima_y, xout = m$location)$y
      }

      m$value <- m$value - baseline
      m
    })
    new_data[[col]] <- new_measure_list(new_data[[col]])
  }

  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_baseline_minima <- function(x, width = max(20, options()$width - 30), ...) {
  title <- paste0("Local minima baseline (window=", x$window_size,
                  ", method=", x$method, ")")
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
tidy.step_measure_baseline_minima <- function(x, ...) {
  tibble::tibble(
    terms = if (is_trained(x)) x$measures else "<all measure columns>",
    window_size = x$window_size,
    method = x$method,
    id = x$id
  )
}

# ==============================================================================
# step_measure_baseline_auto
# ==============================================================================

#' Automatic Baseline Correction Method Selection
#'
#' `step_measure_baseline_auto()` creates a *specification* of a recipe step
#' that automatically selects and applies the best baseline correction method
#' based on signal characteristics.
#'
#' @param recipe A recipe object.
#' @param measures An optional character vector of measure column names.
#' @param methods Character vector of methods to consider. Default includes
#'   all available methods.
#' @param role Not used.
#' @param trained Logical indicating if the step has been trained.
#' @param selected_method The method selected during training (internal).
#' @param skip Logical. Should the step be skipped when baking?
#' @param id Unique step identifier.
#'
#' @return An updated recipe with the new step added.
#'
#' @details
#' This step analyzes the signal characteristics (noise level, baseline
#' curvature, peak density) during training and selects an appropriate
#' baseline correction method. The selected method is then applied
#' consistently during baking.
#'
#' Method selection heuristics:
#' - High noise, smooth baseline: rolling ball
#' - Complex curvature: airPLS or arPLS
#' - Sharp peaks: SNIP or top-hat
#' - Simple baseline: polynomial or minima
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
#'   step_measure_baseline_auto() |>
#'   prep()
step_measure_baseline_auto <- function(
    recipe,
    measures = NULL,
    methods = c("rolling", "airpls", "snip", "tophat", "minima"),
    role = NA,
    trained = FALSE,
    selected_method = NULL,
    skip = FALSE,
    id = recipes::rand_id("measure_baseline_auto")) {

  valid_methods <- c("rolling", "airpls", "arpls", "snip", "tophat", "morph", "minima")
  if (!all(methods %in% valid_methods)) {
    invalid <- setdiff(methods, valid_methods)
    cli::cli_abort("Invalid methods: {.val {invalid}}")
  }

  recipes::add_step(
    recipe,
    step_measure_baseline_auto_new(
      measures = measures,
      methods = methods,
      role = role,
      trained = trained,
      selected_method = selected_method,
      skip = skip,
      id = id
    )
  )
}

step_measure_baseline_auto_new <- function(
    measures, methods, role, trained, selected_method, skip, id) {
  recipes::step(
    subclass = "measure_baseline_auto",
    measures = measures,
    methods = methods,
    role = role,
    trained = trained,
    selected_method = selected_method,
    skip = skip,
    id = id
  )
}

#' Analyze signal to select best baseline method
#' @noRd
.select_baseline_method <- function(measures_list, candidate_methods) {
  # Get first few samples for analysis
  n_samples <- min(5, length(measures_list))
  sample_data <- measures_list[seq_len(n_samples)]

  # Compute signal characteristics
  avg_noise <- mean(vapply(sample_data, function(m) {
    # Estimate noise from high-frequency component
    d2 <- diff(diff(m$value))
    stats::mad(d2, na.rm = TRUE)
  }, numeric(1)))

  avg_range <- mean(vapply(sample_data, function(m) {
    diff(range(m$value, na.rm = TRUE))
  }, numeric(1)))

  # Estimate baseline curvature
  avg_curvature <- mean(vapply(sample_data, function(m) {
    n <- length(m$value)
    # Fit low-order polynomial and measure deviation
    x <- seq_len(n)
    fit <- stats::lm(m$value ~ poly(x, 2))
    stats::sd(stats::residuals(fit))
  }, numeric(1)))

  # Compute signal-to-noise ratio

  snr <- avg_range / (avg_noise + 1e-10)

  # Decision logic
  if (snr < 5) {
    # Low SNR: use smooth methods
    if ("rolling" %in% candidate_methods) return("rolling")
    if ("airpls" %in% candidate_methods) return("airpls")
  }

  if (avg_curvature > avg_range * 0.3) {
    # High curvature: use adaptive methods
    if ("airpls" %in% candidate_methods) return("airpls")
    if ("arpls" %in% candidate_methods) return("arpls")
  }

  if (snr > 20) {
    # High SNR with sharp peaks: morphological
    if ("tophat" %in% candidate_methods) return("tophat")
    if ("snip" %in% candidate_methods) return("snip")
  }

  # Default: minima interpolation or rolling
  if ("minima" %in% candidate_methods) return("minima")
  if ("rolling" %in% candidate_methods) return("rolling")

  # Fallback
  candidate_methods[1]
}

#' @export
prep.step_measure_baseline_auto <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  # Select best method based on training data
  selected <- .select_baseline_method(training[[measure_cols[1]]], x$methods)

  step_measure_baseline_auto_new(
    measures = measure_cols,
    methods = x$methods,
    role = x$role,
    trained = TRUE,
    selected_method = selected,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_baseline_auto <- function(object, new_data, ...) {
  method <- object$selected_method

  for (col in object$measures) {
    new_data[[col]] <- purrr::map(new_data[[col]], function(m) {
      baseline <- switch(method,
        "rolling" = .rolling_ball_baseline(m$value, 100, 50),
        "airpls" = .airpls_baseline(m$value, 1e5, 50, 1e-3),
        "arpls" = .arpls_baseline(m$value, 1e5, 0.001, 50, 1e-3),
        "snip" = .snip_baseline(m$value, 40, TRUE),
        "tophat" = .morph_open(m$value, 50),
        "morph" = {
          b <- m$value
          for (i in 1:10) b <- .morph_open(b, 50)
          b
        },
        "minima" = {
          idx <- .find_local_minima(m$value, 50)
          stats::approx(m$location[idx], m$value[idx], xout = m$location)$y
        }
      )
      m$value <- m$value - baseline
      m
    })
    new_data[[col]] <- new_measure_list(new_data[[col]])
  }

  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_baseline_auto <- function(x, width = max(20, options()$width - 30), ...) {
  if (x$trained) {
    title <- paste0("Auto baseline (selected: ", x$selected_method, ")")
    cat(title, " on <internal measurements>", sep = "")
  } else {
    cat("Auto baseline (methods: ", paste(x$methods, collapse = ", "), ")", sep = "")
  }
  cat("\n")
  invisible(x)
}

#' @rdname tidy.recipe
#' @export
#' @keywords internal
tidy.step_measure_baseline_auto <- function(x, ...) {
  tibble::tibble(
    terms = if (is_trained(x)) x$measures else "<all measure columns>",
    methods = list(x$methods),
    selected_method = x$selected_method %||% NA_character_,
    id = x$id
  )
}
