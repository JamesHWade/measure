#' Asymmetric Least Squares (ALS) Baseline Correction
#'
#' `step_measure_baseline_als()` creates a *specification* of a recipe step that
#' applies Asymmetric Least Squares baseline correction to measurement data.
#' ALS iteratively fits a smooth baseline giving less weight to points above
#' the baseline (peaks).
#'
#' @param recipe A recipe object. The step will be added to the sequence of
#'   operations for this recipe.
#' @param measures An optional character vector of measure column names to
#'   process. If `NULL` (the default), all measure columns (columns with class
#'   `measure_list`) will be processed.
#' @param lambda Smoothness parameter (2nd derivative constraint). Higher values
#'   produce smoother baselines. Default is `1e6`. Typical range is 1e3 to 1e9.
#'   Tunable via [baseline_lambda()].
#' @param p Asymmetry parameter controlling weight for positive residuals.
#'   Values near 0 (e.g., 0.001-0.05) work well for spectra with peaks above
#'   baseline. Default is `0.01`. Tunable via [baseline_asymmetry()].
#' @param max_iter Maximum number of iterations. Default is `20`.
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
#' Asymmetric Least Squares (ALS) baseline correction uses a Whittaker smoother
#' with asymmetric weights to fit a baseline that follows the lower envelope of
#' the spectrum. The algorithm iteratively:
#'
#' 1
#'
#' . Fits a smooth baseline using penalized least squares
#' 2. Calculates residuals (spectrum - baseline)
#' 3. Assigns weights: `p` for positive residuals (peaks), `1-p` for negative
#' 4. Repeats until convergence or max iterations
#'
#' The smoothness is controlled by `lambda`, which penalizes the second
#' derivative of the baseline. Larger `lambda` produces smoother baselines.
#'
#' ALS is particularly effective for:
#' - NIR/IR spectroscopy with broad baseline drift
#' - Raman spectroscopy with fluorescence background
#' - UV-Vis spectroscopy with scattering effects
#'
#' **No selectors should be supplied to this step function**. The data should be
#' in the internal format produced by [step_measure_input_wide()] or
#' [step_measure_input_long()].
#'
#' # Tidying
#'
#' When you [`tidy()`][recipes::tidy.recipe()] this step, a tibble with columns
#' `terms`, `lambda`, `p`, and `id` is returned.
#'
#' # Tuning
#'
#' This step has parameters that can be tuned:
#' - `lambda`: Use [baseline_lambda()] (log10 scale recommended)
#' - `p`: Use [baseline_asymmetry()]
#'
#' @references
#' Eilers, P.H.C. and Boelens, H.F.M. (2005). Baseline Correction with
#' Asymmetric Least Squares Smoothing. Leiden University Medical Centre report.
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
#'   step_measure_baseline_als(lambda = 1e6, p = 0.01) |>
#'   prep()
#'
#' bake(rec, new_data = NULL)
step_measure_baseline_als <- function(
    recipe,
    measures = NULL,
    lambda = 1e6,
    p = 0.01,
    max_iter = 20L,
    role = NA,
    trained = FALSE,
    skip = FALSE,
    id = recipes::rand_id("measure_baseline_als")) {
  recipes::add_step(
    recipe,
    step_measure_baseline_als_new(
      measures = measures,
      lambda = lambda,
      p = p,
      max_iter = max_iter,
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_measure_baseline_als_new <- function(
    measures, lambda, p, max_iter, role, trained, skip, id) {
  recipes::step(
    subclass = "measure_baseline_als",
    measures = measures,
    lambda = lambda,
    p = p,
    max_iter = max_iter,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_baseline_als <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  # Validate parameters
  if (!is.numeric(x$lambda) || length(x$lambda) != 1 || x$lambda <= 0) {
    cli::cli_abort(
      "{.arg lambda} must be a single positive number, not {.val {x$lambda}}."
    )
  }
  if (!is.numeric(x$p) || length(x$p) != 1 || x$p <= 0 || x$p >= 1) {
    cli::cli_abort(
      "{.arg p} must be between 0 and 1 (exclusive), not {.val {x$p}}."
    )
  }
  if (!is.numeric(x$max_iter) || length(x$max_iter) != 1 || x$max_iter < 1) {
    cli::cli_abort(
      "{.arg max_iter} must be a positive integer, not {.val {x$max_iter}}."
    )
  }

  # Resolve which columns to process
  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  step_measure_baseline_als_new(
    measures = measure_cols,
    lambda = x$lambda,
    p = x$p,
    max_iter = as.integer(x$max_iter),
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_baseline_als <- function(object, new_data, ...) {
  for (col in object$measures) {
    result <- .compute_baseline_als(
      new_data[[col]],
      lambda = object$lambda,
      p = object$p,
      max_iter = object$max_iter
    )
    new_data[[col]] <- new_measure_list(result)
  }
  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_baseline_als <- function(
    x,
    width = max(20, options()$width - 30),
    ...) {
  title <- "ALS baseline correction on "

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
tidy.step_measure_baseline_als <- function(x, ...) {
  if (recipes::is_trained(x)) {
    terms_val <- x$measures
  } else {
    terms_val <- "<all measure columns>"
  }
  tibble::tibble(
    terms = terms_val,
    lambda = x$lambda,
    p = x$p,
    id = x$id
  )
}

#' @export
required_pkgs.step_measure_baseline_als <- function(x, ...) {
  "measure"
}

# ------------------------------------------------------------------------------
# Internal computation

#' Compute ALS baseline correction for a measure_list
#'
#' @param dat A measure_list (list of measure_tbl objects).
#' @param lambda Smoothness parameter.
#' @param p Asymmetry parameter.
#' @param max_iter Maximum iterations.
#' @return A list of measure_tbl objects with baseline-corrected values.
#' @noRd
.compute_baseline_als <- function(dat, lambda, p, max_iter) {
  purrr::map(dat, .als_single, lambda = lambda, p = p, max_iter = max_iter)
}

#' Apply ALS baseline correction to a single spectrum
#'
#' Implements the Asymmetric Least Squares algorithm using a sparse
#' Whittaker smoother for efficiency.
#'
#' @param x A measure_tbl with location and value columns.
#' @param lambda Smoothness parameter.
#' @param p Asymmetry parameter.
#' @param max_iter Maximum iterations.
#' @return A measure_tbl with baseline-corrected values.
#' @noRd
.als_single <- function(x, lambda, p, max_iter) {
  y <- x$value
  n <- length(y)

  # Handle edge cases
  if (n < 3) {
    cli::cli_warn("Spectrum has fewer than 3 points; returning unchanged.")
    return(x)
  }

  if (all(is.na(y))) {
    cli::cli_warn("Spectrum is all NA; returning unchanged.")
    return(x)
  }

  # Build the second-difference matrix D (sparse-like using band structure)
  # D is (n-2) x n, D[i,] has [1, -2, 1] at positions [i, i+1, i+2]
  # We compute D'D directly as a banded matrix for efficiency

  # Initialize weights (all equal initially)
  w <- rep(1, n)

  # Precompute D'D as a sparse tridiagonal-like structure
  # D'D is pentadiagonal with bandwidth 2
  # For efficiency, we solve the system iteratively

  baseline <- y  # Initial baseline estimate

  for (iter in seq_len(max_iter)) {
    # Solve: (W + lambda * D'D) * z = W * y
    # Using the sparse Whittaker smoother approach
    baseline_new <- .whittaker_smooth(y, w, lambda)

    # Check convergence
    if (max(abs(baseline_new - baseline)) < 1e-6 * (max(abs(y)) + 1e-10)) {
      baseline <- baseline_new
      break
    }

    baseline <- baseline_new

    # Update weights asymmetrically
    residuals <- y - baseline
    w <- ifelse(residuals > 0, p, 1 - p)
  }

  # Subtract baseline from original signal
  x$value <- y - baseline
  x
}

#' Whittaker smoother with weights
#'
#' Solves the penalized least squares problem:
#' minimize: sum(w * (y - z)^2) + lambda * sum((D*z)^2)
#'
#' Uses Cholesky decomposition of the banded system for efficiency.
#'
#' @param y Signal vector.
#' @param w Weight vector.
#' @param lambda Smoothness parameter.
#' @return Smoothed signal (baseline estimate).
#' @noRd
.whittaker_smooth <- function(y, w, lambda) {
  n <- length(y)

  # Build D'D (second difference penalty matrix)
  # D'D is pentadiagonal: main diagonal and +-1, +-2 off-diagonals
  # We'll construct and solve using base R's sparse-aware methods

  # Create the banded matrix W + lambda * D'D
  # D'D has the pattern from second differences

  # For n points, D is (n-2) x n
  # D'D[i,j] = sum over k of D[k,i]*D[k,j]

  # Direct computation of D'D diagonal bands:
  # Main diagonal: 1, 5, 6, 6, ..., 6, 5, 1 pattern scaled
  # But simpler: use the diff matrix approach

  # Efficient implementation using base R
  # Construct the system matrix as full (for moderate n) or banded

  if (n <= 2000) {
    # For moderate sizes, use direct solve
    # Build D (second difference operator)
    D <- matrix(0, nrow = n - 2, ncol = n)
    for (i in seq_len(n - 2)) {
      D[i, i] <- 1
      D[i, i + 1] <- -2
      D[i, i + 2] <- 1
    }

    # System matrix: W + lambda * D'D
    W <- diag(w)
    DtD <- crossprod(D)
    A <- W + lambda * DtD

    # Right-hand side: W * y
    b <- w * y

    # Solve using Cholesky (A is positive definite)
    z <- tryCatch(
      solve(A, b),
      error = function(e) {
        # Fallback: add small ridge for numerical stability
        solve(A + 1e-10 * diag(n), b)
      }
    )

    return(as.vector(z))
  } else {
    # For large n, use iterative approach or sparse methods
    # Simplified: use a banded solver approximation
    # This is a fallback - most spectral data is < 2000 points
    cli::cli_warn(
      "Spectrum has {n} points; using simplified baseline for large spectra."
    )

    # Use a simple iterative smoothing as fallback
    z <- y
    for (i in 1:50) {
      z_new <- (w * y + lambda * c(0, z[-n]) + lambda * c(z[-1], 0)) /
        (w + 2 * lambda)
      if (max(abs(z_new - z)) < 1e-6) break
      z <- z_new
    }
    return(z)
  }
}
