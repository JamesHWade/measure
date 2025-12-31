# ==============================================================================
# Extended Scatter Correction Methods
#
# This file contains:
# - step_measure_emsc: Extended Multiplicative Scatter Correction
# - step_measure_osc: Orthogonal Signal Correction
# ==============================================================================

# ==============================================================================
# step_measure_emsc - Extended Multiplicative Scatter Correction
# ==============================================================================

#' Extended Multiplicative Scatter Correction (EMSC)
#'
#' `step_measure_emsc()` creates a *specification* of a recipe step that applies
#' Extended Multiplicative Scatter Correction to spectral data. EMSC accounts
#' for wavelength-dependent scatter effects using polynomial terms.
#'
#' @param recipe A recipe object.
#' @param degree Polynomial degree for wavelength-dependent terms. Default is 2.
#'   Higher values can model more complex scatter effects but risk overfitting.
#' @param reference Reference spectrum method: `"mean"` (default) or `"median"`.
#'   Alternatively, a numeric vector can be supplied as the reference spectrum.
#' @param measures An optional character vector of measure column names.
#' @param role Not used.
#' @param trained Logical indicating if the step has been trained.
#' @param ref_spectrum The learned reference spectrum (after training).
#' @param locations The location values for polynomial terms (after training).
#' @param skip Logical. Should the step be skipped when baking?
#' @param id Unique step identifier.
#'
#' @return An updated recipe with the new step added.
#'
#' @details
#' Extended MSC (EMSC) extends standard MSC by modeling wavelength-dependent
#' scatter effects. For a spectrum \eqn{x_i} and reference \eqn{x_r}, the model is:
#'
#' \deqn{x_i = a_i + b_i \cdot x_r + c_i \cdot \lambda + d_i \cdot \lambda^2 + ... + \epsilon}
#'
#' The corrected spectrum is:
#'
#' \deqn{EMSC(x_i) = \frac{x_i - a_i - c_i \cdot \lambda - d_i \cdot \lambda^2 - ...}{b_i}}
#'
#' The polynomial terms (\eqn{\lambda}, \eqn{\lambda^2}, etc.) account for
#' wavelength-dependent baseline effects that vary between samples.
#'
#' **When to use EMSC vs MSC:**
#' - Use MSC for simple additive/multiplicative scatter
#' - Use EMSC when scatter effects vary with wavelength
#' - Start with degree=2, increase if needed for complex scatter
#'
#' @family measure-preprocessing
#' @seealso [step_measure_msc()] for standard MSC
#' @export
#'
#' @examples
#' library(recipes)
#'
#' rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_emsc(degree = 2) |>
#'   prep()
#'
#' bake(rec, new_data = NULL)
step_measure_emsc <- function(
  recipe,
  degree = 2L,
  reference = "mean",
  measures = NULL,
  role = NA,
  trained = FALSE,
  ref_spectrum = NULL,
  locations = NULL,
  skip = FALSE,
  id = recipes::rand_id("measure_emsc")
) {
  if (
    !is.numeric(degree) ||
      length(degree) != 1 ||
      degree < 0 ||
      degree != round(degree)
  ) {
    cli::cli_abort("{.arg degree} must be a non-negative integer.")
  }

  # Validate reference
  if (!is.numeric(reference) && !reference %in% c("mean", "median")) {
    cli::cli_abort(
      "{.arg reference} must be 'mean', 'median', or a numeric vector."
    )
  }

  recipes::add_step(
    recipe,
    step_measure_emsc_new(
      degree = as.integer(degree),
      reference = reference,
      measures = measures,
      role = role,
      trained = trained,
      ref_spectrum = ref_spectrum,
      locations = locations,
      skip = skip,
      id = id
    )
  )
}

step_measure_emsc_new <- function(
  degree,
  reference,
  measures,
  role,
  trained,
  ref_spectrum,
  locations,
  skip,
  id
) {
  recipes::step(
    subclass = "measure_emsc",
    degree = degree,
    reference = reference,
    measures = measures,
    role = role,
    trained = trained,
    ref_spectrum = ref_spectrum,
    locations = locations,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_emsc <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  # Compute reference spectrum
  measure_col <- measure_cols[1]
  mat <- measure_to_matrix(training[[measure_col]])

  if (is.numeric(x$reference)) {
    ref_spectrum <- x$reference
    if (length(ref_spectrum) != ncol(mat)) {
      cli::cli_abort(
        "Reference spectrum length ({length(ref_spectrum)}) does not match \\
        data dimension ({ncol(mat)})."
      )
    }
  } else if (x$reference == "mean") {
    ref_spectrum <- colMeans(mat, na.rm = TRUE)
  } else if (x$reference == "median") {
    ref_spectrum <- apply(mat, 2, stats::median, na.rm = TRUE)
  }

  # Get locations for polynomial terms
  locations <- training[[measure_col]][[1]]$location

  step_measure_emsc_new(
    degree = x$degree,
    reference = x$reference,
    measures = measure_cols,
    role = x$role,
    trained = TRUE,
    ref_spectrum = ref_spectrum,
    locations = locations,
    skip = x$skip,
    id = x$id
  )
}

#' Apply EMSC to a single spectrum
#' @noRd
.emsc_single <- function(x, ref_spectrum, locations, degree) {
  values <- x$value
  n <- length(values)

  # Build design matrix: [1, ref_spectrum, lambda, lambda^2, ...]
  # Normalize locations to [0, 1] for numerical stability
  loc_min <- min(locations)
  loc_max <- max(locations)
  loc_norm <- (locations - loc_min) / (loc_max - loc_min + .Machine$double.eps)

  # Design matrix
  X <- cbind(1, ref_spectrum)

  if (degree > 0) {
    for (d in seq_len(degree)) {
      X <- cbind(X, loc_norm^d)
    }
  }

  # Fit linear model: values = X %*% beta + epsilon
  fit <- tryCatch(
    stats::lm.fit(X, values),
    error = function(e) NULL
  )

  if (is.null(fit) || anyNA(fit$coefficients)) {
    cli::cli_warn(
      "EMSC fitting failed for a spectrum. Returning centered values."
    )
    x$value <- values - mean(values, na.rm = TRUE)
    return(x)
  }

  coeffs <- fit$coefficients
  intercept <- coeffs[1]
  slope <- coeffs[2]

  if (is.na(slope) || abs(slope) < .Machine$double.eps) {
    cli::cli_warn(
      "EMSC multiplicative coefficient is zero. Returning centered values."
    )
    x$value <- values - intercept
    return(x)
  }

  # Subtract polynomial baseline and divide by slope
  # EMSC(x) = (x - intercept - c1*λ - c2*λ² - ...) / slope
  polynomial_contribution <- 0
  if (degree > 0) {
    for (d in seq_len(degree)) {
      polynomial_contribution <- polynomial_contribution +
        coeffs[2 + d] * loc_norm^d
    }
  }

  x$value <- (values - intercept - polynomial_contribution) / slope
  x
}

#' @export
bake.step_measure_emsc <- function(object, new_data, ...) {
  ref_spectrum <- object$ref_spectrum
  locations <- object$locations
  degree <- object$degree

  for (col in object$measures) {
    result <- purrr::map(new_data[[col]], function(m) {
      .emsc_single(m, ref_spectrum, locations, degree)
    })
    new_data[[col]] <- new_measure_list(result)
  }

  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_emsc <- function(
  x,
  width = max(20, options()$width - 30),
  ...
) {
  title <- paste0("EMSC (degree = ", x$degree, ")")
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
tidy.step_measure_emsc <- function(x, ...) {
  tibble::tibble(
    degree = x$degree,
    reference = if (is.numeric(x$reference)) "custom" else x$reference,
    id = x$id
  )
}

#' @rdname required_pkgs.recipe
#' @export
#' @keywords internal
required_pkgs.step_measure_emsc <- function(x, ...) {
  c("measure")
}

#' @rdname tunable_measure
#' @export
tunable.step_measure_emsc <- function(x, ...) {
  tibble::tibble(
    name = "degree",
    call_info = list(
      list(pkg = "measure", fun = "emsc_degree")
    ),
    source = "recipe",
    component = "step_measure_emsc",
    component_id = x$id
  )
}

# ==============================================================================
# step_measure_osc - Orthogonal Signal Correction
# ==============================================================================

#' Orthogonal Signal Correction (OSC)
#'
#' `step_measure_osc()` creates a *specification* of a recipe step that applies
#' Orthogonal Signal Correction to remove variation orthogonal to the outcome.
#'
#' @param recipe A recipe object.
#' @param n_components Number of orthogonal components to remove. Default is 1.
#' @param tolerance Convergence tolerance for NIPALS algorithm. Default is 1e-6.
#' @param max_iter Maximum iterations for NIPALS. Default is 100.
#' @param measures An optional character vector of measure column names.
#' @param role Not used.
#' @param trained Logical indicating if the step has been trained.
#' @param weights The learned orthogonal weights (after training).
#' @param loadings The learned orthogonal loadings (after training).
#' @param skip Logical. Should the step be skipped when baking?
#' @param id Unique step identifier.
#'
#' @return An updated recipe with the new step added.
#'
#' @details
#' Orthogonal Signal Correction (OSC) removes variation in X that is orthogonal
#' to Y (the outcome). This is useful for removing systematic variation that
#' is not related to the response.
#'
#' **Algorithm:**
#' 1. Compute initial score t from Y using SVD
#' 2. Orthogonalize t with respect to Y
#' 3. Iterate NIPALS to find orthogonal components
#' 4. Remove orthogonal components from X
#'
#' **Important:**
#' - The recipe must have at least one outcome variable with role "outcome"
#' - Outcomes are automatically detected from the recipe's role definitions
#' - Multiple outcomes are supported (multivariate Y)
#'
#' OSC was originally described by Wold et al. (1998) for NIR spectroscopy.
#'
#' @references
#' Wold, S., Antti, H., Lindgren, F., and Ohman, J. (1998). Orthogonal signal
#' correction of near-infrared spectra. Chemometrics and Intelligent Laboratory
#' Systems, 44(1-2), 175-185.
#'
#' @family measure-preprocessing
#' @export
#'
#' @examples
#' library(recipes)
#'
#' rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_osc(n_components = 2) |>
#'   prep()
#'
#' bake(rec, new_data = NULL)
step_measure_osc <- function(
  recipe,
  n_components = 1L,
  tolerance = 1e-6,
  max_iter = 100L,
  measures = NULL,
  role = NA,
  trained = FALSE,
  weights = NULL,
  loadings = NULL,
  skip = FALSE,
  id = recipes::rand_id("measure_osc")
) {
  if (
    !is.numeric(n_components) ||
      length(n_components) != 1 ||
      n_components < 1 ||
      n_components != round(n_components)
  ) {
    cli::cli_abort("{.arg n_components} must be a positive integer.")
  }

  if (!is.numeric(tolerance) || length(tolerance) != 1 || tolerance <= 0) {
    cli::cli_abort("{.arg tolerance} must be a positive number.")
  }

  if (
    !is.numeric(max_iter) ||
      length(max_iter) != 1 ||
      max_iter < 1 ||
      max_iter != round(max_iter)
  ) {
    cli::cli_abort("{.arg max_iter} must be a positive integer.")
  }

  recipes::add_step(
    recipe,
    step_measure_osc_new(
      n_components = as.integer(n_components),
      tolerance = tolerance,
      max_iter = as.integer(max_iter),
      measures = measures,
      role = role,
      trained = trained,
      weights = weights,
      loadings = loadings,
      skip = skip,
      id = id
    )
  )
}

step_measure_osc_new <- function(
  n_components,
  tolerance,
  max_iter,
  measures,
  role,
  trained,
  weights,
  loadings,
  skip,
  id
) {
  recipes::step(
    subclass = "measure_osc",
    n_components = n_components,
    tolerance = tolerance,
    max_iter = max_iter,
    measures = measures,
    role = role,
    trained = trained,
    weights = weights,
    loadings = loadings,
    skip = skip,
    id = id
  )
}

#' NIPALS algorithm for computing orthogonal components
#' @noRd
.nipals_osc <- function(X, Y, n_components, tolerance, max_iter) {
  # X: n x p matrix of spectra
  # Y: n x q matrix of outcomes

  n <- nrow(X)
  p <- ncol(X)

  # Store weights and loadings for each component
  all_weights <- matrix(0, nrow = p, ncol = n_components)
  all_loadings <- matrix(0, nrow = p, ncol = n_components)

  # Work with a copy of X
  X_resid <- X

  for (comp in seq_len(n_components)) {
    # Initial score: first left singular vector of X
    svd_result <- tryCatch(
      svd(X_resid, nu = 1, nv = 0),
      error = function(e) NULL
    )

    if (is.null(svd_result)) {
      cli::cli_warn("SVD failed in OSC. Stopping at component {comp - 1}.")
      break
    }

    t_old <- svd_result$u[, 1]

    # NIPALS iteration
    for (iter in seq_len(max_iter)) {
      # Orthogonalize t with respect to Y
      # t_orth = t - Y %*% (Y' Y)^-1 %*% Y' %*% t
      if (ncol(Y) == 1) {
        t_orth <- t_old - Y %*% (sum(Y * t_old) / sum(Y^2))
      } else {
        YtY_inv <- tryCatch(
          solve(t(Y) %*% Y),
          error = function(e) MASS::ginv(t(Y) %*% Y)
        )
        t_orth <- t_old - Y %*% YtY_inv %*% t(Y) %*% t_old
      }

      # Compute weight (loading) w = X' t_orth / (t_orth' t_orth)
      w <- t(X_resid) %*% t_orth / as.numeric(t(t_orth) %*% t_orth)
      w <- w / sqrt(sum(w^2) + .Machine$double.eps) # Normalize

      # Compute new score t = X w / (w' w)
      t_new <- X_resid %*% w / as.numeric(t(w) %*% w)

      # Check convergence
      diff <- sqrt(sum((t_new - t_old)^2))
      if (diff < tolerance) {
        break
      }

      t_old <- t_new
    }

    # Compute loading p = X' t / (t' t)
    p_loading <- t(X_resid) %*% t_new / as.numeric(t(t_new) %*% t_new)

    # Store
    all_weights[, comp] <- as.numeric(w)
    all_loadings[, comp] <- as.numeric(p_loading)

    # Deflate X
    X_resid <- X_resid - t_new %*% t(p_loading)
  }

  list(
    weights = all_weights,
    loadings = all_loadings
  )
}

#' @export
prep.step_measure_osc <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  # Get outcome columns from recipe info
  if (is.null(info)) {
    cli::cli_abort(
      "OSC requires outcome information from the recipe. \\
      Ensure the recipe has outcome variables defined."
    )
  }

  outcome_vars <- info$variable[info$role == "outcome"]

  if (length(outcome_vars) == 0) {
    cli::cli_abort(
      "No outcome variables found. OSC requires at least one outcome variable. \\
      Ensure your recipe formula has a left-hand side (e.g., y ~ .)."
    )
  }

  # Extract outcome matrix
  Y <- as.matrix(training[, outcome_vars, drop = FALSE])

  if (anyNA(Y)) {
    cli::cli_abort(
      "Outcome variables contain NA values. OSC cannot handle missing outcomes."
    )
  }

  # Extract spectral matrix
  X <- measure_to_matrix(training[[measure_cols[1]]])

  # Center X and Y
  X_centered <- scale(X, center = TRUE, scale = FALSE)
  Y_centered <- scale(Y, center = TRUE, scale = FALSE)

  # Compute orthogonal components
  osc_result <- .nipals_osc(
    X_centered,
    Y_centered,
    x$n_components,
    x$tolerance,
    x$max_iter
  )

  step_measure_osc_new(
    n_components = x$n_components,
    tolerance = x$tolerance,
    max_iter = x$max_iter,
    measures = measure_cols,
    role = x$role,
    trained = TRUE,
    weights = osc_result$weights,
    loadings = osc_result$loadings,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_osc <- function(object, new_data, ...) {
  weights <- object$weights
  loadings <- object$loadings
  measure_col <- object$measures[1]

  # Get the spectral matrix
  X <- measure_to_matrix(new_data[[measure_col]])

  # Center using the same center as training (stored implicitly)
  # For simplicity, we center each column to mean 0
  X_centered <- scale(X, center = TRUE, scale = FALSE)

  # Remove orthogonal components
  # X_corrected = X - sum_k(t_k * p_k')
  # where t_k = X * w_k and p_k is the loading
  for (k in seq_len(ncol(weights))) {
    w_k <- weights[, k]
    p_k <- loadings[, k]

    # Score for new data
    t_k <- X_centered %*% w_k

    # Remove this component
    X_centered <- X_centered - t_k %*% t(p_k)
  }

  # Get locations from first spectrum
  loc <- new_data[[measure_col]][[1]]$location

  # Convert back to measure list
  new_data[[measure_col]] <- new_measure_list(
    matrix_to_measure(X_centered, loc)
  )

  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_osc <- function(
  x,
  width = max(20, options()$width - 30),
  ...
) {
  title <- paste0(
    "OSC (",
    x$n_components,
    " component",
    if (x$n_components > 1) "s",
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
tidy.step_measure_osc <- function(x, ...) {
  tibble::tibble(
    n_components = x$n_components,
    tolerance = x$tolerance,
    id = x$id
  )
}

#' @rdname required_pkgs.recipe
#' @export
#' @keywords internal
required_pkgs.step_measure_osc <- function(x, ...) {
  c("measure")
}

#' @rdname tunable_measure
#' @export
tunable.step_measure_osc <- function(x, ...) {
  tibble::tibble(
    name = "n_components",
    call_info = list(
      list(pkg = "measure", fun = "osc_n_components")
    ),
    source = "recipe",
    component = "step_measure_osc",
    component_id = x$id
  )
}
