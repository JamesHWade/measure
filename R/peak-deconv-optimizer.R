# ==============================================================================
# Peak Deconvolution Optimizers
#
# Multiple optimization strategies for fitting peak models to data.
# ==============================================================================

#' Optimize Peak Deconvolution
#'
#' Finds optimal parameters for a set of peak models by minimizing the
#' sum of squared residuals between the observed and fitted values.
#'
#' @param x Numeric vector of x-axis values (e.g., retention time, wavelength).
#' @param y Numeric vector of observed y-axis values.
#' @param models List of `peak_model` objects, one per peak.
#' @param init_params List of initial parameter lists, one per peak.
#' @param optimizer Optimization method: `"auto"`, `"lbfgsb"`, `"multistart"`,
#'   or `"nelder_mead"`.
#' @param max_iter Maximum number of iterations.
#' @param tol Convergence tolerance.
#' @param constrain_positions Logical. If `TRUE`, enforce that peak centers
#'   maintain their relative ordering.
#' @param ... Additional arguments passed to specific optimizers.
#'
#' @return A list containing:
#'   - `parameters`: List of optimized parameter lists
#'   - `fitted_values`: Numeric vector of fitted y values
#'   - `residuals`: Numeric vector of residuals
#'   - `convergence`: Logical indicating convergence
#'   - `n_iterations`: Number of iterations used
#'   - `final_value`: Final objective function value (SSE)
#'   - `optimizer`: Name of optimizer used
#'   - `elapsed_time`: Optimization time in seconds
#'
#' @examples
#' # Create synthetic data with two overlapping Gaussian peaks
#' x <- seq(0, 20, by = 0.1)
#' true_y <- 1.5 * exp(-0.5 * ((x - 8) / 1)^2) +
#'   0.8 * exp(-0.5 * ((x - 12) / 1.5)^2)
#' y <- true_y + rnorm(length(x), sd = 0.05)
#'
#' # Set up models and initial guesses
#' models <- list(gaussian_peak_model(), gaussian_peak_model())
#' init_params <- list(
#'   list(height = 1.2, center = 7.5, width = 1.2),
#'   list(height = 0.6, center = 12.5, width = 1.8)
#' )
#'
#' # Optimize
#' result <- optimize_deconvolution(x, y, models, init_params)
#' print(result$parameters)
#'
#' @family peak-deconvolution
#' @export
optimize_deconvolution <- function(
  x,
  y,
  models,
  init_params,
  optimizer = "auto",
  max_iter = 1000L,
  tol = 1e-6,
  constrain_positions = TRUE,
  ...
) {
  # Validate inputs
  if (!is.numeric(x) || !is.numeric(y)) {
    cli::cli_abort("{.arg x} and {.arg y} must be numeric vectors.")
  }
  if (length(x) != length(y)) {
    cli::cli_abort("{.arg x} and {.arg y} must have the same length.")
  }
  if (!is.list(models) || length(models) == 0) {
    cli::cli_abort(
      "{.arg models} must be a non-empty list of peak_model objects."
    )
  }
  if (length(models) != length(init_params)) {
    cli::cli_abort(
      "Number of models ({length(models)}) must match number of initial parameter sets ({length(init_params)})."
    )
  }

  # Validate all models

  for (i in seq_along(models)) {
    if (!is_peak_model(models[[i]])) {
      cli::cli_abort("Element {i} of {.arg models} is not a peak_model object.")
    }
  }

  n_peaks <- length(models)
  n_total_params <- sum(vapply(models, function(m) m$n_params, integer(1)))

  # Auto-select optimizer if requested
  if (optimizer == "auto") {
    optimizer <- .select_optimizer(n_peaks, n_total_params, y)
  }

  # Validate optimizer choice
  valid_optimizers <- c("lbfgsb", "multistart", "nelder_mead")
  if (!optimizer %in% valid_optimizers) {
    cli::cli_abort(c(
      "Unknown optimizer: {.val {optimizer}}.",
      "i" = "Valid options: {.val {valid_optimizers}}"
    ))
  }

  # Dispatch to specific optimizer
  result <- switch(
    optimizer,
    "lbfgsb" = .opt_lbfgsb(
      x,
      y,
      models,
      init_params,
      max_iter,
      tol,
      constrain_positions
    ),
    "multistart" = .opt_multistart(
      x,
      y,
      models,
      init_params,
      max_iter,
      tol,
      constrain_positions,
      ...
    ),
    "nelder_mead" = .opt_nelder_mead(
      x,
      y,
      models,
      init_params,
      max_iter,
      tol
    )
  )

  result
}


#' Auto-Select Best Optimizer
#'
#' @param n_peaks Number of peaks
#' @param n_params Total number of parameters
#' @param y Response data (for SNR estimation)
#'
#' @return Character name of recommended optimizer
#' @noRd
.select_optimizer <- function(n_peaks, n_params, y) {
  # Estimate signal-to-noise ratio
  signal_max <- max(abs(y))
  noise_est <- stats::mad(diff(y)) / sqrt(2)
  snr <- if (noise_est > 0) signal_max / noise_est else Inf

  # Decision logic
  if (n_peaks <= 2 && snr > 10) {
    # Simple problem with good SNR -> fast L-BFGS-B
    "lbfgsb"
  } else if (n_peaks <= 5 && snr > 3) {
    # Moderate complexity -> multi-start for robustness
    "multistart"
  } else {
    # Complex or noisy -> multi-start with more starts
    "multistart"
  }
}


# ==============================================================================
# L-BFGS-B Optimizer
# ==============================================================================

#' L-BFGS-B Optimization (Fast, Local)
#'
#' @noRd
.opt_lbfgsb <- function(
  x,
  y,
  models,
  init_params,
  max_iter,
  tol,
  constrain_positions
) {
  # Flatten parameters into vector
  param_vec <- .flatten_params(models, init_params)

  # Set up bounds
  bounds <- .compute_bounds(
    models,
    init_params,
    range(x),
    range(y),
    constrain_positions
  )

  # Define objective function (sum of squared errors)
  objective <- function(params) {
    params_list <- .unflatten_params(params, models)
    y_pred <- sum_peak_models(x, models, params_list)
    sum((y - y_pred)^2)
  }

  # Run optimization
  start_time <- Sys.time()

  opt_result <- stats::optim(
    par = param_vec,
    fn = objective,
    method = "L-BFGS-B",
    lower = bounds$lower,
    upper = bounds$upper,
    control = list(
      maxit = max_iter,
      factr = tol / .Machine$double.eps
    )
  )

  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  # Unpack results
  final_params <- .unflatten_params(opt_result$par, models)
  y_fitted <- sum_peak_models(x, models, final_params)
  residuals <- y - y_fitted

  list(
    parameters = final_params,
    fitted_values = y_fitted,
    residuals = residuals,
    convergence = opt_result$convergence == 0,
    n_iterations = as.integer(opt_result$counts["function"]),
    final_value = opt_result$value,
    optimizer = "lbfgsb",
    elapsed_time = elapsed
  )
}


# ==============================================================================
# Multi-Start Optimizer
# ==============================================================================

#' Multi-Start L-BFGS-B (Robust to Local Minima)
#'
#' @param n_starts Number of random starts (default 5)
#' @noRd
.opt_multistart <- function(
  x,
  y,
  models,
  init_params,
  max_iter,
  tol,
  constrain_positions,
  n_starts = 5L,
  ...
) {
  best_result <- NULL
  best_value <- Inf

  for (i in seq_len(n_starts)) {
    # First run uses original initialization, subsequent use perturbed
    if (i == 1L) {
      perturbed_params <- init_params
    } else {
      perturbed_params <- .perturb_params(init_params, scale = 0.2 * i)
    }

    # Run L-BFGS-B
    result <- tryCatch(
      .opt_lbfgsb(
        x,
        y,
        models,
        perturbed_params,
        max_iter,
        tol,
        constrain_positions
      ),
      error = function(e) NULL
    )

    if (!is.null(result) && result$final_value < best_value) {
      best_value <- result$final_value
      best_result <- result
    }
  }

  if (is.null(best_result)) {
    cli::cli_abort("All optimization starts failed.")
  }

  best_result$optimizer <- "multistart"
  best_result$n_starts <- n_starts

  best_result
}


# ==============================================================================
# Nelder-Mead Optimizer
# ==============================================================================

#' Nelder-Mead Optimization (Derivative-Free)
#'
#' @noRd
.opt_nelder_mead <- function(
  x,
  y,
  models,
  init_params,
  max_iter,
  tol
) {
  # Flatten parameters
  param_vec <- .flatten_params(models, init_params)
  bounds <- .compute_bounds(
    models,
    init_params,
    range(x),
    range(y),
    constrain_positions = FALSE
  )

  # Define objective function
  objective <- function(params) {
    params_clamped <- pmin(pmax(params, bounds$lower), bounds$upper)
    params_list <- .unflatten_params(params_clamped, models)
    y_pred <- sum_peak_models(x, models, params_list)
    sum((y - y_pred)^2)
  }

  # Run optimization
  start_time <- Sys.time()

  opt_result <- stats::optim(
    par = param_vec,
    fn = objective,
    method = "Nelder-Mead",
    control = list(
      maxit = max_iter,
      reltol = tol
    )
  )

  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  # Unpack results
  final_params <- .unflatten_params(opt_result$par, models)
  y_fitted <- sum_peak_models(x, models, final_params)
  residuals <- y - y_fitted

  list(
    parameters = final_params,
    fitted_values = y_fitted,
    residuals = residuals,
    convergence = opt_result$convergence == 0,
    n_iterations = as.integer(opt_result$counts["function"]),
    final_value = opt_result$value,
    optimizer = "nelder_mead",
    elapsed_time = elapsed
  )
}


# ==============================================================================
# Parameter Manipulation Utilities
# ==============================================================================

#' Flatten Parameter Lists into Vector
#'
#' @noRd
.flatten_params <- function(models, params_list) {
  unlist(lapply(seq_along(models), function(i) {
    unlist(params_list[[i]][models[[i]]$param_names])
  }))
}

#' Unflatten Parameter Vector into List
#'
#' @noRd
.unflatten_params <- function(param_vec, models) {
  params_list <- vector("list", length(models))
  idx <- 1L

  for (i in seq_along(models)) {
    n_params <- models[[i]]$n_params
    param_names <- models[[i]]$param_names

    params_list[[i]] <- as.list(param_vec[idx:(idx + n_params - 1L)])
    names(params_list[[i]]) <- param_names

    idx <- idx + n_params
  }

  params_list
}

#' Compute Parameter Bounds
#'
#' @noRd
.compute_bounds <- function(
  models,
  init_params,
  x_range,
  y_range,
  constrain_positions
) {
  lower_bounds <- numeric(0)
  upper_bounds <- numeric(0)

  for (i in seq_along(models)) {
    bounds <- peak_model_bounds(models[[i]], x_range, y_range)
    lower_bounds <- c(lower_bounds, bounds$lower)
    upper_bounds <- c(upper_bounds, bounds$upper)

    # Enforce position ordering if requested
    if (constrain_positions && i > 1L) {
      pnames <- models[[i]]$param_names
      center_idx <- which(pnames == "center")

      if (length(center_idx) > 0L) {
        prev_center <- init_params[[i - 1L]][["center"]]
        if (!is.null(prev_center)) {
          global_idx <- sum(vapply(
            models[seq_len(i - 1L)],
            function(m) m$n_params,
            integer(1)
          )) +
            center_idx

          lower_bounds[global_idx] <- max(
            lower_bounds[global_idx],
            prev_center + 0.01
          )
        }
      }
    }
  }

  list(lower = lower_bounds, upper = upper_bounds)
}

#' Perturb Parameters for Multi-Start
#'
#' @noRd
.perturb_params <- function(params_list, scale = 0.2) {
  lapply(params_list, function(params) {
    lapply(params, function(p) {
      p * (1 + stats::rnorm(1, 0, scale))
    })
  })
}
