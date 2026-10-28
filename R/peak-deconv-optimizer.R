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
#' @param constrain_positions Logical. If `TRUE`, fitted centers preserve
#'   the rank of their starting centers while remaining within each model's
#'   center bounds. Duplicate starting centers and incompatible bounds are
#'   rejected.
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
  if (
    !is.logical(constrain_positions) ||
      length(constrain_positions) != 1L ||
      is.na(constrain_positions)
  ) {
    cli::cli_abort("{.arg constrain_positions} must be TRUE or FALSE.")
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
      tol,
      constrain_positions
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
  constrain_positions,
  fit_context = NULL
) {
  if (is.null(fit_context)) {
    fit_context <- .new_deconv_optimizer_context(
      models,
      init_params,
      range(x),
      range(y),
      constrain_positions
    )
  }

  param_vec <- .prepare_deconv_start(models, init_params, fit_context)
  objective <- function(params) {
    .deconv_objective(params, x, y, models, fit_context)
  }

  start_time <- Sys.time()
  opt_result <- stats::optim(
    par = param_vec,
    fn = objective,
    method = "L-BFGS-B",
    lower = fit_context$lower,
    upper = fit_context$upper,
    control = list(
      maxit = max_iter,
      factr = tol / .Machine$double.eps
    )
  )
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  final_param_vec <- .transform_deconv_params(opt_result$par, fit_context)
  final_params <- .unflatten_params(final_param_vec, models)
  y_fitted <- sum_peak_models(x, models, final_params)
  residuals <- y - y_fitted

  list(
    parameters = final_params,
    fitted_values = y_fitted,
    residuals = residuals,
    convergence = opt_result$convergence == 0,
    n_iterations = as.integer(opt_result$counts["function"]),
    final_value = sum(residuals^2),
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
  fit_context <- .new_deconv_optimizer_context(
    models,
    init_params,
    range(x),
    range(y),
    constrain_positions
  )
  best_result <- NULL
  best_value <- Inf

  for (i in seq_len(n_starts)) {
    # Keep the rank from the original initialization even when a jitter crosses.
    if (i == 1L) {
      perturbed_params <- init_params
    } else {
      perturbed_params <- .perturb_params(init_params, scale = 0.2 * i)
    }

    result <- tryCatch(
      .opt_lbfgsb(
        x,
        y,
        models,
        perturbed_params,
        max_iter,
        tol,
        constrain_positions,
        fit_context = fit_context
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
  tol,
  constrain_positions
) {
  fit_context <- .new_deconv_optimizer_context(
    models,
    init_params,
    range(x),
    range(y),
    constrain_positions
  )
  param_vec <- .prepare_deconv_start(models, init_params, fit_context)
  objective <- function(params) {
    .deconv_objective(params, x, y, models, fit_context)
  }

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

  final_param_vec <- .transform_deconv_params(opt_result$par, fit_context)
  final_params <- .unflatten_params(final_param_vec, models)
  y_fitted <- sum_peak_models(x, models, final_params)
  residuals <- y - y_fitted

  list(
    parameters = final_params,
    fitted_values = y_fitted,
    residuals = residuals,
    convergence = opt_result$convergence == 0,
    n_iterations = as.integer(opt_result$counts["function"]),
    final_value = sum(residuals^2),
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

#' Build a Shared Deconvolution Parameter Transform
#'
#' @noRd
.new_deconv_optimizer_context <- function(
  models,
  init_params,
  x_range,
  y_range,
  constrain_positions
) {
  bounds <- .compute_bounds(models, x_range, y_range)
  if (
    length(bounds$lower) != length(bounds$upper) ||
      anyNA(bounds$lower) ||
      anyNA(bounds$upper) ||
      any(bounds$lower > bounds$upper)
  ) {
    cli::cli_abort("Peak model parameter bounds must be valid and non-empty.")
  }

  offsets <- c(
    0L,
    cumsum(vapply(models, function(model) model$n_params, integer(1)))
  )
  center_components <- integer(0)
  center_slots <- integer(0)
  for (i in seq_along(models)) {
    center_index <- which(models[[i]]$param_names == "center")
    if (length(center_index) > 1L) {
      cli::cli_abort(
        "Each peak model may define at most one {.field center} parameter."
      )
    }
    if (length(center_index) == 1L) {
      center_components <- c(center_components, i)
      center_slots <- c(center_slots, offsets[[i]] + center_index)
    }
  }

  rank_slots <- integer(0)
  rank_lower <- numeric(0)
  rank_upper <- numeric(0)
  model_lower <- bounds$lower
  model_upper <- bounds$upper
  fit_lower <- model_lower
  fit_upper <- model_upper

  if (constrain_positions && length(center_slots) > 0L) {
    initial_centers <- vapply(
      center_components,
      function(i) {
        value <- init_params[[i]][["center"]]
        if (
          !is.numeric(value) ||
            length(value) != 1L ||
            !is.finite(value)
        ) {
          cli::cli_abort(
            "Constrained fitting requires one finite starting center per peak."
          )
        }
        as.numeric(value)
      },
      numeric(1)
    )

    if (anyDuplicated(initial_centers)) {
      cli::cli_abort(
        "Constrained fitting requires distinct starting centers; found duplicate starting centers."
      )
    }

    rank <- order(initial_centers, seq_along(initial_centers), method = "radix")
    rank_slots <- center_slots[rank]
    center_lower <- model_lower[rank_slots]
    center_upper <- model_upper[rank_slots]
    rank_lower <- cummax(center_lower)
    rank_upper <- rev(cummin(rev(center_upper)))

    if (any(rank_lower > rank_upper)) {
      cli::cli_abort(
        "The model-specific center bounds cannot satisfy the requested peak position order."
      )
    }
    if (any(rank_lower == rank_upper & !is.finite(rank_lower))) {
      cli::cli_abort(
        "A constrained center cannot have identical infinite lower and upper bounds."
      )
    }

    fit_lower[center_slots] <- -Inf
    fit_upper[center_slots] <- Inf
    fixed <- rank_lower == rank_upper
    fit_lower[rank_slots[fixed]] <- 0
    fit_upper[rank_slots[fixed]] <- 0
  }

  list(
    lower = fit_lower,
    upper = fit_upper,
    model_lower = model_lower,
    model_upper = model_upper,
    constrain_positions = constrain_positions,
    center_slots = center_slots,
    rank_slots = rank_slots,
    rank_lower = rank_lower,
    rank_upper = rank_upper
  )
}

.prepare_deconv_start <- function(models, init_params, fit_context) {
  param_vec <- .flatten_params(models, init_params)
  if (
    length(param_vec) != length(fit_context$lower) ||
      !all(is.finite(param_vec))
  ) {
    cli::cli_abort("Initial peak parameters must be finite and complete.")
  }

  if (!fit_context$constrain_positions) {
    return(
      pmin(
        pmax(param_vec, fit_context$model_lower),
        fit_context$model_upper
      )
    )
  }

  start <- pmin(
    pmax(param_vec, fit_context$model_lower),
    fit_context$model_upper
  )
  previous_center <- -Inf
  for (i in seq_along(fit_context$rank_slots)) {
    slot <- fit_context$rank_slots[[i]]
    lower <- max(fit_context$rank_lower[[i]], previous_center)
    upper <- fit_context$rank_upper[[i]]
    center <- min(max(start[[slot]], lower), upper)
    start[[slot]] <- .encode_ordered_center(center, lower, upper)
    previous_center <- center
  }

  pmin(pmax(start, fit_context$lower), fit_context$upper)
}

.encode_ordered_center <- function(center, lower, upper) {
  if (lower == upper) {
    return(0)
  }
  if (is.finite(lower) && is.finite(upper)) {
    fraction <- (center - lower) / (upper - lower)
    fraction <- min(max(fraction, 1e-8), 1 - 1e-8)
    return(stats::qlogis(fraction))
  }
  if (is.finite(lower)) {
    distance <- max(center - lower, 1e-8)
    return(if (distance > 30) distance else log(expm1(distance)))
  }
  if (is.finite(upper)) {
    distance <- max(upper - center, 1e-8)
    inverse <- if (distance > 30) distance else log(expm1(distance))
    return(-inverse)
  }
  center
}

.ordered_center_value <- function(latent, lower, upper) {
  if (lower == upper) {
    return(lower)
  }
  if (is.finite(lower) && is.finite(upper)) {
    fraction <- stats::plogis(max(min(latent, 36), -36))
    return(lower + (upper - lower) * fraction)
  }
  if (is.finite(lower)) {
    softplus <- if (latent > 30) latent else log1p(exp(latent))
    return(lower + softplus)
  }
  if (is.finite(upper)) {
    negative_latent <- -latent
    softplus <- if (negative_latent > 30) {
      negative_latent
    } else {
      log1p(exp(negative_latent))
    }
    return(upper - softplus)
  }
  latent
}

.transform_deconv_params <- function(param_vec, fit_context) {
  if (!fit_context$constrain_positions) {
    return(
      pmin(
        pmax(param_vec, fit_context$model_lower),
        fit_context$model_upper
      )
    )
  }

  transformed <- pmin(
    pmax(param_vec, fit_context$model_lower),
    fit_context$model_upper
  )
  transformed[fit_context$center_slots] <- param_vec[fit_context$center_slots]

  previous_center <- -Inf
  for (i in seq_along(fit_context$rank_slots)) {
    slot <- fit_context$rank_slots[[i]]
    lower <- max(fit_context$rank_lower[[i]], previous_center)
    center <- .ordered_center_value(
      param_vec[[slot]],
      lower,
      fit_context$rank_upper[[i]]
    )
    transformed[[slot]] <- center
    previous_center <- center
  }
  transformed
}

.deconv_objective <- function(param_vec, x, y, models, fit_context) {
  transformed <- .transform_deconv_params(param_vec, fit_context)
  params_list <- .unflatten_params(transformed, models)
  y_pred <- sum_peak_models(x, models, params_list)
  sum((y - y_pred)^2)
}

.compute_bounds <- function(
  models,
  x_range,
  y_range
) {
  lower_bounds <- numeric(0)
  upper_bounds <- numeric(0)

  for (i in seq_along(models)) {
    param_names <- models[[i]]$param_names
    bounds <- peak_model_bounds(models[[i]], x_range, y_range)
    lower <- bounds$lower
    upper <- bounds$upper

    if (is.null(names(lower))) {
      if (length(lower) == length(param_names)) {
        names(lower) <- param_names
      }
    } else if (all(param_names %in% names(lower))) {
      lower <- lower[param_names]
    } else {
      lower <- numeric(0)
    }

    if (is.null(names(upper))) {
      if (length(upper) == length(param_names)) {
        names(upper) <- param_names
      }
    } else if (all(param_names %in% names(upper))) {
      upper <- upper[param_names]
    } else {
      upper <- numeric(0)
    }

    if (
      !is.numeric(lower) ||
        !is.numeric(upper) ||
        length(lower) != length(param_names) ||
        length(upper) != length(param_names) ||
        anyNA(lower) ||
        anyNA(upper)
    ) {
      cli::cli_abort(
        "Bounds for peak model {.val {models[[i]]$name}} must cover all model parameters."
      )
    }

    lower_bounds <- c(lower_bounds, as.numeric(lower))
    upper_bounds <- c(upper_bounds, as.numeric(upper))
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
