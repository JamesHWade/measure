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
#' @param constrain_positions Logical. If `TRUE`, fitted centers are
#'   nondecreasing in the rank of their starting centers and remain within each
#'   model's center bounds. Tied final centers are allowed. Duplicate starting
#'   centers, models without exactly one center parameter, and incompatible
#'   bounds are rejected. Optimization is local and does not guarantee a global
#'   minimum.
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

  if (constrain_positions) {
    center_counts <- vapply(
      models,
      function(model) {
        sum(model$param_names == "center")
      },
      integer(1)
    )
    if (any(center_counts != 1L)) {
      cli::cli_abort(
        "Constrained fitting requires each peak model to define exactly one {.field center} parameter."
      )
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
  start_time <- Sys.time()

  if (length(param_vec) == 0L) {
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    return(
      .deconv_fit_result(
        param_vec,
        x,
        y,
        models,
        fit_context,
        optimizer = "lbfgsb",
        elapsed_time = elapsed,
        convergence = TRUE,
        n_iterations = 0L
      )
    )
  }

  objective <- function(params) {
    .deconv_objective(params, x, y, models, fit_context)
  }

  opt_result <- .optim_with_projection_restarts(
    param_vec,
    objective,
    fit_context,
    method = "L-BFGS-B",
    max_iter = max_iter,
    tol = tol
  )
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  .deconv_fit_result(
    opt_result$par,
    x,
    y,
    models,
    fit_context,
    optimizer = "lbfgsb",
    elapsed_time = elapsed,
    convergence = opt_result$convergence == 0,
    n_iterations = opt_result$n_iterations
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
  start_time <- Sys.time()

  if (length(param_vec) == 0L) {
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    return(
      .deconv_fit_result(
        param_vec,
        x,
        y,
        models,
        fit_context,
        optimizer = "nelder_mead",
        elapsed_time = elapsed,
        convergence = TRUE,
        n_iterations = 0L
      )
    )
  }

  objective <- function(params) {
    .deconv_objective(params, x, y, models, fit_context)
  }

  opt_result <- .optim_with_projection_restarts(
    param_vec,
    objective,
    fit_context,
    method = "Nelder-Mead",
    max_iter = max_iter,
    tol = tol
  )
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  .deconv_fit_result(
    opt_result$par,
    x,
    y,
    models,
    fit_context,
    optimizer = "nelder_mead",
    elapsed_time = elapsed,
    convergence = opt_result$convergence == 0,
    n_iterations = opt_result$n_iterations
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

#' Build a Deconvolution Fit Context
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
  model_lower <- bounds$lower
  model_upper <- bounds$upper

  if (
    length(model_lower) != length(model_upper) ||
      anyNA(model_lower) ||
      anyNA(model_upper) ||
      any(model_lower > model_upper)
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
  if (constrain_positions) {
    if (length(center_slots) != length(models)) {
      cli::cli_abort(
        "Constrained fitting requires each peak model to define exactly one {.field center} parameter."
      )
    }

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
  }

  fixed_slots <- which(model_lower == model_upper)
  if (!all(is.finite(model_lower[fixed_slots]))) {
    cli::cli_abort("Fixed peak parameter bounds must be finite.")
  }
  free_slots <- which(model_lower < model_upper)
  fixed_values <- rep(NA_real_, length(model_lower))
  fixed_values[fixed_slots] <- model_lower[fixed_slots]

  list(
    lower = model_lower[free_slots],
    upper = model_upper[free_slots],
    model_lower = model_lower,
    model_upper = model_upper,
    fixed_slots = fixed_slots,
    fixed_values = fixed_values,
    free_slots = free_slots,
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
    length(param_vec) != length(fit_context$model_lower) ||
      !all(is.finite(param_vec))
  ) {
    cli::cli_abort("Initial peak parameters must be finite and complete.")
  }

  start <- pmin(
    pmax(param_vec, fit_context$model_lower),
    fit_context$model_upper
  )
  start[fit_context$fixed_slots] <- fit_context$fixed_values[
    fit_context$fixed_slots
  ]
  start <- start[fit_context$free_slots]
  if (fit_context$constrain_positions) {
    start <- .transform_deconv_params(start, fit_context)[
      fit_context$free_slots
    ]
  }
  start
}

.expand_deconv_params <- function(param_vec, fit_context) {
  if (length(param_vec) != length(fit_context$free_slots)) {
    cli::cli_abort("The optimizer returned an incomplete parameter vector.")
  }

  full_params <- fit_context$fixed_values
  full_params[fit_context$free_slots] <- param_vec
  full_params
}

.bounded_isotonic_projection <- function(centers, lower, upper) {
  if (
    !is.numeric(centers) ||
      !is.numeric(lower) ||
      !is.numeric(upper) ||
      length(centers) != length(lower) ||
      length(centers) != length(upper) ||
      !all(is.finite(centers)) ||
      anyNA(lower) ||
      anyNA(upper) ||
      any(lower > upper)
  ) {
    cli::cli_abort(
      "Ordered center projection requires finite centers and valid bounds."
    )
  }
  if (length(centers) == 0L) {
    return(centers)
  }

  lower <- cummax(lower)
  upper <- rev(cummin(rev(upper)))
  if (any(lower > upper)) {
    cli::cli_abort(
      "The model-specific center bounds cannot satisfy the requested peak position order."
    )
  }

  n <- length(centers)
  block_start <- integer(n)
  block_end <- integer(n)
  block_weight <- numeric(n)
  block_sum <- numeric(n)
  block_lower <- numeric(n)
  block_upper <- numeric(n)
  block_value <- numeric(n)
  n_blocks <- 0L

  for (i in seq_len(n)) {
    n_blocks <- n_blocks + 1L
    block_start[[n_blocks]] <- i
    block_end[[n_blocks]] <- i
    block_weight[[n_blocks]] <- 1
    block_sum[[n_blocks]] <- centers[[i]]
    block_lower[[n_blocks]] <- lower[[i]]
    block_upper[[n_blocks]] <- upper[[i]]
    block_value[[n_blocks]] <- min(
      max(centers[[i]], lower[[i]]),
      upper[[i]]
    )

    while (
      n_blocks > 1L &&
        block_value[[n_blocks - 1L]] > block_value[[n_blocks]]
    ) {
      left <- n_blocks - 1L
      block_end[[left]] <- block_end[[n_blocks]]
      block_weight[[left]] <- block_weight[[left]] +
        block_weight[[n_blocks]]
      block_sum[[left]] <- block_sum[[left]] + block_sum[[n_blocks]]
      block_lower[[left]] <- max(
        block_lower[[left]],
        block_lower[[n_blocks]]
      )
      block_upper[[left]] <- min(
        block_upper[[left]],
        block_upper[[n_blocks]]
      )

      if (block_lower[[left]] > block_upper[[left]]) {
        cli::cli_abort(
          "A violated center block has incompatible model-specific bounds."
        )
      }

      block_value[[left]] <- min(
        max(
          block_sum[[left]] / block_weight[[left]],
          block_lower[[left]]
        ),
        block_upper[[left]]
      )
      n_blocks <- n_blocks - 1L
    }
  }

  projected <- numeric(n)
  for (i in seq_len(n_blocks)) {
    projected[seq.int(block_start[[i]], block_end[[i]])] <-
      block_value[[i]]
  }

  names(projected) <- names(centers)
  projected
}

.transform_deconv_params <- function(param_vec, fit_context) {
  raw_params <- .expand_deconv_params(param_vec, fit_context)
  transformed <- pmin(
    pmax(raw_params, fit_context$model_lower),
    fit_context$model_upper
  )

  if (fit_context$constrain_positions) {
    transformed[fit_context$rank_slots] <- .bounded_isotonic_projection(
      raw_params[fit_context$rank_slots],
      fit_context$rank_lower,
      fit_context$rank_upper
    )
  }

  transformed
}

.deconv_projection_changed <- function(param_vec, fit_context) {
  if (!fit_context$constrain_positions) {
    return(FALSE)
  }

  raw_params <- .expand_deconv_params(param_vec, fit_context)
  transformed <- .transform_deconv_params(param_vec, fit_context)
  any(raw_params != transformed)
}

.optim_with_projection_restarts <- function(
  param_vec,
  objective,
  fit_context,
  method,
  max_iter,
  tol,
  max_restarts = 5L
) {
  if (!fit_context$constrain_positions) {
    control <- if (identical(method, "L-BFGS-B")) {
      list(maxit = max_iter, factr = tol / .Machine$double.eps)
    } else {
      list(maxit = max_iter, reltol = tol)
    }
    opt_result <- if (identical(method, "L-BFGS-B")) {
      stats::optim(
        par = param_vec,
        fn = objective,
        method = method,
        lower = fit_context$lower,
        upper = fit_context$upper,
        control = control
      )
    } else {
      stats::optim(
        par = param_vec,
        fn = objective,
        method = method,
        control = control
      )
    }
    opt_result$n_iterations <- as.integer(opt_result$counts["function"])
    return(opt_result)
  }

  current_params <- param_vec
  total_function_evaluations <- 0L
  total_iterations <- 0L
  convergence <- FALSE
  final_params <- current_params
  max_attempts <- if (fit_context$constrain_positions) {
    max_restarts + 1L
  } else {
    1L
  }

  for (attempt in seq_len(max_attempts)) {
    remaining_iterations <- max_iter - total_iterations
    if (remaining_iterations <= 0) {
      break
    }

    control <- if (identical(method, "L-BFGS-B")) {
      list(
        maxit = remaining_iterations,
        factr = tol / .Machine$double.eps
      )
    } else {
      list(maxit = remaining_iterations, reltol = tol)
    }

    opt_result <- if (identical(method, "L-BFGS-B")) {
      stats::optim(
        par = current_params,
        fn = objective,
        method = method,
        lower = fit_context$lower,
        upper = fit_context$upper,
        control = control
      )
    } else {
      stats::optim(
        par = current_params,
        fn = objective,
        method = method,
        control = control
      )
    }

    function_evaluations <- as.integer(opt_result$counts[["function"]])
    iteration_count <- if (identical(method, "L-BFGS-B")) {
      as.integer(opt_result$counts[["gradient"]])
    } else {
      function_evaluations
    }
    total_function_evaluations <- total_function_evaluations +
      function_evaluations
    total_iterations <- total_iterations + iteration_count

    candidate_value <- objective(opt_result$par)
    start_value <- objective(current_params)
    worse_tolerance <- sqrt(.Machine$double.eps) *
      max(
        1,
        abs(candidate_value),
        abs(start_value)
      )
    if (candidate_value > start_value + worse_tolerance) {
      final_params <- current_params
      convergence <- FALSE
      break
    }

    final_params <- opt_result$par
    if (!.deconv_projection_changed(opt_result$par, fit_context)) {
      convergence <- opt_result$convergence == 0
      break
    }

    final_params <- .transform_deconv_params(opt_result$par, fit_context)[
      fit_context$free_slots
    ]
    if (
      attempt == max_attempts ||
        total_iterations >= max_iter
    ) {
      convergence <- FALSE
      break
    }

    current_params <- final_params
  }

  list(
    par = final_params,
    convergence = if (convergence) 0L else 1L,
    n_iterations = total_function_evaluations
  )
}

.deconv_objective <- function(param_vec, x, y, models, fit_context) {
  transformed <- .transform_deconv_params(param_vec, fit_context)
  params_list <- .unflatten_params(transformed, models)
  y_pred <- sum_peak_models(x, models, params_list)
  sum((y - y_pred)^2)
}

.deconv_fit_result <- function(
  param_vec,
  x,
  y,
  models,
  fit_context,
  optimizer,
  elapsed_time,
  convergence,
  n_iterations
) {
  final_param_vec <- .transform_deconv_params(param_vec, fit_context)
  final_params <- .unflatten_params(final_param_vec, models)
  fitted_values <- sum_peak_models(x, models, final_params)
  residuals <- y - fitted_values

  list(
    parameters = final_params,
    fitted_values = fitted_values,
    residuals = residuals,
    convergence = convergence,
    n_iterations = n_iterations,
    final_value = sum(residuals^2),
    optimizer = optimizer,
    elapsed_time = elapsed_time
  )
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
