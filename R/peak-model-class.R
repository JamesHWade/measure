# ==============================================================================
# Peak Model Class Architecture
#
# S3 class system for peak shape models used in deconvolution.
# Provides a consistent interface for evaluation, optimization,
# and parameter handling. Technique packs can register custom models.
# ==============================================================================

# ==============================================================================
# Base Constructor
# ==============================================================================

#' Create a Peak Model Object
#'
#' Creates a new peak model S3 object. This is the base constructor for all
#' peak shape models used in deconvolution.
#'
#' @param name Character name of the model (e.g., "gaussian", "emg").
#' @param n_params Number of parameters in the model.
#' @param param_names Character vector of parameter names.
#' @param description Brief description of the model.
#' @param technique Optional technique name (e.g., "SEC/GPC"). If `NULL`,
#'   model is general-purpose.
#' @param ... Additional model-specific attributes.
#'
#' @return A `peak_model` S3 object with subclass `{name}_peak_model`.
#'
#' @examples
#' # Create a simple Gaussian model
#' model <- new_peak_model(
#'   name = "gaussian",
#'   n_params = 3,
#'   param_names = c("height", "center", "width"),
#'   description = "Symmetric Gaussian peak"
#' )
#' print(model)
#'
#' @seealso [peak_model_value()], [peak_model_gradient()], [peak_model_bounds()]
#' @export
new_peak_model <- function(
  name,
  n_params,
  param_names,
  description = "",
  technique = NULL,
  ...
) {
  if (!is.character(name) || length(name) != 1 || nchar(name) == 0) {
    cli::cli_abort("{.arg name} must be a non-empty string.")
  }
  if (!is.numeric(n_params) || length(n_params) != 1 || n_params < 1) {
    cli::cli_abort("{.arg n_params} must be a positive integer.")
  }
  if (!is.character(param_names) || length(param_names) != n_params) {
    cli::cli_abort(
      "{.arg param_names} must be a character vector of length {n_params}."
    )
  }

  structure(
    list(
      name = name,
      n_params = as.integer(n_params),
      param_names = param_names,
      description = description,
      technique = technique,
      ...
    ),
    class = c(paste0(name, "_peak_model"), "peak_model")
  )
}


#' Test if Object is a Peak Model
#'
#' @param x Object to test.
#' @return Logical indicating if `x` is a `peak_model`.
#' @export
is_peak_model <- function(x) {
  inherits(x, "peak_model")
}


#' @export
print.peak_model <- function(x, ...) {
  cat("<peak_model:", x$name, ">\n")
  cat("  Parameters (", x$n_params, "): ", sep = "")
  cat(paste(x$param_names, collapse = ", "), "\n")
  if (nchar(x$description) > 0) {
    cat("  Description:", x$description, "\n")
  }
  if (!is.null(x$technique)) {
    cat("  Technique:", x$technique, "\n")
  }
  invisible(x)
}


# ==============================================================================
# Generic S3 Methods
# ==============================================================================

#' Evaluate Peak Model
#'
#' Evaluates the peak model at given x values with specified parameters.
#'
#' @param model A `peak_model` object.
#' @param x Numeric vector of x values (e.g., retention time, wavelength).
#' @param params Named list of model parameters.
#'
#' @return Numeric vector of y values (same length as `x`).
#'
#' @examples
#' # Using a registered Gaussian model
#' model <- create_peak_model("gaussian")
#' x <- seq(0, 10, by = 0.1)
#' params <- list(height = 1, center = 5, width = 1)
#' y <- peak_model_value(model, x, params)
#' plot(x, y, type = "l")
#'
#' @seealso [peak_model_gradient()], [peak_model_area()]
#' @export
peak_model_value <- function(model, x, params) {
  UseMethod("peak_model_value")
}

#' @export
peak_model_value.default <- function(model, x, params) {
  cli::cli_abort(
    "No {.fn peak_model_value} method defined for class {.cls {class(model)[1]}}."
  )
}


#' Calculate Peak Model Gradient
#'
#' Calculates partial derivatives of the model with respect to each parameter.
#' Used by optimization algorithms for gradient-based fitting.
#'
#' @param model A `peak_model` object.
#' @param x Numeric vector of x values.
#' @param params Named list of model parameters.
#'
#' @return Matrix of partial derivatives with dimensions `(length(x), n_params)`.
#'   Column names correspond to parameter names.
#'
#' @details
#' If no analytical gradient is available, a numerical gradient can be
#' computed using finite differences. See [peak_model_gradient_numerical()].
#'
#' @seealso [peak_model_value()], [peak_model_gradient_numerical()]
#' @export
peak_model_gradient <- function(model, x, params) {
  UseMethod("peak_model_gradient")
}

#' @export
peak_model_gradient.default <- function(model, x, params) {
  # Fall back to numerical gradient if no analytical version defined
  peak_model_gradient_numerical(model, x, params)
}


#' Get Parameter Bounds for Optimization
#'
#' Returns lower and upper bounds for each parameter, used to constrain
#' optimization during deconvolution.
#'
#' @param model A `peak_model` object.
#' @param x_range Numeric vector of length 2 giving the x-axis range (min, max).
#' @param y_range Numeric vector of length 2 giving the y-axis range (min, max).
#'
#' @return A list with two components:
#'   - `lower`: Named numeric vector of lower bounds
#'   - `upper`: Named numeric vector of upper bounds
#'
#' @examples
#' model <- create_peak_model("gaussian")
#' bounds <- peak_model_bounds(model, c(0, 20), c(0, 100))
#' bounds$lower
#' bounds$upper
#'
#' @seealso [peak_model_initial_guess()]
#' @export
peak_model_bounds <- function(model, x_range, y_range) {
  UseMethod("peak_model_bounds")
}

#' @export
peak_model_bounds.default <- function(model, x_range, y_range) {
  cli::cli_abort(
    "No {.fn peak_model_bounds} method defined for class {.cls {class(model)[1]}}."
  )
}


#' Generate Initial Parameter Guess
#'
#' Estimates initial parameter values from the data, providing a starting
#' point for optimization.
#'
#' @param model A `peak_model` object.
#' @param x Numeric vector of x values.
#' @param y Numeric vector of y values (signal intensity).
#' @param peak_idx Integer index of the peak maximum in `x` and `y`.
#'
#' @return Named list of initial parameter values.
#'
#' @details
#' A good initial guess is crucial for successful optimization. The method
#' should estimate parameters from local features of the data (peak height,
#' width at half maximum, asymmetry, etc.).
#'
#' @examples
#' model <- create_peak_model("gaussian")
#' x <- seq(0, 10, by = 0.1)
#' y <- dnorm(x, mean = 5, sd = 1)
#' peak_idx <- which.max(y)
#' initial <- peak_model_initial_guess(model, x, y, peak_idx)
#' initial
#'
#' @seealso [peak_model_bounds()]
#' @export
peak_model_initial_guess <- function(model, x, y, peak_idx) {
  UseMethod("peak_model_initial_guess")
}

#' @export
peak_model_initial_guess.default <- function(model, x, y, peak_idx) {
  cli::cli_abort(
    "No {.fn peak_model_initial_guess} method defined for class {.cls {class(model)[1]}}."
  )
}


#' Calculate Peak Area
#'
#' Integrates the peak model over a given range to calculate the area.
#'
#' @param model A `peak_model` object.
#' @param params Named list of model parameters.
#' @param x_range Numeric vector of length 2 giving the integration range.
#'   If `NULL`, integrates over the full domain (may require analytical solution).
#'
#' @return Numeric scalar giving the peak area.
#'
#' @details
#' For models with analytical integrals (e.g., Gaussian), this can return
#' an exact value. Otherwise, numerical integration is used.
#'
#' @examples
#' model <- create_peak_model("gaussian")
#' params <- list(height = 1, center = 5, width = 1)
#' area <- peak_model_area(model, params, c(0, 10))
#' area
#'
#' @seealso [peak_model_value()]
#' @export
peak_model_area <- function(model, params, x_range = NULL) {
  UseMethod("peak_model_area")
}

#' @export
peak_model_area.default <- function(model, params, x_range = NULL) {
  # Default: numerical integration
  if (is.null(x_range)) {
    cli::cli_abort(
      "{.arg x_range} is required for numerical integration."
    )
  }
  x <- seq(x_range[1], x_range[2], length.out = 1000)
  y <- peak_model_value(model, x, params)
  # Trapezoidal integration
  sum(diff(x) * (y[-1] + y[-length(y)]) / 2)
}


#' Get Parameter Names from Peak Model
#'
#' @param model A `peak_model` object.
#' @return Character vector of parameter names.
#' @export
peak_model_param_names <- function(model) {
  model$param_names
}


# ==============================================================================
# Utility Functions
# ==============================================================================

#' Numerical Gradient for Peak Model
#'
#' Computes the gradient numerically using finite differences. This is used
#' as a fallback when no analytical gradient is defined.
#'
#' @param model A `peak_model` object.
#' @param x Numeric vector of x values.
#' @param params Named list of model parameters.
#' @param eps Step size for finite differences. Default is `1e-8`.
#'
#' @return Matrix of partial derivatives with dimensions `(length(x), n_params)`.
#'
#' @export
peak_model_gradient_numerical <- function(model, x, params, eps = 1e-8) {
  n_params <- model$n_params
  param_names <- model$param_names

  baseline <- peak_model_value(model, x, params)
  grad_matrix <- matrix(0, nrow = length(x), ncol = n_params)
  colnames(grad_matrix) <- param_names

  for (i in seq_len(n_params)) {
    params_perturbed <- params
    params_perturbed[[param_names[i]]] <- params[[param_names[i]]] + eps

    perturbed_value <- peak_model_value(model, x, params_perturbed)
    grad_matrix[, i] <- (perturbed_value - baseline) / eps
  }

  grad_matrix
}


#' Sum Multiple Peak Models
#'
#' Evaluates multiple peak models and sums their contributions.
#'
#' @param x Numeric vector of x values.
#' @param models List of `peak_model` objects (one per peak).
#' @param params_list List of parameter lists (one per peak).
#'
#' @return Numeric vector of summed peak values.
#'
#' @examples
#' # Two overlapping Gaussian peaks
#' model1 <- create_peak_model("gaussian")
#' model2 <- create_peak_model("gaussian")
#' x <- seq(0, 20, by = 0.1)
#' params1 <- list(height = 1, center = 8, width = 1)
#' params2 <- list(height = 0.8, center = 12, width = 1.5)
#' y <- sum_peak_models(x, list(model1, model2), list(params1, params2))
#' plot(x, y, type = "l")
#'
#' @export
sum_peak_models <- function(x, models, params_list) {
  if (length(models) != length(params_list)) {
    cli::cli_abort("Number of models must match number of parameter sets.")
  }

  total <- rep(0, length(x))
  for (i in seq_along(models)) {
    total <- total + peak_model_value(models[[i]], x, params_list[[i]])
  }

  total
}


#' Validate Peak Model Parameters
#'
#' Checks that a parameter list has all required parameters for a model.
#'
#' @param model A `peak_model` object.
#' @param params Named list of parameters to validate.
#'
#' @return Invisible `TRUE` if valid, otherwise throws an error.
#'
#' @export
validate_peak_model_params <- function(model, params) {
  required <- model$param_names
  provided <- names(params)

  missing <- setdiff(required, provided)
  if (length(missing) > 0) {
    cli::cli_abort(c(
      "Missing required parameters for {.val {model$name}} model:",
      "x" = "Missing: {.val {missing}}"
    ))
  }

  extra <- setdiff(provided, required)
  if (length(extra) > 0) {
    cli::cli_warn(
      "Extra parameters ignored for {.val {model$name}} model: {.val {extra}}"
    )
  }

  invisible(TRUE)
}


# ==============================================================================
# Peak Model Registry
# ==============================================================================

#' Reset the peak model registry
#'
#' Clears all registered peak models. Used internally by `.onLoad()` and tests.
#'
#' @return Invisible `NULL`.
#' @noRd
.peak_model_registry_reset <- function() {
  .measure_registry$peak_models <- list()
  invisible(NULL)
}


#' Register a Peak Model
#'
#' Registers a peak model constructor with the measure package. Technique packs
#' can use this to add custom peak shapes.
#'
#' @param name Model name (e.g., "gaussian", "emg", "fraser_suzuki").
#' @param constructor Function that creates the peak model object.
#' @param pack_name Source package name.
#' @param description Brief description of the model.
#' @param technique Optional technique name (e.g., "SEC/GPC").
#'
#' @return Invisible `TRUE`.
#'
#' @examples
#' \dontrun{
#' # In a technique pack's R/zzz.R:
#' register_peak_model(
#'   name = "fraser_suzuki",
#'   constructor = fraser_suzuki_model,
#'   pack_name = pkgname,
#'   description = "Fraser-Suzuki asymmetric peak",
#'   technique = "SEC/GPC"
#' )
#' }
#'
#' @seealso [peak_models()], [create_peak_model()]
#' @export
register_peak_model <- function(
  name,
  constructor,
  pack_name,
  description = "",
  technique = NULL
) {
  if (!is.character(name) || length(name) != 1 || nchar(name) == 0) {
    cli::cli_abort("{.arg name} must be a non-empty string.")
  }
  if (!is.function(constructor)) {
    cli::cli_abort("{.arg constructor} must be a function.")
  }

  if (is.null(.measure_registry$peak_models)) {
    .measure_registry$peak_models <- list()
  }

  .measure_registry$peak_models[[name]] <- list(
    name = name,
    constructor = constructor,
    pack_name = pack_name,
    description = description,
    technique = technique,
    registered_at = Sys.time()
  )

  invisible(TRUE)
}


#' Unregister a Peak Model
#'
#' Removes a peak model from the registry.
#'
#' @param name Model name to remove.
#' @return Invisible `TRUE` if removed, `FALSE` if not found.
#' @export
unregister_peak_model <- function(name) {
  if (is.null(.measure_registry$peak_models)) {
    return(invisible(FALSE))
  }
  if (!name %in% names(.measure_registry$peak_models)) {
    return(invisible(FALSE))
  }

  .measure_registry$peak_models[[name]] <- NULL
  invisible(TRUE)
}


#' List Available Peak Models
#'
#' Returns a tibble of all registered peak models.
#'
#' @param packs Character vector of pack names to filter by. If `NULL`,
#'   includes all packs.
#' @param techniques Character vector of techniques to filter by. If `NULL`,
#'   includes all (including general-purpose models).
#'
#' @return A tibble with columns: name, pack_name, description, technique.
#'
#' @examples
#' peak_models()
#'
#' @seealso [register_peak_model()], [create_peak_model()]
#' @export
peak_models <- function(packs = NULL, techniques = NULL) {
  models <- .measure_registry$peak_models

  if (is.null(models) || length(models) == 0) {
    return(tibble::tibble(
      name = character(),
      pack_name = character(),
      description = character(),
      technique = character()
    ))
  }

  result <- tibble::tibble(
    name = vapply(models, `[[`, character(1), "name"),
    pack_name = vapply(models, `[[`, character(1), "pack_name"),
    description = vapply(models, `[[`, character(1), "description"),
    technique = vapply(
      models,
      function(x) x$technique %||% NA_character_,
      character(1)
    )
  )

  if (!is.null(packs)) {
    result <- result[result$pack_name %in% packs, , drop = FALSE]
  }

  if (!is.null(techniques)) {
    result <- result[
      is.na(result$technique) | result$technique %in% techniques,
      ,
      drop = FALSE
    ]
  }

  result
}


#' Check if a Peak Model Exists
#'
#' @param name Model name.
#' @return Logical `TRUE` if model exists.
#' @export
has_peak_model <- function(name) {
  !is.null(.measure_registry$peak_models[[name]])
}


#' Create a Peak Model by Name
#'
#' Creates a peak model object from a registered model name.
#'
#' @param name Name of the model (e.g., "gaussian", "emg", "bigaussian").
#'
#' @return A `peak_model` object.
#'
#' @examples
#' model <- create_peak_model("gaussian")
#' print(model)
#'
#' @seealso [peak_models()], [register_peak_model()]
#' @export
create_peak_model <- function(name) {
  model_info <- .measure_registry$peak_models[[name]]

  if (is.null(model_info)) {
    available <- names(.measure_registry$peak_models)
    cli::cli_abort(c(
      "Peak model {.val {name}} not found.",
      "i" = "Available models: {.val {available}}"
    ))
  }

  model_info$constructor()
}
