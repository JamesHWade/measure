# ==============================================================================
# Calibration Steps
#
# This file contains steps for calibrating measurement data:
# - step_measure_calibrate_x: Transform x-axis (location) using calibration
# - step_measure_calibrate_y: Apply response factor to y-axis (value)
# ==============================================================================

# ==============================================================================
# step_measure_calibrate_x
# ==============================================================================

#' Apply X-Axis Calibration
#'
#' `step_measure_calibrate_x()` creates a *specification* of a recipe step
#' that transforms the x-axis (location) values using a calibration function
#' or calibration data.
#'
#' @param recipe A recipe object. The step will be added to the sequence of
#'   operations for this recipe.
#' @param calibration The calibration to apply. Can be:
#'   - A data.frame with columns specified by `from` and `to`
#'   - A function that takes location values and returns calibrated values
#' @param from Column name in calibration data.frame containing original
#'   x values. Default is `"x"`.
#' @param to Column name in calibration data.frame containing calibrated
#'   values. Default is `"y"`.
#' @param method Interpolation method when using calibration data.frame:
#'   - `"linear"`: Linear interpolation
#'   - `"spline"` (default): Cubic spline interpolation
#' @param extrapolate Logical. If `TRUE`, allow extrapolation outside the
#'   calibration range. If `FALSE` (default), values outside the range will
#'   return `NA` for linear interpolation or use spline extrapolation.
#' @param measures An optional character vector of measure column names to
#'   process. If `NULL` (the default), all measure columns will be processed.
#' @param role Not used by this step since no new variables are created.
#' @param trained A logical to indicate if the step has been trained.
#' @param cal_fn The calibration function created during training.
#' @param skip A logical. Should the step be skipped when baking?
#' @param id A character string that is unique to this step.
#'
#' @return An updated version of `recipe` with the new step added.
#'
#' @details
#' X-axis calibration is commonly used to convert raw measurement units to
#' physically meaningful values. Common examples include:
#'
#' - **GPC/SEC**: Convert retention time to molecular weight (via log MW)
#' - **Mass spectrometry**: Apply m/z calibration corrections
#' - **Spectroscopy**: Convert pixel or channel numbers to wavelength/wavenumber
#'
#' The calibration can be provided as either:
#'
#' 1. **Calibration data**: A data.frame with known x→y mappings. The step
#'    will build an interpolation function during `prep()`.
#'
#' 2. **Calibration function**: A function that directly transforms x values.
#'
#' **Warning**: This step modifies the `location` column. Subsequent steps
#' will see the calibrated values. Make sure your calibration is appropriate
#' for your data range.
#'
#' **No selectors should be supplied to this step function**. The data should be
#' in the internal format produced by [step_measure_input_wide()] or
#' [step_measure_input_long()].
#'
#' # Tidying
#'
#' When you [`tidy()`][recipes::tidy.recipe()] this step, a tibble with columns
#' `terms`, `method`, `extrapolate`, and `id` is returned.
#'
#' @family measure-preprocessing
#' @seealso [step_measure_calibrate_y()] for y-axis calibration
#' @export
#'
#' @examples
#' library(recipes)
#'
#' # Example: GPC molecular weight calibration
#' # Calibration standards: retention_time -> log(MW)
#' gpc_cal <- data.frame(
#'   retention_time = c(10, 12, 14, 16, 18),
#'   log_mw = c(6.5, 5.8, 5.0, 4.2, 3.5)
#' )
#'
#' # Note: meats_long doesn't have retention time, this is illustrative
#' rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_calibrate_x(
#'     calibration = function(x) log10(x + 1),  # Example transformation
#'     method = "spline"
#'   )
#'
#' # With calibration data
#' # rec <- recipe(...) |>
#' #   step_measure_calibrate_x(
#' #     calibration = gpc_cal,
#' #     from = "retention_time",
#' #     to = "log_mw",
#' #     method = "spline"
#' #   )
step_measure_calibrate_x <- function(
  recipe,
  calibration,
  from = "x",
  to = "y",
  method = "spline",
  extrapolate = FALSE,
  measures = NULL,
  role = NA,
  trained = FALSE,
  cal_fn = NULL,
  skip = FALSE,
  id = recipes::rand_id("measure_calibrate_x")
) {
  if (missing(calibration)) {
    cli::cli_abort("{.arg calibration} is required.")
  }

  recipes::add_step(
    recipe,
    step_measure_calibrate_x_new(
      calibration = calibration,
      from = from,
      to = to,
      method = method,
      extrapolate = extrapolate,
      measures = measures,
      role = role,
      trained = trained,
      cal_fn = cal_fn,
      skip = skip,
      id = id
    )
  )
}

step_measure_calibrate_x_new <- function(
  calibration,
  from,
  to,
  method,
  extrapolate,
  measures,
  role,
  trained,
  cal_fn,
  skip,
  id
) {
  recipes::step(
    subclass = "measure_calibrate_x",
    calibration = calibration,
    from = from,
    to = to,
    method = method,
    extrapolate = extrapolate,
    measures = measures,
    role = role,
    trained = trained,
    cal_fn = cal_fn,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_calibrate_x <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  # Resolve which columns to process
  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  # Validate method
  if (!x$method %in% c("linear", "spline")) {
    cli::cli_abort(
      "{.arg method} must be {.val linear} or {.val spline}, not {.val {x$method}}."
    )
  }

  # Build calibration function
  cal_fn <- NULL

  if (is.function(x$calibration)) {
    # User provided a function directly
    cal_fn <- x$calibration
  } else if (is.data.frame(x$calibration)) {
    # Build interpolation function from calibration data
    if (!x$from %in% names(x$calibration)) {
      cli::cli_abort(
        "Column {.val {x$from}} not found in calibration data."
      )
    }
    if (!x$to %in% names(x$calibration)) {
      cli::cli_abort(
        "Column {.val {x$to}} not found in calibration data."
      )
    }

    cal_x <- x$calibration[[x$from]]
    cal_y <- x$calibration[[x$to]]

    # Check for sufficient calibration points
    if (length(cal_x) < 2) {
      cli::cli_abort(
        "Calibration data must have at least 2 points."
      )
    }

    # Sort by x values
    ord <- order(cal_x)
    cal_x <- cal_x[ord]
    cal_y <- cal_y[ord]

    # Build interpolation function
    cal_fn <- switch(
      x$method,
      "linear" = {
        rule <- if (x$extrapolate) 2 else 1
        stats::approxfun(cal_x, cal_y, rule = rule)
      },
      "spline" = {
        stats::splinefun(cal_x, cal_y, method = "natural")
      }
    )
  } else {
    cli::cli_abort(c(
      "{.arg calibration} must be a function or data.frame.",
      "i" = "Got {.cls {class(x$calibration)}}."
    ))
  }

  step_measure_calibrate_x_new(
    calibration = x$calibration,
    from = x$from,
    to = x$to,
    method = x$method,
    extrapolate = x$extrapolate,
    measures = measure_cols,
    role = x$role,
    trained = TRUE,
    cal_fn = cal_fn,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_calibrate_x <- function(object, new_data, ...) {
  for (col in object$measures) {
    result <- purrr::map(new_data[[col]], function(x) {
      x$location <- object$cal_fn(x$location)
      x
    })
    new_data[[col]] <- new_measure_list(result)
  }
  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_calibrate_x <- function(
  x,
  width = max(20, options()$width - 30),
  ...
) {
  title <- "X-axis calibration"
  method_str <- paste0(" (", x$method, ")")

  if (x$trained) {
    cat(title, method_str, " on <internal measurements>", sep = "")
  } else {
    cat(title, method_str, sep = "")
  }
  cat("\n")

  invisible(x)
}

#' @rdname tidy.recipe
#' @export
#' @keywords internal
tidy.step_measure_calibrate_x <- function(x, ...) {
  if (is_trained(x)) {
    terms <- x$measures
  } else {
    terms <- "<all measure columns>"
  }

  tibble::tibble(
    terms = terms,
    method = x$method,
    extrapolate = x$extrapolate,
    id = x$id
  )
}

#' @export
required_pkgs.step_measure_calibrate_x <- function(x, ...) {
  c("measure")
}

# ==============================================================================
# step_measure_calibrate_y
# ==============================================================================

#' Apply Y-Axis Calibration (Response Factor)
#'
#' `step_measure_calibrate_y()` creates a *specification* of a recipe step
#' that applies a response factor or calibration function to y-axis (value)
#' values.
#'
#' @param recipe A recipe object. The step will be added to the sequence of
#'   operations for this recipe.
#' @param response_factor A numeric value to multiply all values by.
#'   Default is `1.0` (no change). This is a simple scalar calibration.
#' @param calibration An optional calibration function that takes value(s)
#'   and returns calibrated value(s). If provided, this takes precedence
#'   over `response_factor`.
#' @param measures An optional character vector of measure column names to
#'   process. If `NULL` (the default), all measure columns will be processed.
#' @param role Not used by this step since no new variables are created.
#' @param trained A logical to indicate if the step has been trained.
#' @param cal_fn The calibration function to apply (built during prep).
#' @param skip A logical. Should the step be skipped when baking?
#' @param id A character string that is unique to this step.
#'
#' @return An updated version of `recipe` with the new step added.
#'
#' @details
#' Y-axis calibration is used to convert raw signal intensities to
#' quantitative values. Common examples include:
#'
#' - **Chromatography**: Apply detector response factors
#' - **Spectroscopy**: Apply molar absorptivity corrections
#' - **Mass spectrometry**: Apply ionization efficiency corrections
#'
#' **Simple mode**: Use `response_factor` to multiply all values by a constant.
#'
#' **Complex mode**: Use `calibration` to provide a function for non-linear
#' calibration curves (e.g., from fitting standards).
#'
#' **No selectors should be supplied to this step function**. The data should be
#' in the internal format produced by [step_measure_input_wide()] or
#' [step_measure_input_long()].
#'
#' # Tidying
#'
#' When you [`tidy()`][recipes::tidy.recipe()] this step, a tibble with columns
#' `terms`, `response_factor`, `has_calibration`, and `id` is returned.
#'
#' @family measure-preprocessing
#' @seealso [step_measure_calibrate_x()] for x-axis calibration
#' @export
#'
#' @examples
#' library(recipes)
#'
#' # Simple response factor
#' rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_calibrate_y(response_factor = 2.5)
#'
#' # With calibration function (e.g., log transform)
#' rec2 <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_calibrate_y(calibration = function(x) log10(x + 0.001))
step_measure_calibrate_y <- function(
  recipe,
  response_factor = 1.0,
  calibration = NULL,
  measures = NULL,
  role = NA,
  trained = FALSE,
  cal_fn = NULL,
  skip = FALSE,
  id = recipes::rand_id("measure_calibrate_y")
) {
  recipes::add_step(
    recipe,
    step_measure_calibrate_y_new(
      response_factor = response_factor,
      calibration = calibration,
      measures = measures,
      role = role,
      trained = trained,
      cal_fn = cal_fn,
      skip = skip,
      id = id
    )
  )
}

step_measure_calibrate_y_new <- function(
  response_factor,
  calibration,
  measures,
  role,
  trained,
  cal_fn,
  skip,
  id
) {
  recipes::step(
    subclass = "measure_calibrate_y",
    response_factor = response_factor,
    calibration = calibration,
    measures = measures,
    role = role,
    trained = trained,
    cal_fn = cal_fn,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_calibrate_y <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  # Resolve which columns to process
  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  # Build calibration function
  cal_fn <- NULL

  if (!is.null(x$calibration)) {
    if (!is.function(x$calibration)) {
      cli::cli_abort(c(
        "{.arg calibration} must be a function.",
        "i" = "Got {.cls {class(x$calibration)}}."
      ))
    }
    cal_fn <- x$calibration
  } else {
    # Validate response_factor
    if (!is.numeric(x$response_factor) || length(x$response_factor) != 1) {
      cli::cli_abort(
        "{.arg response_factor} must be a single numeric value."
      )
    }
    # Create simple multiplication function
    rf <- x$response_factor
    cal_fn <- function(v) v * rf
  }

  step_measure_calibrate_y_new(
    response_factor = x$response_factor,
    calibration = x$calibration,
    measures = measure_cols,
    role = x$role,
    trained = TRUE,
    cal_fn = cal_fn,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_calibrate_y <- function(object, new_data, ...) {
  for (col in object$measures) {
    result <- purrr::map(new_data[[col]], function(x) {
      x$value <- object$cal_fn(x$value)
      x
    })
    new_data[[col]] <- new_measure_list(result)
  }
  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_calibrate_y <- function(
  x,
  width = max(20, options()$width - 30),
  ...
) {
  title <- "Y-axis calibration"

  if (x$trained) {
    if (!is.null(x$calibration)) {
      type_str <- " (function)"
    } else {
      type_str <- paste0(" (factor = ", round(x$response_factor, 3), ")")
    }
    cat(title, type_str, " on <internal measurements>", sep = "")
  } else {
    cat(title, sep = "")
  }
  cat("\n")

  invisible(x)
}

#' @rdname tidy.recipe
#' @export
#' @keywords internal
tidy.step_measure_calibrate_y <- function(x, ...) {
  if (is_trained(x)) {
    terms <- x$measures
  } else {
    terms <- "<all measure columns>"
  }
  tibble::tibble(
    terms = terms,
    response_factor = x$response_factor,
    has_calibration = !is.null(x$calibration),
    id = x$id
  )
}

#' @export
required_pkgs.step_measure_calibrate_y <- function(x, ...) {
  c("measure")
}
