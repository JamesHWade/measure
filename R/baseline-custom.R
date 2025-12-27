#' Custom Baseline Correction with User-Provided Function
#'
#' `step_measure_baseline_custom()` creates a *specification* of a recipe step
#' that applies a user-provided function for baseline correction. This allows
#' for flexible, custom baseline estimation algorithms.
#'
#' @param recipe A recipe object. The step will be added to the sequence of
#'   operations for this recipe.
#' @param .fn A function or formula for baseline estimation. The function should
#'   accept a `measure_tbl` (tibble with `location` and `value` columns) and
#'   return a numeric vector of baseline values with the same length as the
#'   input. Formulas are converted to functions via [rlang::as_function()],
#'   where `.x` represents the `measure_tbl`.
#' @param ... Additional arguments passed to `.fn`. These are captured as
#'   quosures and evaluated at bake time.
#' @param subtract If `TRUE` (default), the baseline is subtracted from the
#'   signal. If `FALSE`, the baseline values replace the original values
#'   (useful for extracting baselines).
#' @param measures An optional character vector of measure column names to
#'   process. If `NULL` (the default), all measure columns (columns with class
#'   `measure_list`) will be processed.
#' @param tunable An optional named list specifying which arguments in `...`

#'   are tunable. Each element should be a list with `pkg`, `fun`, and
#'   optionally `range`. See Details.
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
#' This step allows you to use any baseline estimation algorithm by providing
#' a custom function. The function receives a `measure_tbl` object (a tibble
#' with `location` and `value` columns) and should return a numeric vector
#' of the estimated baseline values.
#'
#' ## Function Contract
#'
#' Your function should:
#' - Accept a `measure_tbl` as its first argument
#' - Return a numeric vector of the same length as `nrow(measure_tbl)`
#' - Handle NA values appropriately
#'
#' ## Formula Interface
#'
#' You can use a formula instead of a function. The formula is converted to a
#' function where `.x` represents the `measure_tbl`:
#'
#' ```r
#' # These are equivalent:
#' step_measure_baseline_custom(.fn = function(x) mean(x$value))
#' step_measure_baseline_custom(.fn = ~ mean(.x$value))
#' ```
#'
#' ## Tunability
#'
#' To make parameters tunable with `dials`, provide a `tunable` argument:
#'
#' ```r
#' step_measure_baseline_custom(
#'   .fn = ~ stats::loess(.x$value ~ .x$location, span = span)$fitted,
#'   span = 0.5,
#'   tunable = list(
#'     span = list(pkg = "dials", fun = "degree", range = c(0.1, 0.9))
#'   )
#' )
#' ```
#'
#' **No selectors should be supplied to this step function**. The data should be
#' in the internal format produced by [step_measure_input_wide()] or
#' [step_measure_input_long()].
#'
#' # Tidying
#'
#' When you [`tidy()`][recipes::tidy.recipe()] this step, a tibble with columns
#' `terms`, `subtract`, and `id` is returned.
#'
#' @seealso [step_measure_baseline_als()], [step_measure_baseline_poly()] for
#'   built-in baseline correction methods.
#'
#' @family measure-baseline
#' @export
#'
#' @examples
#' library(recipes)
#'
#' # Simple polynomial baseline using a function
#' poly_baseline <- function(x) {
#'   fit <- lm(x$value ~ poly(x$location, 2))
#'   predict(fit)
#' }
#'
#' rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_baseline_custom(.fn = poly_baseline) |>
#'   prep()
#'
#' bake(rec, new_data = NULL)
#'
#' # Using formula interface with additional parameters
#' rec2 <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_baseline_custom(
#'     .fn = ~ stats::loess(.x$value ~ .x$location, span = span)$fitted,
#'     span = 0.5
#'   ) |>
#'   prep()
step_measure_baseline_custom <- function(
    recipe,
    .fn,
    ...,
    subtract = TRUE,
    measures = NULL,
    tunable = NULL,
    role = NA,
    trained = FALSE,
    skip = FALSE,
    id = recipes::rand_id("measure_baseline_custom")) {

.fn <- rlang::as_function(.fn)
  fn_args <- rlang::enquos(...)

  recipes::add_step(
    recipe,
    step_measure_baseline_custom_new(
      fn = .fn,
      fn_args = fn_args,
      subtract = subtract,
      measures = measures,
      tunable_params = tunable,
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_measure_baseline_custom_new <- function(
    fn, fn_args, subtract, measures, tunable_params, role, trained, skip, id) {
  recipes::step(
    subclass = "measure_baseline_custom",
    fn = fn,
    fn_args = fn_args,
    subtract = subtract,
    measures = measures,
    tunable_params = tunable_params,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_baseline_custom <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  # Validate subtract parameter
  if (!is.logical(x$subtract) || length(x$subtract) != 1) {
    cli::cli_abort(
      "{.arg subtract} must be a single logical value, not {.val {x$subtract}}."
    )
  }

  # Validate that fn is a function
  if (!is.function(x$fn)) {
    cli::cli_abort(
      "{.arg .fn} must be a function or formula, not {.cls {class(x$fn)}}."
    )
  }

  # Resolve which columns to process
  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  step_measure_baseline_custom_new(
    fn = x$fn,
    fn_args = x$fn_args,
    subtract = x$subtract,
    measures = measure_cols,
    tunable_params = x$tunable_params,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_baseline_custom <- function(object, new_data, ...) {
  # Evaluate quosures to get actual argument values
fn_args <- lapply(object$fn_args, rlang::eval_tidy)

  for (col in object$measures) {
    result <- .compute_baseline_custom(
      new_data[[col]],
      fn = object$fn,
      fn_args = fn_args,
      subtract = object$subtract
    )
    new_data[[col]] <- new_measure_list(result)
  }
  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_baseline_custom <- function(
    x,
    width = max(20, options()$width - 30),
    ...) {
  title <- "Custom baseline correction on "

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
tidy.step_measure_baseline_custom <- function(x, ...) {
  if (recipes::is_trained(x)) {
    terms_val <- x$measures
  } else {
    terms_val <- "<all measure columns>"
  }
  tibble::tibble(
    terms = terms_val,
    subtract = x$subtract,
    id = x$id
  )
}

#' @export
required_pkgs.step_measure_baseline_custom <- function(x, ...) {
  "measure"
}

#' @rdname tunable_measure
#' @export
tunable.step_measure_baseline_custom <- function(x, ...) {
  if (is.null(x$tunable_params) || length(x$tunable_params) == 0) {
    # No tunable parameters declared
    return(
      tibble::tibble(
        name = character(0),
        call_info = list(),
        source = character(0),
        component = character(0),
        component_id = character(0)
      )
    )
  }

  # Build tunable tibble from user-declared parameters
  param_names <- names(x$tunable_params)
  call_infos <- lapply(x$tunable_params, function(p) {
    info <- list(pkg = p$pkg, fun = p$fun)
    if (!is.null(p$range)) {
      info$range <- p$range
    }
    info
  })

  tibble::tibble(
    name = param_names,
    call_info = call_infos,
    source = rep("recipe", length(param_names)),
    component = rep("step_measure_baseline_custom", length(param_names)),
    component_id = rep(x$id, length(param_names))
  )
}

# ------------------------------------------------------------------------------
# Internal computation

#' Compute custom baseline correction for a measure_list
#'
#' @param dat A measure_list (list of measure_tbl objects).
#' @param fn The baseline estimation function.
#' @param fn_args Additional arguments for fn.
#' @param subtract Whether to subtract the baseline.
#' @return A list of measure_tbl objects with baseline-corrected values.
#' @noRd
.compute_baseline_custom <- function(dat, fn, fn_args, subtract) {
  purrr::map(
    dat, .custom_baseline_single,
    fn = fn,
    fn_args = fn_args,
    subtract = subtract
  )
}

#' Apply custom baseline correction to a single spectrum
#'
#' @param x A measure_tbl with location and value columns.
#' @param fn The baseline estimation function.
#' @param fn_args Additional arguments for fn.
#' @param subtract Whether to subtract the baseline.
#' @return A measure_tbl with baseline-corrected values.
#' @noRd
.custom_baseline_single <- function(x, fn, fn_args, subtract) {
  n <- nrow(x)

  # Handle edge cases
  if (n < 2) {
    cli::cli_warn(
      "Spectrum has fewer than 2 points; returning unchanged."
    )
    return(x)
  }

  if (all(is.na(x$value))) {
    cli::cli_warn("Spectrum is all NA; returning unchanged.")
    return(x)
  }

  # Call the user function with the measure_tbl and additional args
  # For formula-based functions, inject fn_args into the function's environment
  # so that variables referenced in the formula body are accessible
  if (length(fn_args) > 0) {
    fn_env <- rlang::fn_env(fn)
    for (nm in names(fn_args)) {
      fn_env[[nm]] <- fn_args[[nm]]
    }
  }

  baseline <- tryCatch(
    do.call(fn, c(list(x), fn_args)),
    error = function(e) {
      cli::cli_warn(
        "Custom baseline function failed: {e$message}; returning unchanged."
      )
      return(NULL)
    }
  )

  if (is.null(baseline)) {
    return(x)
  }

  # Validate baseline output
  if (!is.numeric(baseline)) {
    cli::cli_warn(
      "Custom baseline function must return numeric, got {.cls {class(baseline)}}; returning unchanged."
    )
    return(x)
  }

  if (length(baseline) != n) {
    cli::cli_warn(
      "Custom baseline function returned {length(baseline)} values, expected {n}; returning unchanged."
    )
    return(x)
  }

  # Apply baseline correction
  if (subtract) {
    x$value <- x$value - baseline
  } else {
    x$value <- baseline
  }

  x
}
