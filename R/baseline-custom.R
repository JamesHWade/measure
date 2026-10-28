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
#'   quosures and evaluated at bake time. For formulas, named arguments are
#'   available as names in the formula body. For functions, they are passed as
#'   ordinary function arguments.
#' @param subtract If `TRUE` (default), the baseline is subtracted from the
#'   signal. If `FALSE`, the baseline values replace the original values
#'   (useful for extracting baselines).
#' @param measures An optional character vector of measure column names to
#'   process. If `NULL` (the default), all measure columns (columns with class
#'   `measure_list`) will be processed.
#' @param tunable An optional named list specifying which arguments in `...`

#'   are tunable. Each element should be a list with `pkg`, `fun`, and
#'   optionally `range`. Each name must match exactly one named argument in
#'   `...`, and `pkg` and `fun` must be non-empty character strings. Fixed
#'   unnamed arguments are allowed but cannot be declared tunable.
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
#'   .fn = ~ {
#'     fit <- stats::lm(.x$value ~ poly(.x$location, degree))
#'     stats::predict(fit)
#'   },
#'   degree = tune::tune(),
#'   tunable = list(
#'     degree = list(
#'       pkg = "dials",
#'       fun = "degree_int",
#'       range = c(1L, 5L)
#'     )
#'   )
#' )
#' ```
#'
#'
#' In a recipe, [tune::extract_parameter_set_dials()] discovers this argument.
#' Finalize it with [tune::finalize_recipe()] before calling [recipes::prep()].
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
  id = recipes::rand_id("measure_baseline_custom")
) {
  fn_formula <- rlang::is_formula(.fn)
  .fn <- rlang::as_function(.fn)
  fn_args <- rlang::enquos(...)
  .validate_custom_baseline_tunable(tunable, fn_args)

  recipes::add_step(
    recipe,
    step_measure_baseline_custom_new(
      fn = .fn,
      fn_formula = fn_formula,
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
  fn,
  fn_formula,
  fn_args,
  subtract,
  measures,
  tunable_params,
  role,
  trained,
  skip,
  id
) {
  recipes::step(
    subclass = "measure_baseline_custom",
    fn = fn,
    fn_formula = fn_formula,
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
    fn_formula = x$fn_formula,
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
  unresolved <- which(vapply(
    fn_args,
    .contains_tune_marker,
    logical(1)
  ))
  if (length(unresolved) > 0) {
    unresolved_names <- names(fn_args)[unresolved]
    if (is.null(unresolved_names)) {
      unresolved_names <- rep("", length(unresolved))
    }
    unnamed <- is.na(unresolved_names) | !nzchar(unresolved_names)
    unresolved_names[unnamed] <- paste("unnamed argument", unresolved[unnamed])
    cli::cli_abort(c(
      "Custom baseline arguments still contain unresolved tuning markers: {.val {unresolved_names}}.",
      "i" = "Finalize the recipe before {.fn prep} or {.fn bake}."
    ))
  }
  # Steps serialized before fn_formula was recorded used environment injection
  # for any callback arguments. Preserve that behavior without mutating the
  # callback environment while those steps are still in circulation.
  fn_formula <- isTRUE(object$fn_formula) ||
    (is.null(object$fn_formula) && length(fn_args) > 0)

  for (col in object$measures) {
    result <- .compute_baseline_custom(
      new_data[[col]],
      fn = object$fn,
      fn_args = fn_args,
      subtract = object$subtract,
      fn_formula = fn_formula
    )
    new_data[[col]] <- new_measure_list(result)
  }
  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_baseline_custom <- function(
  x,
  width = max(20, options()$width - 30),
  ...
) {
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

#' @importFrom generics tune_args
#' @export
tune_args.step_measure_baseline_custom <- function(object, full = FALSE, ...) {
  # The tuning method expects arguments on the step itself. Make the captured
  # callback arguments visible to the public generic while tunable() continues
  # to describe which names are supported.
  fn_args <- object$fn_args
  tunable_params <- object$tunable_params
  arg_names <- names(fn_args)
  proxy_names <- arg_names
  if (!is.null(arg_names)) {
    for (i in seq_along(fn_args)) {
      nm <- arg_names[[i]]
      if (nzchar(nm)) {
        # Keep the step's tunable metadata available to tunable() when a
        # callback uses an internal field name. Also keep individual arguments
        # separate so multiple tune() markers are not treated as one field.
        proxy_nm <- nm
        if (nm %in% c("fn_args", "tunable_params")) {
          proxy_nm <- paste0(".measure_custom_arg_", nm)
          while (proxy_nm %in% c(names(object), arg_names)) {
            proxy_nm <- paste0(".", proxy_nm)
          }
        }
        proxy_names[[i]] <- proxy_nm
        arg_value <- tryCatch(
          rlang::eval_tidy(fn_args[[i]]),
          error = function(e) rlang::quo_get_expr(fn_args[[i]])
        )
        arg_value <- .custom_baseline_normalize_tune_ids(arg_value, nm)
        object[[proxy_nm]] <- arg_value
        if (identical(nm, "tunable_params")) {
          object$tunable_params <- tunable_params
        }
      }
    }
  }
  object$fn_args <- list()

  res <- NextMethod()
  if (!is.null(arg_names)) {
    collision_names <- intersect(arg_names, c("fn_args", "tunable_params"))
    metadata_row <- res$name %in% collision_names & is.na(res$id)
    res <- res[!metadata_row, , drop = FALSE]
  }
  tunable_names <- generics::tunable(object)$name
  if (!is.null(arg_names)) {
    for (i in seq_along(arg_names)) {
      nm <- arg_names[[i]]
      if (!nzchar(nm)) {
        next
      }
      proxy_nm <- proxy_names[[i]]
      rows <- res$name == proxy_nm
      res$name[rows] <- nm
      res$tunable[rows] <- nm %in% tunable_names & !is.na(res$id[rows])
    }
  }
  res
}

#' @export
update.step_measure_baseline_custom <- function(object, ...) {
  changes <- list(...)
  fn_args <- object$fn_args
  arg_names <- names(fn_args)
  if (is.null(arg_names)) {
    arg_names <- rep("", length(fn_args))
  }

  nested <- names(changes) %in% arg_names
  nested[is.na(nested)] <- FALSE
  nested_changes <- changes[nested]
  step_changes <- changes[!nested]

  # Let recipes' public update generic apply ordinary step-field changes. The
  # temporary class removes this method from dispatch and selects update.step.
  step_object <- object
  step_class <- class(step_object)
  class(step_object) <- c("step_measure_baseline_custom_update_proxy", "step")
  if (length(step_changes) > 0) {
    step_object <- do.call(
      stats::update,
      c(list(object = step_object), step_changes)
    )
  } else {
    step_object <- stats::update(step_object)
  }
  class(step_object) <- step_class

  if (length(nested_changes) > 0) {
    for (nm in names(nested_changes)) {
      value_env <- new.env(parent = emptyenv())
      value_env$.value <- nested_changes[[nm]]
      fn_args[[nm]] <- rlang::new_quosure(
        rlang::sym(".value"),
        env = value_env
      )
    }
    step_object$fn_args <- fn_args
  }

  step_object
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
.compute_baseline_custom <- function(
  dat,
  fn,
  fn_args,
  subtract,
  fn_formula = FALSE
) {
  purrr::map(
    dat,
    .custom_baseline_single,
    fn = fn,
    fn_args = fn_args,
    subtract = subtract,
    fn_formula = fn_formula
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
.custom_baseline_single <- function(
  x,
  fn,
  fn_args,
  subtract,
  fn_formula = FALSE
) {
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

  # Formula shorthand refers to named arguments through lexical lookup rather
  # than explicit formals. Give each invocation a private child environment so
  # those names are available without changing the formula's captured scope.
  if (isTRUE(fn_formula) && length(fn_args) > 0 && !is.null(names(fn_args))) {
    fn_invocation <- fn
    fn_env <- new.env(parent = environment(fn))
    for (i in seq_along(fn_args)) {
      nm <- names(fn_args)[[i]]
      if (!is.na(nm) && nzchar(nm)) {
        fn_env[[nm]] <- fn_args[[i]]
      }
    }
    environment(fn_invocation) <- fn_env
  } else {
    fn_invocation <- fn
  }

  baseline <- tryCatch(
    do.call(fn_invocation, c(list(x), fn_args)),
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

.validate_custom_baseline_tunable <- function(tunable, fn_args) {
  call <- rlang::caller_call()
  if (is.null(tunable) || (is.list(tunable) && length(tunable) == 0L)) {
    return(invisible(NULL))
  }

  if (!is.list(tunable)) {
    cli::cli_abort(
      "{.arg tunable} must be {.val NULL}, an empty list, or a named list.",
      call = call
    )
  }

  param_names <- names(tunable)
  if (
    is.null(param_names) ||
      anyNA(param_names) ||
      !all(nzchar(param_names))
  ) {
    cli::cli_abort(
      "{.arg tunable} must have non-empty names for each declaration.",
      call = call
    )
  }

  duplicate_names <- unique(param_names[duplicated(param_names)])
  if (length(duplicate_names) > 0L) {
    cli::cli_abort(
      "Duplicate parameter names in {.arg tunable}: {.val {duplicate_names}}.",
      call = call
    )
  }

  arg_names <- names(fn_args)
  if (is.null(arg_names)) {
    arg_names <- character(0)
  }
  named_args <- arg_names[!is.na(arg_names) & nzchar(arg_names)]
  arg_counts <- vapply(
    param_names,
    function(nm) sum(named_args == nm),
    integer(1)
  )

  unmatched <- param_names[arg_counts == 0L]
  if (length(unmatched) > 0L) {
    cli::cli_abort(
      "Each {.arg tunable} name must match a named argument in {.arg ...}: {.val {unmatched}}.",
      call = call
    )
  }

  repeated_args <- param_names[arg_counts > 1L]
  if (length(repeated_args) > 0L) {
    cli::cli_abort(
      "Tunable callback arguments must be supplied once: {.val {repeated_args}}.",
      call = call
    )
  }

  is_valid_string <- function(x) {
    is.character(x) &&
      length(x) == 1L &&
      !is.na(x) &&
      nzchar(x)
  }
  is_valid_spec <- function(x) {
    is.list(x) &&
      is_valid_string(x$pkg) &&
      is_valid_string(x$fun)
  }
  invalid_specs <- param_names[!vapply(tunable, is_valid_spec, logical(1))]
  if (length(invalid_specs) > 0L) {
    cli::cli_abort(
      c(
        "Each {.arg tunable} declaration must include non-empty scalar character values for {.field pkg} and {.field fun}.",
        "x" = "Invalid declarations: {.val {invalid_specs}}."
      ),
      call = call
    )
  }

  invisible(NULL)
}

.custom_baseline_normalize_tune_ids <- function(x, default_id) {
  if (is.call(x)) {
    tune_fun <- x[[1L]]
    namespaced_tune <- is.call(tune_fun) &&
      length(tune_fun) == 3L &&
      identical(tune_fun[[1L]], as.name("::")) &&
      identical(tune_fun[[2L]], as.name("tune")) &&
      identical(tune_fun[[3L]], as.name("tune"))
    if (identical(tune_fun, as.name("tune")) || namespaced_tune) {
      tune_args <- as.list(x)[-1L]
      arg_names <- names(tune_args)
      id_index <- if (!is.null(arg_names) && "id" %in% arg_names) {
        match("id", arg_names)
      } else if (length(tune_args) > 0L) {
        1L
      } else {
        NA_integer_
      }
      id <- if (is.na(id_index)) "" else tune_args[[id_index]]
      if (is.character(id) && length(id) == 1L && !is.na(id) && !nzchar(id)) {
        id <- ""
      }
      if (identical(id, "")) {
        return(call("tune", default_id))
      }
      return(x)
    }

    parts <- as.list(x)
    for (i in seq_along(parts)) {
      parts[i] <- list(
        .custom_baseline_normalize_tune_ids(parts[[i]], default_id)
      )
    }
    return(as.call(parts))
  }

  if (is.expression(x)) {
    for (i in seq_along(x)) {
      x[i] <- as.expression(list(
        .custom_baseline_normalize_tune_ids(x[[i]], default_id)
      ))
    }
    return(x)
  }

  if (is.list(x)) {
    for (i in seq_along(x)) {
      x[i] <- list(.custom_baseline_normalize_tune_ids(x[[i]], default_id))
    }
  }

  x
}

.contains_tune_marker <- function(x) {
  if (is.call(x)) {
    if (identical(x[[1L]], as.name("tune"))) {
      return(TRUE)
    }
    tune_fun <- x[[1L]]
    if (
      is.call(tune_fun) &&
        length(tune_fun) == 3L &&
        identical(tune_fun[[1L]], as.name("::")) &&
        identical(tune_fun[[2L]], as.name("tune")) &&
        identical(tune_fun[[3L]], as.name("tune"))
    ) {
      return(TRUE)
    }
    if (
      identical(x[[1L]], as.name("::")) &&
        length(x) >= 3L &&
        identical(x[[2L]], as.name("tune")) &&
        identical(x[[3L]], as.name("tune"))
    ) {
      return(TRUE)
    }
    return(any(vapply(as.list(x)[-1L], .contains_tune_marker, logical(1))))
  }

  if (is.list(x) || is.expression(x)) {
    return(any(vapply(as.list(x), .contains_tune_marker, logical(1))))
  }

  FALSE
}
