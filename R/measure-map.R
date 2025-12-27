# ==============================================================================
# Sample-wise mapping infrastructure for measure columns
#
# This file provides the infrastructure for custom sample-wise transformations
# on measurement data. It follows the recipes design philosophy:
#
# - step_measure_map() is the PRIMARY interface (recipe step)
# - measure_map() is for EXPLORATION only (outside recipes)
# - measure_summarize() is for ANALYSIS (not transformation)
#
# Design Principle: Use recipe steps for reproducible pipelines.
# The standalone functions exist for debugging and prototyping, not production.
#
# See GitHub Issue #9 for context.
# ==============================================================================

# ------------------------------------------------------------------------------
# Recipe Step: step_measure_map (PRIMARY INTERFACE)
# ------------------------------------------------------------------------------

#' Apply a Custom Function to Measurements
#'
#' `step_measure_map()` creates a *specification* of a recipe step that applies
#' a custom function to each sample's measurements. Use this when the built-in
#' preprocessing steps (SNV, MSC, Savitzky-Golay) don't cover your needs.
#'
#' @param recipe A recipe object. The step will be added to the sequence of
#'   operations for this recipe.
#' @param fn A function to apply to each sample's measurement tibble. The
#'
#'   function should accept a tibble with `location` and `value` columns and
#'   return a tibble with the same structure. Can also be a formula (e.g.,
#'   `~ { .x$value <- log1p(.x$value); .x }`) which will be converted via
#'   [rlang::as_function()].
#' @param ... Additional arguments passed to `fn` during baking.
#' @param measures An optional character vector of measure column names to
#'
#'   process. If `NULL` (the default), all measure columns will be processed.
#' @param verbosity An integer controlling output verbosity:
#'   - `0`: Silent - suppress all messages and output from `fn`
#'   - `1`: Normal (default) - show output from `fn`
#' @param role Not used by this step since no new variables are created.
#' @param trained A logical to indicate if the step has been trained.
#' @param skip A logical. Should the step be skipped when the recipe is baked?
#' @param id A character string that is unique to this step.
#'
#' @return An updated version of `recipe` with the new step added.
#'
#' @details
#' This step is the "escape hatch" for custom sample-wise transformations that
#' aren't covered by the built-in steps. It integrates fully with the recipes
#' framework, meaning your custom transformation will be:
#'
#' - Applied consistently during `prep()` and `bake()`
#' - Included when bundling recipes into workflows
#' - Reproducible across sessions
#'
#' ## Function Requirements
#'
#' The function `fn` must:
#' - Accept a tibble with `location` and `value` columns
#' - Return a tibble with `location` and `value` columns
#' - Not change the number of rows (measurements must remain aligned)
#'
#' ## When to Use This Step
#'
#' Use `step_measure_map()` for domain-specific transformations not covered
#' by the built-in steps:
#'
#' - Custom baseline correction algorithms
#' - Specialized normalization methods
#' - Instrument-specific corrections
#' - Experimental preprocessing techniques
#'
#' For common operations, prefer the built-in steps:
#' - Scatter correction → [step_measure_snv()] or [step_measure_msc()]
#' - Smoothing/derivatives → [step_measure_savitzky_golay()]
#'
#' ## Prototyping with measure_map()
#'
#' When developing a custom transformation, you may find it helpful to
#' prototype using [measure_map()] on baked data before wrapping it in
#' a step. Once your function works correctly, use `step_measure_
#'
#' for production pipelines.
#'
#' @seealso
#' - [step_measure_snv()], [step_measure_msc()], [step_measure_savitzky_golay()]
#'   for built-in preprocessing steps
#' - [measure_map()] for prototyping custom transformations
#'
#' @family measure-preprocessing
#' @export
#'
#' @examples
#' library(recipes)
#'
#' # Example 1: Custom log transformation
#' log_transform <- function(x) {
#'   x$value <- log1p(x$value)
#'   x
#' }
#'
#' rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_map(log_transform) |>
#'   step_measure_snv() |>
#'   prep()
#'
#' bake(rec, new_data = NULL)
#'
#' # Example 2: Using formula syntax for inline transformations
#' rec2 <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_map(~ {
#'     # Subtract minimum to remove offset
#'     .x$value <- .x$value - min(.x$value)
#'     .x
#'   }) |>
#'   prep()
#'
#' # Example 3: Using external package functions
#' # (e.g., custom baseline from a spectroscopy package)
#' \dontrun{
#' rec3 <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_map(my_baseline_correction, method = "als") |>
#'   step_measure_output_wide()
#' }
step_measure_map <- function(
    recipe,
    fn,
    ...,
    measures = NULL,
    verbosity = 1L,
    role = NA,
    trained = FALSE,
    skip = FALSE,
    id = recipes::rand_id("measure_map")) {
  # Evaluate and convert the function immediately
  # This avoids issues with recipes' internal get_needs_tuning check
  fn <- rlang::as_function(fn)
  dots <- rlang::enquos(...)

  recipes::add_step(
    recipe,
    step_measure_map_new(
      fn = fn,
      fn_args = dots,
      measures = measures,
      verbosity = verbosity,
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_measure_map_new <- function(fn, fn_args, measures, verbosity, role,
                                 trained, skip, id) {
  recipes::step(
    subclass = "measure_map",
    fn = fn,
    fn_args = fn_args,
    measures = measures,
    verbosity = verbosity,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_map <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  # Resolve which columns to process
  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  step_measure_map_new(
    fn = x$fn,
    fn_args = x$fn_args,
    measures = measure_cols,
    verbosity = x$verbosity,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_map <- function(object, new_data, ...) {
  # fn is already a function (converted in step_measure_map)
  fn <- object$fn
  capture <- object$verbosity == 0

  # Evaluate additional arguments
  fn_args <- lapply(object$fn_args, rlang::eval_tidy)

  for (col in object$measures) {
    # Apply the function to each sample
    result <- purrr::map(new_data[[col]], function(x) {
      .eval_transform(fn, x, fn_args, capture = capture)
    })
    new_data[[col]] <- new_measure_list(result)
  }

  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_map <- function(x, width = max(20, options()$width - 30), ...) {
  title <- "Custom transformation on "

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
tidy.step_measure_map <- function(x, ...) {
  if (is_trained(x)) {
    terms <- x$measures
  } else {
    terms <- "<all measure columns>"
  }
  tibble::tibble(
    terms = terms,
    id = x$id
  )
}


# ------------------------------------------------------------------------------
# Exploratory Functions (for prototyping and debugging)
# ------------------------------------------------------------------------------

#' Apply a Function to Each Sample's Measurements
#'
#' `measure_map()` applies a function to each sample's measurement data.
#' This function is intended for **exploration and prototyping**, not for
#' production pipelines. For reproducible preprocessing, use
#' [step_measure_map()] instead.
#'
#' @param .data A data frame containing one or more `measure_list` columns.
#' @param .f A function or formula to apply to each sample's measurement tibble.
#'   - If a **function**, it is used as-is.
#'   - If a **formula** (e.g., `~ { .x$value <- log(.x$value); .x }`), it is
#'     converted to a function using [rlang::as_function()].
#' @param .cols <[`tidy-select`][dplyr::dplyr_tidy_select]> Columns to apply
#'   the transformation to. Defaults to all `measure_list` columns.
#' @param ... Additional arguments passed to `.f`.
#' @param verbosity An integer controlling output verbosity:
#'   - `0`: Silent - suppress all messages and output from `.f`
#'   - `1`: Normal (default) - show output from `.f`
#' @param .error_call The execution environment for error reporting.
#'
#' @return A data frame with the specified measure columns transformed.
#'
#' @details
#'
#' ## Intended Use: Exploration, Not Production
#'
#' This function is designed for interactive exploration and debugging:
#'
#' ```
#' # Good: Prototyping a new transformation
#' baked_data |>
#'   measure_map(~ { .x$value <- my_experimental_fn(.x$value); .x })
#'
#' # Better: Once it works, put it in a recipe step
#' recipe(...) |>
#'   step_measure_map(my_experimental_fn) |>
#'   prep()
#' ```
#'
#' Unlike recipe steps, transformations applied with `measure_map()` are NOT:
#' - Automatically applied to new data
#' - Bundled into workflows
#' - Reproducible across sessions
#'
#' ## Function Requirements
#'
#' The function `.f` must:
#' - Accept a tibble with `location` and `value` columns
#' - Return a tibble with `location` and `value` columns
#' - Not change the number of rows
#'
#' @seealso
#' - [step_measure_map()] for production use in recipe pipelines
#' - [measure_map_safely()] for fault-tolerant exploration
#' - [measure_summarize()] for computing summary statistics
#'
#' @export
#'
#' @examples
#' library(recipes)
#'
#' # First, get data in internal format
#' rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   prep()
#'
#' baked_data <- bake(rec, new_data = NULL)
#'
#' # Explore a custom transformation
#' result <- measure_map(baked_data, ~ {
#'   # Subtract the minimum value from each spectrum
#'   .x$value <- .x$value - min(.x$value)
#'   .x
#' })
#'
#' # Once you're happy with it, use step_measure_map() in your recipe:
#' # recipe(...) |>
#' #   step_measure_map(~ { .x$value <- .x$value - min(.x$value); .x })
measure_map <- function(.data, .f, .cols = NULL, ...,
                        verbosity = 1L,
                        .error_call = rlang::caller_env()) {
  if (!is.data.frame(.data)) {
    cli::cli_abort(
      "{.arg .data} must be a data frame, not {.obj_type_friendly {(.data)}}.",
      call = .error_call
    )
  }

  # Convert formula to function if needed
  .f <- rlang::as_function(.f)

  # Determine which columns to process
  if (is.null(.cols)) {
    target_cols <- find_measure_cols(.data)
    if (length(target_cols) == 0) {
      cli::cli_abort(
        c(
          "No measure columns found in {.arg .data}.",
          "i" = "Use {.fn step_measure_input_wide} or
                 {.fn step_measure_input_long} first."
        ),
        call = .error_call
      )
    }
  } else {
    # Use tidyselect for column selection
    target_cols <- names(tidyselect::eval_select(
      rlang::enquo(.cols),
      .data,
      allow_rename = FALSE
    ))

    # Validate that selected columns are measure columns
    not_measure <- setdiff(target_cols, find_measure_cols(.data))
    if (length(not_measure) > 0) {
      cli::cli_abort(
        c(
          "Column{?s} {.field {not_measure}} {?is/are} not measure column{?s}.",
          "i" = "Only columns of class {.cls measure_list} can be processed."
        ),
        call = .error_call
      )
    }
  }

  # Apply transformation to each measure column
  capture <- verbosity == 0
  for (col in target_cols) {
    .data[[col]] <- .map_measure_col(
      .data[[col]],
      .f = .f,
      ...,
      .col_name = col,
      .capture = capture,
      .error_call = .error_call
    )
  }

  tibble::as_tibble(.data)
}


#' Apply a Function Safely to Each Sample's Measurements
#'
#' `measure_map_safely()` is a fault-tolerant version of [measure_map()] that
#' captures errors instead of stopping execution. This is useful when exploring
#' data that may have problematic samples.
#'
#' @inheritParams measure_map
#' @param .otherwise Value to use when `.f` fails for a sample. Default is
#'   `NULL`, which keeps the original (untransformed) measurement.
#'
#' @return A list with two elements:
#'   - `result`: A data frame with transformations applied where successful
#'   - `errors`: A tibble with columns `column`, `sample`, and `error`
#'
#' @seealso [measure_map()] for standard (fail-fast) mapping
#'
#' @export
#'
#' @examples
#' library(recipes)
#'
#' rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   prep()
#'
#' baked_data <- bake(rec, new_data = NULL)
#'
#' # A function that might fail for some samples
#' risky_transform <- function(x) {
#'   if (any(x$value < 0)) stop("Negative values not allowed")
#'   x$value <- log(x$value)
#'   x
#' }
#'
#' # Errors are captured, not thrown
#' result <- measure_map_safely(baked_data, risky_transform)
#'
#' # Check which samples failed
#' if (nrow(result$errors) > 0) {
#'   print(result$errors)
#' }
measure_map_safely <- function(.data, .f, .cols = NULL, ...,
                               .otherwise = NULL,
                               .error_call = rlang::caller_env()) {
  if (!is.data.frame(.data)) {
    cli::cli_abort(
      "{.arg .data} must be a data frame, not {.obj_type_friendly {(.data)}}.",
      call = .error_call
    )
  }

  .f <- rlang::as_function(.f)

  if (is.null(.cols)) {
    target_cols <- find_measure_cols(.data)
    if (length(target_cols) == 0) {
      cli::cli_abort(
        c(
          "No measure columns found in {.arg .data}.",
          "i" = "Use {.fn step_measure_input_wide} or
                 {.fn step_measure_input_long} first."
        ),
        call = .error_call
      )
    }
  } else {
    target_cols <- names(tidyselect::eval_select(
      rlang::enquo(.cols),
      .data,
      allow_rename = FALSE
    ))

    not_measure <- setdiff(target_cols, find_measure_cols(.data))
    if (length(not_measure) > 0) {
      cli::cli_abort(
        c(
          "Column{?s} {.field {not_measure}} {?is/are} not measure column{?s}.",
          "i" = "Only columns of class {.cls measure_list} can be processed."
        ),
        call = .error_call
      )
    }
  }

  errors <- tibble::tibble(
    column = character(),
    sample = integer(),
    error = character()
  )

  for (col in target_cols) {
    map_result <- .map_measure_col_safely(
      .data[[col]],
      .f = .f,
      ...,
      .otherwise = .otherwise,
      .col_name = col
    )
    .data[[col]] <- map_result$result
    errors <- rbind(errors, map_result$errors)
  }

  list(
    result = tibble::as_tibble(.data),
    errors = errors
  )
}


# ------------------------------------------------------------------------------
# Analysis Functions (for understanding data, not transforming)
# ------------------------------------------------------------------------------

#' Summarize Measurements Across Samples
#'
#' `measure_summarize()` computes summary statistics for each measurement
#' location across all samples. This is useful for understanding your data,
#' computing reference spectra, or identifying outliers.
#'
#' @param .data A data frame containing one or more `measure_list` columns.
#' @param .cols <[`tidy-select`][dplyr::dplyr_tidy_select]> Columns to
#'   summarize. Defaults to all `measure_list` columns.
#' @param .fns A named list of summary functions. Each function should accept
#'   a numeric vector and return a single value. Default is
#'   `list(mean = mean, sd = sd)`.
#' @param na.rm Logical. Should NA values be removed? Default is `TRUE`.
#'
#' @return A tibble with one row per measurement location and columns for
#'   each summary statistic.
#'
#' @details
#' This function does NOT transform data; it summarizes it. Common uses:
#'
#' - **Mean spectrum**: The average spectrum across all samples
#' - **Reference spectrum**: For MSC-style corrections
#' - **Variability**: Standard deviation at each wavelength
#' - **Quality control**: Identify problematic wavelength regions
#'
#' @export
#'
#' @examples
#' library(recipes)
#' library(ggplot2)
#'
#' rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   prep()
#'
#' baked_data <- bake(rec, new_data = NULL)
#'
#' # Compute mean and SD at each wavelength
#' summary_stats <- measure_summarize(baked_data)
#' summary_stats
#'
#' # Visualize mean spectrum with confidence band
#' ggplot(summary_stats, aes(x = location)) +
#'   geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd), alpha = 0.3) +
#'   geom_line(aes(y = mean)) +
#'   labs(x = "Channel", y = "Transmittance", title = "Mean Spectrum +/- 1 SD")
#'
#' # Custom summary functions
#' measure_summarize(
#'   baked_data,
#'   .fns = list(
#'     median = median,
#'     q25 = function(x) quantile(x, 0.25),
#'     q75 = function(x) quantile(x, 0.75)
#'   )
#' )
measure_summarize <- function(.data, .cols = NULL,
                              .fns = list(mean = mean, sd = stats::sd),
                              na.rm = TRUE) {
  if (!is.data.frame(.data)) {
    cli::cli_abort(
      "{.arg .data} must be a data frame, not {.obj_type_friendly {(.data)}}."
    )
  }

  if (is.null(.cols)) {
    target_cols <- find_measure_cols(.data)
    if (length(target_cols) == 0) {
      cli::cli_abort(
        c(
          "No measure columns found in {.arg .data}.",
          "i" = "Use {.fn step_measure_input_wide} or
                 {.fn step_measure_input_long} first."
        )
      )
    }
  } else {
    target_cols <- names(tidyselect::eval_select(
      rlang::enquo(.cols),
      .data,
      allow_rename = FALSE
    ))
  }

  if (is.null(names(.fns)) || any(names(.fns) == "")) {
    cli::cli_abort("All functions in {.arg .fns} must be named.")
  }

  results <- list()

  for (col in target_cols) {
    measure_col <- .data[[col]]

    # Get locations from first sample (assume all are aligned)
    locations <- measure_col[[1]]$location

    # Convert to matrix for efficient computation
    mat <- measure_to_matrix(measure_col)

    # Compute each summary statistic
    col_results <- tibble::tibble(location = locations)

    for (fn_name in names(.fns)) {
      fn <- .fns[[fn_name]]
      if (na.rm) {
        # Remove NAs before applying function (works with any function)
        col_results[[fn_name]] <- apply(mat, 2, function(x) fn(x[!is.na(x)]))
      } else {
        col_results[[fn_name]] <- apply(mat, 2, fn)
      }
    }

    # Add column prefix if multiple measure columns
    if (length(target_cols) > 1) {
      names(col_results)[-1] <- paste0(col, "_", names(col_results)[-1])
    }

    results[[col]] <- col_results
  }

  if (length(results) == 1) {
    results[[1]]
  } else {
    purrr::reduce(results, dplyr::left_join, by = "location")
  }
}


# ------------------------------------------------------------------------------
# Internal helper functions
# ------------------------------------------------------------------------------

#' Map over a single measure column
#' @noRd
.map_measure_col <- function(x, .f, ..., .col_name, .capture = FALSE,
                             .error_call) {
  n_samples <- length(x)
  result <- vector("list", n_samples)
  fn_args <- list(...)

  for (i in seq_len(n_samples)) {
    result[[i]] <- tryCatch(
      {
        out <- .eval_transform(.f, x[[i]], fn_args, capture = .capture)
        .validate_map_output(out, x[[i]], i, .col_name, .error_call)
        out
      },
      error = function(e) {
        cli::cli_abort(
          c(
            "Error applying function to sample {i} in column {.field {(.col_name)}}.",
            "x" = conditionMessage(e)
          ),
          call = .error_call
        )
      }
    )
  }

  new_measure_list(result)
}

#' Map safely over a single measure column
#' @noRd
.map_measure_col_safely <- function(x, .f, ..., .otherwise, .col_name) {
  n_samples <- length(x)
  result <- vector("list", n_samples)
  errors <- tibble::tibble(
    column = character(),
    sample = integer(),
    error = character()
  )

  for (i in seq_len(n_samples)) {
    tryCatch(
      {
        out <- .f(x[[i]], ...)

        if (!is.data.frame(out) ||
            !all(c("location", "value") %in% names(out)) ||
            nrow(out) != nrow(x[[i]])) {
          stop("Invalid output structure")
        }

        result[[i]] <- out
      },
      error = function(e) {
        errors <<- rbind(
          errors,
          tibble::tibble(
            column = .col_name,
            sample = i,
            error = conditionMessage(e)
          )
        )

        if (is.null(.otherwise)) {
          result[[i]] <<- x[[i]]
        } else {
          result[[i]] <<- .otherwise
        }
      }
    )
  }

  list(
    result = new_measure_list(result),
    errors = errors
  )
}

#' Validate map output structure
#' @noRd
.validate_map_output <- function(out, original, sample_idx, col_name, error_call) {
  if (!is.data.frame(out)) {
    cli::cli_abort(
      c(
        "Mapping function must return a data frame for sample {sample_idx}
         in column {.field {(col_name)}}.",
        "x" = "Got {.obj_type_friendly {(out)}} instead."
      ),
      call = error_call
    )
  }

  required_cols <- c("location", "value")
  missing_cols <- setdiff(required_cols, names(out))
  if (length(missing_cols) > 0) {
    cli::cli_abort(
      c(
        "Mapping function output missing required {cli::qty(missing_cols)}
         column{?s} for sample {sample_idx} in column {.field {(col_name)}}.",
        "x" = "Missing: {.field {missing_cols}}."
      ),
      call = error_call
    )
  }

  if (nrow(out) != nrow(original)) {
    cli::cli_abort(
      c(
        "Mapping function changed the number of measurements for sample
         {sample_idx} in column {.field {(col_name)}}.",
        "x" = "Input had {nrow(original)} rows, output has {nrow(out)} rows.",
        "i" = "The number of measurement points must remain constant."
      ),
      call = error_call
    )
  }
}

#' Evaluate a transformation function with optional output capture
#'
#' Following the parsnip pattern from `eval_mod()`, this function
#' evaluates the transformation function and optionally captures
#' console output when verbosity is set to 0.
#'
#' @param fn The transformation function
#' @param x The input data (measure_tbl)
#' @param fn_args Additional arguments to pass to fn
#' @param capture Logical. If TRUE, suppress console output.
#' @return The result of applying fn to x
#' @noRd
.eval_transform <- function(fn, x, fn_args, capture = FALSE) {
  if (capture) {
    junk <- utils::capture.output(
      res <- do.call(fn, c(list(x), fn_args))
    )
  } else {
    res <- do.call(fn, c(list(x), fn_args))
  }
  res
}
