# ==============================================================================
# Sample-wise mapping infrastructure for measure columns
#
# This file provides utilities for applying transformations to each sample's
# measurements in a data frame with measure_list columns.
#
# See GitHub Issue #9 for context.
#
# Components:
# 1. step_measure_map() - Recipe step for custom transformations (pipeline)
# 2. measure_map() - Standalone function for exploratory use
# 3. measure_transform() - Simple value-only transformation helper
# 4. Convenience functions - Common operations (center, scale, etc.)
# ==============================================================================

# ------------------------------------------------------------------------------
# Recipe Step: step_measure_map
# ------------------------------------------------------------------------------

#' Apply a Custom Function to Measurements
#'
#' `step_measure_map()` creates a *specification* of a recipe step that applies
#' a custom function to each sample's measurements. This allows arbitrary
#' sample-wise transformations to be included in a reproducible recipe pipeline.
#'
#' @param recipe A recipe object. The step will be added to the sequence of
#'   operations for this recipe.
#' @param fn A function to apply to each sample's measurement tibble. The

#'   function should accept a tibble with `location` and `value` columns and
#'   return a tibble with the same structure. Can also be a formula (e.g.,
#'   `~ .x$value * 2`) which will be converted via [rlang::as_function()].
#' @param ... Additional arguments passed to `fn` during baking.
#' @param measures An optional character vector of measure column names to
#'   process. If `NULL` (the default), all measure columns will be processed.
#' @param role Not used by this step since no new variables are created.
#' @param trained A logical to indicate if the step has been trained.
#' @param skip A logical. Should the step be skipped when the recipe is baked?
#' @param id A character string that is unique to this step.
#'
#' @return An updated version of `recipe` with the new step added.
#'
#' @details
#' This step enables custom sample-wise transformations within the recipes
#' framework. Unlike using [measure_map()] directly (which operates on baked
#' data), this step is part of the recipe and will be applied consistently
#' during both `prep()` and `bake()`.
#'
#' ## Function Requirements
#'
#' The function `fn` must:
#' - Accept a tibble with `location` and `value` columns
#' - Return a tibble with `location` and `value` columns
#' - Not change the number of rows
#'
#' ## When to Use
#'
#' Use `step_measure_map()` when you want your custom transformation to be:
#' - Part of a reproducible recipe
#' - Applied consistently to training and test data
#' - Included in a workflow for tuning/resampling
#'
#' Use [measure_map()] directly when you want to:
#' - Quickly explore or prototype transformations
#' - Apply one-off transformations outside a recipe
#'
#' @seealso
#' - [measure_map()] for standalone (non-recipe) usage
#' - [measure_transform()] for simple value-only transformations
#'
#' @family measure-preprocessing
#' @export
#'
#' @examples
#' library(recipes)
#'
#' # Define a custom transformation function
#' log_transform <- function(x) {
#'   x$value <- log1p(x$value)
#'   x
#' }
#'
#' # Use in a recipe pipeline
#' rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_map(log_transform) |>
#'   step_measure_snv() |>
#'   prep()
#'
#' bake(rec, new_data = NULL)
#'
#' # Using formula syntax
#' rec2 <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_map(~ { .x$value <- .x$value^2; .x }) |>
#'   prep()
step_measure_map <- function(
    recipe,
    fn,
    ...,
    measures = NULL,
    role = NA,
    trained = FALSE,
    skip = FALSE,
    id = recipes::rand_id("measure_map")) {
  # Capture the function and additional arguments
  fn <- rlang::enquo(fn)
  dots <- rlang::enquos(...)

  recipes::add_step(
    recipe,
    step_measure_map_new(
      fn = fn,
      fn_args = dots,
      measures = measures,
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_measure_map_new <- function(fn, fn_args, measures, role, trained, skip, id) {
  recipes::step(
    subclass = "measure_map",
    fn = fn,
    fn_args = fn_args,
    measures = measures,
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
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_map <- function(object, new_data, ...) {
  # Evaluate the function
  fn <- rlang::eval_tidy(object$fn)
  fn <- rlang::as_function(fn)

  # Evaluate additional arguments
  fn_args <- lapply(object$fn_args, rlang::eval_tidy)

  for (col in object$measures) {
    # Apply the function to each sample
    result <- purrr::map(new_data[[col]], function(x) {
      do.call(fn, c(list(x), fn_args))
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
# Simple Value Transformation Helper
# ------------------------------------------------------------------------------

#' Transform Measurement Values
#'
#' `measure_transform()` provides a simplified interface for transforming
#' measurement values when you only need to modify the `value` column without
#' accessing `location`. This is more concise than [measure_map()] for simple
#' operations.
#'
#' @param .data A data frame containing one or more `measure_list` columns.
#' @param .f A function or formula that transforms a numeric vector. The
#'   function receives the `value` vector and should return a numeric vector
#'   of the same length.
#' @param .cols Columns to transform. Defaults
#'   to all measure columns.
#' @param ... Additional arguments passed to `.f`.
#'
#' @return A data frame with transformed measure columns.
#'
#' @details
#' This is syntactic sugar for common transformations where you only need
#' to modify values. Instead of:
#'
#' ```
#' measure_map(data, ~ { .x$value <- log(.x$value); .x })
#' ```
#'
#' You can write:
#'
#' ```
#' measure_transform(data, log)
#' ```
#'
#' @seealso [measure_map()] for full access to both location and value
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
#' baked <- bake(rec, new_data = NULL)
#'
#' # Simple transformations
#' measure_transform(baked, log1p)
#' measure_transform(baked, ~ .x * 100)
#' measure_transform(baked, ~ (.x - min(.x)) / (max(.x) - min(.x)))  # min-max scale
measure_transform <- function(.data, .f, .cols = NULL, ...) {
  .f <- rlang::as_function(.f)

  # Wrap the value-only function to work with measure_map
  wrapper_fn <- function(x, ...) {
    x$value <- .f(x$value, ...)
    x
  }

  measure_map(.data, wrapper_fn, .cols = .cols, ...)
}


# ------------------------------------------------------------------------------
# Convenience Functions for Common Operations
# ------------------------------------------------------------------------------

#' Center Measurements
#'
#' Subtract the mean from each sample's values, centering each spectrum
#' around zero.
#'
#' @param .data A data frame with measure columns.
#' @param .cols Columns to center. Defaults to all measure columns.
#'
#' @return Data frame with centered measurements.
#'
#' @export
#' @examples
#' library(recipes)
#'
#' rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   prep()
#'
#' baked <- bake(rec, new_data = NULL)
#'
#' centered <- measure_center(baked)
#' # Each sample now has mean = 0
measure_center <- function(.data, .cols = NULL) {
  measure_map(.data, function(x) {
    x$value <- x$value - mean(x$value, na.rm = TRUE)
    x
  }, .cols = .cols)
}

#' Scale Measurements
#'
#' Divide each sample's values by their standard deviation.
#'
#' @param .data A data frame with measure columns.
#' @param .cols Columns to scale. Defaults to all measure columns.
#' @param center Logical. Should values be centered before scaling? Default
#'   is `FALSE`. Set to `TRUE` for z-score standardization.
#'
#' @return Data frame with scaled measurements.
#'
#' @export
#' @examples
#' library(recipes)
#'
#' rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   prep()
#'
#' baked <- bake(rec, new_data = NULL)
#'
#' # Scale only
#' scaled <- measure_scale(baked)
#'
#' # Center and scale (equivalent to SNV)
#' standardized <- measure_scale(baked, center = TRUE)
measure_scale <- function(.data, .cols = NULL, center = FALSE) {
  measure_map(.data, function(x) {
    values <- x$value
    if (center) {
      values <- values - mean(values, na.rm = TRUE)
    }
    x$value <- values / stats::sd(values, na.rm = TRUE)
    x
  }, .cols = .cols)
}

#' Normalize Measurements to Range
#'
#' Scale each sample's values to a specified range (default 0 to 1).
#'
#' @param .data A data frame with measure columns.
#' @param .cols Columns to normalize. Defaults to all measure columns.
#' @param range A numeric vector of length 2 specifying the target range.
#'   Default is `c(0, 1)`.
#'
#' @return Data frame with range-normalized measurements.
#'
#' @export
#' @examples
#' library(recipes)
#'
#' rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   prep()
#'
#' baked <- bake(rec, new_data = NULL)
#'
#' # Normalize to [0, 1]
#' normalized <- measure_normalize(baked)
#'
#' # Normalize to [-1, 1]
#' normalized2 <- measure_normalize(baked, range = c(-1, 1))
measure_normalize <- function(.data, .cols = NULL, range = c(0, 1)) {
  if (length(range) != 2 || range[1] >= range[2]) {
    cli::cli_abort("{.arg range} must be a numeric vector of length 2 with range[1] < range[2].")
  }

  measure_map(.data, function(x) {
    values <- x$value
    min_val <- min(values, na.rm = TRUE)
    max_val <- max(values, na.rm = TRUE)

    if (max_val == min_val) {
      # All values are the same - use midpoint of range
      x$value <- rep(mean(range), length(values))
    } else {
      # Min-max normalization scaled to target range
      scaled <- (values - min_val) / (max_val - min_val)
      x$value <- scaled * (range[2] - range[1]) + range[1]
    }
    x
  }, .cols = .cols)
}

#' Apply Log Transformation to Measurements
#'
#' Apply log transformation to measurement values. Uses `log1p()` by default
#' for numerical stability with values near zero.
#'
#' @param .data A data frame with measure columns.
#' @param .cols Columns to transform. Defaults to all measure columns.
#' @param base The base of the logarithm. Default is `exp(1)` (natural log).
#'   Use 10 for log10, 2 for log2.
#' @param offset Value added before taking log to handle zeros. Default is 1
#'   (using `log1p()` behavior). Set to 0 for pure log (will produce -Inf for 0).
#'
#' @return Data frame with log-transformed measurements.
#'
#' @export
#' @examples
#' library(recipes)
#'
#' rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   prep()
#'
#' baked <- bake(rec, new_data = NULL)
#'
#' # Natural log with offset (log1p)
#' log_data <- measure_log(baked)
#'
#' # Log base 10
#' log10_data <- measure_log(baked, base = 10)
measure_log <- function(.data, .cols = NULL, base = exp(1), offset = 1) {
  measure_map(.data, function(x) {
    x$value <- log(x$value + offset, base = base)
    x
  }, .cols = .cols)
}


# ------------------------------------------------------------------------------
# Standalone Functions (for exploratory use outside recipes)
# ------------------------------------------------------------------------------

#' Apply a function to each sample's measurements
#'
#' `measure_map()` applies a function to each sample's measurement data
#' (each element of a `measure_list` column). This is the primary interface
#' for implementing sample-wise operations on spectral or measurement data.
#'
#' @param .data A data frame containing one or more `measure_list` columns.
#' @param .f A function or formula to apply to each sample's measurement tibble.
#'   - If a **function**, it is used as-is. The function should accept a
#'     `measure_tbl` (a tibble with `location` and `value` columns) and return
#'     a modified tibble with the same structure.
#'   - If a **formula** (e.g., `~ my_func(.x, arg = 1)`), it is converted to a
#'     function using [rlang::as_function()]. Use `.x` to refer to the input
#'     measurement tibble.
#' @param .cols <[`tidy-select`][dplyr::dplyr_tidy_select]> Columns to apply
#'   the transformation to. Defaults to all `measure_list` columns. Use this
#'   to limit processing to specific measure columns.
#' @param ... Additional arguments passed to `.f`.
#' @param .error_call The execution environment of a currently running function,
#'   e.
#'
#' @return A data frame with the same structure as `.data`, with the specified
#'   measure columns transformed.
#'
#' @details
#' ## Function Requirements
#'
#' The function `.f` must:
#' - Accept a tibble with `location` and `value` columns (a `measure_
#')
#' - Return a tibble with `location` and `value` columns
#' - Not change the number of rows (measurements must remain aligned)
#'
#' Common operations that can be applied include:
#' - Smoothing (LOESS, Savitzky-Golay)
#' - Baseline correction
#' - Normalization
#' - Derivative computation
#'
#' ## Error Handling
#'
#' If `.f` fails for a particular sample, `measure_map()` will:
#' - Report which sample (row number) caused the error
#' - Include the original error message
#' - Stop execution (fail-fast behavior)
#'
#' For more fault-tolerant mapping, use [measure_map_safely()] instead.
#'
#' @seealso
#' - [measure_map_safely()] for fault-tolerant mapping
#' - [measure_summarize()] for extracting summary statistics
#' - [is_measure_list()] for detecting measure columns
#'
#' @export
#'
#' @examples
#' library(recipes)
#' library(dplyr)
#'
#' # Prepare data with measure columns
#' rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   prep()
#'
#' baked_data <- bake(rec, new_data = NULL)
#'
#' # Example 1: Apply a custom centering function
#' center_spectrum <- function(x) {
#'   x$value <- x$value - mean(x$value)
#'   x
#' }
#'
#' centered <- measure_map(baked_data, center_spectrum)
#'
#' # Example 2: Using formula syntax with arguments
#' scale_spectrum <- function(x, scale_factor = 1) {
#'   x$value <- x$value * scale_factor
#'   x
#' }
#'
#' scaled <- measure_map(baked_data, ~ scale_spectrum(.x, scale_factor = 100))
#'
#' # Example 3: Anonymous function with formula
#' log_transformed <- measure_map(baked_data, ~ {
#'   .x$value <- log1p(.x$value)
#'   .x
#' })
measure_map <- function(.data, .f, .cols = NULL, ...,
                        .error_call = rlang::caller_env()) {
  # Validate input

if (!is.data.frame(.data)) {
    cli::cli_abort(
      "{.arg .data} must be a data frame, not {.obj_type_friendly {.data}}.",
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
  for (col in target_cols) {
    .data[[col]] <- .map_measure_col(
      .data[[col]],
      .f = .f,
      ...,
      .col_name = col,
      .error_call = .error_call
    )
  }

  tibble::as_tibble(.data)
}

#' Internal function to map over a single measure column
#'
#' @param x A measure_list column
#' @param .f The function to apply
#' @param ... Additional arguments to .f
#' @param .col_name Name of the column (for error messages)
#' @param .error_call Calling environment for errors
#' @return A new measure_list
#' @noRd
.map_measure_col <- function(x, .f, ..., .col_name, .error_call) {
  n_samples <- length(x)
  result <- vector("list", n_samples)

  for (i in seq_len(n_samples)) {
    result[[i]] <- tryCatch(
      {
        out <- .f(x[[i]], ...)
        .validate_map_output(out, x[[i]], i, .col_name, .error_call)
        out
      },
      error = function(e) {
        cli::cli_abort(
          c(
            "Error applying function to sample {i} in column {.field {.col_name}}.",
            "x" = conditionMessage(e)
          ),
          call = .error_call
        )
      }
    )
  }

  new_measure_list(result)
}

#' Validate the output of a mapping function
#'
#' @param out The output from the mapping function
#' @param original The original input
#' @param sample_idx The sample index (for error messages)
#' @param col_name The column name (for error messages)
#' @param error_call Calling environment for errors
#' @noRd
.validate_map_output <- function(out, original, sample_idx, col_name, error_call) {
  if (!is.data.frame(out)) {
    cli::cli_abort(
      c(
        "Mapping function must return a data frame for sample {sample_idx}
         in column {.field {col_name}}.",
        "x" = "Got {.obj_type_friendly {out}} instead."
      ),
      call = error_call
    )
  }

  required_cols <- c("location", "value")
  missing_cols <- setdiff(required_cols, names(out))
  if (length(missing_cols) > 0) {
    cli::cli_abort(
      c(
        "Mapping function output missing required column{?s} for sample
         {sample_idx} in column {.field {col_name}}.",
        "x" = "Missing: {.field {missing_cols}}."
      ),
      call = error_call
    )
  }

  if (nrow(out) != nrow(original)) {
    cli::cli_abort(
      c(
        "Mapping function changed the number of measurements for sample
         {sample_idx} in column {.field {col_name}}.",
        "x" = "Input had {nrow(original)} rows, output has {nrow(out)} rows.",
        "i" = "The number of measurement points must remain constant."
      ),
      call = error_call
    )
  }
}


#' Apply a function safely to each sample's measurements
#'
#' `measure_map_safely()` is a fault-tolerant version of [measure_map()] that
#' captures errors instead of stopping execution. This is useful for exploratory
#' analysis or when some samples may have problematic data.
#'
#' @inheritParams measure_map
#' @param .otherwise Value to use when `.f` fails for a sample. Default is
#'   `NULL`, which keeps the original (untransformed) measurement.
#'
#' @return A list with two elements:
#'   - `result`: A data frame with the same structure as `.data`, with
#'     transformations applied where successful
#'   - `errors`: A tibble with columns `column`, `sample`, and `error`
#'     describing any failures
#'
#' @export
#'
#' @examples
#' library(recipes)
#'
#' # Prepare data
#' rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   prep()
#'
#' baked_data <- bake(rec, new_data = NULL)
#'
#' # A function that might fail for some samples
#' risky_transform <- function(x) {
#'   if (any(x$value < 0)) {
#'     stop("Negative values not allowed")
#'   }
#'   x$value <- log(x$value)
#'   x
#' }
#'
#' # Apply safely - errors are captured, not thrown
#' result <- measure_map_safely(baked_data, risky_transform)
#'
#' # Check for errors
#' if (nrow(result$errors) > 0) {
#'   print(result$errors)
#' }
#'
#' # Use the (partially) transformed data
#' transformed_data <- result$result
measure_map_safely <- function(.data, .f, .cols = NULL, ...,
                               .otherwise = NULL,
                               .error_call = rlang::caller_env()) {
  # Validate input
  if (!is.data.frame(.data)) {
    cli::cli_abort(
      "{.arg .data} must be a data frame, not {.obj_type_friendly {.data}}.",
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

  # Collect errors
  errors <- tibble::tibble(
    column = character(),
    sample = integer(),
    error = character()
  )

  # Apply transformation to each measure column
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

#' Internal function to map safely over a single measure column
#'
#' @param x A measure_list column
#' @param .f The function to apply
#' @param ... Additional arguments to .f
#' @param .otherwise Value to use on error
#' @param .col_name Name of the column
#' @return A list with result and errors
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

        # Validate output structure
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

        # Use .otherwise or keep original
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


#' Summarize measurements across samples
#'
#' `measure_summarize()` computes summary statistics for each measurement
#' location across all samples. This is useful for computing mean spectra,
#' standard deviations, or other aggregate statistics.
#'
#' @param .data A data frame containing one or more `measure_list` columns.
#' @param .cols <[`tidy-select`][dplyr::dplyr_tidy_select]> Columns to
#'   summarize. Defaults to all `measure_list` columns.
#' @param .fns A list of summary functions to apply. Each function should
#'   accept a numeric vector and return a single numeric value. Default
#'   is `list(mean = mean, sd = sd)`.
#' @param na.rm Logical. Should NA values be removed before computing
#'   summaries? Default is `TRUE`.
#'
#' @return A tibble with one row per measurement location and columns for
#'   each summary statistic. For multiple measure columns, column names
#'   are prefixed with the measure column name.
#'
#' @export
#'
#' @examples
#' library(recipes)
#'
#' # Prepare data
#' rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   prep()
#'
#' baked_data <- bake(rec, new_data = NULL)
#'
#' # Compute mean and SD at each wavelength
#' summary_stats <- measure_summarize(baked_data)
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
      "{.arg .data} must be a data frame, not {.obj_type_friendly {.data}}."
    )
  }

  # Determine which columns to process
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

  # Validate function names
  if (is.null(names(.fns)) || any(names(.fns) == ""))
 {
    cli::cli_abort("All functions in {.arg .fns} must be named.")
  }

  # Compute summaries for each column
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
        col_results[[fn_name]] <- apply(mat, 2, fn, na.rm = TRUE)
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

  # Combine results
  if (length(results) == 1) {
    results[[1]]
  } else {
    # Join by location
    purrr::reduce(results, dplyr::left_join, by = "location")
  }
}
