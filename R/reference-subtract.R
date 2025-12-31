# ==============================================================================
# Reference-Based Corrections: Blank and Reference Subtraction
#
# This file contains steps for subtracting or dividing by reference spectra:
# - step_measure_subtract_blank: Subtract blank (external or learned)
# - step_measure_subtract_reference: Subtract/divide by external reference
# - step_measure_ratio_reference: Compute ratio to reference
# ==============================================================================

# ==============================================================================
# step_measure_subtract_blank
# ==============================================================================

#' Subtract Blank Measurement
#'
#' `step_measure_subtract_blank()` creates a *specification* of a recipe step
#' that subtracts or divides by a blank measurement. The blank can be provided
#' externally or learned from training data.
#'
#' @param recipe A recipe object. The step will be added to the sequence of
#'   operations for this recipe.
#' @param blank An optional external blank to use. Can be:
#'   - A `measure_tbl` object with `location` and `value` columns
#'   - A numeric vector (must match the number of locations in data)
#'   - A data.frame with `location` and `value` columns (will be interpolated)
#'   If `NULL`, the blank is learned from training data using `blank_col` and
#'   `blank_value`.
#' @param blank_col An optional column name (unquoted) that identifies sample
#'   types. Used with `blank_value` to identify blank samples in training data.
#' @param blank_value The value in `blank_col` that identifies blank samples.
#'   When the step is prepped, the mean of all blank samples is computed and
#'   stored for use during baking.
#' @param method The correction method to apply:
#'   - `"subtract"` (default): Subtract the blank from each spectrum
#'   - `"divide"`: Divide each spectrum by the blank
#' @param measures An optional character vector of measure column names to
#'   process. If `NULL` (the default), all measure columns (columns with class
#'   `measure_list`) will be processed.
#' @param role Not used by this step since no new variables are created.
#' @param trained A logical to indicate if the quantities for preprocessing
#'   have been estimated.
#' @param learned_blank A named list containing the learned blank values for
#'   each measure column. This is `NULL` until the step is trained.
#' @param skip A logical. Should the step be skipped when the recipe is baked?
#' @param id A character string that is unique to this step to identify it.
#'
#' @return An updated version of `recipe` with the new step added to the
#'   sequence of any existing operations.
#'
#' @details
#' Blank subtraction is a fundamental preprocessing step in analytical
#' chemistry. It removes background signal that is present in all measurements
#' but is not related to the analyte of interest.
#'
#' **Two modes of operation:**
#'
#' 1. **External blank**: You provide a blank spectrum directly via the `blank`
#'    argument. This is useful when you have a known reference blank.
#'
#' 2. **Learned blank**: You specify which samples are blanks in your training
#'    data using `blank_col` and `blank_value`. During `prep()`, the mean of
#'    all blank samples is computed and stored. This approach is useful for
#'    batch-specific blank correction.
#'
#' **Common use cases:**
#' - UV-Vis: Remove solvent absorbance
#' - IR: Remove atmospheric CO2/H2O interference
#' - Fluorescence: Remove buffer background and Raman scatter
#' - Chromatography: Remove ghost peaks and solvent artifacts
#'
#' **No selectors should be supplied to this step function**. The data should be
#' in the internal format produced by [step_measure_input_wide()] or
#' [step_measure_input_long()].
#'
#' # Tidying
#'
#' When you [`tidy()`][recipes::tidy.recipe()] this step, a tibble with columns
#' `terms`, `method`, `blank_source`, and `id` is returned.
#'
#' @family measure-preprocessing
#' @seealso [step_measure_subtract_reference()] for simpler external reference
#' @export
#'
#' @examples
#' library(recipes)
#'
#' # Example with external blank (numeric vector)
#' blank_spectrum <- rep(0.1, 100)
#'
#' rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_subtract_blank(blank = blank_spectrum)
#'
#' # Example learning blank from training data
#' # (assuming sample_type column with "blank" values)
#' # rec <- recipe(outcome ~ ., data = my_data) |>
#' #   step_measure_input_long(...) |>
#' #   step_measure_subtract_blank(blank_col = sample_type, blank_value = "blank")
step_measure_subtract_blank <- function(
  recipe,
  blank = NULL,
  blank_col = NULL,
  blank_value = NULL,
  method = "subtract",
  measures = NULL,
  role = NA,
  trained = FALSE,
  learned_blank = NULL,
  skip = FALSE,
  id = recipes::rand_id("measure_subtract_blank")
) {
  # Capture blank_col - use NULL directly if not provided, otherwise capture as

  # string to avoid quosure issues with recipes' tune detection
  blank_col_expr <- rlang::enquo(blank_col)
  blank_col_name <- if (rlang::quo_is_null(blank_col_expr)) {
    NULL
  } else {
    rlang::as_name(blank_col_expr)
  }

  recipes::add_step(
    recipe,
    step_measure_subtract_blank_new(
      blank = blank,
      blank_col = blank_col_name,
      blank_value = blank_value,
      method = method,
      measures = measures,
      role = role,
      trained = trained,
      learned_blank = learned_blank,
      skip = skip,
      id = id
    )
  )
}

step_measure_subtract_blank_new <- function(
  blank,
  blank_col,
  blank_value,
  method,
  measures,
  role,
  trained,
  learned_blank,
  skip,
  id
) {
  recipes::step(
    subclass = "measure_subtract_blank",
    blank = blank,
    blank_col = blank_col,
    blank_value = blank_value,
    method = method,
    measures = measures,
    role = role,
    trained = trained,
    learned_blank = learned_blank,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_subtract_blank <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  # Resolve which columns to process
  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  # Validate method
  if (!x$method %in% c("subtract", "divide")) {
    cli::cli_abort(
      "{.arg method} must be {.val subtract} or {.val divide}, not {.val {x$method}}."
    )
  }

  # Determine blank source and compute learned_blank
  learned_blank <- list()

  if (!is.null(x$blank)) {
    # Mode A: External blank provided
    learned_blank <- .validate_external_reference(
      x$blank,
      training,
      measure_cols,
      "blank"
    )
  } else {
    # Mode B: Learn from training data
    if (is.null(x$blank_col) || is.null(x$blank_value)) {
      cli::cli_abort(c(
        "Must provide either {.arg blank} or both {.arg blank_col} and {.arg blank_value}.",
        "i" = "Use {.arg blank} to provide an external blank spectrum.",
        "i" = "Use {.arg blank_col}/{.arg blank_value} to identify blanks in training data."
      ))
    }

    # Get blank column name (already a string from step constructor)
    blank_col_name <- x$blank_col

    if (!blank_col_name %in% names(training)) {
      cli::cli_abort(
        "Column {.val {blank_col_name}} not found in training data."
      )
    }

    # Find blank samples
    is_blank <- training[[blank_col_name]] == x$blank_value
    n_blanks <- sum(is_blank, na.rm = TRUE)

    if (n_blanks == 0) {
      cli::cli_abort(c(
        "No blank samples found in training data.",
        "i" = "Looking for {.val {x$blank_value}} in column {.val {blank_col_name}}.",
        "i" = "Found values: {.val {unique(training[[blank_col_name]])}}."
      ))
    }

    cli::cli_inform(
      "Learning blank from {n_blanks} sample{?s} in training data."
    )

    # Compute mean blank for each measure column
    for (col in measure_cols) {
      blank_spectra <- training[[col]][is_blank]
      learned_blank[[col]] <- .compute_mean_spectrum(blank_spectra)
    }
  }

  step_measure_subtract_blank_new(
    blank = x$blank,
    blank_col = x$blank_col,
    blank_value = x$blank_value,
    method = x$method,
    measures = measure_cols,
    role = x$role,
    trained = TRUE,
    learned_blank = learned_blank,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_subtract_blank <- function(object, new_data, ...) {
  for (col in object$measures) {
    blank <- object$learned_blank[[col]]
    result <- .apply_reference_correction(new_data[[col]], blank, object$method)
    new_data[[col]] <- new_measure_list(result)
  }
  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_subtract_blank <- function(
  x,
  width = max(20, options()$width - 30),
  ...
) {
  title <- "Blank subtraction"
  method_str <- if (x$method == "divide") " (divide)" else ""

  if (x$trained) {
    source <- if (!is.null(x$blank)) "external" else "learned"
    cat(
      title,
      method_str,
      " [",
      source,
      "]",
      " on <internal measurements>",
      sep = ""
    )
  } else {
    cat(title, method_str, sep = "")
  }
  cat("\n")

  invisible(x)
}

#' @rdname tidy.recipe
#' @export
#' @keywords internal
tidy.step_measure_subtract_blank <- function(x, ...) {
  if (is_trained(x)) {
    terms <- x$measures
    blank_source <- if (!is.null(x$blank)) "external" else "learned"
  } else {
    terms <- "<all measure columns>"
    blank_source <- NA_character_
  }
  tibble::tibble(
    terms = terms,
    method = x$method,
    blank_source = blank_source,
    id = x$id
  )
}

#' @export
required_pkgs.step_measure_subtract_blank <- function(x, ...) {
  c("measure")
}

# ==============================================================================
# step_measure_subtract_reference
# ==============================================================================

#' Subtract or Divide by Reference Spectrum
#'
#' `step_measure_subtract_reference()` creates a *specification* of a recipe
#' step that subtracts or divides each spectrum by an external reference.
#' This is a simpler version of [step_measure_subtract_blank()] that always
#' uses an externally provided reference.
#'
#' @inheritParams step_measure_subtract_blank
#' @param reference A required external reference spectrum. Can be:
#'   - A `measure_tbl` object with `location` and `value` columns
#'   - A numeric vector (must match the number of locations in data)
#'   - A data.frame with `location` and `value` columns (will be interpolated)
#' @param learned_ref A named list containing the validated reference values for
#'   each measure column. This is `NULL` until the step is trained.
#'
#' @return An updated version of `recipe` with the new step added.
#'
#' @details
#' This step applies a simple reference correction to each spectrum:
#'
#' - `method = "subtract"`: `result = sample - reference`
#' - `method = "divide"`: `result = sample / reference`
#'
#' Unlike [step_measure_subtract_blank()], this step always requires an
#' externally provided reference and does not support learning from training
#' data.
#'
#' @family measure-preprocessing
#' @seealso [step_measure_subtract_blank()] for blank correction with learning
#' @export
#'
#' @examples
#' library(recipes)
#'
#' # Create a reference spectrum
#' ref_spectrum <- rep(1.0, 100)
#'
#' rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_subtract_reference(reference = ref_spectrum, method = "divide")
step_measure_subtract_reference <- function(
  recipe,
  reference,
  method = "subtract",
  measures = NULL,
  role = NA,
  trained = FALSE,
  learned_ref = NULL,
  skip = FALSE,
  id = recipes::rand_id("measure_subtract_reference")
) {
  if (missing(reference)) {
    cli::cli_abort("{.arg reference} is required.")
  }

  recipes::add_step(
    recipe,
    step_measure_subtract_reference_new(
      reference = reference,
      method = method,
      measures = measures,
      role = role,
      trained = trained,
      learned_ref = learned_ref,
      skip = skip,
      id = id
    )
  )
}

step_measure_subtract_reference_new <- function(
  reference,
  method,
  measures,
  role,
  trained,
  learned_ref,
  skip,
  id
) {
  recipes::step(
    subclass = "measure_subtract_reference",
    reference = reference,
    method = method,
    measures = measures,
    role = role,
    trained = trained,
    learned_ref = learned_ref,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_subtract_reference <- function(
  x,
  training,
  info = NULL,
  ...
) {
  check_for_measure(training)

  # Resolve which columns to process
  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  # Validate method
  if (!x$method %in% c("subtract", "divide")) {
    cli::cli_abort(
      "{.arg method} must be {.val subtract} or {.val divide}, not {.val {x$method}}."
    )
  }

  # Validate and convert reference
  learned_ref <- .validate_external_reference(
    x$reference,
    training,
    measure_cols,
    "reference"
  )

  step_measure_subtract_reference_new(
    reference = x$reference,
    method = x$method,
    measures = measure_cols,
    role = x$role,
    trained = TRUE,
    learned_ref = learned_ref,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_subtract_reference <- function(object, new_data, ...) {
  for (col in object$measures) {
    ref <- object$learned_ref[[col]]
    result <- .apply_reference_correction(new_data[[col]], ref, object$method)
    new_data[[col]] <- new_measure_list(result)
  }
  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_subtract_reference <- function(
  x,
  width = max(20, options()$width - 30),
  ...
) {
  title <- "Reference "
  method_str <- if (x$method == "divide") "division" else "subtraction"

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
tidy.step_measure_subtract_reference <- function(x, ...) {
  if (is_trained(x)) {
    terms <- x$measures
  } else {
    terms <- "<all measure columns>"
  }
  tibble::tibble(
    terms = terms,
    method = x$method,
    id = x$id
  )
}

#' @export
required_pkgs.step_measure_subtract_reference <- function(x, ...) {
  c("measure")
}

# ==============================================================================
# step_measure_ratio_reference
# ==============================================================================

#' Compute Ratio to Reference Spectrum
#'
#' `step_measure_ratio_reference()` creates a *specification* of a recipe step
#' that computes the ratio of each spectrum to a reference, optionally with
#' blank subtraction.
#'
#' @inheritParams step_measure_subtract_blank
#' @param reference A required external reference spectrum. Can be:
#'
#'   - A `measure_tbl` object with `location` and `value` columns
#'   - A numeric vector (must match the number of locations in data)
#'   - A data.frame with `location` and `value` columns (will be interpolated)
#' @param blank An optional blank spectrum to subtract from both sample and
#'   reference before computing the ratio. Same format options as `reference`.
#' @param learned_ref A named list containing the validated reference values for
#'   each measure column. This is `NULL` until the step is trained.
#'
#' @return An updated version of `recipe` with the new step added.
#'
#' @details
#' This step computes a ratio relative to a reference spectrum:
#'
#' - Without blank: `result = sample / reference`
#' - With blank: `result = (sample - blank) / (reference - blank)`
#'
#' This is useful for computing relative measurements, such as absorbance
#' from transmittance when you have both sample and reference scans.
#'
#' @family measure-preprocessing
#' @seealso [step_measure_subtract_blank()] for simple blank subtraction
#' @export
#'
#' @examples
#' library(recipes)
#'
#' # Create reference and blank spectra
#' ref_spectrum <- rep(1.0, 100)
#' blank_spectrum <- rep(0.05, 100)
#'
#' rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_ratio_reference(
#'     reference = ref_spectrum,
#'     blank = blank_spectrum
#'   )
step_measure_ratio_reference <- function(
  recipe,
  reference,
  blank = NULL,
  measures = NULL,
  role = NA,
  trained = FALSE,
  learned_ref = NULL,
  learned_blank = NULL,
  skip = FALSE,
  id = recipes::rand_id("measure_ratio_reference")
) {
  if (missing(reference)) {
    cli::cli_abort("{.arg reference} is required.")
  }

  recipes::add_step(
    recipe,
    step_measure_ratio_reference_new(
      reference = reference,
      blank = blank,
      measures = measures,
      role = role,
      trained = trained,
      learned_ref = learned_ref,
      learned_blank = learned_blank,
      skip = skip,
      id = id
    )
  )
}

step_measure_ratio_reference_new <- function(
  reference,
  blank,
  measures,
  role,
  trained,
  learned_ref,
  learned_blank,
  skip,
  id
) {
  recipes::step(
    subclass = "measure_ratio_reference",
    reference = reference,
    blank = blank,
    measures = measures,
    role = role,
    trained = trained,
    learned_ref = learned_ref,
    learned_blank = learned_blank,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_ratio_reference <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  # Resolve which columns to process
  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  # Validate and convert reference
  learned_ref <- .validate_external_reference(
    x$reference,
    training,
    measure_cols,
    "reference"
  )

  # Validate and convert blank (if provided)
  learned_blank <- NULL
  if (!is.null(x$blank)) {
    learned_blank <- .validate_external_reference(
      x$blank,
      training,
      measure_cols,
      "blank"
    )
  }

  step_measure_ratio_reference_new(
    reference = x$reference,
    blank = x$blank,
    measures = measure_cols,
    role = x$role,
    trained = TRUE,
    learned_ref = learned_ref,
    learned_blank = learned_blank,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_ratio_reference <- function(object, new_data, ...) {
  for (col in object$measures) {
    ref <- object$learned_ref[[col]]
    blank <- object$learned_blank[[col]]
    result <- .compute_ratio_reference(new_data[[col]], ref, blank)
    new_data[[col]] <- new_measure_list(result)
  }
  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_ratio_reference <- function(
  x,
  width = max(20, options()$width - 30),
  ...
) {
  title <- "Reference ratio"
  blank_str <- if (!is.null(x$blank)) " (with blank)" else ""

  if (x$trained) {
    cat(title, blank_str, " on <internal measurements>", sep = "")
  } else {
    cat(title, blank_str, sep = "")
  }
  cat("\n")

  invisible(x)
}

#' @rdname tidy.recipe
#' @export
#' @keywords internal
tidy.step_measure_ratio_reference <- function(x, ...) {
  if (is_trained(x)) {
    terms <- x$measures
  } else {
    terms <- "<all measure columns>"
  }
  tibble::tibble(
    terms = terms,
    has_blank = !is.null(x$blank),
    id = x$id
  )
}

#' @export
required_pkgs.step_measure_ratio_reference <- function(x, ...) {
  c("measure")
}

# ==============================================================================
# Internal helpers
# ==============================================================================

#' Compute mean spectrum from a list of spectra
#'
#' @param spectra_list A measure_list (list of measure_tbl objects)
#' @return Numeric vector of mean values
#' @noRd
.compute_mean_spectrum <- function(spectra_list) {
  mat <- measure_to_matrix(spectra_list)
  colMeans(mat, na.rm = TRUE)
}

#' Validate and convert external reference to numeric vectors
#'
#' @param ref External reference (measure_tbl, numeric, or data.frame)
#' @param training Training data
#' @param measure_cols Columns to process
#' @param arg_name Name of argument for error messages
#' @return Named list of numeric vectors (one per measure column)
#' @noRd
.validate_external_reference <- function(
  ref,
  training,
  measure_cols,
  arg_name
) {
  result <- list()

  for (col in measure_cols) {
    expected_locs <- training[[col]][[1]]$location
    n_expected <- length(expected_locs)

    if (is_measure_tbl(ref)) {
      # Check locations match
      if (!identical(ref$location, expected_locs)) {
        # Try interpolation if locations don't match exactly
        if (length(ref$location) >= 2) {
          interp_values <- stats::approx(
            ref$location,
            ref$value,
            xout = expected_locs,
            rule = 2
          )$y
          result[[col]] <- interp_values
          cli::cli_warn(c(
            "{.arg {arg_name}} locations don't match data exactly.",
            "i" = "Interpolating {.arg {arg_name}} to match data locations."
          ))
        } else {
          cli::cli_abort(
            "{.arg {arg_name}} must have at least 2 points for interpolation."
          )
        }
      } else {
        result[[col]] <- ref$value
      }
    } else if (is.numeric(ref)) {
      if (length(ref) != n_expected) {
        cli::cli_abort(c(
          "{.arg {arg_name}} length ({length(ref)}) doesn't match data ({n_expected}).",
          "i" = "Provide a numeric vector with {n_expected} values."
        ))
      }
      result[[col]] <- ref
    } else if (is.data.frame(ref)) {
      # Check for required columns
      if (!all(c("location", "value") %in% names(ref))) {
        cli::cli_abort(
          "{.arg {arg_name}} data.frame must have {.val location} and {.val value} columns."
        )
      }
      # Interpolate to match locations
      interp_values <- stats::approx(
        ref$location,
        ref$value,
        xout = expected_locs,
        rule = 2
      )$y
      result[[col]] <- interp_values
    } else {
      cli::cli_abort(c(
        "{.arg {arg_name}} must be a measure_tbl, numeric vector, or data.frame.",
        "i" = "Got {.cls {class(ref)}}."
      ))
    }
  }

  result
}

#' Apply reference correction (subtract or divide)
#'
#' @param dat measure_list to correct
#' @param ref Numeric vector of reference values
#' @param method "subtract" or "divide"
#' @return Corrected measure_list
#' @noRd
.apply_reference_correction <- function(dat, ref, method) {
  # Check for division by zero once, before processing all spectra
  if (method == "divide" && any(abs(ref) < .Machine$double.eps, na.rm = TRUE)) {
    cli::cli_warn(
      "Reference contains values near zero; division may produce Inf/NaN."
    )
  }

  purrr::map(dat, function(x) {
    if (method == "subtract") {
      x$value <- x$value - ref
    } else if (method == "divide") {
      x$value <- x$value / ref
    }
    x
  })
}

#' Compute ratio to reference with optional blank subtraction
#'
#' @param dat measure_list to process
#' @param ref Numeric vector of reference values
#' @param blank Numeric vector of blank values (or NULL)
#' @return Processed measure_list
#' @noRd
.compute_ratio_reference <- function(dat, ref, blank) {
  # Compute adjusted reference once (for warning check)
  ref_val <- if (!is.null(blank)) ref - blank else ref

  # Check for division by zero once, before processing all spectra
  if (any(abs(ref_val) < .Machine$double.eps, na.rm = TRUE)) {
    cli::cli_warn(
      "Reference (minus blank) contains values near zero; ratio may produce Inf/NaN."
    )
  }

  purrr::map(dat, function(x) {
    sample_val <- x$value
    if (!is.null(blank)) {
      sample_val <- sample_val - blank
    }
    x$value <- sample_val / ref_val
    x
  })
}
