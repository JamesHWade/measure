# ==============================================================================
# Dilution Correction Step
#
# This file contains a preprocessing step for correcting concentrations
# based on dilution factors applied during sample preparation.
# ==============================================================================

#' Dilution Factor Correction
#'
#' `step_measure_dilution_correct()` creates a *specification* of a recipe step
#' that corrects concentration values by applying dilution factors. This is
#' essential when samples are diluted during preparation and need to be
#' back-calculated to original concentrations.
#'
#' @param recipe A recipe object.
#' @param ... One or more selector functions to choose feature columns
#'   (concentration values) to correct. If empty, all numeric columns
#'   (excluding metadata columns) will be selected.
#' @param dilution_col Name of the column containing dilution factors.
#'   Default is `"dilution_factor"`.
#' @param operation How to apply the dilution factor:
#'   - `"multiply"` (default): `concentration * dilution_factor` (back-calculate
#'     from diluted to original concentration)
#'   - `"divide"`: `concentration / dilution_factor` (apply dilution)
#' @param handle_zero How to handle zero dilution factors:
#'   - `"error"` (default): Stop with an error
#'   - `"warn"`: Warn and set result to NA
#'   - `"skip"`: Silently set result to NA
#' @param role Not used by this step.
#' @param trained Logical indicating if the step has been trained.
#' @param skip Logical. Should the step be skipped when baking?
#' @param id Unique step identifier.
#'
#' @return An updated recipe with the new step added.
#'
#' @details
#' ## Dilution Factor Interpretation
#'
#' The dilution factor represents how much the sample was diluted:
#' - A factor of 1 means no dilution (undiluted)
#' - A factor of 2 means 1:2 dilution (1 part sample + 1 part diluent)
#' - A factor of 10 means 1:10 dilution
#'
#' ## Back-Calculation
#'
#' When using `operation = "multiply"` (the default):
#' `original_concentration = measured_concentration * dilution_factor`
#'
#' This corrects for the dilution to get the true concentration in the
#' original sample.
#'
#' ## When to Use
#'
#' Use this step after quantitation (calibration) when samples were diluted
#' to bring concentrations within the calibration range.
#'
#' @family calibration
#' @seealso [step_measure_surrogate_recovery()], [measure_calibration_predict()]
#'
#' @export
#'
#' @examples
#' library(recipes)
#'
#' # Example: samples diluted to fit calibration range
#' data <- data.frame(
#'   sample_id = paste0("S", 1:6),
#'   dilution_factor = c(1, 2, 5, 10, 1, 1),
#'   analyte = c(50, 45, 42, 48, 51, 49)  # Measured after dilution
#' )
#'
#' rec <- recipe(~ ., data = data) |>
#'   update_role(sample_id, new_role = "id") |>
#'   step_measure_dilution_correct(
#'     analyte,
#'     dilution_col = "dilution_factor",
#'     operation = "multiply"
#'   ) |>
#'   prep()
#'
#' # Back-calculated concentrations
#' bake(rec, new_data = NULL)
#' # S1: 50*1=50, S2: 45*2=90, S3: 42*5=210, S4: 48*10=480
step_measure_dilution_correct <- function(
    recipe,
    ...,
    dilution_col = "dilution_factor",
    operation = c("multiply", "divide"),
    handle_zero = c("error", "warn", "skip"),
    role = NA,
    trained = FALSE,
    skip = FALSE,
    id = recipes::rand_id("measure_dilution_correct")) {

  operation <- match.arg(operation)
  handle_zero <- match.arg(handle_zero)

  recipes::add_step(
    recipe,
    step_measure_dilution_correct_new(
      terms = rlang::enquos(...),
      dilution_col = dilution_col,
      operation = operation,
      handle_zero = handle_zero,
      role = role,
      trained = trained,
      col_names = NULL,
      skip = skip,
      id = id
    )
  )
}

#' Internal constructor
#' @noRd
step_measure_dilution_correct_new <- function(
    terms,
    dilution_col,
    operation,
    handle_zero,
    role,
    trained,
    col_names,
    skip,
    id) {

  recipes::step(
    subclass = "measure_dilution_correct",
    terms = terms,
    dilution_col = dilution_col,
    operation = operation,
    handle_zero = handle_zero,
    role = role,
    trained = trained,
    col_names = col_names,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_dilution_correct <- function(x, training, info = NULL, ...) {

  # Validate dilution column exists
  if (!x$dilution_col %in% names(training)) {
    cli::cli_abort(
      "Dilution column {.field {x$dilution_col}} not found in training data."
    )
  }

  # Validate dilution column is numeric
  if (!is.numeric(training[[x$dilution_col]])) {
    cli::cli_abort(
      "Dilution column {.field {x$dilution_col}} must be numeric."
    )
  }

  # Get selected columns
  col_names <- recipes::recipes_eval_select(x$terms, training, info)

  # If no columns specified, use all numeric columns except dilution_col
  if (length(col_names) == 0) {
    numeric_cols <- names(training)[vapply(training, is.numeric, logical(1))]
    col_names <- setdiff(numeric_cols, x$dilution_col)

    if (length(col_names) == 0) {
      cli::cli_warn(
        "No numeric columns found to apply dilution correction."
      )
    }
  }

  step_measure_dilution_correct_new(
    terms = x$terms,
    dilution_col = x$dilution_col,
    operation = x$operation,
    handle_zero = x$handle_zero,
    role = x$role,
    trained = TRUE,
    col_names = col_names,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_dilution_correct <- function(object, new_data, ...) {

  # Validate dilution column exists in new data

if (!object$dilution_col %in% names(new_data)) {
    cli::cli_abort(
      "Dilution column {.field {object$dilution_col}} not found in new data."
    )
  }

  dilution_factors <- new_data[[object$dilution_col]]

  # Handle zero dilution factors
  zero_idx <- which(dilution_factors == 0)
  if (length(zero_idx) > 0) {
    if (object$handle_zero == "error") {
      cli::cli_abort(
        c(
          "Zero dilution factor{?s} found at row{?s}: {zero_idx}.",
          "i" = "Use {.code handle_zero = 'warn'} or {.code handle_zero = 'skip'} to allow."
        )
      )
    } else if (object$handle_zero == "warn") {
      cli::cli_warn(
        "Zero dilution factor{?s} at row{?s} {zero_idx}; setting result to NA."
      )
    }
  }

  # Handle NA dilution factors
  na_idx <- which(is.na(dilution_factors))
  if (length(na_idx) > 0) {
    cli::cli_warn(
      "NA dilution factor{?s} at row{?s} {na_idx}; result will be NA."
    )
  }

  # Apply correction to each selected column
  for (col in object$col_names) {
    if (!col %in% names(new_data)) {
      cli::cli_warn(
        "Column {.field {col}} not found in new data; skipping."
      )
      next
    }

    values <- new_data[[col]]

    if (object$operation == "multiply") {
      corrected <- values * dilution_factors
    } else {
      # Guard against division by zero
      safe_dilution <- ifelse(dilution_factors == 0, NA_real_, dilution_factors)
      corrected <- values / safe_dilution
    }

    new_data[[col]] <- corrected
  }

  new_data
}

#' @export
print.step_measure_dilution_correct <- function(
    x,
    width = max(20, options()$width - 30),
    ...) {

  title <- "Dilution factor correction"

  if (x$trained) {
    n_cols <- length(x$col_names)
    desc <- glue::glue(
      "{n_cols} column{?s} ({x$operation} by {x$dilution_col})"
    )
  } else {
    desc <- "(not trained)"
  }

  cat(title, ": ", as.character(desc), "\n", sep = "")
  invisible(x)
}

#' @rdname tidy.recipe
#' @export
tidy.step_measure_dilution_correct <- function(x, ...) {
  if (x$trained) {
    tibble::tibble(
      feature = x$col_names,
      dilution_col = x$dilution_col,
      operation = x$operation,
      id = x$id
    )
  } else {
    tibble::tibble(
      feature = character(),
      dilution_col = character(),
      operation = character(),
      id = character()
    )
  }
}
