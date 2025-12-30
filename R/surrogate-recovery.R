# ==============================================================================
# Surrogate/Internal Standard Recovery Step
#
# This file contains a preprocessing step for calculating and reporting
# surrogate or internal standard recovery for quality control purposes.
# ==============================================================================

#' Surrogate/Internal Standard Recovery
#'
#' `step_measure_surrogate_recovery()` creates a *specification* of a recipe step
#' that calculates recovery percentages for surrogate or internal standards.
#' This is essential for quality control in analytical workflows where spiked
#' compounds are used to monitor method performance.
#'
#' @param recipe A recipe object.
#' @param ... One or more selector functions to choose surrogate columns
#'   (measured concentrations or responses).
#' @param expected_col Name of a column containing expected concentrations
#'   for each sample. Mutually exclusive with `expected_value`.
#' @param expected_value A fixed numeric value for expected concentration.
#'   Used when all surrogates have the same expected value. Mutually
#'   exclusive with `expected_col`.
#' @param recovery_suffix Suffix appended to column names for recovery output.
#'   Default is `"_recovery"`.
#' @param action What to do with recovery calculations:
#'   - `"add_column"` (default): Add new columns with recovery percentages
#'   - `"flag"`: Add a boolean column indicating if recovery is within limits
#'   - `"filter"`: Remove rows where any surrogate is outside limits
#' @param flag_col Name of the flag column when `action = "flag"`. Default is
#'   `".surrogate_pass"`.
#' @param min_recovery Minimum acceptable recovery percentage. Default is 70.
#' @param max_recovery Maximum acceptable recovery percentage. Default is 130.
#' @param role Recipe role for new recovery columns. Default is `"surrogate"`.
#' @param trained Logical indicating if the step has been trained.
#' @param skip Logical. Should the step be skipped when baking?
#' @param id Unique step identifier.
#'
#' @return An updated recipe with the new step added.
#'
#' @details
#' ## Recovery Calculation
#'
#' Recovery is calculated as:
#' `recovery_pct = (measured / expected) * 100`
#'
#' Where:
#' - `measured` is the observed concentration/response of the surrogate
#' - `expected` is the known spike amount or theoretical value
#'
#' ## Acceptance Criteria
#'
#' Typical acceptance limits vary by application:
#' - **ICH M10 (Bioanalytical)**: 70-130% for surrogates
#' - **EPA Methods**: Often 50-150% or method-specific
#' - **FDA Guidance**: Application-specific, often 80-120%
#'
#' ## Use Cases
#'
#' - Monitor extraction efficiency in sample preparation
#' - Track instrument performance across runs
#' - Identify samples with matrix effects or procedural errors
#'
#' @family calibration
#' @seealso [step_measure_dilution_correct()], [measure_matrix_effect()]
#'
#' @export
#'
#' @examples
#' library(recipes)
#'
#' # Example: QC data with spiked surrogates
#' qc_data <- data.frame(
#'   sample_id = paste0("QC", 1:10),
#'   surrogate_1 = rnorm(10, mean = 100, sd = 10),
#'   surrogate_2 = rnorm(10, mean = 50, sd = 5),
#'   analyte = rnorm(10, mean = 75, sd = 8)
#' )
#'
#' # Add recovery columns for surrogates with expected value of 100 and 50
#' rec <- recipe(~ ., data = qc_data) |>
#'   update_role(sample_id, new_role = "id") |>
#'   step_measure_surrogate_recovery(
#'     surrogate_1,
#'     expected_value = 100,
#'     min_recovery = 80,
#'     max_recovery = 120
#'   ) |>
#'   prep()
#'
#' bake(rec, new_data = NULL)
step_measure_surrogate_recovery <- function(
    recipe,
    ...,
    expected_col = NULL,
    expected_value = NULL,
    recovery_suffix = "_recovery",
    action = c("add_column", "flag", "filter"),
    flag_col = ".surrogate_pass",
    min_recovery = 70,
    max_recovery = 130,
    role = "surrogate",
    trained = FALSE,
    skip = FALSE,
    id = recipes::rand_id("measure_surrogate_recovery")) {

  action <- match.arg(action)

  # Validate that exactly one of expected_col or expected_value is provided
  if (is.null(expected_col) && is.null(expected_value)) {
    cli::cli_abort(
      "Either {.arg expected_col} or {.arg expected_value} must be provided."
    )
  }
  if (!is.null(expected_col) && !is.null(expected_value)) {
    cli::cli_abort(
      "Only one of {.arg expected_col} or {.arg expected_value} should be provided."
    )
  }

  # Validate recovery limits
  if (min_recovery >= max_recovery) {
    cli::cli_abort(
      "{.arg min_recovery} ({min_recovery}) must be less than {.arg max_recovery} ({max_recovery})."
    )
  }

  recipes::add_step(
    recipe,
    step_measure_surrogate_recovery_new(
      terms = rlang::enquos(...),
      expected_col = expected_col,
      expected_value = expected_value,
      recovery_suffix = recovery_suffix,
      action = action,
      flag_col = flag_col,
      min_recovery = min_recovery,
      max_recovery = max_recovery,
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
step_measure_surrogate_recovery_new <- function(
    terms,
    expected_col,
    expected_value,
    recovery_suffix,
    action,
    flag_col,
    min_recovery,
    max_recovery,
    role,
    trained,
    col_names,
    skip,
    id) {

  recipes::step(
    subclass = "measure_surrogate_recovery",
    terms = terms,
    expected_col = expected_col,
    expected_value = expected_value,
    recovery_suffix = recovery_suffix,
    action = action,
    flag_col = flag_col,
    min_recovery = min_recovery,
    max_recovery = max_recovery,
    role = role,
    trained = trained,
    col_names = col_names,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_surrogate_recovery <- function(x, training, info = NULL, ...) {

  # Validate expected_col if provided
  if (!is.null(x$expected_col)) {
    if (!x$expected_col %in% names(training)) {
      cli::cli_abort(
        "Expected column {.field {x$expected_col}} not found in training data."
      )
    }
    if (!is.numeric(training[[x$expected_col]])) {
      cli::cli_abort(
        "Expected column {.field {x$expected_col}} must be numeric."
      )
    }
  }

  # Get selected surrogate columns
  col_names <- recipes::recipes_eval_select(x$terms, training, info)

  if (length(col_names) == 0) {
    cli::cli_abort(
      "No surrogate columns selected. Use column selectors in {.code ...}."
    )
  }

  # Validate all selected columns are numeric
  for (col in col_names) {
    if (!is.numeric(training[[col]])) {
      cli::cli_abort(
        "Surrogate column {.field {col}} must be numeric."
      )
    }
  }

  step_measure_surrogate_recovery_new(
    terms = x$terms,
    expected_col = x$expected_col,
    expected_value = x$expected_value,
    recovery_suffix = x$recovery_suffix,
    action = x$action,
    flag_col = x$flag_col,
    min_recovery = x$min_recovery,
    max_recovery = x$max_recovery,
    role = x$role,
    trained = TRUE,
    col_names = col_names,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_surrogate_recovery <- function(object, new_data, ...) {

  # Get expected values
  if (!is.null(object$expected_col)) {
    if (!object$expected_col %in% names(new_data)) {
      cli::cli_abort(
        "Expected column {.field {object$expected_col}} not found in new data."
      )
    }
    expected_values <- new_data[[object$expected_col]]
  } else {
    expected_values <- object$expected_value
  }

  # Check for zero expected values
  zero_expected <- if (length(expected_values) == 1) {
    expected_values == 0
  } else {
    any(expected_values == 0, na.rm = TRUE)
  }

  if (zero_expected) {
    cli::cli_abort(
      "Expected values cannot be zero (division by zero)."
    )
  }

  # Calculate recovery for each surrogate column
  recovery_list <- list()
  pass_flags <- rep(TRUE, nrow(new_data))

  for (col in object$col_names) {
    if (!col %in% names(new_data)) {
      cli::cli_warn(
        "Surrogate column {.field {col}} not found in new data; skipping."
      )
      next
    }

    measured <- new_data[[col]]
    recovery_pct <- (measured / expected_values) * 100

    # Check if within limits
    within_limits <- recovery_pct >= object$min_recovery &
                     recovery_pct <= object$max_recovery
    within_limits[is.na(within_limits)] <- FALSE

    # Update overall pass flag
    pass_flags <- pass_flags & within_limits

    # Store recovery
    recovery_col_name <- paste0(col, object$recovery_suffix)
    recovery_list[[recovery_col_name]] <- recovery_pct
  }

  # Apply action
  if (object$action == "add_column") {
    # Add recovery columns
    for (col_name in names(recovery_list)) {
      new_data[[col_name]] <- recovery_list[[col_name]]
    }
  } else if (object$action == "flag") {
    # Add flag column
    new_data[[object$flag_col]] <- pass_flags
  } else if (object$action == "filter") {
    # Filter to passing rows only
    n_before <- nrow(new_data)
    new_data <- new_data[pass_flags, ]
    n_removed <- n_before - nrow(new_data)
    if (n_removed > 0) {
      cli::cli_inform(
        "Removed {n_removed} row{?s} with surrogate recovery outside [{object$min_recovery}%, {object$max_recovery}%]."
      )
    }
  }

  new_data
}

#' @export
print.step_measure_surrogate_recovery <- function(
    x,
    width = max(20, options()$width - 30),
    ...) {

  title <- "Surrogate recovery calculation"

  if (x$trained) {
    n_cols <- length(x$col_names)
    limits <- glue::glue("[{x$min_recovery}%, {x$max_recovery}%]")
    desc <- glue::glue(
      "{n_cols} surrogate{?s}, action={x$action}, limits={limits}"
    )
  } else {
    desc <- "(not trained)"
  }

  cat(title, ": ", as.character(desc), "\n", sep = "")
  invisible(x)
}

#' @rdname tidy.recipe
#' @export
tidy.step_measure_surrogate_recovery <- function(x, ...) {
  if (x$trained) {
    expected_source <- if (!is.null(x$expected_col)) {
      x$expected_col
    } else {
      as.character(x$expected_value)
    }

    tibble::tibble(
      surrogate = x$col_names,
      expected_source = expected_source,
      min_recovery = x$min_recovery,
      max_recovery = x$max_recovery,
      action = x$action,
      id = x$id
    )
  } else {
    tibble::tibble(
      surrogate = character(),
      expected_source = character(),
      min_recovery = numeric(),
      max_recovery = numeric(),
      action = character(),
      id = character()
    )
  }
}
