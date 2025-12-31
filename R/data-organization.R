#' Data Organization and Role Assignment
#'
#' Functions for automatically detecting column types in analytical data
#' and assigning appropriate roles in recipes.
#'
#' @name data-organization
#' @keywords internal
NULL


# Column type patterns ---------------------------------------------------

#' Common column naming patterns for analytical data
#'
#' Named list of regex patterns for detecting measurement column types.
#' Used by `measure_identify_columns()` for auto-detection. Users can
#' extend or modify these patterns and pass them to detection functions.
#'
#' @format Named list with regex patterns:
#' \describe{
#'   \item{wavenumber}{`wn_` prefix for IR wavenumber (cm^-1)}
#'   \item{wavelength}{`nm_` prefix for wavelength (nm)}
#'   \item{retention_time}{`rt_` prefix for chromatography retention time}
#'   \item{mz}{`mz_` prefix for mass-to-charge ratio (MS)}
#'   \item{ppm}{`ppm_` prefix for NMR chemical shift}
#'   \item{channel}{`ch_` prefix for numbered channels}
#'   \item{generic}{`x_` prefix for generic/unknown axis}
#' }
#'
#' @examples
#' # View default patterns
#' measure_column_patterns
#'
#' # Create custom patterns
#' my_patterns <- c(measure_column_patterns, list(custom = "^my_prefix_"))
#'
#' @export
measure_column_patterns <- list(
  wavenumber = "^wn_",
  wavelength = "^nm_",

  retention_time = "^rt_",
  mz = "^mz_",
  ppm = "^ppm_",
  channel = "^ch_",
  generic = "^x_"
)


#' Identify Column Types in Analytical Data
#'
#' Automatically detects column types in a data frame based on naming
#' conventions common in analytical chemistry. This helps set up recipes
#' with appropriate roles for different column types.
#'
#' @param data A data frame to analyze.
#' @param patterns Named list of regex patterns for column detection.
#'   Defaults to `measure_column_patterns`. Custom patterns can be
#'   provided as a named list where names become the detected type.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{column}{Column name}
#'   \item{type}{Detected type (from pattern names, or "other" if no match)}
#'   \item{suggested_role}{Suggested recipe role based on type}
#'   \item{n_values}{Number of non-NA values}
#'   \item{class}{R class of the column}
#' }
#'
#' @details
#' Column type detection uses the following naming conventions:
#'
#' | Prefix | Type | Suggested Role | Use Case |
#' |--------|------|----------------|----------|
#' | `wn_*` | wavenumber | predictor | IR spectroscopy (cm^-1) |
#' | `nm_*` | wavelength | predictor | UV-Vis, NIR spectroscopy |
#' | `rt_*` | retention_time | predictor | Chromatography |
#' | `mz_*` | mz | predictor | Mass spectrometry |
#' | `ppm_*` | ppm | predictor | NMR spectroscopy |
#' | `ch_*` | channel | predictor | Generic channel data |
#' | `x_*` | generic | predictor | Generic measurements |
#'
#' Columns not matching any pattern are classified as "other" and
#' suggested as either "outcome" (if numeric), "id" (if character/factor
#' with unique values), or "predictor".
#'
#' @examples
#' # Wide format spectral data
#' df <- data.frame(
#'   sample_id = 1:5,
#'   outcome = rnorm(5),
#'   wn_1000 = rnorm(5),
#'   wn_1001 = rnorm(5),
#'   wn_1002 = rnorm(5)
#' )
#' measure_identify_columns(df)
#'
#' # Chromatography data
#' df2 <- data.frame(
#'   id = letters[1:3],
#'   concentration = c(1.2, 2.3, 3.4),
#'   rt_0.5 = rnorm(3),
#'   rt_1.0 = rnorm(3),
#'   rt_1.5 = rnorm(3)
#' )
#' measure_identify_columns(df2)
#'
#' @export
measure_identify_columns <- function(data, patterns = measure_column_patterns) {
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame.")
  }

  col_names <- names(data)

  # Detect type for each column
  types <- vapply(
    col_names,
    function(col) {
      for (type_name in names(patterns)) {
        if (grepl(patterns[[type_name]], col, ignore.case = TRUE)) {
          return(type_name)
        }
      }
      return("other")
    },
    character(1),
    USE.NAMES = FALSE
  )

  # Determine suggested roles
  suggested_roles <- vapply(
    seq_along(col_names),
    function(i) {
      col <- col_names[i]
      type <- types[i]
      col_data <- data[[col]]

      # Measurement columns become predictors
      if (type != "other") {
        return("predictor")
      }

      # For "other" columns, use heuristics
      if (is.character(col_data) || is.factor(col_data)) {
        # Check if it looks like an ID (mostly unique values)
        unique_ratio <- length(unique(col_data)) / length(col_data)
        if (unique_ratio > 0.9) {
          return("id")
        }
        return("predictor")
      }

      if (is.numeric(col_data)) {
        # Could be outcome or predictor - suggest based on column name
        outcome_hints <- c(
          "outcome",
          "target",
          "response",
          "y",
          "concentration",
          "conc",
          "amount",
          "water",
          "fat",
          "protein",
          "moisture"
        )
        if (
          any(vapply(
            outcome_hints,
            function(h) {
              grepl(h, col, ignore.case = TRUE)
            },
            logical(1)
          ))
        ) {
          return("outcome")
        }
        return("predictor")
      }

      "predictor"
    },
    character(1)
  )

  # Count non-NA values
  n_values <- vapply(
    col_names,
    function(col) {
      sum(!is.na(data[[col]]))
    },
    integer(1),
    USE.NAMES = FALSE
  )

  # Get column classes
  classes <- vapply(
    col_names,
    function(col) {
      paste(class(data[[col]]), collapse = "/")
    },
    character(1),
    USE.NAMES = FALSE
  )

  tibble::tibble(
    column = col_names,
    type = types,
    suggested_role = suggested_roles,
    n_values = n_values,
    class = classes
  )
}


#' Set Measure Roles in a Recipe
#'
#' Batch assign roles to columns based on their detected types or
#' explicit patterns. This is a convenience wrapper around
#' [recipes::update_role()] for common analytical data patterns.
#'
#' @param recipe A recipe object.
#' @param id_cols Column(s) to assign "id" role. Accepts tidyselect.
#' @param blank_cols Column(s) to assign "blank" role. Accepts tidyselect.
#' @param qc_cols Column(s) to assign "qc" role. Accepts tidyselect.
#' @param standard_cols Column(s) to assign "standard" role. Accepts tidyselect.
#' @param metadata_cols Column(s) to assign "metadata" role. Accepts tidyselect.
#' @param measure_cols Column(s) to assign "measure" role. Accepts tidyselect.
#'
#' @return Updated recipe object with roles assigned.
#'
#' @details
#' Common roles for analytical chemistry workflows:
#'
#' | Role | Purpose |
#' |------|---------|
#' | id | Sample identifiers (not used in modeling) |
#' | blank | Blank/background samples for subtraction |
#' | qc | Quality control samples |
#' | standard | Calibration standards |
#' | metadata | Sample metadata (not used in modeling) |
#' | measure | Measurement columns for input steps |
#' | predictor | Columns used as model predictors |
#' | outcome | Target variable(s) for modeling |
#'
#' @examples
#' \dontrun{
#' library(recipes)
#'
#' # Basic role assignment
#' rec <- recipe(outcome ~ ., data = my_data) |>
#'   set_measure_roles(
#'     id_cols = sample_id,
#'     metadata_cols = c(batch, operator)
#'   )
#'
#' # With QC and blank identification by column name patterns
#' rec <- recipe(outcome ~ ., data = my_data) |>
#'   set_measure_roles(
#'     id_cols = sample_id,
#'     blank_cols = starts_with("blank_"),
#'     qc_cols = starts_with("qc_")
#'   )
#' }
#'
#' @export
set_measure_roles <- function(
  recipe,
  id_cols = NULL,
  blank_cols = NULL,
  qc_cols = NULL,
  standard_cols = NULL,
  metadata_cols = NULL,
  measure_cols = NULL
) {
  if (!inherits(recipe, "recipe")) {
    cli::cli_abort("{.arg recipe} must be a recipe object.")
  }

  # Helper to update role if columns specified
  update_if_specified <- function(rec, cols_expr, role) {
    cols_quo <- rlang::enquo(cols_expr)
    if (!rlang::quo_is_null(cols_quo)) {
      rec <- recipes::update_role(rec, !!cols_quo, new_role = role)
    }
    rec
  }

  # Apply role updates
  recipe <- update_if_specified(recipe, {{ id_cols }}, "id")
  recipe <- update_if_specified(recipe, {{ blank_cols }}, "blank")
  recipe <- update_if_specified(recipe, {{ qc_cols }}, "qc")
  recipe <- update_if_specified(recipe, {{ standard_cols }}, "standard")
  recipe <- update_if_specified(recipe, {{ metadata_cols }}, "metadata")
  recipe <- update_if_specified(recipe, {{ measure_cols }}, "measure")

  recipe
}


#' Check Measure Recipe Structure
#'
#' Validates that a recipe is properly structured for measure operations.
#' Checks for common issues like missing input steps, incompatible column
#' types, and role conflicts.
#'
#' @param recipe A recipe object to validate.
#' @param strict Logical. If TRUE (default), returns errors as a tibble.
#'   If FALSE, issues cli warnings and returns the recipe invisibly.
#'
#' @return If `strict = TRUE`, returns a tibble with columns:
#' \describe{
#'   \item{level}{Severity: "error", "warning", or "info"}
#'   \item{check}{Name of the check that triggered the message}
#'   \item{message}{Description of the issue}
#' }
#' If `strict = FALSE`, returns the recipe invisibly after printing warnings.
#'
#' @details
#' The following checks are performed:
#'
#' **Errors** (will cause failures):
#' - No input step (`step_measure_input_*`)
#' - Output step before input step
#' - Multiple input steps
#'
#' **Warnings** (may cause issues):
#' - No output step (data stays in internal format)
#' - Processing steps after output step
#' - No predictor columns identified
#'
#' **Info** (suggestions):
#' - Large number of measurement columns (consider dimension reduction)
#' - No ID column identified
#'
#' @examples
#' \dontrun{
#' library(recipes)
#'
#' # Check a properly structured recipe
#' rec <- recipe(outcome ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_snv() |>
#'   step_measure_output_wide()
#'
#' check_measure_recipe(rec)
#'
#' # Check a recipe with issues
#' bad_rec <- recipe(outcome ~ ., data = my_data) |>
#'   step_measure_snv()  # Missing input step!
#'
#' check_measure_recipe(bad_rec)
#' }
#'
#' @export
check_measure_recipe <- function(recipe, strict = TRUE) {
  if (!inherits(recipe, "recipe")) {
    cli::cli_abort("{.arg recipe} must be a recipe object.")
  }

  issues <- tibble::tibble(
    level = character(),
    check = character(),
    message = character()
  )

  add_issue <- function(level, check, message) {
    issues <<- tibble::add_row(
      issues,
      level = level,
      check = check,
      message = message
    )
  }

  # Get step classes
  step_classes <- vapply(recipe$steps, function(s) class(s)[1], character(1))

  # Check for input step
  input_steps <- grep("^step_measure_input", step_classes)
  if (length(input_steps) == 0) {
    add_issue(
      "error",
      "no_input",
      "Recipe has no input step. Add step_measure_input_wide() or step_measure_input_long()."
    )
  } else if (length(input_steps) > 1) {
    add_issue(
      "error",
      "multiple_inputs",
      paste(
        "Recipe has",
        length(input_steps),
        "input steps. Only one is allowed."
      )
    )
  }

  # Check for output step
  output_steps <- grep("^step_measure_output", step_classes)
  if (length(output_steps) == 0) {
    add_issue(
      "warning",
      "no_output",
      "Recipe has no output step. Data will remain in internal .measures format."
    )
  }

  # Check step order
  if (length(input_steps) > 0 && length(output_steps) > 0) {
    if (min(output_steps) < max(input_steps)) {
      add_issue(
        "error",
        "step_order",
        "Output step appears before input step. Reorder your recipe steps."
      )
    }

    # Check for processing after output
    processing_steps <- grep(
      "^step_measure_(?!input|output)",
      step_classes,
      perl = TRUE
    )
    if (length(processing_steps) > 0 && length(output_steps) > 0) {
      if (any(processing_steps > min(output_steps))) {
        add_issue(
          "warning",
          "post_output_processing",
          "Processing steps found after output step. These won't affect the output."
        )
      }
    }
  }

  # Check role assignments
  var_info <- recipe$var_info
  if (!is.null(var_info)) {
    n_predictors <- sum(var_info$role == "predictor", na.rm = TRUE)
    n_outcomes <- sum(var_info$role == "outcome", na.rm = TRUE)
    has_id <- any(var_info$role == "id", na.rm = TRUE)

    if (n_predictors == 0 && length(input_steps) == 0) {
      add_issue(
        "warning",
        "no_predictors",
        "No predictor columns identified. Check your recipe formula."
      )
    }

    if (!has_id) {
      add_issue(
        "info",
        "no_id",
        "No ID column identified. Consider using update_role(col, new_role = 'id')."
      )
    }

    # Large number of columns warning
    if (n_predictors > 1000) {
      add_issue(
        "info",
        "many_predictors",
        paste0(
          "Recipe has ",
          n_predictors,
          " predictor columns. ",
          "Consider dimension reduction (PCA, binning, etc.)."
        )
      )
    }
  }

  if (strict) {
    return(issues)
  }

  # Non-strict mode: print and return recipe
  if (nrow(issues) > 0) {
    for (i in seq_len(nrow(issues))) {
      row <- issues[i, ]
      if (row$level == "error") {
        cli::cli_alert_danger(row$message)
      } else if (row$level == "warning") {
        cli::cli_alert_warning(row$message)
      } else {
        cli::cli_alert_info(row$message)
      }
    }
  } else {
    cli::cli_alert_success("Recipe structure looks good!")
  }

  invisible(recipe)
}


#' Get Column Summary by Type
#'
#' Summarizes columns by their detected type, useful for understanding
#' the structure of analytical datasets.
#'
#' @param data A data frame to analyze.
#' @param patterns Named list of regex patterns. Defaults to
#'   `measure_column_patterns`.
#'
#' @return A tibble summarizing each detected type:
#' \describe{
#'   \item{type}{Column type}
#'   \item{n_columns}{Number of columns of this type}
#'   \item{example_cols}{First 3 column names of this type}
#' }
#'
#' @examples
#' df <- data.frame(
#'   id = 1:5,
#'   wn_1000 = rnorm(5), wn_1001 = rnorm(5), wn_1002 = rnorm(5),
#'   concentration = rnorm(5)
#' )
#' measure_column_summary(df)
#'
#' @export
measure_column_summary <- function(data, patterns = measure_column_patterns) {
  col_info <- measure_identify_columns(data, patterns)

  col_info |>
    dplyr::group_by(.data$type) |>
    dplyr::summarize(
      n_columns = dplyr::n(),
      example_cols = paste(utils::head(.data$column, 3), collapse = ", "),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(.data$n_columns))
}
