# ==============================================================================
# Metadata Validation for Analytical Workflows
#
# This file provides validation helpers for analytical metadata columns
# required by Milestone 2 functions (drift correction, calibration, etc.)
# ==============================================================================

#' Canonical Sample Types
#'
#' The allowed values for the `sample_type` column in analytical workflows.
#'
#' @export
measure_sample_types <- c("qc", "standard", "blank", "unknown", "reference")

#' Validate Analytical Metadata
#'
#' Validates that a data frame contains the required metadata columns for

#' analytical workflows. This function checks for column presence, correct
#' data types, and valid values (e.g., sample_type levels).
#'
#' @param data A data frame to validate.
#' @param require Character vector of required columns. Common columns include:
#'   - `"sample_type"`: Sample classification (qc, standard, blank, unknown, reference)
#'   - `"run_order"`: Injection/measurement sequence (integer)
#'   - `"batch_id"`: Batch identifier (character/factor)
#'   - `"nominal_conc"`: Known concentration for standards (numeric)
#'   - `"sample_id"`: Unique sample identifier
#'   - `"analyst_id"`, `"day"`, `"instrument_id"`: Precision study factors
#' @param sample_types Allowed values for `sample_type` column. Default is
#'   [measure_sample_types]: "qc", "standard", "blank", "unknown", "reference".
#' @param action What to do when validation fails:
#'   - `"error"` (default): Stop with an informative error
#'   - `"warn"`: Issue warnings but continue
#'   - `"message"`: Issue messages but continue
#'
#' @return Invisibly returns a list with validation results:
#'   - `valid`: Logical, TRUE if all checks passed
#'   - `checks`: List of individual check results
#'   - `data`: The original data (unchanged)
#'
#' @details
#' ## Canonical Columns
#'
#' Milestone 2 functions expect specific column names with specific types:
#'
#' | Column | Type | Description |
#' |--------|------|-------------|
#' | `sample_type` | character/factor | Sample classification |
#' | `run_order` | integer | Injection sequence within batch |
#' | `batch_id` | character/factor | Batch identifier |
#' | `nominal_conc` | numeric | Known concentration (standards) |
#' | `sample_id` | character/factor | Unique sample identifier |
#' | `analyst_id` | character/factor | Analyst performing measurement |
#' | `day` | character/Date | Day of measurement |
#' | `instrument_id` | character/factor | Instrument identifier |
#' | `dilution_factor` | numeric | Sample dilution factor |
#'
#' ## Sample Type Values
#'
#' The `sample_type` column must contain only values from [measure_sample_types]:
#' - `"qc"`: Quality control sample (pooled QC, system suitability)
#' - `"standard"`: Calibration standard with known concentration
#' - `"blank"`: Blank sample (solvent, matrix blank)
#' - `"unknown"`: Sample with unknown concentration
#' - `"reference"`: Reference material for batch correction
#'
#' @seealso [measure_standardize_sample_type()] for converting non-standard
#'   sample type values to canonical form.
#'
#' @export
#'
#' @examples
#' # Create sample analytical data
#' data <- data.frame(
#'   sample_id = paste0("S", 1:10),
#'   sample_type = c("qc", "standard", "standard", "unknown", "unknown",
#'                   "unknown", "qc", "blank", "unknown", "qc"),
#'   run_order = 1:10,
#'   batch_id = "B001",
#'   nominal_conc = c(NA, 10, 50, NA, NA, NA, NA, 0, NA, NA),
#'   response = rnorm(10, mean = 100)
#' )
#'
#' # Validate required columns
#' measure_validate_metadata(data, require = c("sample_type", "run_order"))
#'
#' # Validate for calibration workflow
#' measure_validate_metadata(
#'   data,
#'   require = c("sample_type", "nominal_conc")
#' )
#'
#' # More lenient validation (warnings only)
#' measure_validate_metadata(
#'   data,
#'   require = c("sample_type", "run_order", "missing_col"),
#'   action = "warn"
#' )
measure_validate_metadata <- function(
    data,
    require = NULL,
    sample_types = measure_sample_types,
    action = c("error", "warn", "message")) {

  action <- match.arg(action)

  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame.")
  }

  results <- list(
    valid = TRUE,
    checks = list(),
    data = data
  )

  issues <- character()


  # Check for required columns
  if (!is.null(require)) {
    missing_cols <- setdiff(require, names(data))
    if (length(missing_cols) > 0) {
      results$valid <- FALSE
      results$checks$missing_columns <- list(
        valid = FALSE,
        message = paste("Missing required columns:", paste(missing_cols, collapse = ", ")),
        columns = missing_cols
      )
      issues <- c(issues, sprintf(
        "Missing required column(s): %s",
        paste(cli::style_bold(missing_cols), collapse = ", ")
      ))
    } else {
      results$checks$missing_columns <- list(
        valid = TRUE,
        message = "All required columns present",
        columns = character()
      )
    }
  }

  # Validate sample_type column if present
  if ("sample_type" %in% names(data)) {
    sample_type_result <- .validate_sample_type(data, sample_types)
    results$checks$sample_type <- sample_type_result
    if (!sample_type_result$valid) {
      results$valid <- FALSE
      issues <- c(issues, sample_type_result$message)
    }
  }

  # Validate run_order column if present
  if ("run_order" %in% names(data)) {
    run_order_result <- .validate_run_order(data)
    results$checks$run_order <- run_order_result
    if (!run_order_result$valid) {
      results$valid <- FALSE
      issues <- c(issues, run_order_result$message)
    }
  }

  # Validate batch_id column if present
  if ("batch_id" %in% names(data)) {
    batch_id_result <- .validate_batch_id(data)
    results$checks$batch_id <- batch_id_result
    if (!batch_id_result$valid) {
      results$valid <- FALSE
      issues <- c(issues, batch_id_result$message)
    }
  }

  # Validate nominal_conc column if present
  if ("nominal_conc" %in% names(data)) {
    nominal_conc_result <- .validate_nominal_conc(data)
    results$checks$nominal_conc <- nominal_conc_result
    if (!nominal_conc_result$valid) {
      results$valid <- FALSE
      issues <- c(issues, nominal_conc_result$message)
    }
  }

  # Validate dilution_factor column if present
  if ("dilution_factor" %in% names(data)) {
    dilution_result <- .validate_dilution_factor(data)
    results$checks$dilution_factor <- dilution_result
    if (!dilution_result$valid) {
      results$valid <- FALSE
      issues <- c(issues, dilution_result$message)
    }
  }

  # Report issues based on action
  if (length(issues) > 0) {
    if (action == "error") {
      cli::cli_abort(c(
        "Metadata validation failed:",
        stats::setNames(issues, rep("x", length(issues)))
      ))
    } else if (action == "warn") {
      cli::cli_warn(c(
        "Metadata validation issues:",
        stats::setNames(issues, rep("!", length(issues)))
      ))
    } else {
      cli::cli_inform(c(
        "Metadata validation notes:",
        stats::setNames(issues, rep("i", length(issues)))
      ))
    }
  }

  invisible(results)
}


#' Standardize Sample Type Values
#'
#' Converts non-standard sample type values to canonical form using a
#' user-specified mapping. This is useful when data uses different
#' naming conventions (e.g., "QC", "quality_control", "pooled_qc").
#'
#' @param data A data frame containing a sample type column.
#' @param col Name of the sample type column. Default is `"sample_type"`.
#' @param mapping A named list mapping canonical types to vectors of aliases.
#'   For example: `list(qc = c("QC", "quality_control", "pooled_qc"))`.
#'   If NULL, uses default case-insensitive matching.
#' @param unknown_action What to do with values that don't match any mapping:
#'   - `"error"` (default): Stop with error
#'   - `"warn"`: Warn and keep original value
#'   - `"keep"`: Silently keep original value
#'   - `"unknown"`: Convert to "unknown"
#'
#' @return The data frame with standardized sample_type values.
#'
#' @export
#'
#' @examples
#' # Data with non-standard sample types
#' data <- data.frame(
#'   sample_id = 1:5,
#'   sample_type = c("QC", "STD", "BLK", "UNK", "REF")
#' )
#'
#' # Standardize with custom mapping
#' measure_standardize_sample_type(
#'   data,
#'   mapping = list(
#'     qc = c("QC", "qc", "quality_control"),
#'     standard = c("STD", "std", "cal"),
#'     blank = c("BLK", "blk", "blank"),
#'     unknown = c("UNK", "unk", "sample"),
#'     reference = c("REF", "ref")
#'   )
#' )
measure_standardize_sample_type <- function(
    data,
    col = "sample_type",
    mapping = NULL,
    unknown_action = c("error", "warn", "keep", "unknown")) {

  unknown_action <- match.arg(unknown_action)

  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame.")
  }

  if (!col %in% names(data)) {
    cli::cli_abort("Column {.field {col}} not found in data.")
  }

  original_values <- as.character(data[[col]])

  # Default mapping: case-insensitive match to canonical names
  if (is.null(mapping)) {
    mapping <- list(
      qc = c("qc", "QC", "Qc", "quality_control", "pooled_qc", "system_suitability"),
      standard = c("standard", "Standard", "STANDARD", "std", "STD", "cal", "calibrator"),
      blank = c("blank", "Blank", "BLANK", "blk", "BLK", "solvent_blank", "matrix_blank"),
      unknown = c("unknown", "Unknown", "UNKNOWN", "unk", "UNK", "sample", "SAMPLE"),
      reference = c("reference", "Reference", "REFERENCE", "ref", "REF", "reference_material")
    )
  }

  # Build reverse lookup
  reverse_map <- list()
  for (canonical in names(mapping)) {
    for (alias in mapping[[canonical]]) {
      reverse_map[[alias]] <- canonical
    }
  }

  # Apply mapping
  new_values <- character(length(original_values))
  unmatched <- character()

  for (i in seq_along(original_values)) {
    val <- original_values[i]
    if (is.na(val)) {
      new_values[i] <- NA_character_
    } else if (val %in% names(reverse_map)) {
      new_values[i] <- reverse_map[[val]]
    } else {
      unmatched <- c(unmatched, val)
      new_values[i] <- val  # Keep original for now
    }
  }

  # Handle unmatched values
  unmatched <- unique(unmatched)
  if (length(unmatched) > 0) {
    if (unknown_action == "error") {
      cli::cli_abort(c(
        "Unknown sample type values found:",
        "x" = paste(unmatched, collapse = ", "),
        "i" = "Use {.arg mapping} to specify how to convert these values."
      ))
    } else if (unknown_action == "warn") {
      cli::cli_warn(c(
        "Unknown sample type values kept as-is:",
        "!" = paste(unmatched, collapse = ", ")
      ))
    } else if (unknown_action == "unknown") {
      new_values[original_values %in% unmatched & !is.na(original_values)] <- "unknown"
    }
    # "keep" does nothing extra
  }

  data[[col]] <- new_values
  data
}


# ==============================================================================
# Internal validation helpers
# ==============================================================================

#' Validate sample_type column
#' @noRd
.validate_sample_type <- function(data, allowed_types) {
  values <- data$sample_type

  # Check type
  if (!is.character(values) && !is.factor(values)) {
    return(list(
      valid = FALSE,
      message = sprintf(
        "Column {.field sample_type} must be character or factor, not %s",
        class(values)[1]
      ),
      invalid_values = character()
    ))
  }

  # Convert factor to character for checking
  values_char <- as.character(values)

  # Check for invalid values (excluding NA)
  invalid <- unique(values_char[!is.na(values_char) & !(values_char %in% allowed_types)])

  if (length(invalid) > 0) {
    return(list(
      valid = FALSE,
      message = sprintf(
        "Column {.field sample_type} contains invalid values: %s. Allowed values: %s",
        paste(invalid, collapse = ", "),
        paste(allowed_types, collapse = ", ")
      ),
      invalid_values = invalid
    ))
  }

  list(
    valid = TRUE,
    message = "sample_type values are valid",
    invalid_values = character()
  )
}

#' Validate run_order column
#' @noRd
.validate_run_order <- function(data) {
  values <- data$run_order

  # Check type
  if (!is.numeric(values)) {
    return(list(
      valid = FALSE,
      message = sprintf(
        "Column {.field run_order} must be numeric (integer), not %s",
        class(values)[1]
      )
    ))
  }

  # Check for non-integer values (allow doubles that are whole numbers)
  non_integer <- values[!is.na(values) & values != floor(values)]
  if (length(non_integer) > 0) {
    return(list(
      valid = FALSE,
      message = "Column {.field run_order} contains non-integer values"
    ))
  }

  # Check for negative values
  negative <- values[!is.na(values) & values < 0]
  if (length(negative) > 0) {
    return(list(
      valid = FALSE,
      message = "Column {.field run_order} contains negative values"
    ))
  }

  # Check for duplicates within batch (if batch_id present)
  if ("batch_id" %in% names(data)) {
    # Group by batch and check for duplicate run orders
    dup_check <- data |>
      dplyr::group_by(.data$batch_id) |>
      dplyr::summarize(
        has_dups = anyDuplicated(.data$run_order[!is.na(.data$run_order)]) > 0,
        .groups = "drop"
      )

    if (any(dup_check$has_dups)) {
      return(list(
        valid = FALSE,
        message = "Column {.field run_order} contains duplicates within batch(es)"
      ))
    }
  }

  list(
    valid = TRUE,
    message = "run_order values are valid"
  )
}

#' Validate batch_id column
#' @noRd
.validate_batch_id <- function(data) {
  values <- data$batch_id

  # Check type
  if (!is.character(values) && !is.factor(values)) {
    return(list(
      valid = FALSE,
      message = sprintf(
        "Column {.field batch_id} must be character or factor, not %s",
        class(values)[1]
      )
    ))
  }

  # Check for all NA
  if (all(is.na(values))) {
    return(list(
      valid = FALSE,
      message = "Column {.field batch_id} contains only NA values"
    ))
  }

  list(
    valid = TRUE,
    message = "batch_id values are valid"
  )
}

#' Validate nominal_conc column
#' @noRd
.validate_nominal_conc <- function(data) {
  values <- data$nominal_conc

  # Check type
  if (!is.numeric(values)) {
    return(list(
      valid = FALSE,
      message = sprintf(
        "Column {.field nominal_conc} must be numeric, not %s",
        class(values)[1]
      )
    ))
  }

  # Check for negative values (excluding NA)
  negative <- values[!is.na(values) & values < 0]
  if (length(negative) > 0) {
    return(list(
      valid = FALSE,
      message = "Column {.field nominal_conc} contains negative values"
    ))
  }

  list(
    valid = TRUE,
    message = "nominal_conc values are valid"
  )
}

#' Validate dilution_factor column
#' @noRd
.validate_dilution_factor <- function(data) {
  values <- data$dilution_factor

  # Check type
  if (!is.numeric(values)) {
    return(list(
      valid = FALSE,
      message = sprintf(
        "Column {.field dilution_factor} must be numeric, not %s",
        class(values)[1]
      )
    ))
  }

  # Check for non-positive values (excluding NA)
  invalid <- values[!is.na(values) & values <= 0]
  if (length(invalid) > 0) {
    return(list(
      valid = FALSE,
      message = "Column {.field dilution_factor} must be positive (> 0)"
    ))
  }

  list(
    valid = TRUE,
    message = "dilution_factor values are valid"
  )
}
