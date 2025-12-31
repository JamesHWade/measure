# validation-report.R
# Validation report generation for analytical method validation
# Supports ICH Q2(R2) and USP <1225> style layouts

# ==============================================================================
# measure_validation_report() - Create a validation report object
# ==============================================================================

#' Create an Analytical Method Validation Report
#'
#' @description
#' Creates a structured validation report object that collects results from
#' various validation studies (calibration, precision, accuracy, etc.) and can
#' be rendered to HTML, PDF, or Word formats using standardized templates.
#'
#' This function supports two major validation frameworks:
#' - **ICH Q2(R2)**: International harmonized guidelines for analytical validation
#' - **USP <1225>**: United States Pharmacopeia compendial validation procedures
#'
#' @param title Report title. Default: "Analytical Method Validation Report"
#' @param method_name Name of the analytical method being validated.
#' @param method_description Brief description of the method (technique, analyte, matrix).
#' @param analyst Name of the analyst(s) performing validation.
#' @param reviewer Name of the reviewer (optional).
#' @param lab Laboratory name or identifier.
#' @param date Date of the validation study. Default: current date.
#' @param instrument Instrument details (name, model, serial number).
#' @param software Software used for data acquisition/processing.
#' @param calibration A `measure_calibration` object from [measure_calibration_fit()].
#' @param lod_loq LOD/LOQ results from [measure_lod()], [measure_loq()], or
#'   [measure_lod_loq()]. Can be a single object or a list.
#' @param accuracy Accuracy results from [measure_accuracy()].
#' @param precision A list containing precision study results:
#'   - `repeatability`: from [measure_repeatability()]
#'   - `intermediate`: from [measure_intermediate_precision()]
#'   - `reproducibility`: from [measure_reproducibility()] (optional)
#' @param linearity Linearity results from [measure_linearity()].
#' @param range A list with `lower` and `upper` validated range limits,
#'   or results supporting range determination.
#' @param specificity User-provided specificity/selectivity assessment.
#'   Can be text, a data frame of interference results, or a list.
#' @param robustness User-provided robustness study results.
#'   Can be text, a data frame, or structured results.
#' @param carryover Carryover results from [measure_carryover()].
#' @param system_suitability System suitability results from
#'   [measure_system_suitability()].
#' @param uncertainty Uncertainty budget from [measure_uncertainty_budget()].
#' @param method_comparison Method comparison results (Bland-Altman, Deming,
#'   Passing-Bablok) from the corresponding functions.
#' @param stability User-provided stability data (solution stability,
#'   freeze-thaw, etc.).
#' @param criteria A `measure_criteria` object defining acceptance criteria,
#'   or a named list of criteria objects for different sections.
#' @param conclusions User-provided conclusions text or a list with
#'   `summary` and `recommendations`.
#' @param references Character vector of references cited.
#' @param appendices Named list of additional content to include as appendices.
#' @param ... Additional metadata to include in the report.
#'
#' @return A `measure_validation_report` object containing:
#'
#' - `metadata`: Report metadata (title, analyst, date, etc.)
#' - `sections`: Named list of validation results by section
#' - `criteria`: Acceptance criteria used
#' - `provenance`: Data provenance and computational environment info
#' - `call`: The function call
#'
#' @details
#' ## Workflow
#'
#' 1. Run individual validation studies using measure functions
#' 2. Collect results into a validation report object
#' 3. Render to desired format using [render_validation_report()]
#'
#' ## Supported Validation Characteristics (ICH Q2)
#'
#' - **Specificity/Selectivity**: Ability to assess analyte in presence of
#'   interferences
#' - **Linearity**: Proportional response over concentration range
#' - **Range**: Validated concentration interval
#' - **Accuracy**: Closeness to true value (trueness)
#' - **Precision**: Repeatability, intermediate precision, reproducibility
#' - **Detection Limit (LOD)**: Lowest detectable amount
#' - **Quantitation Limit (LOQ)**: Lowest quantifiable amount with acceptable
#'   precision/accuracy
#' - **Robustness**: Capacity to remain unaffected by small method variations
#'
#' ## Data Provenance
#'
#' The report automatically captures:
#' - R version and package versions
#' - Date/time of report generation
#' - Function calls used to generate each section
#'
#' @seealso
#' [render_validation_report()] to generate the final report document.
#'
#' Related validation functions:
#' - [measure_calibration_fit()], [measure_calibration_predict()]
#' - [measure_lod()], [measure_loq()], [measure_lod_loq()]
#' - [measure_accuracy()], [measure_linearity()], [measure_carryover()]
#' - [measure_repeatability()], [measure_intermediate_precision()]
#' - [measure_uncertainty_budget()]
#'
#' @export
#' @examples
#' # Create sample validation data
#' set.seed(123)
#' cal_data <- data.frame(
#'   nominal_conc = rep(c(1, 5, 10, 25, 50, 100), each = 3),
#'   response = c(1, 5, 10, 25, 50, 100) * 1000 +
#'     rnorm(18, sd = 50),
#'   sample_type = "standard"
#' )
#'
#' # Fit calibration
#' cal_fit <- measure_calibration_fit(
#'   cal_data,
#'   formula = response ~ nominal_conc,
#'   weights = "1/x"
#' )
#'
#' # Calculate LOD/LOQ (requires sample_type column)
#' blank_data <- data.frame(
#'   response = rnorm(10, mean = 50, sd = 15),
#'   sample_type = "blank"
#' )
#' lod_result <- measure_lod(blank_data, response_col = "response")
#'
#' # Create precision data
#' precision_data <- data.frame(
#'   concentration = rep(c(10, 50, 100), each = 6),
#'   replicate = rep(1:6, 3),
#'   response = c(
#'     rnorm(6, 10000, 200),
#'     rnorm(6, 50000, 800),
#'     rnorm(6, 100000, 1500)
#'   )
#' )
#' repeatability <- measure_repeatability(
#'   precision_data,
#'   response_col = "response",
#'   group_col = "concentration"
#' )
#'
#' # Create validation report
#' report <- measure_validation_report(
#'   title = "Validation of HPLC Method for Compound X",
#'   method_name = "HPLC-UV Assay",
#'   method_description = "Reversed-phase HPLC with UV detection at 254 nm",
#'   analyst = "J. Smith",
#'   lab = "Analytical Development Lab",
#'   calibration = cal_fit,
#'   lod_loq = lod_result,
#'   precision = list(repeatability = repeatability),
#'   conclusions = "Method meets all acceptance criteria for intended use."
#' )
#'
#' print(report)
measure_validation_report <- function(
    title = "Analytical Method Validation Report",
    method_name = NULL,
    method_description = NULL,
    analyst = NULL,
    reviewer = NULL,
    lab = NULL,
    date = Sys.Date(),
    instrument = NULL,
    software = NULL,
    calibration = NULL,
    lod_loq = NULL,
    accuracy = NULL,
    precision = NULL,
    linearity = NULL,
    range = NULL,
    specificity = NULL,
    robustness = NULL,
    carryover = NULL,
    system_suitability = NULL,
    uncertainty = NULL,
    method_comparison = NULL,
    stability = NULL,
    criteria = NULL,
    conclusions = NULL,
    references = NULL,
    appendices = NULL,
    ...) {
  # Capture call
  call <- match.call()

 # Validate inputs
  .validate_report_inputs(
    calibration = calibration,
    lod_loq = lod_loq,
    accuracy = accuracy,
    precision = precision,
    linearity = linearity,
    uncertainty = uncertainty,
    method_comparison = method_comparison
  )

  # Build metadata
metadata <- list(
    title = title,
    method_name = method_name,
    method_description = method_description,
    analyst = analyst,
    reviewer = reviewer,
    lab = lab,
    date = as.Date(date),
    instrument = instrument,
    software = software,
    generated_at = Sys.time(),
    ...
  )

  # Build sections (only include non-NULL)
  sections <- list()

  if (!is.null(calibration)) {
    sections$calibration <- .wrap_section(calibration, "calibration")
  }

  if (!is.null(lod_loq)) {
    sections$lod_loq <- .wrap_section(lod_loq, "lod_loq")
  }

  if (!is.null(accuracy)) {
    sections$accuracy <- .wrap_section(accuracy, "accuracy")
  }

  if (!is.null(precision)) {
    sections$precision <- .wrap_section(precision, "precision")
  }

  if (!is.null(linearity)) {
    sections$linearity <- .wrap_section(linearity, "linearity")
  }

  if (!is.null(range)) {
    sections$range <- .wrap_section(range, "range")
  }

  if (!is.null(specificity)) {
    sections$specificity <- .wrap_section(specificity, "specificity")
  }

  if (!is.null(robustness)) {
    sections$robustness <- .wrap_section(robustness, "robustness")
  }

  if (!is.null(carryover)) {
    sections$carryover <- .wrap_section(carryover, "carryover")
  }

  if (!is.null(system_suitability)) {
    sections$system_suitability <- .wrap_section(
      system_suitability,
      "system_suitability"
    )
  }

  if (!is.null(uncertainty)) {
    sections$uncertainty <- .wrap_section(uncertainty, "uncertainty")
  }

  if (!is.null(method_comparison)) {
    sections$method_comparison <- .wrap_section(
      method_comparison,
      "method_comparison"
    )
  }

  if (!is.null(stability)) {
    sections$stability <- .wrap_section(stability, "stability")
  }

  if (!is.null(conclusions)) {
    sections$conclusions <- .wrap_section(conclusions, "conclusions")
  }

  if (!is.null(references)) {
    sections$references <- references
  }

  if (!is.null(appendices)) {
    sections$appendices <- appendices
  }

  # Build provenance
  provenance <- .build_provenance(sections)

  # Create report object
  structure(
    list(
      metadata = metadata,
      sections = sections,
      criteria = criteria,
      provenance = provenance,
      call = call
    ),
    class = "measure_validation_report"
  )
}

# ==============================================================================
# render_validation_report() - Render report to document
# ==============================================================================

#' Render a Validation Report to Document Format
#'
#' @description
#' Renders a `measure_validation_report` object to HTML, PDF, or Word format
#' using standardized Quarto templates. Templates follow either ICH Q2(R2) or
#' USP <1225> validation report structures.
#'
#' @param report A `measure_validation_report` object created by
#'   [measure_validation_report()].
#' @param output_file Output file path. If NULL, uses the report title
#'   with appropriate extension.
#' @param output_format Output format: "html" (default), "pdf", or "docx".
#'   PDF requires a LaTeX installation (e.g., TinyTeX).
#' @param template Template style: "ich_q2" (default) for ICH Q2(R2) layout,
#'   or "usp_1225" for USP <1225> compendial layout.
#' @param output_dir Directory for output file. Default: current directory.
#' @param include_plots Logical; include diagnostic plots? Default: TRUE.
#' @param include_raw_data Logical; include raw data tables in appendix?
#'
#'   Default: FALSE.
#' @param open Logical; open the rendered document? Default: TRUE in
#'   interactive sessions.
#' @param quiet Logical; suppress Quarto rendering messages? Default: FALSE.
#' @param ... Additional arguments passed to `quarto::quarto_render()`.
#'
#' @return Invisibly returns the path to the rendered document.
#'
#' @details
#' ## Template Styles
#'
#' **ICH Q2(R2) Template** (`template = "ich_q2"`):
#' - Organized by validation characteristic (specificity, linearity, etc.)
#' - Includes performance-based lifecycle considerations
#' - Structured for regulatory submission
#'
#' **USP <1225> Template** (`template = "usp_1225"`):
#' - Compendial validation structure
#' - Category-based organization (I, II, III, IV)
#' - Emphasis on system suitability
#'
#' ## Requirements
#'
#' - **HTML output**: Requires `quarto` package
#' - **PDF output**: Requires `quarto` package and LaTeX (TinyTeX recommended)
#' - **DOCX output**: Requires `quarto` package
#'
#' Install Quarto from <https://quarto.org/docs/get-started/>.
#' Install TinyTeX with `quarto::quarto_install_tinytex()`.
#'
#' @seealso [measure_validation_report()] to create the report object.
#'
#' @export
#' @examples
#' \dontrun{
#' # Create a validation report (see measure_validation_report examples)
#' report <- measure_validation_report(
#'   title = "Method Validation Report",
#'   method_name = "HPLC Assay",
#'   analyst = "J. Smith"
#' )
#'
#' # Render to HTML with ICH Q2 template
#' render_validation_report(report, output_format = "html")
#'
#' # Render to PDF with USP template
#' render_validation_report(
#'   report,
#'   output_format = "pdf",
#'   template = "usp_1225",
#'   output_file = "validation_report.pdf"
#' )
#'
#' # Render to Word for editing
#' render_validation_report(report, output_format = "docx")
#' }
render_validation_report <- function(
    report,
    output_file = NULL,
    output_format = c("html", "pdf", "docx"),
    template = c("ich_q2", "usp_1225"),
    output_dir = ".",
    include_plots = TRUE,
    include_raw_data = FALSE,
    open = interactive(),
    quiet = FALSE,
    ...) {
  # Validate report
  if (!inherits(report, "measure_validation_report")) {
    cli::cli_abort(
      "{.arg report} must be a {.cls measure_validation_report} object."
    )
  }

  # Check for quarto
rlang::check_installed("quarto", reason = "to render validation reports")

  output_format <- match.arg(output_format)
  template <- match.arg(template)

  # Determine output file
  if (is.null(output_file)) {
    safe_title <- gsub("[^a-zA-Z0-9_-]", "_", report$metadata$title)
    safe_title <- gsub("_+", "_", safe_title)
    safe_title <- tolower(safe_title)
    ext <- switch(output_format, html = "html", pdf = "pdf", docx = "docx")
    output_file <- paste0(safe_title, ".", ext)
  } else {
    # Validate output_file doesn't contain path separators (path traversal protection)
    if (grepl("[/\\\\]", output_file) || grepl("\\.\\.", output_file)) {
      cli::cli_abort(
        c(
          "{.arg output_file} must be a filename, not a path.",
          "i" = "Use {.arg output_dir} to specify the output directory."
        )
      )
    }
  }

  # Resolve output directory
  output_dir <- normalizePath(output_dir, mustWork = FALSE)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Get template path
  template_file <- .get_template_path(template)

  # Create temporary directory for rendering
  temp_dir <- tempfile("validation_report_")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # Copy template to temp dir
  temp_qmd <- file.path(temp_dir, "report.qmd")
  file.copy(template_file, temp_qmd)

  # Save report data for template
  report_data_file <- file.path(temp_dir, "report_data.rds")
  report_data <- list(
    report = report,
    include_plots = include_plots,
    include_raw_data = include_raw_data
  )
  saveRDS(report_data, report_data_file)

  # Render with quarto
  if (!quiet) {
    cli::cli_alert_info("Rendering validation report...")
  }

  output_path <- file.path(output_dir, output_file)

  quarto::quarto_render(
    input = temp_qmd,
    output_format = output_format,
    output_file = output_file,
    execute_params = list(report_data_path = report_data_file),
    quiet = quiet,
    ...
  )

  # Move rendered file to output directory
  rendered_file <- file.path(temp_dir, output_file)
  if (!file.exists(rendered_file)) {
    cli::cli_abort(
      c(
        "Rendering failed: output file was not created.",
        "i" = "Check Quarto output for errors."
      )
    )
  }

  copy_success <- file.copy(rendered_file, output_path, overwrite = TRUE)
  if (!copy_success) {
    cli::cli_abort(
      c(
        "Failed to copy rendered file to {.path {output_path}}.",
        "i" = "Check disk space and file permissions."
      )
    )
  }

  if (!quiet) {
    cli::cli_alert_success("Report saved to {.path {output_path}}")
  }

  # Open if requested
  if (open && file.exists(output_path)) {
    utils::browseURL(output_path)
  }

  invisible(output_path)
}

# ==============================================================================
# print method
# ==============================================================================

#' Print a Validation Report
#'
#' @description
#' Displays a formatted summary of a validation report object, including
#' metadata, section status, conclusions, and provenance information.
#'
#' @param x A `measure_validation_report` object.
#' @param ... Additional arguments (currently ignored).
#'
#' @return Invisibly returns the input object.
#'
#' @export
#' @examples
#' report <- measure_validation_report(
#'   title = "Test Report",
#'   method_name = "HPLC Assay",
#'   analyst = "J. Smith"
#' )
#' print(report)
print.measure_validation_report <- function(x, ...) {
  cli::cli_h1("Validation Report")

  # Metadata
  cli::cli_text("{.strong Title}: {x$metadata$title}")
  if (!is.null(x$metadata$method_name)) {
    cli::cli_text("{.strong Method}: {x$metadata$method_name}")
  }
  if (!is.null(x$metadata$analyst)) {
    cli::cli_text("{.strong Analyst}: {x$metadata$analyst}")
  }
  if (!is.null(x$metadata$lab)) {
    cli::cli_text("{.strong Lab}: {x$metadata$lab}")
  }
  cli::cli_text("{.strong Date}: {x$metadata$date}")
  cli::cli_text("")

  # Sections summary
  section_names <- names(x$sections)
  section_names <- setdiff(section_names, c("conclusions", "references", "appendices"))

  if (length(section_names) > 0) {
    cli::cli_h2("Validation Sections")

    for (sec in section_names) {
      section <- x$sections[[sec]]
      status <- .get_section_status(section)
      label <- .section_label(sec)

      if (status == "pass") {
        cli::cli_alert_success("{label}")
      } else if (status == "fail") {
        cli::cli_alert_danger("{label}")
      } else {
        cli::cli_alert_info("{label}")
      }
    }
    cli::cli_text("")
  }

  # Conclusions
  if (!is.null(x$sections$conclusions)) {
    cli::cli_h2("Conclusions")
    conclusions <- x$sections$conclusions
    if (is.character(conclusions$data)) {
      cli::cli_text(conclusions$data)
    } else if (is.list(conclusions$data) && !is.null(conclusions$data$summary)) {
      cli::cli_text(conclusions$data$summary)
    }
    cli::cli_text("")
  }

  # Provenance
  cli::cli_h2("Provenance")
  cli::cli_text("Generated: {x$provenance$timestamp}")
  cli::cli_text("R version: {x$provenance$r_version}")
  cli::cli_text("measure version: {x$provenance$measure_version}")

  cli::cli_text("")
  cli::cli_alert_info(
    "Use {.code render_validation_report()} to generate document"
  )

  invisible(x)
}

# ==============================================================================
# summary method
# ==============================================================================

#' Summarize a Validation Report
#'
#' @description
#' Creates a summary table of all validation sections in the report,
#' showing section status, result counts, and notes.
#'
#' @param object A `measure_validation_report` object.
#' @param ... Additional arguments (currently ignored).
#'
#' @return A tibble with columns:
#'   - `section`: Section name
#'   - `status`: Pass/fail/info status
#'   - `n_results`: Number of results in section
#'   - `notes`: Additional notes
#'
#'   Returns `NULL` invisibly if the report has no validation sections.
#'
#' @export
#' @examples
#' # Create a report with some sections
#' report <- measure_validation_report(
#'   title = "Test Report",
#'   specificity = "No interference observed"
#' )
#' summary(report)
summary.measure_validation_report <- function(object, ...) {
  # Build summary table
  sections <- object$sections
  section_names <- names(sections)
  section_names <- setdiff(
    section_names,
    c("conclusions", "references", "appendices")
  )

  if (length(section_names) == 0) {
    cli::cli_alert_warning("No validation sections in report")
    return(invisible(NULL))
  }

  summary_data <- lapply(section_names, function(sec) {
    sec_data <- sections[[sec]]
    tibble::tibble(
      section = .section_label(sec),
      status = .get_section_status(sec_data),
      n_results = .count_results(sec_data),
      notes = .get_section_notes(sec_data)
    )
  })

  summary_tbl <- do.call(rbind, summary_data)

  # Print summary
  cli::cli_h1("Validation Report Summary")
  cli::cli_text("{.strong Method}: {object$metadata$method_name %||% 'Not specified'}")
  cli::cli_text("{.strong Date}: {object$metadata$date}")
  cli::cli_text("")

  print(summary_tbl)

  # Overall status
  all_pass <- all(summary_tbl$status %in% c("pass", "info"))
  any_fail <- any(summary_tbl$status == "fail")

  cli::cli_text("")
  if (any_fail) {
    cli::cli_alert_danger("One or more sections did not meet acceptance criteria")
  } else if (all_pass) {
    cli::cli_alert_success("All sections meet acceptance criteria")
  }

  invisible(summary_tbl)
}

# ==============================================================================
# tidy method
# ==============================================================================

#' Tidy a Validation Report
#'
#' @description
#' Extracts key parameters and statistics from all validation sections
#' into a tidy tibble format suitable for further analysis or reporting.
#'
#' @param x A `measure_validation_report` object.
#' @param ... Additional arguments (currently ignored).
#'
#' @return A tibble with columns:
#'   - `section`: Section name
#'   - `parameter`: Parameter name
#'   - `value`: Parameter value
#'   - `unit`: Unit of measurement (if available)
#'   - `status`: Pass/fail status (if available)
#'
#'   Returns an empty tibble if no sections contain tidy-able data.
#'
#' @export
#' @importFrom generics tidy
#' @examples
#' # Create sample data
#' blank_data <- data.frame(
#'   response = rnorm(10, mean = 50, sd = 15),
#'   sample_type = "blank"
#' )
#' lod_result <- measure_lod(blank_data, response_col = "response")
#'
#' report <- measure_validation_report(
#'   title = "Test Report",
#'   lod_loq = lod_result
#' )
#' tidy(report)
tidy.measure_validation_report <- function(x, ...) {
  sections <- x$sections
  section_names <- names(sections)
  section_names <- setdiff(
    section_names,
    c("conclusions", "references", "appendices")
  )

  if (length(section_names) == 0) {
    return(tibble::tibble(
      section = character(),
      parameter = character(),
      value = numeric(),
      unit = character(),
      status = character()
    ))
  }

  results <- lapply(section_names, function(sec) {
    section <- sections[[sec]]
    tidy_section <- .tidy_section(section, sec)
    if (!is.null(tidy_section) && nrow(tidy_section) > 0) {
      tidy_section$section <- .section_label(sec)
      tidy_section
    } else {
      NULL
    }
  })

  results <- results[!vapply(results, is.null, logical(1))]

  if (length(results) == 0) {
    return(tibble::tibble(
      section = character(),
      parameter = character(),
      value = numeric(),
      unit = character(),
      status = character()
    ))
  }

  # Use bind_rows to handle different column structures
  result <- dplyr::bind_rows(results)

  # Reorder so section is first
  if ("section" %in% names(result)) {
    result <- result[, c("section", setdiff(names(result), "section"))]
  }

  tibble::as_tibble(result)
}

# ==============================================================================
# Helper functions
# ==============================================================================

#' Wrap section data with metadata
#' @noRd
.wrap_section <- function(data, section_type) {
  list(
    data = data,
    type = section_type,
    timestamp = Sys.time()
  )
}

#' Validate report inputs
#' @noRd
.validate_report_inputs <- function(
    calibration = NULL,
    lod_loq = NULL,
    accuracy = NULL,
    precision = NULL,
    linearity = NULL,
    uncertainty = NULL,
    method_comparison = NULL) {
  # Check calibration
  if (!is.null(calibration)) {
    if (!inherits(calibration, "measure_calibration")) {
      cli::cli_warn(
        c(
          "{.arg calibration} is not a {.cls measure_calibration} object.",
          "i" = "Expected output from {.fn measure_calibration_fit}."
        )
      )
    }
  }

  # Check precision is a list
  if (!is.null(precision) && !is.list(precision)) {
    cli::cli_abort(
      c(
        "{.arg precision} must be a list.",
        "i" = paste(
          "Expected a list with components like",
          "{.val repeatability}, {.val intermediate}."
        )
      )
    )
  }

  # Check uncertainty
  if (!is.null(uncertainty)) {
    if (!inherits(uncertainty, "measure_uncertainty_budget")) {
      cli::cli_warn(
        c(
          "{.arg uncertainty} is not a {.cls measure_uncertainty_budget} object.",
          "i" = "Expected output from {.fn measure_uncertainty_budget}."
        )
      )
    }
  }
}

#' Build provenance information
#' @noRd
.build_provenance <- function(sections) {
  # Collect provenance from sections
  section_provenance <- lapply(names(sections), function(sec) {
    section <- sections[[sec]]
    if (is.list(section) && !is.null(section$data)) {
      data <- section$data
      # Extract call if available - use [[ to avoid tibble warnings
      if (is.list(data) && "call" %in% names(data)) {
        list(section = sec, call = deparse(data[["call"]]))
      } else if (!is.null(attr(data, "call"))) {
        list(section = sec, call = deparse(attr(data, "call")))
      } else {
        NULL
      }
    } else {
      NULL
    }
  })
  section_provenance <- section_provenance[
    !vapply(section_provenance, is.null, logical(1))
  ]

  list(
    timestamp = Sys.time(),
    r_version = paste(R.version$major, R.version$minor, sep = "."),
    measure_version = as.character(utils::packageVersion("measure")),
    platform = R.version$platform,
    section_calls = section_provenance
  )
}

#' Get template path
#' @noRd
.get_template_path <- function(template) {
  template_name <- switch(
    template,
    ich_q2 = "validation-report-ich-q2.qmd",
    usp_1225 = "validation-report-usp-1225.qmd"
  )

  path <- system.file(
    "templates", template_name,
    package = "measure",
    mustWork = FALSE
  )

  if (path == "" || !file.exists(path)) {
    cli::cli_abort(
      c(
        "Template {.val {template}} not found.",
        "i" = "Available templates: {.val ich_q2}, {.val usp_1225}"
      )
    )
  }

  path
}

#' Get section status
#' @noRd
.get_section_status <- function(section) {
  if (!is.list(section) || is.null(section$data)) {
    return("info")
  }

  data <- section$data

  # Check for pass/fail indicators - use %in% and [[ to avoid tibble warnings
  if (is.list(data)) {
    # Check common pass/fail fields
    if ("pass" %in% names(data)) {
      return(if (isTRUE(data[["pass"]])) "pass" else "fail")
    }
    if ("all_pass" %in% names(data)) {
      return(if (isTRUE(data[["all_pass"]])) "pass" else "fail")
    }
    if ("meets_criteria" %in% names(data)) {
      return(if (isTRUE(data[["meets_criteria"]])) "pass" else "fail")
    }
    # Check statistics for assessment
    if ("statistics" %in% names(data) && is.list(data[["statistics"]])) {
      stats <- data[["statistics"]]
      if ("pass" %in% names(stats)) {
        return(if (isTRUE(stats[["pass"]])) "pass" else "fail")
      }
    }
    # Check assessment
    if ("assessment" %in% names(data) && is.data.frame(data[["assessment"]])) {
      assessment <- data[["assessment"]]
      if ("pass" %in% names(assessment)) {
        all_pass <- all(assessment[["pass"]], na.rm = TRUE)
        return(if (all_pass) "pass" else "fail")
      }
    }
  }

  "info"
}

#' Get section label
#' @noRd
.section_label <- function(section_name) {
  labels <- c(
    calibration = "Calibration",
    lod_loq = "LOD/LOQ",
    accuracy = "Accuracy",
    precision = "Precision",
    linearity = "Linearity",
    range = "Range",
    specificity = "Specificity/Selectivity",
    robustness = "Robustness",
    carryover = "Carryover",
    system_suitability = "System Suitability",
    uncertainty = "Measurement Uncertainty",
    method_comparison = "Method Comparison",
    stability = "Stability"
  )

  if (section_name %in% names(labels)) {
    labels[[section_name]]
  } else {
    tools::toTitleCase(gsub("_", " ", section_name))
  }
}

#' Count results in section
#' @noRd
.count_results <- function(section) {
  if (!is.list(section) || is.null(section$data)) {
    return(NA_integer_)
  }

  data <- section$data

  if (is.data.frame(data)) {
    return(nrow(data))
  }

  if (is.list(data)) {
    # Check for results data frame
    if (!is.null(data$results) && is.data.frame(data$results)) {
      return(nrow(data$results))
    }
    # Check for data data frame
    if (!is.null(data$data) && is.data.frame(data$data)) {
      return(nrow(data$data))
    }
    # Check for statistics
    if (!is.null(data$statistics)) {
      if (is.data.frame(data$statistics)) {
        return(nrow(data$statistics))
      }
      if (is.list(data$statistics) && !is.null(data$statistics$n)) {
        return(data$statistics$n)
      }
    }
  }

  NA_integer_
}

#' Get section notes
#' @noRd
.get_section_notes <- function(section) {
  if (!is.list(section) || is.null(section$data)) {
    return("")
  }

  data <- section$data

  # Check for notes field - use [[ to avoid tibble warnings
  if (is.list(data) && "notes" %in% names(data)) {
    return(as.character(data[["notes"]])[1])
  }

  # Check for method field - use [[ to avoid tibble warnings
  if (is.list(data) && "method" %in% names(data)) {
    return(paste("Method:", data[["method"]]))
  }

  ""
}

#' Tidy a section for summary output
#' @noRd
.tidy_section <- function(section, section_name) {
  if (!is.list(section) || is.null(section$data)) {
    return(NULL)
  }

  data <- section$data

  # Try to use tidy method if available
  if (inherits(data, c(
    "measure_calibration",
    "measure_lod",
    "measure_loq",
    "measure_accuracy",
    "measure_linearity",
    "measure_precision",
    "measure_uncertainty_budget",
    "measure_bland_altman",
    "measure_deming_regression"
  ))) {
    tryCatch(
      {
        tidy_result <- generics::tidy(data)
        if (is.data.frame(tidy_result)) {
          return(tidy_result)
        }
      },
      error = function(e) NULL
    )
  }

  # Fallback: try to extract key statistics
  if (is.list(data) && !is.null(data$statistics)) {
    stats <- data$statistics
    if (is.list(stats)) {
      # Convert list to tibble
      stat_df <- tibble::tibble(
        parameter = names(stats),
        value = vapply(stats, function(x) {
          if (is.numeric(x) && length(x) == 1) x else NA_real_
        }, numeric(1))
      )
      stat_df <- stat_df[!is.na(stat_df$value), ]
      if (nrow(stat_df) > 0) {
        stat_df$unit <- ""
        stat_df$status <- ""
        return(stat_df)
      }
    }
  }

  NULL
}

# ==============================================================================
# Utility functions for report generation
# ==============================================================================

#' Check if validation report has a section
#'
#' @param report A `measure_validation_report` object.
#' @param section Section name to check.
#' @return Logical indicating if section exists and has data.
#' @export
#' @examples
#' report <- measure_validation_report(title = "Test Report")
#' has_validation_section(report, "calibration")  # FALSE
has_validation_section <- function(report, section) {
  if (!inherits(report, "measure_validation_report")) {
    return(FALSE)
  }

  if (!section %in% names(report$sections)) {
    return(FALSE)
  }

  sec <- report$sections[[section]]
  !is.null(sec) && !is.null(sec$data)
}

#' Get validation section data
#'
#' @param report A `measure_validation_report` object.
#' @param section Section name to retrieve.
#' @return The section data, or NULL if not found.
#' @export
#' @examples
#' report <- measure_validation_report(title = "Test Report")
#' get_validation_section(report, "calibration")  # NULL
get_validation_section <- function(report, section) {
  if (!has_validation_section(report, section)) {
    return(NULL)
  }

  report$sections[[section]]$data
}

#' Add or update a validation section
#'
#' @param report A `measure_validation_report` object.
#' @param section Section name.
#' @param data Section data to add.
#' @return Updated `measure_validation_report` object.
#' @export
#' @examples
#' report <- measure_validation_report(title = "Test Report")
#' # Add custom section
#' report <- add_validation_section(
#'   report,
#'   "custom_study",
#'   list(results = data.frame(x = 1:3, y = 4:6))
#' )
add_validation_section <- function(report, section, data) {
  if (!inherits(report, "measure_validation_report")) {
    cli::cli_abort(
      "{.arg report} must be a {.cls measure_validation_report} object."
    )
  }

  report$sections[[section]] <- .wrap_section(data, section)
  report$provenance$timestamp <- Sys.time()

  report
}
