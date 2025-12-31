# Tests for validation report functions

# ==============================================================================
# measure_validation_report()
# ==============================================================================

test_that("measure_validation_report creates basic report object", {
  report <- measure_validation_report(
    title = "Test Validation Report",
    method_name = "Test Method",
    analyst = "Test Analyst"
  )

  expect_s3_class(report, "measure_validation_report")
  expect_named(report, c("metadata", "sections", "criteria", "provenance", "call"))

  # Check metadata
  expect_equal(report$metadata$title, "Test Validation Report")
  expect_equal(report$metadata$method_name, "Test Method")
  expect_equal(report$metadata$analyst, "Test Analyst")
})

test_that("measure_validation_report accepts calibration results", {
  # Create calibration data
  cal_data <- data.frame(
    nominal_conc = rep(c(1, 5, 10, 25, 50), each = 3),
    response = rep(c(1, 5, 10, 25, 50), each = 3) * 1000 + rnorm(15, sd = 50),
    sample_type = "standard"
  )

  cal_fit <- measure_calibration_fit(
    cal_data,
    formula = response ~ nominal_conc,
    weights = "1/x"
  )

  report <- measure_validation_report(
    title = "Calibration Test",
    calibration = cal_fit
  )

  expect_true(has_validation_section(report, "calibration"))
  expect_s3_class(get_validation_section(report, "calibration"), "measure_calibration")
})

test_that("measure_validation_report accepts precision results", {
  # Create precision data
  precision_data <- data.frame(
    level = rep(c("Low", "Mid", "High"), each = 6),
    replicate = rep(1:6, 3),
    value = c(rnorm(6, 10, 0.5), rnorm(6, 50, 2), rnorm(6, 100, 4))
  )

  rep_result <- measure_repeatability(
    precision_data,
    response_col = "value",
    group_col = "level"
  )

  report <- measure_validation_report(
    title = "Precision Test",
    precision = list(repeatability = rep_result)
  )

  expect_true(has_validation_section(report, "precision"))

  prec_section <- get_validation_section(report, "precision")
  expect_true(is.list(prec_section))
  expect_true("repeatability" %in% names(prec_section))
})

test_that("measure_validation_report accepts LOD/LOQ results", {
  # Need sample_type column for measure_lod
  blank_data <- data.frame(
    response = rnorm(10, mean = 50, sd = 15),
    sample_type = "blank"
  )

  lod_result <- measure_lod(blank_data, response_col = "response")

  report <- measure_validation_report(
    title = "LOD Test",
    lod_loq = lod_result
  )

  expect_true(has_validation_section(report, "lod_loq"))
})

test_that("measure_validation_report accepts accuracy results", {
  acc_data <- data.frame(
    level = rep(c("80%", "100%", "120%"), each = 3),
    reference = rep(c(80, 100, 120), each = 3),
    measured = c(
      rnorm(3, 80, 2),
      rnorm(3, 100, 2),
      rnorm(3, 120, 2)
    )
  )

  acc_result <- measure_accuracy(
    acc_data,
    measured_col = "measured",
    reference_col = "reference",
    group_col = "level"
  )

  report <- measure_validation_report(
    title = "Accuracy Test",
    accuracy = acc_result
  )

  expect_true(has_validation_section(report, "accuracy"))
})

test_that("measure_validation_report accepts uncertainty budget", {
  unc_budget <- measure_uncertainty_budget(
    uncertainty_component("repeatability", value = 0.5, type = "A"),
    uncertainty_component("calibration", value = 0.3, type = "A"),
    uncertainty_component("balance", value = 0.1, type = "B"),
    k = 2
  )

  report <- measure_validation_report(
    title = "Uncertainty Test",
    uncertainty = unc_budget
  )

  expect_true(has_validation_section(report, "uncertainty"))
})

test_that("measure_validation_report accepts text sections", {
  report <- measure_validation_report(
    title = "Text Sections Test",
    specificity = "No interfering peaks observed at the analyte retention time.",
    robustness = "Method showed acceptable robustness within tested parameter ranges.",
    conclusions = "Method meets all acceptance criteria."
  )

  expect_true(has_validation_section(report, "specificity"))
  expect_true(has_validation_section(report, "robustness"))
  expect_true(has_validation_section(report, "conclusions"))
})

test_that("measure_validation_report handles list-based sections", {
  report <- measure_validation_report(
    title = "List Sections Test",
    specificity = list(
      description = "Forced degradation study performed.",
      results = data.frame(
        condition = c("Acid", "Base", "Oxidation"),
        interference = c("None", "None", "Minor")
      ),
      conclusion = "Method is specific for the analyte."
    ),
    range = list(
      lower = 1,
      upper = 100,
      units = "ng/mL"
    ),
    conclusions = list(
      summary = "Method validated successfully.",
      recommendations = c("Use within validated range", "Monitor system suitability")
    )
  )

  expect_true(has_validation_section(report, "specificity"))
  expect_true(has_validation_section(report, "range"))

  range_data <- get_validation_section(report, "range")
  expect_equal(range_data$lower, 1)
  expect_equal(range_data$upper, 100)
})

test_that("measure_validation_report includes provenance", {
  report <- measure_validation_report(title = "Provenance Test")

  expect_true(!is.null(report$provenance))
  expect_true(!is.null(report$provenance$timestamp))
  expect_true(!is.null(report$provenance$r_version))
  expect_true(!is.null(report$provenance$measure_version))
})

test_that("measure_validation_report accepts references", {
  report <- measure_validation_report(
    title = "References Test",
    references = c(
      "ICH Q2(R2) Validation of Analytical Procedures (2023)",
      "USP <1225> Validation of Compendial Procedures"
    )
  )

  expect_equal(length(report$sections$references), 2)
})

test_that("measure_validation_report accepts appendices", {
  report <- measure_validation_report(
    title = "Appendices Test",
    appendices = list(
      raw_data = data.frame(x = 1:5, y = 6:10),
      notes = "Additional experimental notes here."
    )
  )

  expect_true(!is.null(report$sections$appendices))
  expect_equal(length(report$sections$appendices), 2)
})

# ==============================================================================
# print method
# ==============================================================================

test_that("print.measure_validation_report works", {
  report <- measure_validation_report(
    title = "Print Test Report",
    method_name = "Test Method",
    analyst = "Test Analyst",
    lab = "Test Lab"
  )

  # Just verify it prints without error
  expect_no_error(print(report))
})

test_that("print.measure_validation_report shows section status", {
  # Create a report with some sections
  blank_data <- data.frame(
    response = rnorm(10, mean = 50, sd = 15),
    sample_type = "blank"
  )
  lod_result <- measure_lod(blank_data, response_col = "response")

  report <- measure_validation_report(
    title = "Status Test",
    lod_loq = lod_result,
    conclusions = "All tests pass."
  )

  # Just verify it prints without error
  expect_no_error(print(report))
})

# ==============================================================================
# summary method
# ==============================================================================

test_that("summary.measure_validation_report returns tibble", {
  precision_data <- data.frame(
    level = rep("QC", 6),
    value = rnorm(6, 100, 2)
  )

  rep_result <- measure_repeatability(
    precision_data,
    response_col = "value",
    group_col = "level"
  )

  report <- measure_validation_report(
    title = "Summary Test",
    precision = list(repeatability = rep_result)
  )

  summary_result <- summary(report)

  expect_s3_class(summary_result, "tbl_df")
  expect_true("section" %in% names(summary_result))
  expect_true("status" %in% names(summary_result))
})

# ==============================================================================
# tidy method
# ==============================================================================

test_that("tidy.measure_validation_report returns tibble", {
  blank_data <- data.frame(
    response = rnorm(10, mean = 50, sd = 15),
    sample_type = "blank"
  )
  lod_result <- measure_lod(blank_data, response_col = "response")

  report <- measure_validation_report(
    title = "Tidy Test",
    lod_loq = lod_result
  )

  tidy_result <- tidy(report)

  expect_s3_class(tidy_result, "tbl_df")
})

test_that("tidy.measure_validation_report handles empty report", {
  report <- measure_validation_report(title = "Empty Report")

  tidy_result <- tidy(report)

  expect_s3_class(tidy_result, "tbl_df")
  expect_equal(nrow(tidy_result), 0)
})

# ==============================================================================
# helper functions
# ==============================================================================

test_that("has_validation_section works correctly", {
  report <- measure_validation_report(
    title = "Helper Test",
    specificity = "Test specificity"
  )

  expect_true(has_validation_section(report, "specificity"))
  expect_false(has_validation_section(report, "calibration"))
  expect_false(has_validation_section(report, "nonexistent"))
})

test_that("get_validation_section works correctly", {
  report <- measure_validation_report(
    title = "Helper Test",
    specificity = "Test specificity content"
  )

  spec <- get_validation_section(report, "specificity")
  expect_equal(spec, "Test specificity content")

  cal <- get_validation_section(report, "calibration")
  expect_null(cal)
})

test_that("add_validation_section works correctly", {
  report <- measure_validation_report(title = "Add Section Test")

  # Add a new section
  report <- add_validation_section(
    report,
    "custom_study",
    list(results = data.frame(x = 1:3, y = 4:6))
  )

  expect_true(has_validation_section(report, "custom_study"))

  custom <- get_validation_section(report, "custom_study")
  expect_true(is.data.frame(custom$results))
})

test_that("add_validation_section validates input", {
  expect_error(
    add_validation_section("not_a_report", "section", "data"),
    "measure_validation_report"
  )
})

# ==============================================================================
# render_validation_report() - basic validation only
# ==============================================================================

test_that("render_validation_report validates report input", {
  skip_if_not_installed("quarto")

  expect_error(
    render_validation_report("not_a_report"),
    "measure_validation_report"
  )
})

test_that("render_validation_report requires quarto package", {
  skip_if(requireNamespace("quarto", quietly = TRUE), "quarto is installed")

  report <- measure_validation_report(title = "Test")

  # This should error about quarto package
  expect_error(
    render_validation_report(report),
    "quarto"
  )
})

# ==============================================================================
# Integration tests
# ==============================================================================

test_that("measure_validation_report creates comprehensive report", {
  set.seed(123)

  # Calibration
  cal_data <- data.frame(
    nominal_conc = rep(c(1, 5, 10, 25, 50, 100), each = 3),
    response = rep(c(1, 5, 10, 25, 50, 100), each = 3) * 1000 + rnorm(18, sd = 50),
    sample_type = "standard"
  )
  cal_fit <- measure_calibration_fit(cal_data, response ~ nominal_conc, weights = "1/x")

  # LOD
  blank_data <- data.frame(
    response = rnorm(10, mean = 50, sd = 15),
    sample_type = "blank"
  )
  lod_result <- measure_lod(blank_data, response_col = "response")

  # Precision
  precision_data <- data.frame(
    level = rep(c("Low", "Mid", "High"), each = 6),
    value = c(rnorm(6, 10, 0.5), rnorm(6, 50, 2), rnorm(6, 100, 4))
  )
  rep_result <- measure_repeatability(precision_data, "value", "level")

  # Accuracy
  acc_data <- data.frame(
    level = rep(c("80%", "100%", "120%"), each = 3),
    reference = rep(c(80, 100, 120), each = 3),
    measured = c(rnorm(3, 80, 2), rnorm(3, 100, 2), rnorm(3, 120, 2))
  )
  acc_result <- measure_accuracy(acc_data, "measured", "reference", "level")

  # Uncertainty
  unc_budget <- measure_uncertainty_budget(
    uncertainty_component("repeatability", value = 0.5, type = "A"),
    uncertainty_component("calibration", value = 0.3, type = "A"),
    k = 2
  )

  # Create comprehensive report
  report <- measure_validation_report(
    title = "Comprehensive Validation Report",
    method_name = "HPLC-UV Assay for Compound X",
    method_description = "Reversed-phase HPLC with UV detection",
    analyst = "J. Smith",
    reviewer = "A. Jones",
    lab = "Analytical Development",
    instrument = "Agilent 1260 Infinity II",
    calibration = cal_fit,
    lod_loq = lod_result,
    accuracy = acc_result,
    precision = list(repeatability = rep_result),
    linearity = NULL,  # Would come from measure_linearity()
    range = list(lower = 1, upper = 100, units = "ng/mL"),
    specificity = "No interference observed from matrix components.",
    robustness = list(
      factors = c("Flow rate", "Column temperature", "Mobile phase pH"),
      conclusion = "Method is robust within tested ranges."
    ),
    uncertainty = unc_budget,
    conclusions = list(
      summary = "The analytical method meets all acceptance criteria.",
      recommendations = c(
        "Method is suitable for intended use",
        "Revalidate if significant changes are made"
      )
    ),
    references = c(
      "ICH Q2(R2) Validation of Analytical Procedures (2023)",
      "USP <1225> Validation of Compendial Procedures"
    )
  )

  # Verify report structure
  expect_s3_class(report, "measure_validation_report")

  # Check all sections are present
  expect_true(has_validation_section(report, "calibration"))
  expect_true(has_validation_section(report, "lod_loq"))
  expect_true(has_validation_section(report, "accuracy"))
  expect_true(has_validation_section(report, "precision"))
  expect_true(has_validation_section(report, "range"))
  expect_true(has_validation_section(report, "specificity"))
  expect_true(has_validation_section(report, "robustness"))
  expect_true(has_validation_section(report, "uncertainty"))
  expect_true(has_validation_section(report, "conclusions"))

  # Check references
  expect_equal(length(report$sections$references), 2)

  # Check metadata
  expect_equal(report$metadata$method_name, "HPLC-UV Assay for Compound X")
  expect_equal(report$metadata$analyst, "J. Smith")

  # Print and summary should work
  expect_no_error(print(report))

  summary_tbl <- summary(report)
  expect_s3_class(summary_tbl, "tbl_df")
  expect_true(nrow(summary_tbl) > 0)

  # Tidy should work
  tidy_tbl <- tidy(report)
  expect_s3_class(tidy_tbl, "tbl_df")
})

# ==============================================================================
# Edge cases
# ==============================================================================

test_that("measure_validation_report handles NULL metadata gracefully", {
  report <- measure_validation_report(
    title = "Minimal Report"
    # All other fields are NULL by default
  )

  expect_s3_class(report, "measure_validation_report")
  expect_null(report$metadata$method_name)
  expect_null(report$metadata$analyst)

  # Should still print without error
  expect_no_error(print(report))
})

test_that("measure_validation_report warns on non-standard input types", {
  # Should warn but not error
  expect_warning(
    report <- measure_validation_report(
      title = "Warning Test",
      calibration = data.frame(x = 1:3)  # Not a measure_calibration object
    ),
    "measure_calibration"
  )
})

test_that("measure_validation_report preserves extra metadata via ...", {
  report <- measure_validation_report(
    title = "Extra Metadata Test",
    project_code = "PROJ-001",
    study_number = "STUDY-123"
  )

  expect_equal(report$metadata$project_code, "PROJ-001")
  expect_equal(report$metadata$study_number, "STUDY-123")
})

# ==============================================================================
# Error path tests
# ==============================================================================

test_that("measure_validation_report errors when precision is not a list", {
  # Passing a non-list value (e.g., a data.frame or vector) should error
  expect_error(
    measure_validation_report(
      title = "Precision Error Test",
      precision = "not a list"
    ),
    "precision.*must be a list"
  )

  expect_error(
    measure_validation_report(
      title = "Precision Error Test",
      precision = 1:10
    ),
    "precision.*must be a list"
  )
})

test_that("summary.measure_validation_report returns NULL for empty report", {
  report <- measure_validation_report(title = "Empty Summary Test")

  # Should return NULL invisibly (cli::cli_alert_warning prints message, not R warning)
  result <- suppressMessages(summary(report))

  expect_null(result)
})

test_that("render_validation_report errors on invalid template", {
  skip_if_not_installed("quarto")

  report <- measure_validation_report(title = "Template Error Test")

  # Invalid template should error with match.arg
  expect_error(
    render_validation_report(report, template = "nonexistent_template"),
    "should be one of"
  )
})

test_that("render_validation_report errors on path traversal in output_file", {
  skip_if_not_installed("quarto")

  report <- measure_validation_report(title = "Path Traversal Test")

  # Path with directory separator should error
  expect_error(
    render_validation_report(report, output_file = "../malicious.html"),
    "output_file.*must be a filename"
  )

  # Path with backslash should error
  expect_error(
    render_validation_report(report, output_file = "..\\malicious.html"),
    "output_file.*must be a filename"
  )

  # Path with .. should error
  expect_error(
    render_validation_report(report, output_file = "..report.html"),
    "output_file.*must be a filename"
  )
})
