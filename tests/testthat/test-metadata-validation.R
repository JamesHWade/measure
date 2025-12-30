# ==============================================================================
# Tests for metadata-validation.R
# ==============================================================================

test_that("measure_sample_types is exported and correct", {
  expect_equal(
    measure_sample_types,
    c("qc", "standard", "blank", "unknown", "reference")
  )
})

# ==============================================================================
# measure_validate_metadata() tests
# ==============================================================================

test_that("measure_validate_metadata validates required columns", {
  data <- data.frame(
    sample_id = 1:5,
    sample_type = c("qc", "standard", "blank", "unknown", "reference"),
    run_order = 1:5
  )

  # Should pass when required columns present
  result <- measure_validate_metadata(data, require = c("sample_id", "sample_type"))
  expect_true(result$valid)


  # Should fail when required column is missing
  expect_error(
    measure_validate_metadata(data, require = c("sample_id", "missing_col")),
    "Missing required column"
  )

  # Should warn instead of error with action = "warn"
  expect_warning(
    result <- measure_validate_metadata(
      data,
      require = c("sample_id", "missing_col"),
      action = "warn"
    )
  )
  expect_false(result$valid)
})

test_that("measure_validate_metadata validates sample_type values", {
  data <- data.frame(
    sample_type = c("qc", "standard", "invalid_type"),
    value = 1:3
  )

  expect_error(
    measure_validate_metadata(data, require = "sample_type"),
    "invalid values"
  )

  # Valid sample types should pass
  valid_data <- data.frame(
    sample_type = c("qc", "standard", "blank", "unknown", "reference"),
    value = 1:5
  )
  result <- measure_validate_metadata(valid_data, require = "sample_type")
  expect_true(result$valid)
})

test_that("measure_validate_metadata validates run_order column",
{
  # Non-numeric should fail
  data <- data.frame(
    run_order = c("a", "b", "c")
  )
  expect_error(
    measure_validate_metadata(data, require = "run_order"),
    "must be numeric"
  )

  # Non-integer should fail
  data <- data.frame(
    run_order = c(1.5, 2.5, 3.5)
  )
  expect_error(
    measure_validate_metadata(data, require = "run_order"),
    "non-integer"
  )

  # Negative values should fail
  data <- data.frame(
    run_order = c(-1, 0, 1)
  )
  expect_error(
    measure_validate_metadata(data, require = "run_order"),
    "negative"
  )

  # Valid run_order should pass
  data <- data.frame(
    run_order = 1:5
  )
  result <- measure_validate_metadata(data, require = "run_order")
  expect_true(result$valid)
})

test_that("measure_validate_metadata validates batch_id column", {
  # Non-character/factor should fail
  data <- data.frame(
    batch_id = 1:3
  )
  expect_error(
    measure_validate_metadata(data, require = "batch_id"),
    "must be character or factor"
  )

  # All NA should fail
  data <- data.frame(
    batch_id = c(NA_character_, NA_character_)
  )
  expect_error(
    measure_validate_metadata(data, require = "batch_id"),
    "only NA"
  )

  # Valid batch_id should pass
  data <- data.frame(
    batch_id = c("B001", "B001", "B002")
  )
  result <- measure_validate_metadata(data, require = "batch_id")
  expect_true(result$valid)
})

test_that("measure_validate_metadata validates nominal_conc column", {
  # Non-numeric should fail
  data <- data.frame(
    nominal_conc = c("low", "high")
  )
  expect_error(
    measure_validate_metadata(data, require = "nominal_conc"),
    "must be numeric"
  )

  # Negative values should fail
  data <- data.frame(
    nominal_conc = c(-1, 10, 50)
  )
  expect_error(
    measure_validate_metadata(data, require = "nominal_conc"),
    "negative"
  )

  # Valid nominal_conc (including NA for non-standards) should pass
  data <- data.frame(
    nominal_conc = c(0, 10, 50, NA, NA)
  )
  result <- measure_validate_metadata(data, require = "nominal_conc")
  expect_true(result$valid)
})

test_that("measure_validate_metadata validates dilution_factor column", {
  # Non-numeric should fail
  data <- data.frame(
    dilution_factor = c("1x", "2x")
  )
  expect_error(
    measure_validate_metadata(data, require = "dilution_factor"),
    "must be numeric"
  )

  # Zero or negative should fail
  data <- data.frame(
    dilution_factor = c(0, 1, 2)
  )
  expect_error(
    measure_validate_metadata(data, require = "dilution_factor"),
    "must be positive"
  )

  # Valid dilution_factor should pass
  data <- data.frame(
    dilution_factor = c(1, 2, 10)
  )
  result <- measure_validate_metadata(data, require = "dilution_factor")
  expect_true(result$valid)
})

test_that("measure_validate_metadata checks for duplicate run_order within batch", {
  data <- data.frame(
    batch_id = c("B1", "B1", "B1"),
    run_order = c(1, 1, 2)  # Duplicate run_order in B1
  )
  expect_error(
    measure_validate_metadata(data, require = c("batch_id", "run_order")),
    "duplicates within batch"
  )

  # Same run_order in different batches is OK
  data <- data.frame(
    batch_id = c("B1", "B1", "B2", "B2"),
    run_order = c(1, 2, 1, 2)
  )
  result <- measure_validate_metadata(data, require = c("batch_id", "run_order"))
  expect_true(result$valid)
})

test_that("measure_validate_metadata returns structured results", {
  data <- data.frame(
    sample_type = c("qc", "standard"),
    run_order = 1:2,
    batch_id = "B001"
  )

  result <- measure_validate_metadata(
    data,
    require = c("sample_type", "run_order")
  )

  expect_type(result, "list")
  expect_true(result$valid)
  expect_type(result$checks, "list")
  expect_identical(result$data, data)
})

test_that("measure_validate_metadata requires data frame input", {
  expect_error(
    measure_validate_metadata("not a data frame"),
    "must be a data frame"
  )
})

# ==============================================================================
# measure_standardize_sample_type() tests
# ==============================================================================

test_that("measure_standardize_sample_type uses default mapping", {
  data <- data.frame(
    sample_type = c("QC", "STD", "BLK", "UNK", "REF")
  )

  result <- measure_standardize_sample_type(data)

  expect_equal(
    result$sample_type,
    c("qc", "standard", "blank", "unknown", "reference")
  )
})

test_that("measure_standardize_sample_type uses custom mapping", {
  data <- data.frame(
    sample_type = c("pool", "calibrator", "solvent")
  )

  result <- measure_standardize_sample_type(
    data,
    mapping = list(
      qc = "pool",
      standard = "calibrator",
      blank = "solvent"
    )
  )

  expect_equal(result$sample_type, c("qc", "standard", "blank"))
})

test_that("measure_standardize_sample_type handles unknown values", {
  data <- data.frame(
    sample_type = c("qc", "weird_type")
  )

  # Default: error on unknown

  expect_error(
    measure_standardize_sample_type(data),
    "Unknown sample type"
  )

  # Warn and keep
  expect_warning(
    result <- measure_standardize_sample_type(data, unknown_action = "warn")
  )
  expect_equal(result$sample_type, c("qc", "weird_type"))

  # Silently keep
  result <- measure_standardize_sample_type(data, unknown_action = "keep")
  expect_equal(result$sample_type, c("qc", "weird_type"))

  # Convert to unknown
  result <- measure_standardize_sample_type(data, unknown_action = "unknown")
  expect_equal(result$sample_type, c("qc", "unknown"))
})

test_that("measure_standardize_sample_type handles NA values", {
  data <- data.frame(
    sample_type = c("qc", NA, "standard")
  )

  result <- measure_standardize_sample_type(data)
  expect_equal(result$sample_type, c("qc", NA, "standard"))
})

test_that("measure_standardize_sample_type works with custom column name", {
  data <- data.frame(
    type = c("QC", "STD")
  )

  result <- measure_standardize_sample_type(data, col = "type")
  expect_equal(result$type, c("qc", "standard"))
})

test_that("measure_standardize_sample_type errors on missing column", {
  data <- data.frame(value = 1:3)

  expect_error(
    measure_standardize_sample_type(data, col = "sample_type"),
    "not found"
  )
})

test_that("measure_standardize_sample_type requires data frame", {
  expect_error(
    measure_standardize_sample_type("not a data frame"),
    "must be a data frame"
  )
})
