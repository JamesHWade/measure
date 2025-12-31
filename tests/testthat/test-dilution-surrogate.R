# Tests for dilution correction and surrogate recovery steps

# ==============================================================================
# step_measure_dilution_correct()
# ==============================================================================

test_that("step_measure_dilution_correct multiplies correctly", {
  data <- data.frame(
    sample_id = paste0("S", 1:5),
    dilution_factor = c(1, 2, 5, 10, 1),
    analyte = c(50, 45, 42, 48, 51)
  )

  rec <- recipes::recipe(~., data = data) |>
    recipes::update_role(sample_id, new_role = "id") |>
    step_measure_dilution_correct(
      analyte,
      dilution_col = "dilution_factor",
      operation = "multiply"
    ) |>
    recipes::prep()

  result <- recipes::bake(rec, new_data = NULL)

  # Check back-calculated concentrations
  expect_equal(result$analyte[1], 50 * 1)
  expect_equal(result$analyte[2], 45 * 2)
  expect_equal(result$analyte[3], 42 * 5)
  expect_equal(result$analyte[4], 48 * 10)
})

test_that("step_measure_dilution_correct divides correctly", {
  data <- data.frame(
    dilution_factor = c(2, 4),
    concentration = c(100, 200)
  )

  rec <- recipes::recipe(~., data = data) |>
    step_measure_dilution_correct(
      concentration,
      dilution_col = "dilution_factor",
      operation = "divide"
    ) |>
    recipes::prep()

  result <- recipes::bake(rec, new_data = NULL)

  expect_equal(result$concentration, c(50, 50))
})

test_that("step_measure_dilution_correct handles zero with error", {
  data <- data.frame(
    dilution_factor = c(1, 0, 2),
    analyte = c(100, 100, 100)
  )

  # Error occurs during prep (bake is called on training data)
  expect_error(
    recipes::recipe(~., data = data) |>
      step_measure_dilution_correct(
        analyte,
        dilution_col = "dilution_factor",
        handle_zero = "error"
      ) |>
      recipes::prep(),
    "Zero dilution"
  )
})

test_that("step_measure_dilution_correct handles zero with warn", {
  data <- data.frame(
    dilution_factor = c(1, 0, 2),
    analyte = c(100, 100, 100)
  )

  # Warning occurs during prep (bake is called on training data)
  expect_warning(
    rec <- recipes::recipe(~., data = data) |>
      step_measure_dilution_correct(
        analyte,
        dilution_col = "dilution_factor",
        handle_zero = "warn"
      ) |>
      recipes::prep(),
    "Zero dilution"
  )

  result <- recipes::bake(rec, new_data = NULL)

  # Row with zero should have multiply result of 0
  expect_equal(result$analyte, c(100, 0, 200))
})

test_that("step_measure_dilution_correct handles NA factors", {
  data <- data.frame(
    dilution_factor = c(1, NA, 2),
    analyte = c(100, 100, 100)
  )

  # Warning occurs during prep (bake is called on training data)
  expect_warning(
    rec <- recipes::recipe(~., data = data) |>
      step_measure_dilution_correct(
        analyte,
        dilution_col = "dilution_factor"
      ) |>
      recipes::prep(),
    "NA dilution"
  )

  result <- recipes::bake(rec, new_data = NULL)

  expect_true(is.na(result$analyte[2]))
})

test_that("step_measure_dilution_correct validates dilution column", {
  data <- data.frame(
    wrong_name = c(1, 2),
    analyte = c(100, 200)
  )

  expect_error(
    recipes::recipe(~., data = data) |>
      step_measure_dilution_correct(
        analyte,
        dilution_col = "dilution_factor"
      ) |>
      recipes::prep(),
    "not found"
  )
})

test_that("tidy.step_measure_dilution_correct returns expected format", {
  data <- data.frame(
    dilution_factor = c(1, 2),
    analyte1 = c(100, 100),
    analyte2 = c(50, 50)
  )

  rec <- recipes::recipe(~., data = data) |>
    step_measure_dilution_correct(
      analyte1,
      analyte2,
      dilution_col = "dilution_factor"
    ) |>
    recipes::prep()

  tidy_dc <- tidy(rec, number = 1)

  expect_s3_class(tidy_dc, "tbl_df")
  expect_equal(nrow(tidy_dc), 2) # 2 analytes
  expect_true("feature" %in% names(tidy_dc))
  expect_true("operation" %in% names(tidy_dc))
})

# ==============================================================================
# step_measure_surrogate_recovery()
# ==============================================================================

test_that("step_measure_surrogate_recovery calculates recovery", {
  data <- data.frame(
    sample_id = paste0("QC", 1:5),
    surrogate = c(95, 105, 90, 110, 100) # Expected = 100
  )

  rec <- recipes::recipe(~., data = data) |>
    recipes::update_role(sample_id, new_role = "id") |>
    step_measure_surrogate_recovery(
      surrogate,
      expected_value = 100
    ) |>
    recipes::prep()

  result <- recipes::bake(rec, new_data = NULL)

  expect_true("surrogate_recovery" %in% names(result))
  expect_equal(result$surrogate_recovery, c(95, 105, 90, 110, 100))
})

test_that("step_measure_surrogate_recovery uses expected_col", {
  data <- data.frame(
    surrogate = c(45, 95, 140),
    expected = c(50, 100, 150)
  )

  rec <- recipes::recipe(~., data = data) |>
    step_measure_surrogate_recovery(
      surrogate,
      expected_col = "expected"
    ) |>
    recipes::prep()

  result <- recipes::bake(rec, new_data = NULL)

  expect_equal(result$surrogate_recovery, c(90, 95, 140 / 1.5))
})

test_that("step_measure_surrogate_recovery adds flag", {
  data <- data.frame(
    surrogate = c(75, 100, 135) # first and last outside 70-130
  )

  rec <- recipes::recipe(~., data = data) |>
    step_measure_surrogate_recovery(
      surrogate,
      expected_value = 100,
      action = "flag",
      min_recovery = 80,
      max_recovery = 120
    ) |>
    recipes::prep()

  result <- recipes::bake(rec, new_data = NULL)

  expect_true(".surrogate_pass" %in% names(result))
  expect_equal(result$.surrogate_pass, c(FALSE, TRUE, FALSE))
})

test_that("step_measure_surrogate_recovery filters rows", {
  data <- data.frame(
    surrogate = c(75, 100, 135, 110)
  )

  # Message occurs during prep (bake is called on training data)
  expect_message(
    rec <- recipes::recipe(~., data = data) |>
      step_measure_surrogate_recovery(
        surrogate,
        expected_value = 100,
        action = "filter",
        min_recovery = 80,
        max_recovery = 120
      ) |>
      recipes::prep(),
    "Removed 2 row"
  )

  result <- recipes::bake(rec, new_data = NULL)

  expect_equal(nrow(result), 2)
})

test_that("step_measure_surrogate_recovery validates expected inputs", {
  data <- data.frame(surrogate = c(95, 100, 105))

  # Neither expected_col nor expected_value
  expect_error(
    recipes::recipe(~., data = data) |>
      step_measure_surrogate_recovery(surrogate) |>
      recipes::prep(),
    "expected_col.*expected_value"
  )

  # Both provided
  expect_error(
    recipes::recipe(~., data = data) |>
      step_measure_surrogate_recovery(
        surrogate,
        expected_col = "expected",
        expected_value = 100
      ),
    "Only one of"
  )
})

test_that("step_measure_surrogate_recovery validates recovery limits", {
  data <- data.frame(surrogate = c(95, 100))

  expect_error(
    recipes::recipe(~., data = data) |>
      step_measure_surrogate_recovery(
        surrogate,
        expected_value = 100,
        min_recovery = 120,
        max_recovery = 80
      ),
    "must be less than"
  )
})

test_that("step_measure_surrogate_recovery handles zero expected", {
  data <- data.frame(
    surrogate = c(100, 100),
    expected = c(100, 0) # Second is zero
  )

  # Error occurs during prep (bake is called on training data)
  expect_error(
    recipes::recipe(~., data = data) |>
      step_measure_surrogate_recovery(
        surrogate,
        expected_col = "expected"
      ) |>
      recipes::prep(),
    "cannot be zero"
  )
})

test_that("step_measure_surrogate_recovery handles multiple surrogates", {
  data <- data.frame(
    surrogate_1 = c(95, 100, 105),
    surrogate_2 = c(48, 52, 50)
  )

  # This is a simplified case - in practice you'd need different expected values
  rec <- recipes::recipe(~., data = data) |>
    step_measure_surrogate_recovery(
      surrogate_1,
      expected_value = 100
    ) |>
    recipes::prep()

  result <- recipes::bake(rec, new_data = NULL)

  expect_true("surrogate_1_recovery" %in% names(result))
  expect_true("surrogate_2" %in% names(result)) # Unchanged
})

test_that("tidy.step_measure_surrogate_recovery returns expected format", {
  data <- data.frame(
    surrogate_1 = c(95, 100),
    surrogate_2 = c(48, 52)
  )

  rec <- recipes::recipe(~., data = data) |>
    step_measure_surrogate_recovery(
      surrogate_1,
      surrogate_2,
      expected_value = 100,
      min_recovery = 70,
      max_recovery = 130
    ) |>
    recipes::prep()

  tidy_sr <- tidy(rec, number = 1)

  expect_s3_class(tidy_sr, "tbl_df")
  expect_equal(nrow(tidy_sr), 2) # 2 surrogates
  expect_true("surrogate" %in% names(tidy_sr))
  expect_true("min_recovery" %in% names(tidy_sr))
  expect_true("max_recovery" %in% names(tidy_sr))
})

test_that("print.step_measure_surrogate_recovery works", {
  data <- data.frame(surrogate = c(95, 100, 105))

  rec <- recipes::recipe(~., data = data) |>
    step_measure_surrogate_recovery(
      surrogate,
      expected_value = 100
    ) |>
    recipes::prep()

  expect_output(print(rec), "Surrogate recovery")
})

# ==============================================================================
# Criteria presets for P2 functions
# ==============================================================================

test_that("criteria_bland_altman creates valid criteria", {
  crit <- criteria_bland_altman(loa_width = 20, bias_max = 5)

  expect_s3_class(crit, "measure_criteria")
  expect_true(length(crit) >= 2)
})

test_that("criteria_method_comparison creates valid criteria", {
  crit <- criteria_method_comparison(
    slope_range = c(0.9, 1.1),
    r_squared = 0.95
  )

  expect_s3_class(crit, "measure_criteria")
  expect_true("slope" %in% names(crit))
  expect_true("r_squared" %in% names(crit))
})

test_that("criteria_proficiency_testing creates valid criteria", {
  crit <- criteria_proficiency_testing(max_z_score = 2, pct_satisfactory = 95)

  expect_s3_class(crit, "measure_criteria")
  expect_equal(crit$max_abs_score$threshold, 2)
})

test_that("criteria_matrix_effects creates valid criteria", {
  crit <- criteria_matrix_effects(me_range = c(80, 120), me_cv = 15)

  expect_s3_class(crit, "measure_criteria")
  expect_true("mean_me_pct" %in% names(crit))
  expect_true("cv_me_pct" %in% names(crit))
})

test_that("criteria_surrogate_recovery creates valid criteria", {
  crit <- criteria_surrogate_recovery(surrogate_recovery = c(70, 130))

  expect_s3_class(crit, "measure_criteria")
  expect_true("min_recovery" %in% names(crit))
  expect_true("max_recovery" %in% names(crit))
})

# ==============================================================================
# Edge case tests (added for PR review fixes)
# ==============================================================================

test_that("step_measure_dilution_correct handle_zero='skip' continues silently", {
  data <- data.frame(
    dilution_factor = c(1, 0, 2),
    analyte = c(100, 100, 100)
  )

  # No warning or error expected with skip mode
  rec <- recipes::recipe(~., data = data) |>
    step_measure_dilution_correct(
      analyte,
      dilution_col = "dilution_factor",
      handle_zero = "skip"
    ) |>
    recipes::prep()

  result <- recipes::bake(rec, new_data = NULL)

  expect_equal(result$analyte[1], 100)
  expect_equal(result$analyte[2], 0) # Zero * 100 = 0 (multiply mode default)
  expect_equal(result$analyte[3], 200)
})

test_that("step_measure_dilution_correct errors on negative factors", {
  data <- data.frame(
    dilution_factor = c(1, -2, 2),
    analyte = c(100, 100, 100)
  )

  expect_error(
    recipes::recipe(~., data = data) |>
      step_measure_dilution_correct(
        analyte,
        dilution_col = "dilution_factor"
      ) |>
      recipes::prep(),
    "Negative"
  )
})

test_that("step_measure_dilution_correct divide with zero handled", {
  data <- data.frame(
    dilution_factor = c(2, 0, 4),
    analyte = c(100, 100, 100)
  )

  expect_warning(
    rec <- recipes::recipe(~., data = data) |>
      step_measure_dilution_correct(
        analyte,
        dilution_col = "dilution_factor",
        operation = "divide",
        handle_zero = "warn"
      ) |>
      recipes::prep(),
    "Zero dilution"
  )

  result <- recipes::bake(rec, new_data = NULL)

  expect_equal(result$analyte[1], 50) # 100 / 2
  expect_true(is.na(result$analyte[2])) # 100 / 0 = NA (warned)
  expect_equal(result$analyte[3], 25) # 100 / 4
})
