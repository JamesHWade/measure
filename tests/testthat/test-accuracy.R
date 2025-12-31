# ==============================================================================
# Tests for accuracy.R
# ==============================================================================

# ==============================================================================
# measure_accuracy() tests
# ==============================================================================

test_that("measure_accuracy calculates basic accuracy metrics", {
  set.seed(123)
  data <- data.frame(
    measured = c(101, 99, 100, 102, 98),
    reference = rep(100, 5)
  )

  result <- measure_accuracy(data, "measured", "reference")

  expect_s3_class(result, "measure_accuracy")
  expect_equal(nrow(result), 1)
  expect_equal(result$n, 5)
  expect_true("bias" %in% names(result))
  expect_true("mean_recovery" %in% names(result))
})

test_that("measure_accuracy calculates correct values", {
  # Known values: measured = reference, so recovery should be 100%
  data <- data.frame(
    measured = c(100, 100, 100),
    reference = c(100, 100, 100)
  )

  result <- measure_accuracy(data, "measured", "reference")

  expect_equal(result$mean_recovery, 100)
  expect_equal(result$bias, 0)
  expect_equal(result$bias_pct, 0)
})

test_that("measure_accuracy handles grouping", {
  set.seed(123)
  data <- data.frame(
    level = rep(c("low", "mid", "high"), each = 5),
    measured = c(
      rnorm(5, 10.2, 0.3),
      rnorm(5, 50, 1.5),
      rnorm(5, 100, 3)
    ),
    reference = rep(c(10, 50, 100), each = 5)
  )

  result <- measure_accuracy(data, "measured", "reference", group_col = "level")

  expect_equal(nrow(result), 3)
  expect_true(all(c("low", "mid", "high") %in% result$group))
})

test_that("measure_accuracy validates inputs", {
  data <- data.frame(x = 1:5)

  expect_error(
    measure_accuracy(data, "missing", "x"),
    "not found"
  )

  expect_error(
    measure_accuracy(data, "x", "missing"),
    "not found"
  )
})

test_that("measure_accuracy calculates confidence intervals", {
  set.seed(123)
  data <- data.frame(
    measured = rnorm(10, 100, 2),
    reference = rep(100, 10)
  )

  result <- measure_accuracy(data, "measured", "reference")

  expect_true("recovery_ci_lower" %in% names(result))
  expect_true("recovery_ci_upper" %in% names(result))
  expect_true(result$recovery_ci_lower < result$mean_recovery)
  expect_true(result$recovery_ci_upper > result$mean_recovery)
})

test_that("measure_accuracy print method works", {
  data <- data.frame(
    measured = c(101, 99, 100, 102, 98),
    reference = rep(100, 5)
  )
  result <- measure_accuracy(data, "measured", "reference")

  expect_output(print(result), "measure_accuracy")
  expect_output(print(result), "Bias")
  expect_output(print(result), "Recovery")
})

# ==============================================================================
# measure_linearity() tests
# ==============================================================================

test_that("measure_linearity calculates regression statistics", {
  set.seed(123)
  data <- data.frame(
    concentration = rep(c(10, 25, 50, 75, 100), each = 3),
    response = rep(c(10, 25, 50, 75, 100), each = 3) * 1.5 + rnorm(15, 0, 2)
  )

  result <- measure_linearity(data, "concentration", "response")

  expect_s3_class(result, "measure_linearity")
  expect_true("r_squared" %in% names(result))
  expect_true("slope" %in% names(result))
  expect_true("intercept" %in% names(result))
})

test_that("measure_linearity reports high R-squared for linear data", {
  # Perfect linear relationship with slight noise
  set.seed(123)
  data <- data.frame(
    concentration = 1:100,
    response = 1:100 * 2 + 5 + rnorm(100, 0, 0.5)
  )

  result <- measure_linearity(data, "concentration", "response")

  expect_true(result$r_squared > 0.99)
  expect_equal(unname(result$slope), 2, tolerance = 0.1)
  expect_equal(unname(result$intercept), 5, tolerance = 1)
})

test_that("measure_linearity calculates confidence intervals", {
  set.seed(123)
  data <- data.frame(
    concentration = rep(c(10, 50, 100), each = 5),
    response = rep(c(10, 50, 100), each = 5) + rnorm(15, 0, 2)
  )

  result <- measure_linearity(data, "concentration", "response")

  expect_true("slope_ci_lower" %in% names(result))
  expect_true("slope_ci_upper" %in% names(result))
  expect_true(result$slope_ci_lower < result$slope)
  expect_true(result$slope_ci_upper > result$slope)
})

test_that("measure_linearity performs lack-of-fit test with replicates", {
  set.seed(123)
  data <- data.frame(
    concentration = rep(c(10, 25, 50, 75, 100), each = 3),
    response = rep(c(10, 25, 50, 75, 100), each = 3) * 1.5 + rnorm(15, 0, 2)
  )

  result <- measure_linearity(data, "concentration", "response")

  expect_true(!is.null(result$lack_of_fit))
  expect_true("f_statistic" %in% names(result$lack_of_fit))
  expect_true("p_value" %in% names(result$lack_of_fit))
})

test_that("measure_linearity validates inputs", {
  data <- data.frame(x = 1:5)

  expect_error(
    measure_linearity(data, "missing", "x"),
    "not found"
  )

  expect_error(
    measure_linearity(data, "x", "missing"),
    "not found"
  )
})

test_that("measure_linearity requires at least 3 points", {
  data <- data.frame(
    concentration = c(1, 2),
    response = c(1, 2)
  )

  expect_error(
    measure_linearity(data, "concentration", "response"),
    "At least 3"
  )
})

test_that("measure_linearity print method works", {
  set.seed(123)
  data <- data.frame(
    concentration = rep(c(10, 50, 100), each = 3),
    response = rep(c(10, 50, 100), each = 3) + rnorm(9, 0, 1)
  )
  result <- measure_linearity(data, "concentration", "response")

  expect_output(print(result), "measure_linearity")
  expect_output(print(result), "R-squared")
  expect_output(print(result), "Slope")
})

test_that("measure_linearity stores model object", {
  set.seed(42)
  data <- data.frame(
    concentration = 1:10,
    response = 1:10 * 2 + 5 + rnorm(10, 0, 0.5) # Add noise to avoid perfect fit warning
  )

  result <- measure_linearity(data, "concentration", "response")

  expect_s3_class(result$model, "lm")
})

# ==============================================================================
# measure_carryover() tests
# ==============================================================================

test_that("measure_carryover calculates carryover", {
  data <- data.frame(
    run_order = 1:10,
    sample_type = c(
      "std",
      "std",
      "std",
      "high",
      "blank",
      "qc",
      "qc",
      "high",
      "blank",
      "std"
    ),
    response = c(100, 500, 1000, 5000, 5, 500, 510, 4900, 8, 100)
  )

  result <- measure_carryover(
    data,
    response_col = "response",
    sample_type_col = "sample_type",
    run_order_col = "run_order",
    lloq = 50
  )

  expect_s3_class(result, "measure_carryover")
  expect_equal(result$n_pairs, 2)
  expect_true("carryover_pct" %in% names(result))
  expect_true("pass" %in% names(result))
})

test_that("measure_carryover calculates percentage correctly", {
  data <- data.frame(
    run_order = 1:4,
    sample_type = c("std", "high", "blank", "std"),
    response = c(100, 1000, 10, 100) # Blank = 10
  )

  # With LLOQ = 100, carryover = 10/100 * 100 = 10%
  result <- measure_carryover(
    data,
    response_col = "response",
    sample_type_col = "sample_type",
    run_order_col = "run_order",
    lloq = 100
  )

  expect_equal(result$carryover_pct, 10)
})

test_that("measure_carryover pass/fail based on threshold", {
  data <- data.frame(
    run_order = 1:4,
    sample_type = c("std", "high", "blank", "std"),
    response = c(100, 1000, 10, 100)
  )

  # Carryover = 10%, threshold = 20% -> PASS
  result_pass <- measure_carryover(
    data,
    response_col = "response",
    sample_type_col = "sample_type",
    run_order_col = "run_order",
    lloq = 100,
    threshold = 20
  )
  expect_true(result_pass$pass)

  # Carryover = 10%, threshold = 5% -> FAIL
  result_fail <- measure_carryover(
    data,
    response_col = "response",
    sample_type_col = "sample_type",
    run_order_col = "run_order",
    lloq = 100,
    threshold = 5
  )
  expect_false(result_fail$pass)
})

test_that("measure_carryover uses high response when LLOQ not provided", {
  data <- data.frame(
    run_order = 1:4,
    sample_type = c("std", "high", "blank", "std"),
    response = c(100, 1000, 10, 100)
  )

  result <- measure_carryover(
    data,
    response_col = "response",
    sample_type_col = "sample_type",
    run_order_col = "run_order"
  )

  expect_equal(result$reference_type, "high")
  expect_equal(result$carryover_pct, 1) # 10/1000 * 100 = 1%
})

test_that("measure_carryover validates inputs", {
  data <- data.frame(x = 1:5)

  expect_error(
    measure_carryover(data, "missing", "x", "x"),
    "not found"
  )
})

test_that("measure_carryover errors when no blanks after highs", {
  data <- data.frame(
    run_order = 1:5,
    sample_type = c("std", "high", "qc", "blank", "std"), # Blank not after high
    response = 1:5
  )

  expect_error(
    measure_carryover(
      data,
      response_col = "response",
      sample_type_col = "sample_type",
      run_order_col = "run_order"
    ),
    "No blank samples"
  )
})

test_that("measure_carryover print method works", {
  data <- data.frame(
    run_order = 1:4,
    sample_type = c("std", "high", "blank", "std"),
    response = c(100, 1000, 10, 100)
  )

  result <- measure_carryover(
    data,
    response_col = "response",
    sample_type_col = "sample_type",
    run_order_col = "run_order",
    lloq = 100
  )

  expect_output(print(result), "measure_carryover")
  expect_output(print(result), "Carryover")
  expect_output(print(result), "PASS")
})

# ==============================================================================
# tidy methods tests
# ==============================================================================

test_that("tidy.measure_accuracy returns tibble", {
  data <- data.frame(
    measured = c(101, 99, 100),
    reference = rep(100, 3)
  )
  result <- measure_accuracy(data, "measured", "reference")

  tidy_result <- tidy(result)
  expect_s3_class(tidy_result, "tbl_df")
})

test_that("tidy.measure_linearity returns tibble", {
  set.seed(42)
  data <- data.frame(
    concentration = 1:10,
    response = 1:10 * 2 + 5 + rnorm(10, 0, 0.5) # Add noise to avoid perfect fit warning
  )
  result <- measure_linearity(data, "concentration", "response")

  tidy_result <- tidy(result)
  expect_s3_class(tidy_result, "tbl_df")
  expect_equal(nrow(tidy_result), 1)
  expect_true("r_squared" %in% names(tidy_result))
})

test_that("tidy.measure_carryover returns tibble", {
  data <- data.frame(
    run_order = 1:4,
    sample_type = c("std", "high", "blank", "std"),
    response = c(100, 1000, 10, 100)
  )

  result <- measure_carryover(
    data,
    response_col = "response",
    sample_type_col = "sample_type",
    run_order_col = "run_order"
  )

  tidy_result <- tidy(result)
  expect_s3_class(tidy_result, "tbl_df")
  expect_true("pass" %in% names(tidy_result))
})

# ==============================================================================
# autoplot tests
# ==============================================================================

test_that("autoplot.measure_linearity creates ggplot", {
  skip_if_not_installed("ggplot2")

  data <- data.frame(
    concentration = 1:10,
    response = 1:10 * 2 + 5 + rnorm(10, 0, 0.5)
  )
  result <- measure_linearity(data, "concentration", "response")

  # Fit plot
  p_fit <- autoplot(result, type = "fit")
  expect_s3_class(p_fit, "ggplot")

  # Residual plot
  p_resid <- autoplot(result, type = "residuals")
  expect_s3_class(p_resid, "ggplot")
})

# ==============================================================================
# Integration tests
# ==============================================================================

test_that("accuracy workflow with criteria integration", {
  set.seed(42)
  data <- data.frame(
    level = rep(c("low", "mid", "high"), each = 5),
    measured = c(
      rnorm(5, 10, 0.3),
      rnorm(5, 50, 1.5),
      rnorm(5, 100, 3)
    ),
    reference = rep(c(10, 50, 100), each = 5)
  )

  result <- measure_accuracy(data, "measured", "reference", group_col = "level")

  # All recoveries should be close to 100%
  expect_true(all(result$mean_recovery > 90 & result$mean_recovery < 110))
})
