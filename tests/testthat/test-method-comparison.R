# Tests for method comparison functions

# ==============================================================================
# measure_bland_altman()
# ==============================================================================

test_that("measure_bland_altman calculates basic statistics", {
  set.seed(123)
  data <- data.frame(
    method1 = rnorm(30, mean = 100, sd = 10),
    method2 = rnorm(30, mean = 102, sd = 10)
  )

  ba <- measure_bland_altman(data, method1_col = "method1", method2_col = "method2")

  expect_s3_class(ba, "measure_bland_altman")
  expect_named(ba, c("data", "statistics", "regression", "call"))

  # Check statistics
  stats <- ba$statistics
  expect_equal(stats$n, 30)
  expect_true(is.numeric(stats$mean_bias))
  expect_true(is.numeric(stats$sd_diff))
  expect_true(is.numeric(stats$lower_loa))
  expect_true(is.numeric(stats$upper_loa))
  expect_true(stats$upper_loa > stats$lower_loa)
})

test_that("measure_bland_altman tests for proportional bias", {
  # Create data with proportional bias
  set.seed(42)
  x <- seq(50, 150, length.out = 30)
  y <- x + 0.1 * x + rnorm(30, sd = 3)  # Proportional bias

  data <- data.frame(method1 = x, method2 = y)
  ba <- measure_bland_altman(data, "method1", "method2", regression = "linear")

  expect_false(is.null(ba$regression))
  expect_true(is.numeric(ba$regression$slope))
  expect_true(is.numeric(ba$regression$slope_p_value))
})

test_that("measure_bland_altman handles missing values", {
  data <- data.frame(
    method1 = c(100, 102, NA, 98, 101),
    method2 = c(101, 103, 99, NA, 100)
  )

  expect_warning(
    ba <- measure_bland_altman(data, "method1", "method2"),
    "missing values"
  )

  expect_equal(ba$statistics$n, 3)
})

test_that("measure_bland_altman validates inputs", {
  data <- data.frame(method1 = 1:10, method2 = 2:11)

  expect_error(
    measure_bland_altman(data, "nonexistent", "method2"),
    "not found"
  )

  expect_error(
    measure_bland_altman(data.frame(method1 = 1:2, method2 = 2:3), "method1", "method2"),
    "At least 3"
  )
})

test_that("tidy.measure_bland_altman returns expected format", {
  set.seed(123)
  data <- data.frame(method1 = rnorm(30, 100), method2 = rnorm(30, 100))
  ba <- measure_bland_altman(data, "method1", "method2")

  tidy_ba <- tidy(ba)
  expect_s3_class(tidy_ba, "tbl_df")
  expect_named(tidy_ba, c("statistic", "value"))
  expect_true("mean_bias" %in% tidy_ba$statistic)
})

test_that("glance.measure_bland_altman returns expected format", {
  set.seed(123)
  data <- data.frame(method1 = rnorm(30, 100), method2 = rnorm(30, 100))
  ba <- measure_bland_altman(data, "method1", "method2", regression = "linear")

  glance_ba <- glance(ba)
  expect_s3_class(glance_ba, "tbl_df")
  expect_equal(nrow(glance_ba), 1)
  expect_true("mean_bias" %in% names(glance_ba))
  expect_true("proportional_bias" %in% names(glance_ba))
})

# ==============================================================================
# measure_deming_regression()
# ==============================================================================

test_that("measure_deming_regression calculates coefficients", {
  data <- data.frame(
    reference = c(5, 10, 15, 25, 50, 75, 100),
    new_method = c(5.1, 10.2, 15.3, 25.4, 50.2, 75.5, 100.8)
  )

  result <- measure_deming_regression(data, "reference", "new_method")

  expect_s3_class(result, "measure_deming_regression")
  expect_s3_class(result$coefficients, "tbl_df")
  expect_equal(result$coefficients$term, c("intercept", "slope"))

  # Slope should be close to 1 for this data
  expect_true(result$coefficients$estimate[2] > 0.9)
  expect_true(result$coefficients$estimate[2] < 1.1)
})

test_that("measure_deming_regression accepts error ratio", {
  data <- data.frame(
    reference = c(5, 10, 15, 25, 50),
    new_method = c(5.1, 10.2, 15.3, 25.4, 50.2)
  )

  # With error ratio
  result1 <- measure_deming_regression(data, "reference", "new_method", error_ratio = 1)
  result2 <- measure_deming_regression(data, "reference", "new_method", error_ratio = 2)

  # Results should differ with different error ratios
  expect_false(identical(result1$coefficients$estimate, result2$coefficients$estimate))
})

test_that("measure_deming_regression can use bootstrap CIs", {
  skip_if(requireNamespace("mcr", quietly = TRUE), "mcr available, using mcr instead of bootstrap")

  data <- data.frame(
    reference = c(5, 10, 15, 25, 50, 75, 100),
    new_method = c(5.1, 10.2, 15.3, 25.4, 50.2, 75.5, 100.8)
  )

  result <- measure_deming_regression(
    data, "reference", "new_method",
    bootstrap = TRUE, bootstrap_n = 100
  )

  expect_false(is.null(result$bootstrap))
  expect_false(is.na(result$coefficients$ci_lower[2]))
})

test_that("tidy.measure_deming_regression returns coefficients",
{
  data <- data.frame(reference = 1:10, new_method = 1.1 * (1:10))
  result <- measure_deming_regression(data, "reference", "new_method")

  tidy_result <- tidy(result)
  expect_s3_class(tidy_result, "tbl_df")
  expect_equal(tidy_result$term, c("intercept", "slope"))
})

test_that("glance.measure_deming_regression returns one-row summary", {
  data <- data.frame(reference = 1:10, new_method = 1.1 * (1:10))
  result <- measure_deming_regression(data, "reference", "new_method")

  glance_result <- glance(result)
  expect_equal(nrow(glance_result), 1)
  expect_true("slope" %in% names(glance_result))
  expect_true("slope_ci_includes_1" %in% names(glance_result))
})

# ==============================================================================
# measure_passing_bablok()
# ==============================================================================

test_that("measure_passing_bablok requires mcr package", {
  skip_if(requireNamespace("mcr", quietly = TRUE), "mcr is available")

  data <- data.frame(reference = 1:15, new_method = 1:15 + rnorm(15, sd = 0.5))

  expect_error(
    measure_passing_bablok(data, "reference", "new_method"),
    "mcr"
  )
})

test_that("measure_passing_bablok works with mcr", {
  skip_if_not_installed("mcr")

  set.seed(123)
  data <- data.frame(
    reference = c(5, 10, 15, 25, 50, 75, 100, 125, 150, 175),
    new_method = c(5.2, 10.3, 15.1, 25.5, 50.1, 75.3, 100.2, 125.4, 150.1, 175.5)
  )

  result <- measure_passing_bablok(data, "reference", "new_method")

  expect_s3_class(result, "measure_passing_bablok")
  expect_s3_class(result$coefficients, "tbl_df")
  expect_true(!is.null(result$linearity))
})

# ==============================================================================
# measure_proficiency_score()
# ==============================================================================

test_that("measure_proficiency_score calculates z-scores", {
  data <- data.frame(
    lab = paste0("Lab_", 1:10),
    measured = c(99, 101, 98, 102, 97, 103, 100, 99, 101, 95),
    reference = rep(100, 10)
  )

  result <- measure_proficiency_score(
    data, "measured", "reference",
    score_type = "z_score", sigma = 2
  )

  expect_s3_class(result, "measure_proficiency_score")

  # Check scores are calculated correctly
  expected_z <- (data$measured - data$reference) / 2
  expect_equal(result$scores$score, expected_z)
})

test_that("measure_proficiency_score flags appropriately", {
  data <- data.frame(
    measured = c(100, 105, 110),  # Progressively worse
    reference = rep(100, 3)
  )

  result <- measure_proficiency_score(
    data, "measured", "reference",
    score_type = "z_score", sigma = 2
  )

  # First should be satisfactory, third should be unsatisfactory
  expect_equal(result$scores$flag[1], "satisfactory")
  expect_equal(result$scores$flag[3], "unsatisfactory")
})

test_that("measure_proficiency_score calculates En scores", {
  data <- data.frame(
    measured = c(100, 102, 98),
    reference = c(100, 100, 100),
    uncertainty = c(1.5, 2.0, 1.8)
  )

  result <- measure_proficiency_score(
    data, "measured", "reference",
    uncertainty_col = "uncertainty",
    score_type = "en_score"
  )

  expect_s3_class(result, "measure_proficiency_score")
  expect_equal(result$statistics$score_type, "en_score")
})

test_that("measure_proficiency_score validates inputs", {
  data <- data.frame(measured = 1:5, reference = rep(1, 5))

  # En score requires uncertainty
  expect_error(
    measure_proficiency_score(data, "measured", "reference", score_type = "en_score"),
    "uncertainty_col"
  )
})

test_that("tidy.measure_proficiency_score returns scores or summary", {
  data <- data.frame(measured = c(99, 101, 98), reference = rep(100, 3))
  result <- measure_proficiency_score(data, "measured", "reference", sigma = 2)

  # Default returns scores
  tidy_scores <- tidy(result)
  expect_s3_class(tidy_scores, "tbl_df")
  expect_true("score" %in% names(tidy_scores))

  # With type = "summary"
  tidy_summary <- tidy(result, type = "summary")
  expect_true("pct_satisfactory" %in% tidy_summary$statistic)
})

test_that("glance.measure_proficiency_score returns one-row summary", {
  data <- data.frame(measured = c(99, 101, 98), reference = rep(100, 3))
  result <- measure_proficiency_score(data, "measured", "reference", sigma = 2)

  glance_result <- glance(result)
  expect_equal(nrow(glance_result), 1)
  expect_true("pct_satisfactory" %in% names(glance_result))
  expect_true("all_satisfactory" %in% names(glance_result))
})

# ==============================================================================
# Print methods
# ==============================================================================

test_that("print methods work without error", {
  set.seed(123)
  data <- data.frame(method1 = rnorm(20, 100), method2 = rnorm(20, 100))
  ba <- measure_bland_altman(data, "method1", "method2", regression = "linear")

  expect_output(print(ba), "measure_bland_altman")
  expect_output(print(ba), "Bias Statistics")
  expect_output(print(ba), "Limits of Agreement")
})

test_that("autoplot methods return ggplot objects", {
  skip_if_not_installed("ggplot2")

  set.seed(123)
  data <- data.frame(method1 = rnorm(20, 100), method2 = rnorm(20, 100))
  ba <- measure_bland_altman(data, "method1", "method2")

  p <- autoplot(ba)
  expect_s3_class(p, "ggplot")
})

# ==============================================================================
# Edge case tests (added for PR review fixes)
# ==============================================================================

test_that("measure_proficiency_score errors when IQR is zero (identical differences)", {
  # All differences are identical, so IQR = 0
  data <- data.frame(
    measured = c(100, 101, 102),
    reference = c(100, 101, 102)  # Differences are all 0
  )

  expect_error(
    measure_proficiency_score(
      data,
      measured_col = "measured",
      reference_col = "reference",
      score_type = "z_score"
      # sigma not provided, will try to estimate from IQR
    ),
    "IQR = 0"
  )
})

test_that("measure_proficiency_score errors on zero sigma", {
  data <- data.frame(
    measured = c(100, 105, 110),
    reference = c(100, 100, 100)
  )

  expect_error(
    measure_proficiency_score(
      data,
      measured_col = "measured",
      reference_col = "reference",
      score_type = "z_score",
      sigma = 0  # Zero sigma should error
    ),
    "positive"
  )
})

test_that("measure_proficiency_score errors on zero combined uncertainty", {
  data <- data.frame(
    measured = c(100, 105, 110),
    reference = c(100, 100, 100),
    u_measured = c(0, 0, 0),  # All zeros
    u_reference = c(0, 0, 0)
  )

  expect_error(
    measure_proficiency_score(
      data,
      measured_col = "measured",
      reference_col = "reference",
      score_type = "en_score",
      uncertainty_col = "u_measured",
      reference_uncertainty_col = "u_reference"
    ),
    "zero"
  )
})
