# ==============================================================================
# Tests for precision.R
# ==============================================================================

# ==============================================================================
# measure_repeatability() tests
# ==============================================================================

test_that("measure_repeatability calculates basic statistics", {
  set.seed(42)
  data <- data.frame(
    sample_id = rep("QC1", 10),
    concentration = rnorm(10, mean = 100, sd = 2)
  )

  result <- measure_repeatability(data, "concentration")

  expect_s3_class(result, "measure_precision")
  expect_equal(nrow(result), 1)
  expect_equal(result$n, 10)
  expect_true(result$mean > 95 && result$mean < 105)
  expect_true(result$sd > 0)
  expect_true(result$cv > 0)
  expect_true(result$ci_lower < result$mean)
  expect_true(result$ci_upper > result$mean)
})

test_that("measure_repeatability works with grouping", {
  set.seed(42)
  data <- data.frame(
    level = rep(c("low", "mid", "high"), each = 6),
    concentration = c(
      rnorm(6, 10, 0.5),
      rnorm(6, 50, 2),
      rnorm(6, 100, 4)
    )
  )

  result <- measure_repeatability(data, "concentration", group_col = "level")

  expect_equal(nrow(result), 3)
  expect_equal(result$n, c(6, 6, 6))
  expect_true(all(c("low", "mid", "high") %in% result$group))
})

test_that("measure_repeatability validates input columns", {
  data <- data.frame(x = 1:10)

  expect_error(
    measure_repeatability(data, "missing_col"),
    "not found"
  )

  expect_error(
    measure_repeatability(data, "x", group_col = "missing_group"),
    "not found"
  )
})

test_that("measure_repeatability requires at least 2 observations", {
  data <- data.frame(concentration = 100)

  expect_error(
    measure_repeatability(data, "concentration"),
    "At least 2"
  )
})

test_that("measure_repeatability CV is calculated correctly", {
  # Known values
  data <- data.frame(
    concentration = c(100, 102, 98, 101, 99)
  )

  result <- measure_repeatability(data, "concentration")

  expected_mean <- mean(data$concentration)
  expected_sd <- sd(data$concentration)
  expected_cv <- 100 * expected_sd / expected_mean

  expect_equal(result$mean, expected_mean)
  expect_equal(result$sd, expected_sd)
  expect_equal(result$cv, expected_cv)
})

test_that("measure_repeatability print method works", {
  set.seed(42)
  data <- data.frame(concentration = rnorm(10, 100, 2))
  result <- measure_repeatability(data, "concentration")

  expect_output(print(result), "measure_precision")
  expect_output(print(result), "repeatability")
  expect_output(print(result), "CV")
})

# ==============================================================================
# measure_intermediate_precision() tests
# ==============================================================================

test_that("measure_intermediate_precision calculates variance components", {
  set.seed(123)
  data <- data.frame(
    day = rep(1:5, each = 6),
    concentration = rnorm(30, mean = 100, sd = 3) +
      rep(rnorm(5, 0, 2), each = 6)
  )

  result <- measure_intermediate_precision(data, "concentration", factors = "day")

  expect_s3_class(result, "measure_precision")
  expect_true("component" %in% names(result))
  expect_true("variance" %in% names(result))
  expect_true("percent_variance" %in% names(result))
  expect_true(sum(result$percent_variance) > 99.9)  # Should sum to ~100%
})

test_that("measure_intermediate_precision validates inputs", {
  data <- data.frame(day = 1:10, conc = rnorm(10))

  expect_error(
    measure_intermediate_precision(data, "missing", factors = "day"),
    "not found"
  )

  expect_error(
    measure_intermediate_precision(data, "conc", factors = "missing_factor"),
    "not found"
  )
})

test_that("measure_intermediate_precision handles multiple factors", {
  set.seed(123)
  data <- expand.grid(
    day = 1:3,
    analyst = c("A", "B"),
    rep = 1:4
  )
  data$concentration <- rnorm(nrow(data), 100, 2) +
    data$day * 0.5 +
    ifelse(data$analyst == "A", 1, -1)

  result <- measure_intermediate_precision(
    data, "concentration",
    factors = c("day", "analyst")
  )

  expect_true("day" %in% result$component)
  expect_true("analyst" %in% result$component)
  expect_true("Residual" %in% result$component)
})

test_that("measure_intermediate_precision print method works", {
  set.seed(123)
  data <- data.frame(
    day = rep(1:3, each = 5),
    concentration = rnorm(15, 100, 2)
  )
  result <- measure_intermediate_precision(data, "concentration", factors = "day")

  expect_output(print(result), "measure_precision")
  expect_output(print(result), "intermediate")
  expect_output(print(result), "Variance Components")
})

# ==============================================================================
# measure_reproducibility() tests
# ==============================================================================

test_that("measure_reproducibility calculates lab variance components", {
  set.seed(123)
  data <- data.frame(
    lab_id = rep(c("Lab_A", "Lab_B", "Lab_C"), each = 10),
    concentration = rnorm(30, mean = 100, sd = 2) +
      rep(c(0, 3, -2), each = 10)
  )

  result <- measure_reproducibility(data, "concentration", lab_col = "lab_id")

  expect_s3_class(result, "measure_precision")
  expect_true("reproducibility_sd" %in% names(result))
  expect_true("reproducibility_cv" %in% names(result))
  expect_true(result$reproducibility_sd[1] > 0)
})

test_that("measure_reproducibility validates lab column", {
  data <- data.frame(conc = rnorm(10))

  expect_error(
    measure_reproducibility(data, "conc", lab_col = "missing_lab"),
    "not found"
  )
})

# ==============================================================================
# measure_gage_rr() tests
# ==============================================================================

test_that("measure_gage_rr calculates variance components", {
  set.seed(123)
  data <- expand.grid(
    part = 1:10,
    operator = c("A", "B", "C"),
    replicate = 1:2
  )
  data$measurement <- 50 +
    (data$part - 5) * 2 +
    ifelse(data$operator == "A", 0.5,
           ifelse(data$operator == "B", -0.3, 0)) +
    rnorm(nrow(data), 0, 0.5)

  result <- measure_gage_rr(
    data,
    response_col = "measurement",
    part_col = "part",
    operator_col = "operator"
  )

  expect_s3_class(result, "measure_gage_rr")
  expect_equal(nrow(result), 4)
  expect_true(all(c("Repeatability", "Reproducibility", "Total R&R", "Part-to-Part") %in% result$source))
})

test_that("measure_gage_rr calculates percentages correctly", {
  set.seed(123)
  data <- expand.grid(
    part = 1:5,
    operator = c("A", "B"),
    replicate = 1:3
  )
  data$measurement <- rnorm(nrow(data), 100, 2)

  result <- measure_gage_rr(
    data,
    response_col = "measurement",
    part_col = "part",
    operator_col = "operator"
  )

  # Percentages should sum appropriately
  expect_true(all(result$pct_contribution >= 0))
  expect_true(all(result$pct_study_var >= 0))
})

test_that("measure_gage_rr calculates tolerance percentage when provided", {
  set.seed(123)
  data <- expand.grid(
    part = 1:5,
    operator = c("A", "B"),
    replicate = 1:2
  )
  data$measurement <- 50 + (data$part - 3) * 5 + rnorm(nrow(data), 0, 1)

  result <- measure_gage_rr(
    data,
    response_col = "measurement",
    part_col = "part",
    operator_col = "operator",
    tolerance = 20
  )

  expect_false(anyNA(result$pct_tolerance))
  expect_true(all(result$pct_tolerance >= 0))
})

test_that("measure_gage_rr stores metadata", {
  set.seed(123)
  data <- expand.grid(
    part = 1:10,
    operator = c("A", "B", "C"),
    replicate = 1:2
  )
  data$measurement <- rnorm(nrow(data))

  result <- measure_gage_rr(
    data,
    response_col = "measurement",
    part_col = "part",
    operator_col = "operator"
  )

  expect_equal(attr(result, "n_parts"), 10)
  expect_equal(attr(result, "n_operators"), 3)
  expect_equal(attr(result, "n_replicates"), 2)
  expect_true(!is.null(attr(result, "ndc")))
})

test_that("measure_gage_rr validates inputs", {
  data <- data.frame(x = 1:10)

  expect_error(
    measure_gage_rr(data, "missing", "part", "operator"),
    "not found"
  )

  expect_error(
    measure_gage_rr(data, "x", "missing_part", "operator"),
    "not found"
  )
})

test_that("measure_gage_rr print method works", {
  set.seed(123)
  data <- expand.grid(
    part = 1:5,
    operator = c("A", "B"),
    replicate = 1:2
  )
  data$measurement <- rnorm(nrow(data), 50, 2)

  result <- measure_gage_rr(
    data,
    response_col = "measurement",
    part_col = "part",
    operator_col = "operator",
    tolerance = 20
  )

  expect_output(print(result), "measure_gage_rr")
  expect_output(print(result), "Variance Components")
  expect_output(print(result), "% Study Variation")
  expect_output(print(result), "% Tolerance")
  expect_output(print(result), "ndc")
})

test_that("measure_gage_rr ndc calculation is reasonable", {
  set.seed(123)
  # Create data with large part-to-part variation relative to R&R
  data <- expand.grid(
    part = 1:10,
    operator = c("A", "B"),
    replicate = 1:3
  )
  data$measurement <- 50 +
    (data$part - 5) * 10 +  # Large part variation
    rnorm(nrow(data), 0, 0.5)  # Small measurement error

  result <- measure_gage_rr(
    data,
    response_col = "measurement",
    part_col = "part",
    operator_col = "operator"
  )

  # With large part variation and small error, ndc should be high
  expect_true(attr(result, "ndc") >= 5)
})

# ==============================================================================
# tidy methods tests
# ==============================================================================

test_that("tidy.measure_precision returns tibble", {
  set.seed(42)
  data <- data.frame(concentration = rnorm(10, 100, 2))
  result <- measure_repeatability(data, "concentration")

  tidy_result <- tidy(result)
  expect_s3_class(tidy_result, "tbl_df")
})

test_that("tidy.measure_gage_rr returns tibble", {
  set.seed(123)
  data <- expand.grid(
    part = 1:5,
    operator = c("A", "B"),
    replicate = 1:2
  )
  data$measurement <- rnorm(nrow(data))

  result <- measure_gage_rr(
    data,
    response_col = "measurement",
    part_col = "part",
    operator_col = "operator"
  )

  tidy_result <- tidy(result)
  expect_s3_class(tidy_result, "tbl_df")
  expect_equal(nrow(tidy_result), 4)
})

# ==============================================================================
# Integration tests
# ==============================================================================

test_that("precision workflow with criteria integration works", {
  set.seed(42)
  data <- data.frame(
    level = rep(c("low", "mid", "high"), each = 10),
    concentration = c(
      rnorm(10, 10, 0.3),
      rnorm(10, 50, 1.5),
      rnorm(10, 100, 3)
    )
  )

  result <- measure_repeatability(data, "concentration", group_col = "level")

  # All CVs should be reasonable
  expect_true(all(result$cv < 10))  # Expect CV < 10%
  expect_equal(nrow(result), 3)
})
