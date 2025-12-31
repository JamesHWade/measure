# ==============================================================================
# Tests for uncertainty.R
# ==============================================================================

# ==============================================================================
# uncertainty_component() tests
# ==============================================================================

test_that("uncertainty_component creates valid object", {
  uc <- uncertainty_component("Test", 0.05, type = "A", df = 9)

  expect_s3_class(uc, "uncertainty_component")
  expect_equal(uc$name, "Test")
  expect_equal(uc$value, 0.05)
  expect_equal(uc$type, "A")
  expect_equal(uc$df, 9)
})

test_that("uncertainty_component calculates contribution", {
  # With sensitivity = 1
  uc <- uncertainty_component("Test", 0.05, sensitivity = 1)
  expect_equal(uc$contribution, 0.05)

  # With sensitivity = 2
  uc <- uncertainty_component("Test", 0.05, sensitivity = 2)
  expect_equal(uc$contribution, 0.10)

  # With negative sensitivity (absolute value used)
  uc <- uncertainty_component("Test", 0.05, sensitivity = -2)
  expect_equal(uc$contribution, 0.10)
})

test_that("uncertainty_component applies coverage factor", {
  # Value given as expanded uncertainty with k=2
  uc <- uncertainty_component("Test", 0.10, coverage_factor = 2)
  expect_equal(uc$value, 0.05) # Should be divided by k
})

test_that("uncertainty_component validates inputs", {
  expect_error(
    uncertainty_component(123, 0.05), # name must be character
    "character"
  )

  expect_error(
    uncertainty_component("Test", -0.05), # value must be non-negative
    "non-negative"
  )

  expect_error(
    uncertainty_component("Test", 0.05, df = -5), # df must be positive
    "positive"
  )
})

test_that("uncertainty_component handles different types", {
  uc_a <- uncertainty_component("Test", 0.05, type = "A")
  expect_equal(uc_a$type, "A")

  uc_b <- uncertainty_component("Test", 0.05, type = "B")
  expect_equal(uc_b$type, "B")
})

test_that("uncertainty_component handles distributions", {
  uc <- uncertainty_component("Test", 0.05, distribution = "rectangular")
  expect_equal(uc$distribution, "rectangular")
})

test_that("print.uncertainty_component works", {
  uc <- uncertainty_component("Repeatability", 0.05, type = "A", df = 9)
  expect_output(print(uc), "uncertainty_component")
  expect_output(print(uc), "Repeatability")
  expect_output(print(uc), "Type: A")
})

# ==============================================================================
# measure_uncertainty_budget() tests
# ==============================================================================

test_that("measure_uncertainty_budget combines components correctly", {
  u1 <- uncertainty_component("A", 0.03, type = "A")
  u2 <- uncertainty_component("B", 0.04, type = "B")

  budget <- measure_uncertainty_budget(u1, u2)

  expect_s3_class(budget, "measure_uncertainty_budget")

  # Combined uncertainty should be sqrt(0.03^2 + 0.04^2) = 0.05
  expect_equal(budget$combined_u, 0.05)
})

test_that("measure_uncertainty_budget calculates expanded uncertainty", {
  u1 <- uncertainty_component("A", 0.05)

  budget <- measure_uncertainty_budget(u1, k = 2)
  expect_equal(budget$expanded_U, 0.10)

  budget_k3 <- measure_uncertainty_budget(u1, k = 3)
  expect_equal(budget_k3$expanded_U, 0.15)
})

test_that("measure_uncertainty_budget calculates effective df", {
  # Single component: effective df should equal component df
  u1 <- uncertainty_component("A", 0.05, df = 9)
  budget <- measure_uncertainty_budget(u1)
  expect_equal(budget$effective_df, 9)

  # Multiple components with infinite df: effective df is infinite
  u1 <- uncertainty_component("A", 0.03, df = Inf)
  u2 <- uncertainty_component("B", 0.04, df = Inf)
  budget <- measure_uncertainty_budget(u1, u2)
  expect_true(is.infinite(budget$effective_df))
})

test_that("measure_uncertainty_budget handles sensitivity coefficients", {
  u1 <- uncertainty_component("A", 0.05, sensitivity = 2)
  u2 <- uncertainty_component("B", 0.05, sensitivity = 1)

  budget <- measure_uncertainty_budget(u1, u2)

  # Contributions: 0.05*2 = 0.10 and 0.05*1 = 0.05
  # Combined: sqrt(0.10^2 + 0.05^2) = sqrt(0.0125) ≈ 0.1118
  expect_equal(budget$combined_u, sqrt(0.0125), tolerance = 1e-6)
})

test_that("measure_uncertainty_budget calculates relative uncertainty", {
  u1 <- uncertainty_component("A", 0.1)

  budget <- measure_uncertainty_budget(u1, result_value = 10)

  expect_equal(budget$relative_u, 0.01) # 0.1 / 10
  expect_equal(budget$relative_U, 0.02) # 0.2 / 10 (with k=2)
})

test_that("measure_uncertainty_budget validates inputs", {
  expect_error(
    measure_uncertainty_budget("not a component"),
    "uncertainty_component"
  )

  expect_error(
    measure_uncertainty_budget(), # No components
    "At least one"
  )
})

test_that("measure_uncertainty_budget accepts .list argument", {
  components <- list(
    uncertainty_component("A", 0.03),
    uncertainty_component("B", 0.04)
  )

  budget <- measure_uncertainty_budget(.list = components)
  expect_equal(budget$combined_u, 0.05)
})

test_that("print.measure_uncertainty_budget works", {
  u1 <- uncertainty_component("A", 0.05, type = "A")
  budget <- measure_uncertainty_budget(u1)

  expect_output(print(budget), "measure_uncertainty_budget")
  expect_output(print(budget), "Combined u")
  expect_output(print(budget), "Expanded U")
})

test_that("print.measure_uncertainty_budget shows relative when result provided", {
  u1 <- uncertainty_component("A", 0.05)
  budget <- measure_uncertainty_budget(u1, result_value = 10)

  expect_output(print(budget), "Result")
  expect_output(print(budget), "Relative")
})

# ==============================================================================
# measure_uncertainty() tests
# ==============================================================================

test_that("measure_uncertainty returns simplified result", {
  u1 <- uncertainty_component("A", 0.03)
  u2 <- uncertainty_component("B", 0.04)

  result <- measure_uncertainty(u1, u2)

  expect_type(result, "list")
  expect_equal(
    names(result),
    c("combined_u", "expanded_U", "effective_df", "coverage_factor")
  )
  expect_equal(result$combined_u, 0.05)
})

# ==============================================================================
# tidy.measure_uncertainty_budget() tests
# ==============================================================================

test_that("tidy.measure_uncertainty_budget returns components", {
  u1 <- uncertainty_component("Repeatability", 0.03, type = "A")
  u2 <- uncertainty_component("Calibrator", 0.04, type = "B")
  budget <- measure_uncertainty_budget(u1, u2)

  tidy_result <- tidy(budget)

  expect_s3_class(tidy_result, "tbl_df")
  expect_equal(nrow(tidy_result), 2)
  expect_true("name" %in% names(tidy_result))
  expect_true("percent_contribution" %in% names(tidy_result))

  # Percentages should sum to 100
  expect_equal(sum(tidy_result$percent_contribution), 100)
})

test_that("tidy.measure_uncertainty_budget returns summary", {
  u1 <- uncertainty_component("A", 0.05)
  budget <- measure_uncertainty_budget(u1, k = 2)

  summary <- tidy(budget, type = "summary")

  expect_s3_class(summary, "tbl_df")
  expect_equal(nrow(summary), 1)
  expect_true("combined_u" %in% names(summary))
  expect_true("expanded_U" %in% names(summary))
})

# ==============================================================================
# autoplot.measure_uncertainty_budget() tests
# ==============================================================================

test_that("autoplot.measure_uncertainty_budget creates plot", {
  skip_if_not_installed("ggplot2")

  u1 <- uncertainty_component("Repeatability", 0.05, type = "A")
  u2 <- uncertainty_component("Calibrator", 0.03, type = "B")
  u3 <- uncertainty_component("Temperature", 0.02, type = "B")
  budget <- measure_uncertainty_budget(u1, u2, u3)

  p <- autoplot(budget)

  expect_s3_class(p, "ggplot")
})

# ==============================================================================
# Helper function tests
# ==============================================================================

test_that("uncertainty_type_a calculates from measurements", {
  measurements <- c(10.0, 10.2, 9.8, 10.1, 10.0)

  uc <- uncertainty_type_a(measurements, "Repeatability")

  expect_s3_class(uc, "uncertainty_component")
  expect_equal(uc$type, "A")
  expect_equal(uc$df, 4) # n - 1

  # SE should be sd / sqrt(n)
  expected_se <- sd(measurements) / sqrt(5)
  expect_equal(uc$value, expected_se)
})

test_that("uncertainty_type_a requires multiple measurements", {
  expect_error(
    uncertainty_type_a(10.0, "Test"),
    "At least 2"
  )
})

test_that("uncertainty_type_b_expanded creates from expanded uncertainty", {
  uc <- uncertainty_type_b_expanded(0.10, k = 2, name = "Calibrator")

  expect_s3_class(uc, "uncertainty_component")
  expect_equal(uc$type, "B")
  expect_equal(uc$value, 0.05) # Divided by k
})

test_that("uncertainty_type_b_rectangular creates from half-width", {
  uc <- uncertainty_type_b_rectangular(0.5, name = "Temperature")

  expect_s3_class(uc, "uncertainty_component")
  expect_equal(uc$type, "B")
  expect_equal(uc$distribution, "rectangular")
  expect_equal(uc$value, 0.5 / sqrt(3))
})

# ==============================================================================
# Integration tests
# ==============================================================================

test_that("full uncertainty budget workflow works", {
  # Real-world example: calibration uncertainty budget
  # Repeatability from 6 measurements
  measurements <- c(100.1, 100.3, 99.9, 100.2, 100.0, 99.8)
  u_repeat <- uncertainty_type_a(measurements, "Repeatability")

  # Calibrator certificate: U = 0.5, k = 2
  u_cal <- uncertainty_type_b_expanded(0.5, k = 2, name = "Calibrator", df = 60)

  # Temperature stability: +/- 0.2
  u_temp <- uncertainty_type_b_rectangular(0.2, name = "Temperature")

  # Create budget
  budget <- measure_uncertainty_budget(
    u_repeat,
    u_cal,
    u_temp,
    k = 2,
    result_value = mean(measurements)
  )

  # Should have valid results
  expect_true(budget$combined_u > 0)
  expect_true(budget$expanded_U > budget$combined_u)
  expect_true(budget$effective_df > 0)
  expect_true(!is.null(budget$relative_u))

  # Tidy should work
  tidy_components <- tidy(budget)
  expect_equal(nrow(tidy_components), 3)
  expect_equal(sum(tidy_components$percent_contribution), 100, tolerance = 0.01)
})
