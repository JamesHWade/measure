# Tests for matrix effects and standard addition functions

# ==============================================================================
# measure_matrix_effect()
# ==============================================================================

test_that("measure_matrix_effect calculates basic ME", {
  data <- data.frame(
    sample_type = rep(c("matrix", "neat"), each = 3),
    response = c(90, 95, 92, 100, 100, 100)
  )

  me <- measure_matrix_effect(
    data,
    response_col = "response",
    sample_type_col = "sample_type",
    matrix_level = "matrix",
    neat_level = "neat"
  )

  expect_s3_class(me, "measure_matrix_effect")

  # Mean ME should be ~92.3%
  expect_true(me$statistics$mean_me < 100)  # Suppression
  expect_true(me$statistics$mean_me > 80)
})

test_that("measure_matrix_effect handles grouped data", {
  data <- data.frame(
    sample_type = rep(c("matrix", "neat"), each = 4),
    concentration = rep(c("low", "high", "low", "high"), 2),
    response = c(
      # Matrix
      45, 90, 48, 95,
      # Neat
      50, 100, 50, 100
    )
  )

  me <- measure_matrix_effect(
    data,
    response_col = "response",
    sample_type_col = "sample_type",
    matrix_level = "matrix",
    neat_level = "neat",
    concentration_col = "concentration"
  )

  # Should have 2 groups (low and high)
  expect_equal(nrow(me$results), 2)
  expect_true("concentration" %in% names(me$results))
})

test_that("measure_matrix_effect classifies suppression/enhancement", {
  # Strong suppression
  data_suppress <- data.frame(
    sample_type = c("matrix", "neat"),
    response = c(50, 100)  # 50% ME
  )

  me_suppress <- measure_matrix_effect(
    data_suppress, "response", "sample_type", "matrix", "neat"
  )
  expect_equal(me_suppress$results$interpretation, "strong_suppression")

  # Enhancement
  data_enhance <- data.frame(
    sample_type = c("matrix", "neat"),
    response = c(110, 100)  # 110% ME
  )

  me_enhance <- measure_matrix_effect(
    data_enhance, "response", "sample_type", "matrix", "neat"
  )
  expect_equal(me_enhance$results$interpretation, "enhancement")
})

test_that("measure_matrix_effect validates inputs", {
  data <- data.frame(sample_type = c("A", "B"), response = c(1, 2))

  # Missing matrix level
  expect_error(
    measure_matrix_effect(data, "response", "sample_type", "matrix", "neat"),
    "not found"
  )

  # Non-numeric response
  data2 <- data.frame(sample_type = c("matrix", "neat"), response = c("a", "b"))
  expect_error(
    measure_matrix_effect(data2, "response", "sample_type", "matrix", "neat"),
    "must be numeric"
  )
})

test_that("tidy.measure_matrix_effect returns results", {
  data <- data.frame(
    sample_type = rep(c("matrix", "neat"), each = 3),
    response = c(90, 95, 92, 100, 100, 100)
  )

  me <- measure_matrix_effect(data, "response", "sample_type", "matrix", "neat")
  tidy_me <- tidy(me)

  expect_s3_class(tidy_me, "tbl_df")
  expect_true("matrix_effect_pct" %in% names(tidy_me))
})

test_that("glance.measure_matrix_effect returns summary", {
  data <- data.frame(
    sample_type = rep(c("matrix", "neat"), each = 3),
    response = c(90, 95, 92, 100, 100, 100)
  )

  me <- measure_matrix_effect(data, "response", "sample_type", "matrix", "neat")
  glance_me <- glance(me)

  expect_equal(nrow(glance_me), 1)
  expect_true("mean_me_pct" %in% names(glance_me))
  expect_true("pct_acceptable" %in% names(glance_me))
})

test_that("autoplot.measure_matrix_effect returns ggplot", {
  skip_if_not_installed("ggplot2")

  data <- data.frame(
    sample_type = rep(c("matrix", "neat"), each = 6),
    concentration = rep(c("low", "mid", "high"), 4),
    response = c(90, 95, 92, 88, 97, 91, 100, 100, 100, 100, 100, 100)
  )

  me <- measure_matrix_effect(
    data, "response", "sample_type", "matrix", "neat",
    concentration_col = "concentration"
  )

  p <- autoplot(me, type = "bar")
  expect_s3_class(p, "ggplot")

  p2 <- autoplot(me, type = "forest")
  expect_s3_class(p2, "ggplot")
})

# ==============================================================================
# step_measure_standard_addition()
# ==============================================================================

test_that("step_measure_standard_addition calculates concentrations", {
  # Standard addition data: response = intercept + slope * addition
  # Original concentration = -intercept / slope
  sa_data <- data.frame(
    sample_id = rep(c("S1", "S2"), each = 4),
    addition = rep(c(0, 10, 20, 30), 2),
    response = c(
      # S1: slope=10, intercept=150 -> original = 15
      150, 250, 350, 450,
      # S2: slope=10, intercept=250 -> original = 25
      250, 350, 450, 550
    )
  )

  rec <- recipes::recipe(~ ., data = sa_data) |>
    step_measure_standard_addition(
      response,
      addition_col = "addition",
      sample_id_col = "sample_id"
    ) |>
    recipes::prep()

  result <- recipes::bake(rec, new_data = NULL)

  expect_true("response_corrected" %in% names(result))

  # Check calculated concentrations (approximately)
  s1_conc <- result$response_corrected[result$sample_id == "S1"][1]
  s2_conc <- result$response_corrected[result$sample_id == "S2"][1]

  expect_equal(s1_conc, 15, tolerance = 0.5)
  expect_equal(s2_conc, 25, tolerance = 0.5)
})

test_that("step_measure_standard_addition includes diagnostics", {
  sa_data <- data.frame(
    sample_id = rep("S1", 4),
    addition = c(0, 10, 20, 30),
    response = c(150, 250, 350, 450)
  )

  rec <- recipes::recipe(~ ., data = sa_data) |>
    step_measure_standard_addition(
      response,
      addition_col = "addition",
      sample_id_col = "sample_id",
      diagnostics = TRUE
    ) |>
    recipes::prep()

  result <- recipes::bake(rec, new_data = NULL)

  expect_true("response_sa_slope" %in% names(result))
  expect_true("response_sa_intercept" %in% names(result))
  expect_true("response_sa_rsquared" %in% names(result))

  # R-squared should be very high for this perfect data
  expect_equal(result$response_sa_rsquared[1], 1, tolerance = 0.01)
})

test_that("step_measure_standard_addition validates inputs", {
  sa_data <- data.frame(
    sample_id = rep("S1", 3),
    addition = c(0, 10, 20),
    response = c(100, 200, 300)
  )

  # Missing sample_id_col
expect_error(
    recipes::recipe(~ ., data = sa_data) |>
      step_measure_standard_addition(response, addition_col = "addition"),
    "sample_id_col"
  )

  # min_points too small
  expect_error(
    recipes::recipe(~ ., data = sa_data) |>
      step_measure_standard_addition(
        response,
        addition_col = "addition",
        sample_id_col = "sample_id",
        min_points = 1
      ),
    "at least 2"
  )
})

test_that("step_measure_standard_addition handles insufficient points", {
  sa_data <- data.frame(
    sample_id = c("S1", "S1", "S2"),
    addition = c(0, 10, 0),  # S2 has only 1 point
    response = c(100, 200, 150)
  )

  expect_warning(
    rec <- recipes::recipe(~ ., data = sa_data) |>
      step_measure_standard_addition(
        response,
        addition_col = "addition",
        sample_id_col = "sample_id",
        min_points = 2
      ) |>
      recipes::prep(),
    "only"
  )
})

test_that("tidy.step_measure_standard_addition returns calibrations", {
  sa_data <- data.frame(
    sample_id = rep(c("S1", "S2"), each = 3),
    addition = rep(c(0, 10, 20), 2),
    response = c(100, 200, 300, 150, 250, 350)
  )

  rec <- recipes::recipe(~ ., data = sa_data) |>
    step_measure_standard_addition(
      response,
      addition_col = "addition",
      sample_id_col = "sample_id"
    ) |>
    recipes::prep()

  tidy_sa <- tidy(rec, number = 1)

  expect_s3_class(tidy_sa, "tbl_df")
  expect_equal(nrow(tidy_sa), 2)  # 2 samples
  expect_true("original_concentration" %in% names(tidy_sa))
  expect_true("slope" %in% names(tidy_sa))
  expect_true("r_squared" %in% names(tidy_sa))
})

test_that("print.step_measure_standard_addition works", {
  sa_data <- data.frame(
    sample_id = rep("S1", 3),
    addition = c(0, 10, 20),
    response = c(100, 200, 300)
  )

  rec <- recipes::recipe(~ ., data = sa_data) |>
    step_measure_standard_addition(
      response,
      addition_col = "addition",
      sample_id_col = "sample_id"
    ) |>
    recipes::prep()

  expect_output(print(rec), "Standard addition")
})
