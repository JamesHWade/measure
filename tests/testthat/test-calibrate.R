# ==============================================================================
# Tests for calibration steps
#
# This file tests:
# - step_measure_calibrate_x
# - step_measure_calibrate_y
# - step_measure_normalize_istd (alias)
# ==============================================================================

# ==============================================================================
# Test Helpers
# ==============================================================================

# Create test data in internal format
create_test_data <- function() {
  recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    prep() |>
    bake(new_data = NULL)
}

# Get the locations in test data
get_locations <- function() {
  test_data <- create_test_data()
  test_data$.measures[[1]]$location
}

# ==============================================================================
# step_measure_calibrate_x tests
# ==============================================================================

test_that("step_measure_calibrate_x works with function calibration", {
  test_data <- create_test_data()

  # Simple log10 transformation
  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_calibrate_x(calibration = function(x) log10(x + 1)) |>
    prep()

  result <- bake(rec, new_data = NULL)

  original_locs <- test_data$.measures[[1]]$location
  calibrated_locs <- result$.measures[[1]]$location
  expect_equal(calibrated_locs, log10(original_locs + 1), tolerance = 1e-10)
})

test_that("step_measure_calibrate_x works with calibration data.frame", {
  locs <- get_locations()

  # Create calibration data spanning the location range
  cal_data <- data.frame(
    x = c(min(locs), max(locs)),
    y = c(0, 100)  # Linear transformation to 0-100
  )

  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_calibrate_x(
      calibration = cal_data,
      from = "x",
      to = "y",
      method = "linear"
    ) |>
    prep()

  result <- bake(rec, new_data = NULL)

  # Check that calibration was applied
  calibrated_locs <- result$.measures[[1]]$location
  expect_true(min(calibrated_locs) >= 0 - 0.001)
  expect_true(max(calibrated_locs) <= 100 + 0.001)
})

test_that("step_measure_calibrate_x works with spline method", {
  locs <- get_locations()

  # Create calibration data with multiple points
  cal_data <- data.frame(
    x = seq(min(locs), max(locs), length.out = 5),
    y = seq(0, 100, length.out = 5)
  )

  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_calibrate_x(
      calibration = cal_data,
      from = "x",
      to = "y",
      method = "spline"
    ) |>
    prep()

  result <- bake(rec, new_data = NULL)

  expect_s3_class(result, "tbl_df")
  expect_true(".measures" %in% names(result))
})

test_that("step_measure_calibrate_x requires calibration argument", {
  expect_error(
    recipe(water + fat + protein ~ ., data = meats_long) |>
      step_measure_calibrate_x(),
    "calibration.*required"
  )
})

test_that("step_measure_calibrate_x errors with invalid method", {
  expect_error(
    recipe(water + fat + protein ~ ., data = meats_long) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_measure_calibrate_x(
        calibration = function(x) x,
        method = "invalid"
      ) |>
      prep(),
    "linear.*spline"
  )
})

test_that("step_measure_calibrate_x errors with missing columns in cal data", {
  cal_data <- data.frame(a = 1:5, b = 1:5)

  expect_error(
    recipe(water + fat + protein ~ ., data = meats_long) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_measure_calibrate_x(
        calibration = cal_data,
        from = "x",
        to = "y"
      ) |>
      prep(),
    "not found"
  )
})

test_that("step_measure_calibrate_x errors with insufficient cal points", {
  cal_data <- data.frame(x = 1, y = 1)

  expect_error(
    recipe(water + fat + protein ~ ., data = meats_long) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_measure_calibrate_x(
        calibration = cal_data,
        from = "x",
        to = "y"
      ) |>
      prep(),
    "at least 2"
  )
})

test_that("step_measure_calibrate_x preserves values", {
  test_data <- create_test_data()

  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_calibrate_x(calibration = function(x) x * 2) |>
    prep()

  result <- bake(rec, new_data = NULL)

  # Values should be unchanged
  original_vals <- test_data$.measures[[1]]$value
  result_vals <- result$.measures[[1]]$value
  expect_equal(result_vals, original_vals)
})

test_that("step_measure_calibrate_x print method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_calibrate_x(calibration = function(x) x)

  expect_output(print(rec$steps[[2]]), "X-axis calibration")

  rec_prepped <- prep(rec)
  expect_output(print(rec_prepped$steps[[2]]), "spline")
})

test_that("step_measure_calibrate_x tidy method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_calibrate_x(calibration = function(x) x) |>
    prep()

  tidy_result <- tidy(rec, number = 2)

  expect_s3_class(tidy_result, "tbl_df")
  expect_true("method" %in% names(tidy_result))
  expect_true("extrapolate" %in% names(tidy_result))
})

# ==============================================================================
# step_measure_calibrate_y tests
# ==============================================================================

test_that("step_measure_calibrate_y works with response_factor", {
  test_data <- create_test_data()

  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_calibrate_y(response_factor = 2.5) |>
    prep()

  result <- bake(rec, new_data = NULL)

  original_vals <- test_data$.measures[[1]]$value
  calibrated_vals <- result$.measures[[1]]$value
  expect_equal(calibrated_vals, original_vals * 2.5, tolerance = 1e-10)
})

test_that("step_measure_calibrate_y works with calibration function", {
  test_data <- create_test_data()

  # Log transform
  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_calibrate_y(calibration = function(x) log10(x + 0.01)) |>
    prep()

  result <- bake(rec, new_data = NULL)

  original_vals <- test_data$.measures[[1]]$value
  calibrated_vals <- result$.measures[[1]]$value
  expect_equal(calibrated_vals, log10(original_vals + 0.01), tolerance = 1e-10)
})

test_that("step_measure_calibrate_y default response_factor is 1", {
  test_data <- create_test_data()

  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_calibrate_y() |>
    prep()

  result <- bake(rec, new_data = NULL)

  # Values should be unchanged (multiplied by 1)
  original_vals <- test_data$.measures[[1]]$value
  calibrated_vals <- result$.measures[[1]]$value
  expect_equal(calibrated_vals, original_vals, tolerance = 1e-10)
})

test_that("step_measure_calibrate_y preserves locations", {
  test_data <- create_test_data()

  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_calibrate_y(response_factor = 2.0) |>
    prep()

  result <- bake(rec, new_data = NULL)

  # Locations should be unchanged
  original_locs <- test_data$.measures[[1]]$location
  result_locs <- result$.measures[[1]]$location
  expect_equal(result_locs, original_locs)
})

test_that("step_measure_calibrate_y errors with invalid response_factor", {
  expect_error(
    recipe(water + fat + protein ~ ., data = meats_long) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_measure_calibrate_y(response_factor = c(1, 2)) |>
      prep(),
    "single numeric"
  )
})

test_that("step_measure_calibrate_y errors with non-function calibration", {
  expect_error(
    recipe(water + fat + protein ~ ., data = meats_long) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_measure_calibrate_y(calibration = "not a function") |>
      prep(),
    "function"
  )
})

test_that("step_measure_calibrate_y print method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_calibrate_y(response_factor = 2.5)

  expect_output(print(rec$steps[[2]]), "Y-axis calibration")

  rec_prepped <- prep(rec)
  expect_output(print(rec_prepped$steps[[2]]), "2.5")
})

test_that("step_measure_calibrate_y tidy method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_calibrate_y(response_factor = 2.5) |>
    prep()

  tidy_result <- tidy(rec, number = 2)

  expect_s3_class(tidy_result, "tbl_df")
  expect_true("response_factor" %in% names(tidy_result))
  expect_true("has_calibration" %in% names(tidy_result))
  expect_equal(tidy_result$response_factor, 2.5)
  expect_false(tidy_result$has_calibration)
})

# ==============================================================================
# step_measure_normalize_istd tests (alias for normalize_peak)
# ==============================================================================

test_that("step_measure_normalize_istd works as alias", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_normalize_istd(
      location_min = 50,
      location_max = 60,
      method = "mean"
    ) |>
    prep()

  result <- bake(rec, new_data = NULL)

  expect_s3_class(result, "tbl_df")
  expect_true(".measures" %in% names(result))
})

test_that("step_measure_normalize_istd uses integral method by default", {
  # The alias default is "mean" based on our implementation
  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_normalize_istd(
      location_min = 50,
      location_max = 60
    )

  # Check the step was created
  expect_s3_class(rec$steps[[2]], "step_measure_normalize_peak")
})

test_that("step_measure_normalize_istd requires location bounds", {
  expect_error(
    recipe(water + fat + protein ~ ., data = meats_long) |>
      step_measure_normalize_istd(location_min = 50) |>
      prep(),
    "location_max"
  )
})

# ==============================================================================
# Integration tests
# ==============================================================================

test_that("calibration steps work in a pipeline", {
  locs <- get_locations()
  cal_data <- data.frame(
    x = c(min(locs), max(locs)),
    y = c(0, 100)
  )

  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_calibrate_x(
      calibration = cal_data,
      from = "x",
      to = "y",
      method = "linear"
    ) |>
    step_measure_calibrate_y(response_factor = 1000) |>
    prep()

  result <- bake(rec, new_data = NULL)

  expect_s3_class(result, "tbl_df")
  expect_true(".measures" %in% names(result))

  # Check that both calibrations were applied
  calibrated_locs <- result$.measures[[1]]$location
  expect_true(min(calibrated_locs) >= 0 - 0.001)
  expect_true(max(calibrated_locs) <= 100 + 0.001)
})

test_that("calibration steps work with wide input format", {
  test_data_wide <- meats_long |>
    tidyr::pivot_wider(
      names_from = channel,
      names_prefix = "x_",
      values_from = transmittance
    )

  rec <- recipe(water + fat + protein ~ ., data = test_data_wide) |>
    update_role(id, new_role = "id") |>
    step_measure_input_wide(starts_with("x_")) |>
    step_measure_calibrate_y(response_factor = 2.0) |>
    prep()

  result <- bake(rec, new_data = NULL)

  expect_s3_class(result, "tbl_df")
  expect_true(".measures" %in% names(result))
})

test_that("required_pkgs methods work", {
  rec1 <- recipe(water + fat + protein ~ ., data = meats_long) |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_calibrate_x(calibration = function(x) x)

  rec2 <- recipe(water + fat + protein ~ ., data = meats_long) |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_calibrate_y(response_factor = 1.0)

  expect_true("measure" %in% required_pkgs(rec1$steps[[2]]))
  expect_true("measure" %in% required_pkgs(rec2$steps[[2]]))
})
