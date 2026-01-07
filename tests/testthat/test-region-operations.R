# ==============================================================================
# Tests for region operations
#
# This file tests:
# - step_measure_trim
# - step_measure_exclude
# - step_measure_resample
# ==============================================================================

# ==============================================================================
# Test Helpers
# ==============================================================================

# Create test data in internal format
create_test_data <- function() {
  recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    prep() |>
    bake(new_data = NULL)
}

# Get original values from test data
get_original_values <- function() {
  test_data <- create_test_data()
  test_data$.measures[[1]]$value
}

# Get locations from test data
get_locations <- function() {
  test_data <- create_test_data()
  test_data$.measures[[1]]$location
}

# ==============================================================================
# step_measure_trim tests
# ==============================================================================

test_that("step_measure_trim keeps only points in range", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_trim(range = c(20, 80)) |>
    prep()

  result <- bake(rec, new_data = NULL)
  locs <- result$.measures[[1]]$location

  expect_true(all(locs >= 20))
  expect_true(all(locs <= 80))
})

test_that("step_measure_trim reduces number of points", {
  test_data <- create_test_data()
  original_n <- nrow(test_data$.measures[[1]])

  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_trim(range = c(20, 80)) |>
    prep()

  result <- bake(rec, new_data = NULL)
  result_n <- nrow(result$.measures[[1]])

  expect_lt(result_n, original_n)
})

test_that("step_measure_trim preserves values in range", {
  test_data <- create_test_data()
  original <- test_data$.measures[[1]]

  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_trim(range = c(20, 80)) |>
    prep()

  result <- bake(rec, new_data = NULL)
  trimmed <- result$.measures[[1]]

  # Values in trimmed result should match original values at those locations
  for (i in seq_along(trimmed$location)) {
    orig_idx <- which(original$location == trimmed$location[i])
    expect_equal(trimmed$value[i], original$value[orig_idx])
  }
})

test_that("step_measure_trim errors with invalid range", {
  expect_error(
    recipe(water + fat + protein ~ ., data = meats_small) |>
      step_measure_trim(range = c(80, 20)),
    "must have the minimum value first"
  )

  expect_error(
    recipe(water + fat + protein ~ ., data = meats_small) |>
      step_measure_trim(range = c(50, 50)),
    "must have the minimum value first"
  )

  expect_error(
    recipe(water + fat + protein ~ ., data = meats_small) |>
      step_measure_trim(range = c(20)),
    "must be a numeric vector of length 2"
  )
})

test_that("step_measure_trim errors when range is missing", {
  expect_error(
    recipe(water + fat + protein ~ ., data = meats_small) |>
      step_measure_trim(),
    "must be provided"
  )
})

test_that("step_measure_trim print method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_trim(range = c(20, 80))

  expect_output(print(rec$steps[[2]]), "Trim measurements to \\[20, 80\\]")

  rec_prepped <- prep(rec)
  expect_output(
    print(rec_prepped$steps[[2]]),
    "Trim measurements to \\[20, 80\\]"
  )
})

test_that("step_measure_trim tidy method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_trim(range = c(20, 80)) |>
    prep()

  tidy_result <- tidy(rec, number = 2)

  expect_s3_class(tidy_result, "tbl_df")
  expect_true("terms" %in% names(tidy_result))
  expect_true("range_min" %in% names(tidy_result))
  expect_true("range_max" %in% names(tidy_result))
  expect_equal(tidy_result$range_min, 20)
  expect_equal(tidy_result$range_max, 80)
})

# ==============================================================================
# step_measure_exclude tests
# ==============================================================================

test_that("step_measure_exclude removes points in ranges", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_exclude(ranges = list(c(1, 10), c(90, 100))) |>
    prep()

  result <- bake(rec, new_data = NULL)
  locs <- result$.measures[[1]]$location

  # No points should be in excluded ranges
  expect_false(any(locs >= 1 & locs <= 10))
  expect_false(any(locs >= 90 & locs <= 100))
})

test_that("step_measure_exclude reduces number of points", {
  test_data <- create_test_data()
  original_n <- nrow(test_data$.measures[[1]])

  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_exclude(ranges = list(c(1, 20))) |>
    prep()

  result <- bake(rec, new_data = NULL)
  result_n <- nrow(result$.measures[[1]])

  expect_lt(result_n, original_n)
})

test_that("step_measure_exclude accepts single range as vector", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_exclude(ranges = c(1, 10)) |>
    prep()

  result <- bake(rec, new_data = NULL)
  locs <- result$.measures[[1]]$location

  expect_false(any(locs >= 1 & locs <= 10))
})

test_that("step_measure_exclude preserves values outside ranges", {
  test_data <- create_test_data()
  original <- test_data$.measures[[1]]

  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_exclude(ranges = list(c(1, 10))) |>
    prep()

  result <- bake(rec, new_data = NULL)
  excluded <- result$.measures[[1]]

  # Values not in excluded range should match original
  for (i in seq_along(excluded$location)) {
    orig_idx <- which(original$location == excluded$location[i])
    expect_equal(excluded$value[i], original$value[orig_idx])
  }
})

test_that("step_measure_exclude errors with invalid ranges", {
  expect_error(
    recipe(water + fat + protein ~ ., data = meats_small) |>
      step_measure_exclude(ranges = list(c(80, 20))),
    "must have the minimum value first"
  )

  expect_error(
    recipe(water + fat + protein ~ ., data = meats_small) |>
      step_measure_exclude(ranges = list(c(20))),
    "must be a numeric vector of length 2"
  )
})

test_that("step_measure_exclude errors when ranges is missing", {
  expect_error(
    recipe(water + fat + protein ~ ., data = meats_small) |>
      step_measure_exclude(),
    "must be provided"
  )
})

test_that("step_measure_exclude print method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_exclude(ranges = list(c(1, 10), c(90, 100)))

  expect_output(print(rec$steps[[2]]), "Exclude 2 ranges")

  rec_prepped <- prep(rec)
  expect_output(print(rec_prepped$steps[[2]]), "Exclude 2 ranges")
})

test_that("step_measure_exclude tidy method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_exclude(ranges = list(c(1, 10), c(90, 100))) |>
    prep()

  tidy_result <- tidy(rec, number = 2)

  expect_s3_class(tidy_result, "tbl_df")
  expect_true("terms" %in% names(tidy_result))
  expect_true("range_min" %in% names(tidy_result))
  expect_true("range_max" %in% names(tidy_result))
  expect_equal(nrow(tidy_result), 2) # One row per exclusion range
})

# ==============================================================================
# step_measure_resample tests
# ==============================================================================

test_that("step_measure_resample creates specified number of points with n", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_resample(n = 50) |>
    prep()

  result <- bake(rec, new_data = NULL)
  result_n <- nrow(result$.measures[[1]])

  expect_equal(result_n, 50)
})

test_that("step_measure_resample creates evenly spaced grid", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_resample(n = 50) |>
    prep()

  result <- bake(rec, new_data = NULL)
  locs <- result$.measures[[1]]$location

  # Check spacing is uniform
  diffs <- diff(locs)
  expect_equal(max(diffs) - min(diffs), 0, tolerance = 1e-10)
})

test_that("step_measure_resample with spacing parameter works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_resample(spacing = 5) |>
    prep()

  result <- bake(rec, new_data = NULL)
  locs <- result$.measures[[1]]$location

  # Check spacing is approximately 5 (except possibly last interval)
  diffs <- diff(locs)
  expect_true(all(abs(diffs[seq_len(length(diffs) - 1)] - 5) < 1e-10))
})

test_that("step_measure_resample linear method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_resample(n = 50, method = "linear") |>
    prep()

  result <- bake(rec, new_data = NULL)

  # Should have values (not NA)
  expect_false(anyNA(result$.measures[[1]]$value))
})

test_that("step_measure_resample spline method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_resample(n = 50, method = "spline") |>
    prep()

  result <- bake(rec, new_data = NULL)

  # Should have values (not NA)
  expect_false(anyNA(result$.measures[[1]]$value))
})

test_that("step_measure_resample respects custom range", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_resample(n = 25, range = c(25, 75)) |>
    prep()

  result <- bake(rec, new_data = NULL)
  locs <- result$.measures[[1]]$location

  expect_equal(min(locs), 25)
  expect_equal(max(locs), 75)
  expect_equal(length(locs), 25)
})

test_that("step_measure_resample errors when neither n nor spacing provided", {
  expect_error(
    recipe(water + fat + protein ~ ., data = meats_small) |>
      step_measure_resample(),
    "Either.*n.*or.*spacing.*must be specified"
  )
})

test_that("step_measure_resample errors when both n and spacing provided", {
  expect_error(
    recipe(water + fat + protein ~ ., data = meats_small) |>
      step_measure_resample(n = 50, spacing = 5),
    "n.*and.*spacing.*are mutually exclusive"
  )
})

test_that("step_measure_resample errors with invalid n", {
  expect_error(
    recipe(water + fat + protein ~ ., data = meats_small) |>
      step_measure_resample(n = 1),
    "must be a positive integer >= 2"
  )

  expect_error(
    recipe(water + fat + protein ~ ., data = meats_small) |>
      step_measure_resample(n = -5),
    "must be a positive integer >= 2"
  )

  expect_error(
    recipe(water + fat + protein ~ ., data = meats_small) |>
      step_measure_resample(n = 2.5),
    "must be a positive integer >= 2"
  )
})

test_that("step_measure_resample errors with invalid spacing", {
  expect_error(
    recipe(water + fat + protein ~ ., data = meats_small) |>
      step_measure_resample(spacing = 0),
    "must be a positive number"
  )

  expect_error(
    recipe(water + fat + protein ~ ., data = meats_small) |>
      step_measure_resample(spacing = -1),
    "must be a positive number"
  )
})

test_that("step_measure_resample errors with invalid range", {
  expect_error(
    recipe(water + fat + protein ~ ., data = meats_small) |>
      step_measure_resample(n = 50, range = c(80, 20)),
    "must have the minimum value first"
  )
})

test_that("step_measure_resample print method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_resample(n = 50)

  expect_output(print(rec$steps[[2]]), "Resample to 50 points")

  rec_prepped <- prep(rec)
  expect_output(print(rec_prepped$steps[[2]]), "Resample to 50 points")
})

test_that("step_measure_resample tidy method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_resample(n = 50, method = "spline") |>
    prep()

  tidy_result <- tidy(rec, number = 2)

  expect_s3_class(tidy_result, "tbl_df")
  expect_true("terms" %in% names(tidy_result))
  expect_true("n_points" %in% names(tidy_result))
  expect_true("method" %in% names(tidy_result))
  expect_equal(tidy_result$n_points, 50)
  expect_equal(tidy_result$method, "spline")
})

test_that("step_measure_resample works on new data", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_resample(n = 50) |>
    prep()

  # Bake on new data (first 50 rows)
  new_data <- meats_small[1:100, ]
  result <- bake(rec, new_data = new_data)

  expect_equal(nrow(result$.measures[[1]]), 50)
})

test_that("step_measure_resample learns grid from training data", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_resample(n = 50) |>
    prep()

  # The new_locations should be learned during prep
  expect_true(length(rec$steps[[2]]$new_locations) == 50)
})

# ==============================================================================
# Integration tests
# ==============================================================================

test_that("region operations can be combined", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_trim(range = c(10, 90)) |>
    step_measure_exclude(ranges = list(c(40, 50))) |>
    step_measure_resample(n = 30) |>
    prep()

  result <- bake(rec, new_data = NULL)

  # Final result should have 30 evenly spaced points
  expect_equal(nrow(result$.measures[[1]]), 30)

  # Range should be within 10-90
  locs <- result$.measures[[1]]$location
  expect_gte(min(locs), 10)
  expect_lte(max(locs), 90)
})

test_that("region operations preserve measure class", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_trim(range = c(20, 80)) |>
    prep()

  result <- bake(rec, new_data = NULL)

  expect_true(is_measure_list(result$.measures))
  expect_true(is_measure_tbl(result$.measures[[1]]))
})

test_that("region operations work with wide input", {
  # Create wide format data
  wide_data <- meats_small |>
    tidyr::pivot_wider(
      names_from = channel,
      values_from = transmittance,
      names_prefix = "x_"
    )

  rec <- recipe(water + fat + protein ~ ., data = wide_data) |>
    update_role(id, new_role = "id") |>
    step_measure_input_wide(starts_with("x_")) |>
    step_measure_trim(range = c(20, 80)) |>
    prep()

  result <- bake(rec, new_data = NULL)

  expect_true(is_measure_list(result$.measures))
  locs <- result$.measures[[1]]$location
  expect_true(all(locs >= 20))
  expect_true(all(locs <= 80))
})
