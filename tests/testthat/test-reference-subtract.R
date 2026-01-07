# ==============================================================================
# Tests for reference-based corrections
#
# This file tests:
# - step_measure_subtract_blank
# - step_measure_subtract_reference
# - step_measure_ratio_reference
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

# Get the number of locations in test data
get_n_locations <- function() {
  test_data <- create_test_data()
  length(test_data$.measures[[1]]$location)
}

# ==============================================================================
# step_measure_subtract_blank tests
# ==============================================================================

test_that("step_measure_subtract_blank works with external numeric blank", {
  test_data <- create_test_data()
  n_locs <- get_n_locations()

  # Create external blank (all zeros - no change)
  blank <- rep(0, n_locs)

  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_subtract_blank(blank = blank) |>
    prep()

  result <- bake(rec, new_data = NULL)

  # Values should be unchanged (subtracting zero)
  original <- test_data$.measures[[1]]$value
  corrected <- result$.measures[[1]]$value
  expect_equal(corrected, original)
})

test_that("step_measure_subtract_blank actually subtracts values", {
  test_data <- create_test_data()
  n_locs <- get_n_locations()

  # Create blank with value 0.1
  blank <- rep(0.1, n_locs)

  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_subtract_blank(blank = blank) |>
    prep()

  result <- bake(rec, new_data = NULL)

  # Values should be reduced by 0.1
  original <- test_data$.measures[[1]]$value
  corrected <- result$.measures[[1]]$value
  expect_equal(corrected, original - 0.1, tolerance = 1e-10)
})

test_that("step_measure_subtract_blank divide method works", {
  test_data <- create_test_data()
  n_locs <- get_n_locations()

  # Create blank with value 2.0 (will divide)
  blank <- rep(2.0, n_locs)

  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_subtract_blank(blank = blank, method = "divide") |>
    prep()

  result <- bake(rec, new_data = NULL)

  # Values should be halved
  original <- test_data$.measures[[1]]$value
  corrected <- result$.measures[[1]]$value
  expect_equal(corrected, original / 2.0, tolerance = 1e-10)
})

test_that("step_measure_subtract_blank errors without blank or blank_col", {
  expect_error(
    recipe(water + fat + protein ~ ., data = meats_small) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_measure_subtract_blank() |>
      prep(),
    "blank.*blank_col"
  )
})

test_that("step_measure_subtract_blank errors with wrong blank length", {
  expect_error(
    recipe(water + fat + protein ~ ., data = meats_small) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_measure_subtract_blank(blank = c(1, 2, 3)) |> # Wrong length
      prep(),
    "length"
  )
})

test_that("step_measure_subtract_blank errors with invalid method", {
  n_locs <- get_n_locations()
  blank <- rep(0.1, n_locs)

  expect_error(
    recipe(water + fat + protein ~ ., data = meats_small) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_measure_subtract_blank(blank = blank, method = "invalid") |>
      prep(),
    "subtract.*divide"
  )
})

test_that("step_measure_subtract_blank preserves locations", {
  test_data <- create_test_data()
  n_locs <- get_n_locations()
  blank <- rep(0.1, n_locs)

  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_subtract_blank(blank = blank) |>
    prep()

  result <- bake(rec, new_data = NULL)

  # Locations should be unchanged
  original_locs <- test_data$.measures[[1]]$location
  result_locs <- result$.measures[[1]]$location
  expect_equal(result_locs, original_locs)
})

test_that("step_measure_subtract_blank print method works", {
  n_locs <- get_n_locations()
  blank <- rep(0.1, n_locs)

  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_subtract_blank(blank = blank)

  # Before training
  expect_output(print(rec$steps[[2]]), "Blank subtraction")

  # After training
  rec_prepped <- prep(rec)
  expect_output(print(rec_prepped$steps[[2]]), "external")
})

test_that("step_measure_subtract_blank tidy method works", {
  n_locs <- get_n_locations()
  blank <- rep(0.1, n_locs)

  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_subtract_blank(blank = blank) |>
    prep()

  tidy_result <- tidy(rec, number = 2)

  expect_s3_class(tidy_result, "tbl_df")
  expect_true("terms" %in% names(tidy_result))
  expect_true("method" %in% names(tidy_result))
  expect_true("blank_source" %in% names(tidy_result))
  expect_equal(tidy_result$blank_source, "external")
})

test_that("step_measure_subtract_blank works without input step error", {
  expect_error(
    recipe(water + fat + protein ~ ., data = meats_small) |>
      update_role(id, new_role = "id") |>
      step_measure_subtract_blank(blank = rep(0.1, 100)) |>
      prep(),
    "measures"
  )
})

# ==============================================================================
# step_measure_subtract_reference tests
# ==============================================================================

test_that("step_measure_subtract_reference works with numeric reference", {
  test_data <- create_test_data()
  n_locs <- get_n_locations()

  ref <- rep(0.5, n_locs)

  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_subtract_reference(reference = ref) |>
    prep()

  result <- bake(rec, new_data = NULL)

  original <- test_data$.measures[[1]]$value
  corrected <- result$.measures[[1]]$value
  expect_equal(corrected, original - 0.5, tolerance = 1e-10)
})

test_that("step_measure_subtract_reference divide method works", {
  test_data <- create_test_data()
  n_locs <- get_n_locations()

  ref <- rep(2.0, n_locs)

  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_subtract_reference(reference = ref, method = "divide") |>
    prep()

  result <- bake(rec, new_data = NULL)

  original <- test_data$.measures[[1]]$value
  corrected <- result$.measures[[1]]$value
  expect_equal(corrected, original / 2.0, tolerance = 1e-10)
})

test_that("step_measure_subtract_reference requires reference argument", {
  expect_error(
    recipe(water + fat + protein ~ ., data = meats_small) |>
      step_measure_subtract_reference(),
    "reference.*required"
  )
})

test_that("step_measure_subtract_reference print method works", {
  n_locs <- get_n_locations()
  ref <- rep(1.0, n_locs)

  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_subtract_reference(reference = ref, method = "divide") |>
    prep()

  expect_output(print(rec$steps[[2]]), "Reference division")
})

test_that("step_measure_subtract_reference tidy method works", {
  n_locs <- get_n_locations()
  ref <- rep(1.0, n_locs)

  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_subtract_reference(reference = ref) |>
    prep()

  tidy_result <- tidy(rec, number = 2)

  expect_s3_class(tidy_result, "tbl_df")
  expect_true("terms" %in% names(tidy_result))
  expect_true("method" %in% names(tidy_result))
})

# ==============================================================================
# step_measure_ratio_reference tests
# ==============================================================================

test_that("step_measure_ratio_reference works without blank", {
  test_data <- create_test_data()
  n_locs <- get_n_locations()

  ref <- rep(2.0, n_locs)

  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_ratio_reference(reference = ref) |>
    prep()

  result <- bake(rec, new_data = NULL)

  # Should divide by reference
  original <- test_data$.measures[[1]]$value
  corrected <- result$.measures[[1]]$value
  expect_equal(corrected, original / 2.0, tolerance = 1e-10)
})

test_that("step_measure_ratio_reference works with blank", {
  test_data <- create_test_data()
  n_locs <- get_n_locations()

  ref <- rep(4.0, n_locs)
  blank <- rep(2.0, n_locs)

  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_ratio_reference(reference = ref, blank = blank) |>
    prep()

  result <- bake(rec, new_data = NULL)

  # Should compute (sample - blank) / (ref - blank)
  # = (sample - 2) / (4 - 2) = (sample - 2) / 2
  original <- test_data$.measures[[1]]$value
  corrected <- result$.measures[[1]]$value
  expected <- (original - 2.0) / (4.0 - 2.0)
  expect_equal(corrected, expected, tolerance = 1e-10)
})

test_that("step_measure_ratio_reference requires reference argument", {
  expect_error(
    recipe(water + fat + protein ~ ., data = meats_small) |>
      step_measure_ratio_reference(),
    "reference.*required"
  )
})

test_that("step_measure_ratio_reference print method works", {
  n_locs <- get_n_locations()
  ref <- rep(2.0, n_locs)
  blank <- rep(0.1, n_locs)

  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_ratio_reference(reference = ref, blank = blank) |>
    prep()

  expect_output(print(rec$steps[[2]]), "with blank")
})

test_that("step_measure_ratio_reference tidy method works", {
  n_locs <- get_n_locations()
  ref <- rep(2.0, n_locs)

  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_ratio_reference(reference = ref) |>
    prep()

  tidy_result <- tidy(rec, number = 2)

  expect_s3_class(tidy_result, "tbl_df")
  expect_true("has_blank" %in% names(tidy_result))
  expect_false(tidy_result$has_blank)
})

# ==============================================================================
# Integration tests
# ==============================================================================

test_that("reference steps work in a pipeline", {
  n_locs <- get_n_locations()
  blank <- rep(0.1, n_locs)
  ref <- rep(1.0, n_locs)

  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_subtract_blank(blank = blank) |>
    step_measure_subtract_reference(reference = ref, method = "divide") |>
    prep()

  result <- bake(rec, new_data = NULL)

  expect_s3_class(result, "tbl_df")
  expect_true(".measures" %in% names(result))
})

test_that("reference steps work with wide input format", {
  test_data_wide <- meats_small |>
    tidyr::pivot_wider(
      names_from = channel,
      names_prefix = "x_",
      values_from = transmittance
    )

  # Get number of measurement columns
  meas_cols <- grep("^x_", names(test_data_wide), value = TRUE)
  n_locs <- length(meas_cols)

  blank <- rep(0.1, n_locs)

  rec <- recipe(water + fat + protein ~ ., data = test_data_wide) |>
    update_role(id, new_role = "id") |>
    step_measure_input_wide(starts_with("x_")) |>
    step_measure_subtract_blank(blank = blank) |>
    prep()

  result <- bake(rec, new_data = NULL)

  expect_s3_class(result, "tbl_df")
  expect_true(".measures" %in% names(result))
})

test_that("required_pkgs methods work", {
  n_locs <- get_n_locations()
  blank <- rep(0.1, n_locs)
  ref <- rep(1.0, n_locs)

  rec1 <- recipe(water + fat + protein ~ ., data = meats_small) |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_subtract_blank(blank = blank)

  rec2 <- recipe(water + fat + protein ~ ., data = meats_small) |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_subtract_reference(reference = ref)

  rec3 <- recipe(water + fat + protein ~ ., data = meats_small) |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_ratio_reference(reference = ref)

  expect_true("measure" %in% required_pkgs(rec1$steps[[2]]))
  expect_true("measure" %in% required_pkgs(rec2$steps[[2]]))
  expect_true("measure" %in% required_pkgs(rec3$steps[[2]]))
})
