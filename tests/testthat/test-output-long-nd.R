# ==============================================================================
# Tests for N-Dimensional Long Output
# ==============================================================================

# Create test data for 2D measurements
create_2d_test_data <- function() {
  tibble::tibble(
    sample_id = rep(1:2, each = 12),
    time = rep(rep(c(0, 5, 10), each = 4), 2),
    wavelength = rep(c(254, 280, 320, 350), 6),
    absorbance = rnorm(24),
    concentration = rep(c(10, 20), each = 12)
  )
}


# ------------------------------------------------------------------------------
# Basic 2D output tests
# ------------------------------------------------------------------------------

test_that("step_measure_output_long works with 2D data", {
  data <- create_2d_test_data()

  rec <- recipes::recipe(concentration ~ ., data = data) |>
    recipes::update_role(sample_id, new_role = "id") |>
    step_measure_input_long(
      absorbance,
      location = dplyr::vars(time, wavelength)
    ) |>
    step_measure_output_long() |>
    recipes::prep()

  result <- recipes::bake(rec, new_data = NULL)

  # Should have the expected columns
  expect_true(".measure" %in% names(result))
  expect_true(".location_1" %in% names(result))
  expect_true(".location_2" %in% names(result))

  # Should not have the nested measure column
  expect_false(".measures" %in% names(result))

  # Should have correct number of rows (2 samples * 12 points each)
  expect_equal(nrow(result), 24)
})


test_that("2D output preserves data values correctly", {
  data <- create_2d_test_data()

  rec <- recipes::recipe(concentration ~ ., data = data) |>
    recipes::update_role(sample_id, new_role = "id") |>
    step_measure_input_long(
      absorbance,
      location = dplyr::vars(time, wavelength)
    ) |>
    step_measure_output_long() |>
    recipes::prep()

  result <- recipes::bake(rec, new_data = NULL)

  # Check that location values are preserved
  expect_equal(sort(unique(result$.location_1)), c(0, 5, 10))
  expect_equal(sort(unique(result$.location_2)), c(254, 280, 320, 350))
})


test_that("2D output respects values_to and location_to parameters", {
  data <- create_2d_test_data()

  rec <- recipes::recipe(concentration ~ ., data = data) |>
    recipes::update_role(sample_id, new_role = "id") |>
    step_measure_input_long(
      absorbance,
      location = dplyr::vars(time, wavelength)
    ) |>
    step_measure_output_long(
      values_to = "intensity",
      location_to = "coord"
    ) |>
    recipes::prep()

  result <- recipes::bake(rec, new_data = NULL)

  expect_true("intensity" %in% names(result))
  expect_true("coord_1" %in% names(result))
  expect_true("coord_2" %in% names(result))
})


test_that("2D output with custom measure column name", {
  data <- create_2d_test_data()

  rec <- recipes::recipe(concentration ~ ., data = data) |>
    recipes::update_role(sample_id, new_role = "id") |>
    step_measure_input_long(
      absorbance,
      location = dplyr::vars(time, wavelength),
      col_name = ".spectra"
    ) |>
    step_measure_output_long(measures = ".spectra") |>
    recipes::prep()

  result <- recipes::bake(rec, new_data = NULL)

  expect_true(".measure" %in% names(result))
  expect_true(".location_1" %in% names(result))
  expect_true(".location_2" %in% names(result))
  expect_false(".spectra" %in% names(result))
})


# ------------------------------------------------------------------------------
# 3D output tests
# ------------------------------------------------------------------------------

test_that("step_measure_output_long works with 3D data", {
  data <- tibble::tibble(
    sample_id = rep(1:2, each = 12),
    dim1 = rep(rep(1:2, each = 6), 2),
    dim2 = rep(rep(1:3, each = 2), 4),
    dim3 = rep(1:2, 12),
    value = rnorm(24),
    outcome = rep(c("A", "B"), each = 12)
  )

  rec <- recipes::recipe(outcome ~ ., data = data) |>
    recipes::update_role(sample_id, new_role = "id") |>
    step_measure_input_long(
      value,
      location = dplyr::vars(dim1, dim2, dim3)
    ) |>
    step_measure_output_long() |>
    recipes::prep()

  result <- recipes::bake(rec, new_data = NULL)

  expect_true(".measure" %in% names(result))
  expect_true(".location_1" %in% names(result))
  expect_true(".location_2" %in% names(result))
  expect_true(".location_3" %in% names(result))
  expect_equal(nrow(result), 24)
})


# ------------------------------------------------------------------------------
# Backward compatibility with 1D
# ------------------------------------------------------------------------------

test_that("1D output still works correctly", {
  data <- tibble::tibble(
    sample_id = rep(1:3, each = 5),
    channel = rep(1:5, 3),
    transmittance = rnorm(15),
    water = rep(c(10, 20, 30), each = 5)
  )

  rec <- recipes::recipe(water ~ ., data = data) |>
    recipes::update_role(sample_id, new_role = "id") |>
    step_measure_input_long(
      transmittance,
      location = dplyr::vars(channel)
    ) |>
    step_measure_output_long() |>
    recipes::prep()

  result <- recipes::bake(rec, new_data = NULL)

  # 1D should use single .location column (not .location_1)
  expect_true(".measure" %in% names(result))
  expect_true(".location" %in% names(result))
  expect_false(".location_1" %in% names(result))
  expect_equal(nrow(result), 15)
})


# ------------------------------------------------------------------------------
# Round-trip tests (input -> output)
# ------------------------------------------------------------------------------

test_that("2D input -> output preserves all original data", {
  set.seed(42)
  original <- create_2d_test_data()

  rec <- recipes::recipe(concentration ~ ., data = original) |>
    recipes::update_role(sample_id, new_role = "id") |>
    step_measure_input_long(
      absorbance,
      location = dplyr::vars(time, wavelength)
    ) |>
    step_measure_output_long(
      values_to = "absorbance",
      location_to = ".loc"
    ) |>
    recipes::prep()

  result <- recipes::bake(rec, new_data = NULL)

  # Check that we have the same number of rows
  expect_equal(nrow(result), nrow(original))

  # Check that we have the same sample_id values
  expect_setequal(unique(result$sample_id), unique(original$sample_id))

  # Check that we have the same concentration values (per sample)
  orig_conc <- unique(original[, c("sample_id", "concentration")])
  result_conc <- unique(result[, c("sample_id", "concentration")])
  expect_equal(
    orig_conc[order(orig_conc$sample_id), ],
    result_conc[order(result_conc$sample_id), ],
    ignore_attr = TRUE
  )
})


# ------------------------------------------------------------------------------
# Edge cases
# ------------------------------------------------------------------------------

test_that("2D output handles irregular grids", {
  data <- tibble::tibble(
    sample_id = rep(1:2, c(5, 6)),
    time = c(0, 1, 2, 3, 4, 0, 2, 4, 6, 8, 10),
    wavelength = c(rep(254, 5), rep(280, 6)),
    intensity = rnorm(11),
    outcome = rep(c(1, 2), c(5, 6))
  )

  rec <- recipes::recipe(outcome ~ ., data = data) |>
    recipes::update_role(sample_id, new_role = "id") |>
    step_measure_input_long(
      intensity,
      location = dplyr::vars(time, wavelength)
    ) |>
    step_measure_output_long() |>
    recipes::prep()

  result <- recipes::bake(rec, new_data = NULL)

  # Should still work with different sample sizes
  expect_equal(nrow(result), 11)
})


test_that("print works for output step", {
  data <- create_2d_test_data()

  rec <- recipes::recipe(concentration ~ ., data = data) |>
    recipes::update_role(sample_id, new_role = "id") |>
    step_measure_input_long(
      absorbance,
      location = dplyr::vars(time, wavelength)
    ) |>
    step_measure_output_long() |>
    recipes::prep()

  # Print should not error
  step <- rec$steps[[2]]
  expect_no_error(print(step))
})
