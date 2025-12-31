# ==============================================================================
# Tests for N-Dimensional Long Input
# ==============================================================================

# Create test data for 2D measurements (e.g., LC-DAD or EEM)
create_2d_test_data <- function() {
  # 2 samples, each with a 3x4 grid (time x wavelength)
  tibble::tibble(
    sample_id = rep(1:2, each = 12),
    time = rep(rep(c(0, 5, 10), each = 4), 2),
    wavelength = rep(c(254, 280, 320, 350), 6),
    absorbance = rnorm(24),
    concentration = rep(c(10, 20), each = 12)
  )
}

# Create test data for 3D measurements
create_3d_test_data <- function() {
  # 2 samples, each with a 2x3x2 grid
  tibble::tibble(
    sample_id = rep(1:2, each = 12),
    dim1 = rep(rep(1:2, each = 6), 2),
    dim2 = rep(rep(1:3, each = 2), 4),
    dim3 = rep(1:2, 12),
    value = rnorm(24),
    outcome = rep(c("A", "B"), each = 12)
  )
}

# Create irregular grid test data
create_irregular_test_data <- function() {
  tibble::tibble(
    sample_id = rep(1:2, c(5, 6)),
    time = c(0, 1, 2, 3, 4, 0, 2, 4, 6, 8, 10), # Different grid sizes
    wavelength = c(rep(254, 5), rep(280, 6)),
    intensity = rnorm(11),
    concentration = rep(c(1, 2), c(5, 6))
  )
}


# ------------------------------------------------------------------------------
# Basic 2D input tests
# ------------------------------------------------------------------------------

test_that("step_measure_input_long creates 2D measure_nd_list", {
  data <- create_2d_test_data()

  rec <- recipes::recipe(concentration ~ ., data = data) |>
    recipes::update_role(sample_id, new_role = "id") |>
    step_measure_input_long(
      absorbance,
      location = dplyr::vars(time, wavelength)
    ) |>
    recipes::prep()

  result <- recipes::bake(rec, new_data = NULL)

  expect_s3_class(result$.measures, "measure_nd_list")
  expect_length(result$.measures, 2) # 2 samples
  expect_s3_class(result$.measures[[1]], "measure_nd_tbl")
  expect_equal(measure_ndim(result$.measures[[1]]), 2L)
})


test_that("2D input preserves data values correctly", {
  data <- create_2d_test_data()

  rec <- recipes::recipe(concentration ~ ., data = data) |>
    recipes::update_role(sample_id, new_role = "id") |>
    step_measure_input_long(
      absorbance,
      location = dplyr::vars(time, wavelength)
    ) |>
    recipes::prep()

  result <- recipes::bake(rec, new_data = NULL)

  # Check first sample
  m1 <- result$.measures[[1]]
  expect_equal(nrow(m1), 12) # 3 times x 4 wavelengths
  expect_equal(names(m1), c("location_1", "location_2", "value"))
  expect_equal(sort(unique(m1$location_1)), c(0, 5, 10)) # times
  expect_equal(sort(unique(m1$location_2)), c(254, 280, 320, 350)) # wavelengths
})


test_that("2D input accepts dim_names and dim_units", {
  data <- create_2d_test_data()

  rec <- recipes::recipe(concentration ~ ., data = data) |>
    recipes::update_role(sample_id, new_role = "id") |>
    step_measure_input_long(
      absorbance,
      location = dplyr::vars(time, wavelength),
      dim_names = c("retention_time", "wavelength"),
      dim_units = c("min", "nm")
    ) |>
    recipes::prep()

  result <- recipes::bake(rec, new_data = NULL)

  m1 <- result$.measures[[1]]
  expect_equal(measure_dim_names(m1), c("retention_time", "wavelength"))
  expect_equal(measure_dim_units(m1), c("min", "nm"))
})


test_that("2D input validates dim_names length", {
  data <- create_2d_test_data()

  expect_error(
    recipes::recipe(concentration ~ ., data = data) |>
      recipes::update_role(sample_id, new_role = "id") |>
      step_measure_input_long(
        absorbance,
        location = dplyr::vars(time, wavelength),
        dim_names = c("only_one") # Wrong length
      ) |>
      recipes::prep(),
    "dim_names"
  )
})


test_that("2D input validates dim_units length", {
  data <- create_2d_test_data()

  expect_error(
    recipes::recipe(concentration ~ ., data = data) |>
      recipes::update_role(sample_id, new_role = "id") |>
      step_measure_input_long(
        absorbance,
        location = dplyr::vars(time, wavelength),
        dim_units = c("min", "nm", "extra") # Wrong length
      ) |>
      recipes::prep(),
    "dim_units"
  )
})


# ------------------------------------------------------------------------------
# 3D input tests
# ------------------------------------------------------------------------------

test_that("step_measure_input_long creates 3D measure_nd_list", {
  data <- create_3d_test_data()

  rec <- recipes::recipe(outcome ~ ., data = data) |>
    recipes::update_role(sample_id, new_role = "id") |>
    step_measure_input_long(
      value,
      location = dplyr::vars(dim1, dim2, dim3)
    ) |>
    recipes::prep()

  result <- recipes::bake(rec, new_data = NULL)

  expect_s3_class(result$.measures, "measure_nd_list")
  expect_length(result$.measures, 2)
  expect_equal(measure_ndim(result$.measures[[1]]), 3L)
  expect_equal(nrow(result$.measures[[1]]), 12) # 2 x 3 x 2
})


test_that("3D input names dimensions correctly", {
  data <- create_3d_test_data()

  rec <- recipes::recipe(outcome ~ ., data = data) |>
    recipes::update_role(sample_id, new_role = "id") |>
    step_measure_input_long(
      value,
      location = dplyr::vars(dim1, dim2, dim3)
    ) |>
    recipes::prep()

  result <- recipes::bake(rec, new_data = NULL)

  m1 <- result$.measures[[1]]
  expect_equal(names(m1), c("location_1", "location_2", "location_3", "value"))
})


# ------------------------------------------------------------------------------
# Custom column name tests
# ------------------------------------------------------------------------------

test_that("2D input respects col_name parameter", {
  data <- create_2d_test_data()

  rec <- recipes::recipe(concentration ~ ., data = data) |>
    recipes::update_role(sample_id, new_role = "id") |>
    step_measure_input_long(
      absorbance,
      location = dplyr::vars(time, wavelength),
      col_name = ".spectra_2d"
    ) |>
    recipes::prep()

  result <- recipes::bake(rec, new_data = NULL)

  expect_true(".spectra_2d" %in% names(result))
  expect_s3_class(result$.spectra_2d, "measure_nd_list")
})


# ------------------------------------------------------------------------------
# Irregular grid tests
# ------------------------------------------------------------------------------

test_that("2D input handles irregular grids", {
  data <- create_irregular_test_data()

  rec <- recipes::recipe(concentration ~ ., data = data) |>
    recipes::update_role(sample_id, new_role = "id") |>
    step_measure_input_long(
      intensity,
      location = dplyr::vars(time, wavelength)
    ) |>
    recipes::prep()

  result <- recipes::bake(rec, new_data = NULL)

  # Should work despite different sample sizes

  expect_s3_class(result$.measures, "measure_nd_list")
  expect_length(result$.measures, 2)
  expect_equal(nrow(result$.measures[[1]]), 5)
  expect_equal(nrow(result$.measures[[2]]), 6)
})


# ------------------------------------------------------------------------------
# Grid regularity detection
# ------------------------------------------------------------------------------

test_that("regular grid is detected correctly", {
  data <- create_2d_test_data()

  rec <- recipes::recipe(concentration ~ ., data = data) |>
    recipes::update_role(sample_id, new_role = "id") |>
    step_measure_input_long(
      absorbance,
      location = dplyr::vars(time, wavelength)
    ) |>
    recipes::prep()

  result <- recipes::bake(rec, new_data = NULL)

  expect_true(measure_is_regular(result$.measures[[1]]))
  expect_true(measure_is_regular(result$.measures))
})


test_that("irregular grid is detected correctly", {
  # Create data with missing combinations
  data <- tibble::tibble(
    sample_id = rep(1, 5),
    time = c(0, 0, 5, 5, 10), # Missing (10, 254)
    wavelength = c(254, 280, 254, 280, 280),
    intensity = rnorm(5),
    outcome = 1
  )

  rec <- recipes::recipe(outcome ~ ., data = data) |>
    recipes::update_role(sample_id, new_role = "id") |>
    step_measure_input_long(
      intensity,
      location = dplyr::vars(time, wavelength)
    ) |>
    recipes::prep()

  result <- recipes::bake(rec, new_data = NULL)

  expect_false(measure_is_regular(result$.measures[[1]]))
})


# ------------------------------------------------------------------------------
# Grid info extraction
# ------------------------------------------------------------------------------

test_that("measure_grid_info returns correct information", {
  data <- create_2d_test_data()

  rec <- recipes::recipe(concentration ~ ., data = data) |>
    recipes::update_role(sample_id, new_role = "id") |>
    step_measure_input_long(
      absorbance,
      location = dplyr::vars(time, wavelength),
      dim_names = c("time", "wavelength"),
      dim_units = c("min", "nm")
    ) |>
    recipes::prep()

  result <- recipes::bake(rec, new_data = NULL)

  info <- measure_grid_info(result$.measures[[1]])

  expect_equal(info$ndim, 2L)
  expect_equal(info$dim_names, c("time", "wavelength"))
  expect_equal(info$dim_units, c("min", "nm"))
  expect_equal(info$shape, c(dim_1 = 3L, dim_2 = 4L))
  expect_equal(info$n_points, 12L)
  expect_true(info$is_regular)
})


# ------------------------------------------------------------------------------
# Backward compatibility with 1D
# ------------------------------------------------------------------------------

test_that("single location column still creates 1D measure_list", {
  # Use existing meats_long data pattern
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
    recipes::prep()

  result <- recipes::bake(rec, new_data = NULL)

  # Should be regular measure_list, not measure_nd_list
  expect_s3_class(result$.measures, "measure_list")
  expect_false(inherits(result$.measures, "measure_nd_list"))
  expect_s3_class(result$.measures[[1]], "measure_tbl")
})


# ------------------------------------------------------------------------------
# Tidy method tests
# ------------------------------------------------------------------------------

test_that("tidy works for 2D input step", {
  data <- create_2d_test_data()

  rec <- recipes::recipe(concentration ~ ., data = data) |>
    recipes::update_role(sample_id, new_role = "id") |>
    step_measure_input_long(
      absorbance,
      location = dplyr::vars(time, wavelength)
    ) |>
    recipes::prep()

  tidy_result <- recipes::tidy(rec, number = 1)

  expect_s3_class(tidy_result, "tbl_df")
  expect_true("terms" %in% names(tidy_result))
  expect_true("absorbance" %in% tidy_result$terms)
  expect_true("time" %in% tidy_result$terms)
  expect_true("wavelength" %in% tidy_result$terms)
})


# ------------------------------------------------------------------------------
# Print method tests
# ------------------------------------------------------------------------------

test_that("print works for 2D input step", {
  data <- create_2d_test_data()

  rec <- recipes::recipe(concentration ~ ., data = data) |>
    recipes::update_role(sample_id, new_role = "id") |>
    step_measure_input_long(
      absorbance,
      location = dplyr::vars(time, wavelength)
    ) |>
    recipes::prep()

  # Verify the step has proper columns stored indicating 2D
  step <- rec$steps[[1]]
  expect_equal(length(step$columns), 3) # absorbance, time, wavelength
  expect_equal(step$columns[1], "absorbance")
  expect_true("time" %in% step$columns)
  expect_true("wavelength" %in% step$columns)

  # Print should not error
  expect_no_error(print(step))
})


# ------------------------------------------------------------------------------
# Edge cases
# ------------------------------------------------------------------------------

test_that("2D input handles NA values in measurements", {
  data <- create_2d_test_data()
  data$absorbance[5] <- NA

  rec <- recipes::recipe(concentration ~ ., data = data) |>
    recipes::update_role(sample_id, new_role = "id") |>
    step_measure_input_long(
      absorbance,
      location = dplyr::vars(time, wavelength)
    ) |>
    recipes::prep()

  result <- recipes::bake(rec, new_data = NULL)

  # Should contain the NA
  expect_true(anyNA(result$.measures[[1]]$value))

  # Grid info should report NA
  info <- measure_grid_info(result$.measures[[1]])
  expect_true(info$has_na)
})


test_that("2D input rejects non-numeric location columns", {
  data <- tibble::tibble(
    sample_id = rep(1:2, each = 4),
    time = rep(c("early", "late"), 4), # Character, not numeric
    wavelength = rep(1:2, 4),
    intensity = rnorm(8),
    outcome = rep(1:2, each = 4)
  )

  expect_error(
    recipes::recipe(outcome ~ ., data = data) |>
      recipes::update_role(sample_id, new_role = "id") |>
      step_measure_input_long(
        intensity,
        location = dplyr::vars(time, wavelength)
      ) |>
      recipes::prep(),
    "numeric|double|integer"
  )
})
