# Test axis validation utilities

test_that("validate_measure works with measure_tbl", {
  spec <- new_measure_tbl(location = 1:100, value = sin(1:100 / 10))

  # Should pass without errors
 expect_silent(validate_measure(spec, action = "message"))

  result <- validate_measure(spec, action = "message")
  expect_true(result$monotonic$valid)
  expect_true(result$duplicates$valid)
  expect_true(result$missing$valid)
  expect_true(result$spacing$valid)
})

test_that("validate_measure works with measure_list", {
  specs <- new_measure_list(list(
    new_measure_tbl(location = 1:10, value = rnorm(10)),
    new_measure_tbl(location = 1:10, value = rnorm(10))
  ))

  expect_silent(validate_measure(specs, action = "message"))
})

test_that("check_monotonic detects non-monotonic axes", {
  # Non-monotonic data
  spec <- new_measure_tbl(
    location = c(1, 2, 3, 2, 4, 5),
    value = 1:6
  )

  expect_error(validate_measure(spec), "monotonic")
})

test_that("check_no_duplicates detects duplicate locations", {
  spec <- new_measure_tbl(
    location = c(1, 2, 2, 3, 4),
    value = 1:5
  )

  expect_error(validate_measure(spec), "Duplicate")
})

test_that("check_no_missing detects NA values", {
  spec <- new_measure_tbl(
    location = c(1, 2, 3, NA, 5),
    value = c(1, 2, NA, 4, 5)
  )

  expect_error(validate_measure(spec), "Missing")
})

test_that("check_regular_spacing detects irregular spacing", {
  spec <- new_measure_tbl(
    location = c(1, 2, 3, 5, 6, 7),  # Gap at 4
    value = 1:6
  )

  expect_error(validate_measure(spec), "Irregular")
})

test_that("validate_measure respects action parameter", {
  spec <- new_measure_tbl(location = c(1, 2, 2, 3), value = 1:4)

  expect_error(validate_measure(spec, action = "error"))
  expect_warning(validate_measure(spec, action = "warn"))
  expect_message(validate_measure(spec, action = "message"))
})

test_that("validate_measure allows selecting specific checks", {
  spec <- new_measure_tbl(location = c(1, 2, 2, 3), value = 1:4)

  # Only check monotonic (should pass)
  result <- validate_measure(spec, checks = "monotonic", action = "message")
  expect_true(result$monotonic$valid)
  expect_null(result$duplicates)

  # Only check duplicates (should fail)
  expect_error(
    validate_measure(spec, checks = "duplicates", action = "error"),
    "Duplicate"
  )
})

# measure_axis_info tests

test_that("measure_axis_info extracts correct information", {
  spec <- new_measure_tbl(
    location = seq(1000, 2500, by = 2),
    value = rnorm(751)
  )

  info <- measure_axis_info(spec)

  expect_equal(info$min, 1000)
  expect_equal(info$max, 2500)
  expect_equal(info$n_points, 751)
  expect_equal(info$spacing, 2)
  expect_equal(info$direction, "increasing")
  expect_true(info$regular)
})

test_that("measure_axis_info detects decreasing axis", {
  spec <- new_measure_tbl(
    location = seq(4000, 400, by = -4),
    value = rnorm(901)
  )

  info <- measure_axis_info(spec)
  expect_equal(info$direction, "decreasing")
})

test_that("measure_axis_info works with measure_list", {
  specs <- new_measure_list(list(
    new_measure_tbl(location = 1:10, value = rnorm(10)),
    new_measure_tbl(location = 1:10, value = rnorm(10))
  ))

  info <- measure_axis_info(specs, sample = 2)
  expect_equal(info$n_points, 10)
})

# infer_axis_type tests

test_that("infer_axis_type identifies wavelength", {
  expect_equal(infer_axis_type(seq(1000, 2500, by = 2)), "wavelength_nm")
  expect_equal(infer_axis_type(seq(400, 700, by = 1)), "wavelength_nm")
})

test_that("infer_axis_type identifies wavenumber", {
  expect_equal(infer_axis_type(seq(4000, 400, by = -4)), "wavenumber")
  expect_equal(infer_axis_type(seq(400, 4000, by = 4)), "wavenumber")
})

test_that("infer_axis_type identifies retention time", {
  expect_equal(infer_axis_type(seq(0, 30, by = 0.01)), "retention_time")
  expect_equal(infer_axis_type(seq(0, 60, by = 0.1)), "retention_time")
})

test_that("infer_axis_type identifies ppm", {
  expect_equal(infer_axis_type(seq(0, 12, by = 0.001)), "ppm")
  expect_equal(infer_axis_type(seq(-1, 10, by = 0.01)), "ppm")
})

test_that("infer_axis_type handles edge cases", {
  expect_equal(infer_axis_type(numeric(0)), "unknown")
  expect_equal(infer_axis_type(c(NA, NA)), "unknown")
})

# check_axis_consistency tests

test_that("check_axis_consistency passes for consistent axes", {
  specs <- new_measure_list(list(
    new_measure_tbl(location = 1:10, value = rnorm(10)),
    new_measure_tbl(location = 1:10, value = rnorm(10)),
    new_measure_tbl(location = 1:10, value = rnorm(10))
  ))

  result <- check_axis_consistency(specs, action = "message")
  expect_true(result$consistent)
  expect_equal(length(result$inconsistent_samples), 0)
})

test_that("check_axis_consistency detects different lengths", {
  specs <- new_measure_list(list(
    new_measure_tbl(location = 1:10, value = rnorm(10)),
    new_measure_tbl(location = 1:11, value = rnorm(11))
  ))

  expect_error(check_axis_consistency(specs, action = "error"), "inconsistent")
})

test_that("check_axis_consistency detects different values", {
  specs <- new_measure_list(list(
    new_measure_tbl(location = 1:10, value = rnorm(10)),
    new_measure_tbl(location = 2:11, value = rnorm(10))
  ))

  expect_error(check_axis_consistency(specs, action = "error"), "inconsistent")
})

test_that("check_axis_consistency respects tolerance", {
  loc1 <- seq(0, 1, length.out = 100)
  loc2 <- loc1 + 1e-15  # Tiny floating point difference

  specs <- new_measure_list(list(
    new_measure_tbl(location = loc1, value = rnorm(100)),
    new_measure_tbl(location = loc2, value = rnorm(100))
  ))

  # Should pass with default tolerance
  result <- check_axis_consistency(specs, tolerance = 1e-10, action = "message")
  expect_true(result$consistent)
})

# measure_quality_summary tests

test_that("measure_quality_summary produces output", {
  specs <- new_measure_list(list(
    new_measure_tbl(location = seq(1000, 2500, by = 2), value = rnorm(751)),
    new_measure_tbl(location = seq(1000, 2500, by = 2), value = rnorm(751))
  ))

  result <- measure_quality_summary(specs, verbose = FALSE)

  expect_equal(result$n_samples, 2)
  expect_equal(result$axis_info$n_points, 751)
  expect_true(result$consistency$consistent)
})

test_that("measure_quality_summary works with single sample",
{
  spec <- new_measure_tbl(location = 1:100, value = rnorm(100))

  result <- measure_quality_summary(spec, verbose = FALSE)

  expect_equal(result$n_samples, 1)
  expect_true(result$consistency$consistent)
})

# Additional edge case tests

test_that("validate_measure works with data frame input", {
  specs <- new_measure_list(list(
    new_measure_tbl(location = 1:10, value = rnorm(10))
  ))
  df <- tibble::tibble(.measures = specs)

  expect_silent(validate_measure(df, action = "message"))
})

test_that("validate_measure errors on data frame without measure column", {
  df <- tibble::tibble(x = 1:10, y = rnorm(10))

  expect_error(validate_measure(df), "No measure column found")
})

test_that("validate_measure errors on invalid input type", {
  expect_error(validate_measure(1:10), "must be")
  expect_error(validate_measure("string"), "must be")
})

test_that("measure_axis_info handles empty measure_tbl", {
  empty_spec <- new_measure_tbl(location = numeric(0), value = numeric(0))

  info <- measure_axis_info(empty_spec)

  expect_true(is.na(info$min) || is.infinite(info$min))
  expect_true(is.na(info$max) || is.infinite(info$max))
  expect_equal(info$n_points, 0L)
  expect_true(is.na(info$spacing))
  expect_true(is.na(info$axis_type))
})

test_that("validation handles single-element measure data", {
  single <- new_measure_tbl(location = 1, value = 0.5)

  # Should pass all checks (nothing to validate with single point)
  result <- validate_measure(single, action = "message")
  expect_true(result$monotonic$valid)
  expect_true(result$duplicates$valid)
  expect_true(result$missing$valid)
  expect_true(result$spacing$valid)

  # Axis info should handle single point
  info <- measure_axis_info(single)
  expect_equal(info$n_points, 1)
  expect_true(is.na(info$direction))
  expect_true(info$regular)
})

test_that("validation handles two-element measure data", {
  two_pts <- new_measure_tbl(location = c(1, 2), value = c(0.5, 0.6))

  result <- validate_measure(two_pts, action = "message")
  expect_true(result$monotonic$valid)
  expect_true(result$spacing$valid)

  info <- measure_axis_info(two_pts)
  expect_equal(info$direction, "increasing")
  expect_true(info$regular)
})

test_that("check_axis_consistency handles single sample", {
  single <- new_measure_list(list(
    new_measure_tbl(location = 1:10, value = rnorm(10))
  ))

  result <- check_axis_consistency(single, action = "message")
  expect_true(result$consistent)
  expect_equal(result$reference_locations, 1:10)
  expect_equal(length(result$inconsistent_samples), 0)
})

test_that("measure_axis_info errors on invalid sample index", {
  specs <- new_measure_list(list(
    new_measure_tbl(location = 1:10, value = rnorm(10))
  ))

  expect_error(measure_axis_info(specs, sample = 5), "does not exist")
})

test_that("measure_quality_summary errors on empty data", {
  empty_list <- new_measure_list(list())

  expect_error(measure_quality_summary(empty_list), "empty")
})

test_that("check_regular_spacing handles zero median spacing", {
  # All identical locations - median diff is zero
  spec <- new_measure_tbl(
    location = rep(5, 10),
    value = 1:10
  )

  # Should detect as having duplicates (not crash from division by zero)
  expect_error(validate_measure(spec), "Duplicate")
})

test_that("measure_axis_info handles constant locations", {
  # All same location values
  spec <- new_measure_tbl(
    location = rep(100, 5),
    value = 1:5
  )

  # Should not crash - should handle zero spacing gracefully
  info <- measure_axis_info(spec)
  expect_equal(info$n_points, 5)
  expect_equal(info$spacing, 0)
})
