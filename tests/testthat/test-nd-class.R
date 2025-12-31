# ==============================================================================
# Tests for N-Dimensional Measurement Classes
# ==============================================================================

# ------------------------------------------------------------------------------
# measure_nd_tbl tests
# ------------------------------------------------------------------------------

test_that("new_measure_nd_tbl creates valid 2D objects", {
  m2d <- new_measure_nd_tbl(
    location_1 = rep(1:3, each = 2),
    location_2 = rep(1:2, times = 3),
    value = 1:6
  )

  expect_s3_class(m2d, "measure_nd_tbl")
  expect_s3_class(m2d, "measure_tbl") # Inherits from 1D
  expect_s3_class(m2d, "tbl_df")

  expect_equal(nrow(m2d), 6)
  expect_equal(ncol(m2d), 3)
  expect_equal(names(m2d), c("location_1", "location_2", "value"))

  expect_equal(attr(m2d, "ndim"), 2L)
})


test_that("new_measure_nd_tbl creates valid 3D objects", {
  m3d <- new_measure_nd_tbl(
    location_1 = rep(1:2, each = 6),
    location_2 = rep(rep(1:2, each = 3), 2),
    location_3 = rep(1:3, times = 4),
    value = 1:12
  )

  expect_s3_class(m3d, "measure_nd_tbl")
  expect_equal(nrow(m3d), 12)
  expect_equal(ncol(m3d), 4)
  expect_equal(names(m3d), c("location_1", "location_2", "location_3", "value"))
  expect_equal(attr(m3d, "ndim"), 3L)
})


test_that("new_measure_nd_tbl stores dimension metadata", {
  m2d <- new_measure_nd_tbl(
    location_1 = 1:5,
    location_2 = rep(1, 5),
    value = rnorm(5),
    dim_names = c("retention_time", "wavelength"),
    dim_units = c("min", "nm")
  )

  expect_equal(attr(m2d, "dim_names"), c("retention_time", "wavelength"))
  expect_equal(attr(m2d, "dim_units"), c("min", "nm"))
  expect_equal(attr(m2d, "dim_order"), c(1L, 2L))
})


test_that("new_measure_nd_tbl validates inputs", {
  # Missing names
  expect_error(
    new_measure_nd_tbl(1:3, 1:3, value = 1:3),
    "must be named"
  )

  # Invalid names
  expect_error(
    new_measure_nd_tbl(x = 1:3, y = 1:3, value = 1:3),
    "must be named.*location_1"
  )

  # Non-numeric locations
  expect_error(
    new_measure_nd_tbl(
      location_1 = letters[1:3],
      location_2 = 1:3,
      value = 1:3
    ),
    "must be numeric"
  )

  # Non-numeric value
  expect_error(
    new_measure_nd_tbl(
      location_1 = 1:3,
      location_2 = 1:3,
      value = letters[1:3]
    ),
    "must be numeric"
  )

  # Mismatched lengths
  expect_error(
    new_measure_nd_tbl(location_1 = 1:3, location_2 = 1:5, value = 1:3),
    "same length"
  )

  # Single dimension (should use new_measure_tbl)
  expect_error(
    new_measure_nd_tbl(location_1 = 1:5, value = 1:5),
    "At least 2 location dimensions"
  )

  # Wrong dim_names length
  expect_error(
    new_measure_nd_tbl(
      location_1 = 1:3,
      location_2 = 1:3,
      value = 1:3,
      dim_names = c("only_one")
    ),
    "dim_names.*must have length 2"
  )
})


test_that("is_measure_nd_tbl correctly identifies objects", {
  m2d <- new_measure_nd_tbl(
    location_1 = 1:3,
    location_2 = 1:3,
    value = 1:3
  )
  m1d <- new_measure_tbl(location = 1:3, value = 1:3)

  expect_true(is_measure_nd_tbl(m2d))
  expect_false(is_measure_nd_tbl(m1d))
  expect_false(is_measure_nd_tbl(tibble::tibble(x = 1:3)))

  # measure_nd_tbl IS a measure_tbl (inheritance)
  expect_true(is_measure_tbl(m2d))
})


test_that("measure_ndim works correctly", {
  m1d <- new_measure_tbl(location = 1:5, value = 1:5)
  m2d <- new_measure_nd_tbl(location_1 = 1:5, location_2 = 1:5, value = 1:5)
  m3d <- new_measure_nd_tbl(
    location_1 = 1:4,
    location_2 = 1:4,
    location_3 = 1:4,
    value = 1:4
  )

  expect_equal(measure_ndim(m1d), 1L)
  expect_equal(measure_ndim(m2d), 2L)
  expect_equal(measure_ndim(m3d), 3L)
})


test_that("measure_dim_names and measure_dim_units work", {
  m2d <- new_measure_nd_tbl(
    location_1 = 1:3,
    location_2 = 1:3,
    value = 1:3,
    dim_names = c("time", "wavelength"),
    dim_units = c("sec", "nm")
  )

  expect_equal(measure_dim_names(m2d), c("time", "wavelength"))
  expect_equal(measure_dim_units(m2d), c("sec", "nm"))

  # Without metadata
  m2d_plain <- new_measure_nd_tbl(
    location_1 = 1:3,
    location_2 = 1:3,
    value = 1:3
  )
  expect_null(measure_dim_names(m2d_plain))
  expect_null(measure_dim_units(m2d_plain))
})


test_that("as_measure_nd_tbl coerces data frames", {
  df <- tibble::tibble(
    location_1 = rep(1:2, each = 3),
    location_2 = rep(1:3, times = 2),
    value = 1:6
  )

  m2d <- as_measure_nd_tbl(df)
  expect_s3_class(m2d, "measure_nd_tbl")
  expect_equal(attr(m2d, "ndim"), 2L)

  # Already a measure_nd_tbl
  expect_identical(as_measure_nd_tbl(m2d), m2d)

  # Missing columns
  expect_error(
    as_measure_nd_tbl(tibble::tibble(x = 1:3)),
    "at least 2 location columns"
  )

  expect_error(
    as_measure_nd_tbl(tibble::tibble(location_1 = 1:3, location_2 = 1:3)),
    "value"
  )
})


test_that("print.measure_nd_tbl works", {
  m2d <- new_measure_nd_tbl(
    location_1 = 1:3,
    location_2 = 1:3,
    value = 1:3,
    dim_names = c("time", "wavelength")
  )

  output <- capture.output(print(m2d))
  expect_true(any(grepl("measure_nd_tbl", output, fixed = TRUE)))
  expect_true(any(grepl("time x wavelength", output, fixed = TRUE)))
})


# ------------------------------------------------------------------------------
# measure_nd_list tests
# ------------------------------------------------------------------------------

test_that("new_measure_nd_list creates valid objects", {
  m1 <- new_measure_nd_tbl(
    location_1 = 1:4,
    location_2 = rep(1:2, each = 2),
    value = rnorm(4)
  )
  m2 <- new_measure_nd_tbl(
    location_1 = 1:4,
    location_2 = rep(1:2, each = 2),
    value = rnorm(4)
  )

  ml <- new_measure_nd_list(list(m1, m2))

  expect_s3_class(ml, "measure_nd_list")
  expect_s3_class(ml, "measure_list") # Inherits from 1D
  expect_length(ml, 2)
})


test_that("new_measure_nd_list validates consistent dimensions", {
  m2d <- new_measure_nd_tbl(location_1 = 1:3, location_2 = 1:3, value = 1:3)
  m3d <- new_measure_nd_tbl(
    location_1 = 1:3,
    location_2 = 1:3,
    location_3 = 1:3,
    value = 1:3
  )

  expect_error(
    new_measure_nd_list(list(m2d, m3d)),
    "same dimensionality"
  )
})


test_that("new_measure_nd_list coerces data frames", {
  df1 <- tibble::tibble(
    location_1 = 1:3,
    location_2 = 1:3,
    value = 1:3
  )
  df2 <- tibble::tibble(
    location_1 = 4:6,
    location_2 = 1:3,
    value = 4:6
  )

  ml <- new_measure_nd_list(list(df1, df2))
  expect_s3_class(ml, "measure_nd_list")
  expect_s3_class(ml[[1]], "measure_nd_tbl")
})


test_that("empty measure_nd_list works", {
  ml <- new_measure_nd_list(list())
  expect_s3_class(ml, "measure_nd_list")
  expect_length(ml, 0)
})


test_that("is_measure_nd_list correctly identifies objects", {
  ml_nd <- new_measure_nd_list(list(
    new_measure_nd_tbl(location_1 = 1:3, location_2 = 1:3, value = 1:3)
  ))
  ml_1d <- new_measure_list(list(
    new_measure_tbl(location = 1:3, value = 1:3)
  ))

  expect_true(is_measure_nd_list(ml_nd))
  expect_false(is_measure_nd_list(ml_1d))

  # measure_nd_list IS a measure_list (inheritance)
  expect_true(is_measure_list(ml_nd))
})


test_that("measure_ndim works for lists", {
  ml_1d <- new_measure_list(list(
    new_measure_tbl(location = 1:5, value = 1:5)
  ))
  ml_2d <- new_measure_nd_list(list(
    new_measure_nd_tbl(location_1 = 1:5, location_2 = 1:5, value = 1:5)
  ))

  expect_equal(measure_ndim(ml_1d), 1L)
  expect_equal(measure_ndim(ml_2d), 2L)
})


test_that("subsetting preserves measure_nd_list class", {
  m1 <- new_measure_nd_tbl(location_1 = 1:3, location_2 = 1:3, value = 1:3)
  m2 <- new_measure_nd_tbl(location_1 = 4:6, location_2 = 1:3, value = 4:6)
  ml <- new_measure_nd_list(list(m1, m2))

  # Subset with [
  subset <- ml[1]
  expect_s3_class(subset, "measure_nd_list")
  expect_length(subset, 1)

  # Extract with [[
  element <- ml[[1]]
  expect_s3_class(element, "measure_nd_tbl")
})


test_that("concatenation preserves measure_nd_list class", {
  m1 <- new_measure_nd_tbl(location_1 = 1:3, location_2 = 1:3, value = 1:3)
  m2 <- new_measure_nd_tbl(location_1 = 4:6, location_2 = 1:3, value = 4:6)
  ml1 <- new_measure_nd_list(list(m1))
  ml2 <- new_measure_nd_list(list(m2))

  combined <- c(ml1, ml2)
  expect_s3_class(combined, "measure_nd_list")
  expect_length(combined, 2)
})


test_that("print.measure_nd_list works", {
  m <- new_measure_nd_tbl(location_1 = 1:10, location_2 = 1:10, value = 1:10)
  ml <- new_measure_nd_list(list(m, m, m))

  output <- capture.output(print(ml))
  expect_true(any(grepl("measure_nd_list", output, fixed = TRUE)))
  expect_true(any(grepl("3 measurements", output, fixed = TRUE)))
  expect_true(any(grepl("2D", output, fixed = TRUE)))
})


test_that("format.measure_nd_list produces correct abbreviations", {
  m <- new_measure_nd_tbl(location_1 = 1:5, location_2 = 1:5, value = 1:5)
  ml <- new_measure_nd_list(list(m))

  formatted <- format(ml)
  expect_true(grepl("meas2d", formatted, fixed = TRUE))
  expect_true(grepl("\\[5\\]", formatted))
})


# ------------------------------------------------------------------------------
# Grid validation tests
# ------------------------------------------------------------------------------

test_that("measure_is_regular detects regular grids", {
  # Regular 2x3 grid
  regular <- new_measure_nd_tbl(
    location_1 = rep(1:2, each = 3),
    location_2 = rep(1:3, times = 2),
    value = 1:6
  )
  expect_true(measure_is_regular(regular))

  # Irregular grid (missing combination)
  irregular <- new_measure_nd_tbl(
    location_1 = c(1, 1, 2),
    location_2 = c(1, 2, 1),
    value = 1:3
  )
  expect_false(measure_is_regular(irregular))
})


test_that("measure_grid_info returns correct information", {
  m2d <- new_measure_nd_tbl(
    location_1 = rep(c(0, 5, 10), each = 4),
    location_2 = rep(c(254, 280, 320, 350), times = 3),
    value = rnorm(12),
    dim_names = c("time", "wavelength"),
    dim_units = c("min", "nm")
  )

  info <- measure_grid_info(m2d)

  expect_equal(info$ndim, 2L)
  expect_equal(info$dim_names, c("time", "wavelength"))
  expect_equal(info$dim_units, c("min", "nm"))
  expect_equal(info$shape, c(dim_1 = 3L, dim_2 = 4L))
  expect_equal(info$n_points, 12L)
  expect_true(info$is_regular)
})


# ------------------------------------------------------------------------------
# find_measure_cols and get_measure_col_ndim tests
# ------------------------------------------------------------------------------

test_that("find_measure_cols detects both 1D and nD columns", {
  ml_1d <- new_measure_list(list(
    new_measure_tbl(location = 1:3, value = 1:3)
  ))
  ml_nd <- new_measure_nd_list(list(
    new_measure_nd_tbl(location_1 = 1:3, location_2 = 1:3, value = 1:3)
  ))

  df <- tibble::tibble(
    id = 1:2,
    spectrum_1d = c(ml_1d, ml_1d),
    spectrum_2d = c(ml_nd, ml_nd)
  )

  cols <- find_measure_cols(df)
  expect_setequal(cols, c("spectrum_1d", "spectrum_2d"))
})


test_that("find_measure_nd_cols detects only nD columns", {
  ml_1d <- new_measure_list(list(
    new_measure_tbl(location = 1:3, value = 1:3)
  ))
  ml_nd <- new_measure_nd_list(list(
    new_measure_nd_tbl(location_1 = 1:3, location_2 = 1:3, value = 1:3)
  ))

  df <- tibble::tibble(
    id = 1,
    spectrum_1d = ml_1d,
    spectrum_2d = ml_nd
  )

  nd_cols <- find_measure_nd_cols(df)
  expect_equal(nd_cols, "spectrum_2d")
})


test_that("get_measure_col_ndim returns correct dimensions", {
  ml_1d <- new_measure_list(list(
    new_measure_tbl(location = 1:3, value = 1:3)
  ))
  ml_2d <- new_measure_nd_list(list(
    new_measure_nd_tbl(location_1 = 1:3, location_2 = 1:3, value = 1:3)
  ))

  df <- tibble::tibble(
    id = 1,
    spectrum_1d = ml_1d,
    spectrum_2d = ml_2d
  )

  expect_equal(get_measure_col_ndim(df, "spectrum_1d"), 1L)
  expect_equal(get_measure_col_ndim(df, "spectrum_2d"), 2L)
})
