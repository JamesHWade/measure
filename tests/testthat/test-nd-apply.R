# ==============================================================================
# Tests for N-Dimensional Apply Dispatcher
# ==============================================================================

# Helper function: simple identity transform
identity_fn <- function(x) x

# Helper function: simple scale transform
scale_fn <- function(x, factor = 2) {
  x$value <- x$value * factor
  x
}

# Helper function: filter/trim transform
trim_fn <- function(x, min_loc = 2, max_loc = 9) {
  x[x$location >= min_loc & x$location <= max_loc, ]
}


# ------------------------------------------------------------------------------
# measure_apply on 1D data (measure_tbl)
# ------------------------------------------------------------------------------

test_that("measure_apply works on 1D measure_tbl", {
  m1d <- new_measure_tbl(location = 1:10, value = 1:10)

  result <- measure_apply(m1d, scale_fn, factor = 3)

  expect_s3_class(result, "measure_tbl")
  expect_equal(result$value, (1:10) * 3)
  expect_equal(result$location, 1:10)
})


test_that("measure_apply identity works on 1D", {
  m1d <- new_measure_tbl(location = 1:5, value = rnorm(5))

  result <- measure_apply(m1d, identity_fn)

  expect_equal(result, m1d)
})


test_that("measure_apply errors when fn returns wrong type", {
  m1d <- new_measure_tbl(location = 1:5, value = 1:5)

  bad_fn <- function(x) data.frame(a = 1:3)

  expect_error(
    measure_apply(m1d, bad_fn),
    "measure_tbl"
  )
})


# ------------------------------------------------------------------------------
# measure_apply on 2D data (measure_nd_tbl)
# ------------------------------------------------------------------------------

test_that("measure_apply works on 2D data along dimension 1", {
  # 3 time points x 4 wavelengths
  m2d <- new_measure_nd_tbl(
    location_1 = rep(1:3, each = 4), # time
    location_2 = rep(1:4, times = 3), # wavelength
    value = as.double(1:12)
  )

  result <- measure_apply(m2d, scale_fn, along = 1, factor = 2)

  expect_s3_class(result, "measure_nd_tbl")
  expect_equal(nrow(result), 12)
  expect_equal(result$value, (1:12) * 2)
})


test_that("measure_apply works on 2D data along dimension 2", {
  m2d <- new_measure_nd_tbl(
    location_1 = rep(1:3, each = 4),
    location_2 = rep(1:4, times = 3),
    value = as.double(1:12)
  )

  result <- measure_apply(m2d, scale_fn, along = 2, factor = 3)

  expect_s3_class(result, "measure_nd_tbl")
  expect_equal(result$value, (1:12) * 3)
})


test_that("measure_apply preserves dimension metadata", {
  m2d <- new_measure_nd_tbl(
    location_1 = rep(1:3, each = 4),
    location_2 = rep(1:4, times = 3),
    value = rnorm(12),
    dim_names = c("time", "wavelength"),
    dim_units = c("min", "nm")
  )

  result <- measure_apply(m2d, identity_fn, along = 1)

  expect_equal(measure_dim_names(result), c("time", "wavelength"))
  expect_equal(measure_dim_units(result), c("min", "nm"))
})


test_that("measure_apply along dimension trims correctly", {
  # Create 5 time points x 3 wavelengths
  m2d <- new_measure_nd_tbl(
    location_1 = rep(1:5, each = 3), # time: 1, 2, 3, 4, 5
    location_2 = rep(1:3, times = 5), # wavelength
    value = as.double(1:15)
  )

  # Trim to time points 2-4 only
  result <- measure_apply(m2d, trim_fn, along = 1, min_loc = 2, max_loc = 4)

  expect_s3_class(result, "measure_nd_tbl")
  # Should have 3 time points x 3 wavelengths = 9 points
  expect_equal(nrow(result), 9)
  expect_equal(sort(unique(result$location_1)), c(2, 3, 4))
})


test_that("measure_apply validates along parameter", {
  m2d <- new_measure_nd_tbl(
    location_1 = 1:5,
    location_2 = 1:5,
    value = rnorm(5)
  )

  expect_error(
    measure_apply(m2d, identity_fn, along = 3),
    "between 1 and 2"
  )

  expect_error(
    measure_apply(m2d, identity_fn, along = 0),
    "between 1 and 2"
  )

  expect_error(
    measure_apply(m2d, identity_fn, along = "first"),
    "integer vector"
  )
})


# ------------------------------------------------------------------------------
# measure_apply on 3D data
# ------------------------------------------------------------------------------

test_that("measure_apply works on 3D data", {
  # 2 x 3 x 2 grid
  m3d <- new_measure_nd_tbl(
    location_1 = rep(rep(1:2, each = 6), 1),
    location_2 = rep(rep(1:3, each = 2), 2),
    location_3 = rep(1:2, 6),
    value = as.double(1:12)
  )

  result <- measure_apply(m3d, scale_fn, along = 1, factor = 10)

  expect_s3_class(result, "measure_nd_tbl")
  expect_equal(measure_ndim(result), 3L)
  expect_equal(result$value, (1:12) * 10)
})


# ------------------------------------------------------------------------------
# measure_apply on measure_list
# ------------------------------------------------------------------------------

test_that("measure_apply works on measure_list", {
  m1 <- new_measure_tbl(location = 1:5, value = rep(1, 5))
  m2 <- new_measure_tbl(location = 1:5, value = rep(2, 5))
  ml <- new_measure_list(list(m1, m2))

  result <- measure_apply(ml, scale_fn, factor = 3)

  expect_s3_class(result, "measure_list")
  expect_length(result, 2)
  expect_equal(result[[1]]$value, rep(3, 5))
  expect_equal(result[[2]]$value, rep(6, 5))
})


# ------------------------------------------------------------------------------
# measure_apply on measure_nd_list
# ------------------------------------------------------------------------------

test_that("measure_apply works on measure_nd_list", {
  m1 <- new_measure_nd_tbl(
    location_1 = rep(1:3, each = 2),
    location_2 = rep(1:2, times = 3),
    value = rep(1, 6)
  )
  m2 <- new_measure_nd_tbl(
    location_1 = rep(1:3, each = 2),
    location_2 = rep(1:2, times = 3),
    value = rep(2, 6)
  )
  ml <- new_measure_nd_list(list(m1, m2))

  result <- measure_apply(ml, scale_fn, along = 1, factor = 5)

  expect_s3_class(result, "measure_nd_list")
  expect_length(result, 2)
  expect_equal(result[[1]]$value, rep(5, 6))
  expect_equal(result[[2]]$value, rep(10, 6))
})


# ------------------------------------------------------------------------------
# Complex transformation tests
# ------------------------------------------------------------------------------

test_that("measure_apply with cumsum-like operation", {
  # Create 2D data: 4 time points x 3 wavelengths
  m2d <- new_measure_nd_tbl(
    location_1 = rep(1:4, each = 3),
    location_2 = rep(1:3, times = 4),
    value = rep(1, 12)
  )

  # Apply cumsum along dimension 1 (time)
  cumsum_fn <- function(x) {
    x$value <- cumsum(x$value)
    x
  }

  result <- measure_apply(m2d, cumsum_fn, along = 1)

  # For each wavelength slice, cumsum of [1,1,1,1] = [1,2,3,4]
  # Result should be: for wavelength 1: 1,2,3,4; wavelength 2: 1,2,3,4; etc.
  expect_s3_class(result, "measure_nd_tbl")

  # Check a specific slice
  wl1 <- result[result$location_2 == 1, ]
  expect_equal(wl1$value[order(wl1$location_1)], c(1, 2, 3, 4))
})


test_that("measure_apply along dimension 2 with diff-like operation", {
  # Create 2D data: 3 time points x 5 wavelengths
  m2d <- new_measure_nd_tbl(
    location_1 = rep(1:3, each = 5),
    location_2 = rep(1:5, times = 3),
    value = rep(1:5, 3) # Each time slice has values 1,2,3,4,5
  )

  # Compute difference along wavelength dimension
  diff_fn <- function(x) {
    if (nrow(x) < 2) {
      return(x)
    }
    new_measure_tbl(
      location = x$location[-1],
      value = diff(x$value)
    )
  }

  result <- measure_apply(m2d, diff_fn, along = 2)

  # Each time slice had values 1,2,3,4,5 -> diff = 1,1,1,1
  # Result should have 4 wavelength points per time slice = 12 total
  expect_s3_class(result, "measure_nd_tbl")
  expect_equal(nrow(result), 12) # 3 time points x 4 diff values
  expect_equal(unique(result$value), 1) # All diffs are 1
})


# ------------------------------------------------------------------------------
# Edge cases
# ------------------------------------------------------------------------------

test_that("measure_apply handles empty result from fn", {
  m1d <- new_measure_tbl(location = 1:5, value = 1:5)

  # Function that removes all data
  empty_fn <- function(x) {
    x[x$location > 100, ] # No points match
  }

  result <- measure_apply(m1d, empty_fn)

  expect_s3_class(result, "measure_tbl")
  expect_equal(nrow(result), 0)
})


test_that("measure_apply handles single-point slices", {
  # 1 time point x 3 wavelengths
  m2d <- new_measure_nd_tbl(
    location_1 = rep(1, 3),
    location_2 = 1:3,
    value = c(10, 20, 30)
  )

  result <- measure_apply(m2d, scale_fn, along = 1, factor = 2)

  expect_s3_class(result, "measure_nd_tbl")
  expect_equal(result$value, c(20, 40, 60))
})


test_that("measure_apply works with irregular grids", {
  # Non-rectangular grid
  m2d <- new_measure_nd_tbl(
    location_1 = c(1, 1, 2, 2, 2),
    location_2 = c(1, 2, 1, 2, 3),
    value = c(1, 2, 3, 4, 5)
  )

  result <- measure_apply(m2d, scale_fn, along = 1, factor = 2)

  expect_s3_class(result, "measure_nd_tbl")
  expect_equal(result$value, c(2, 4, 6, 8, 10))
})
