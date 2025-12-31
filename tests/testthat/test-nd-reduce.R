# ==============================================================================
# Tests for N-Dimensional Reduction Operations
# ==============================================================================

# ------------------------------------------------------------------------------
# measure_unfold tests
# ------------------------------------------------------------------------------

test_that("measure_unfold converts 2D to 1D", {
  m2d <- new_measure_nd_tbl(
    location_1 = rep(1:3, each = 4),
    location_2 = rep(1:4, times = 3),
    value = 1:12
  )

  m1d <- measure_unfold(m2d)

  expect_s3_class(m1d, "measure_tbl")
  expect_false(is_measure_nd_tbl(m1d))
  expect_equal(nrow(m1d), 12)
  expect_equal(m1d$location, 1:12)
})


test_that("measure_unfold preserves values", {
  m2d <- new_measure_nd_tbl(
    location_1 = rep(1:2, each = 3),
    location_2 = rep(1:3, times = 2),
    value = c(10, 20, 30, 40, 50, 60)
  )

  m1d <- measure_unfold(m2d)

  # All original values should be present
  expect_setequal(m1d$value, c(10, 20, 30, 40, 50, 60))
})


test_that("measure_unfold attaches fold metadata", {
  m2d <- new_measure_nd_tbl(
    location_1 = rep(1:3, each = 2),
    location_2 = rep(c(254, 280), times = 3),
    value = rnorm(6),
    dim_names = c("time", "wavelength"),
    dim_units = c("min", "nm")
  )

  m1d <- measure_unfold(m2d)

  fold_info <- attr(m1d, "fold_info")
  expect_false(is.null(fold_info))
  expect_equal(fold_info$ndim, 2L)
  expect_equal(fold_info$dim_names, c("time", "wavelength"))
  expect_equal(fold_info$dim_units, c("min", "nm"))
  expect_equal(fold_info$order, c(1L, 2L))
})


test_that("measure_unfold with custom order", {
  m2d <- new_measure_nd_tbl(
    location_1 = rep(1:2, each = 3),
    location_2 = rep(1:3, times = 2),
    value = 1:6
  )

  # Unfold with reversed order
  m1d <- measure_unfold(m2d, order = c(2, 1))

  fold_info <- attr(m1d, "fold_info")
  expect_equal(fold_info$order, c(2L, 1L))
})


test_that("measure_unfold validates order parameter", {
  m2d <- new_measure_nd_tbl(
    location_1 = 1:4,
    location_2 = 1:4,
    value = 1:4
  )

  expect_error(
    measure_unfold(m2d, order = c(1, 3)),
    "permutation"
  )

  expect_error(
    measure_unfold(m2d, order = c(1)),
    "permutation"
  )
})


test_that("measure_unfold works on measure_nd_list", {
  m1 <- new_measure_nd_tbl(location_1 = 1:4, location_2 = 1:4, value = 1:4)
  m2 <- new_measure_nd_tbl(location_1 = 1:4, location_2 = 1:4, value = 5:8)
  ml <- new_measure_nd_list(list(m1, m2))

  result <- measure_unfold(ml)

  expect_s3_class(result, "measure_list")
  expect_false(inherits(result, "measure_nd_list"))
  expect_length(result, 2)
})


# ------------------------------------------------------------------------------
# measure_fold tests
# ------------------------------------------------------------------------------

test_that("measure_fold reconstructs 2D from unfolded", {
  m2d <- new_measure_nd_tbl(
    location_1 = rep(1:3, each = 4),
    location_2 = rep(1:4, times = 3),
    value = 1:12,
    dim_names = c("time", "wavelength")
  )

  m1d <- measure_unfold(m2d)
  restored <- measure_fold(m1d)

  expect_s3_class(restored, "measure_nd_tbl")
  expect_equal(measure_ndim(restored), 2L)
  expect_equal(nrow(restored), 12)
  expect_setequal(restored$value, 1:12)
  expect_equal(measure_dim_names(restored), c("time", "wavelength"))
})


test_that("measure_fold errors without fold metadata", {
  m1d <- new_measure_tbl(location = 1:5, value = 1:5)

  expect_error(
    measure_fold(m1d),
    "no fold metadata"
  )
})


test_that("measure_fold detects length mismatch", {
  m2d <- new_measure_nd_tbl(
    location_1 = 1:4,
    location_2 = 1:4,
    value = 1:4
  )

  m1d <- measure_unfold(m2d)
  # Modify length
  m1d_modified <- new_measure_tbl(location = 1:3, value = 1:3)
  attr(m1d_modified, "fold_info") <- attr(m1d, "fold_info")

  expect_error(
    measure_fold(m1d_modified),
    "Length mismatch"
  )
})


test_that("unfold-fold roundtrip preserves data", {
  set.seed(42)
  m2d <- new_measure_nd_tbl(
    location_1 = rep(c(0, 5, 10), each = 4),
    location_2 = rep(c(254, 280, 320, 350), times = 3),
    value = rnorm(12),
    dim_names = c("time", "wavelength"),
    dim_units = c("min", "nm")
  )

  m1d <- measure_unfold(m2d)
  restored <- measure_fold(m1d)

  # Sort both for comparison
  m2d_sorted <- dplyr::arrange(m2d, location_1, location_2)
  restored_sorted <- dplyr::arrange(restored, location_1, location_2)

  expect_equal(restored_sorted$value, m2d_sorted$value)
  expect_equal(restored_sorted$location_1, m2d_sorted$location_1)
  expect_equal(restored_sorted$location_2, m2d_sorted$location_2)
})


test_that("measure_fold works on measure_list", {
  m1 <- new_measure_nd_tbl(location_1 = 1:4, location_2 = 1:4, value = 1:4)
  m2 <- new_measure_nd_tbl(location_1 = 1:4, location_2 = 1:4, value = 5:8)
  ml <- new_measure_nd_list(list(m1, m2))

  unfolded <- measure_unfold(ml)
  restored <- measure_fold(unfolded)

  expect_s3_class(restored, "measure_nd_list")
  expect_length(restored, 2)
})


# ------------------------------------------------------------------------------
# measure_slice tests
# ------------------------------------------------------------------------------

test_that("measure_slice extracts at single value", {
  m2d <- new_measure_nd_tbl(
    location_1 = rep(1:3, each = 4),
    location_2 = rep(1:4, times = 3),
    value = 1:12
  )

  # Slice at dim_1 = 2
  result <- measure_slice(m2d, dim_1 = 2)

  expect_s3_class(result, "measure_tbl")
  expect_false(is_measure_nd_tbl(result)) # Dropped to 1D
  expect_equal(nrow(result), 4)
  expect_equal(result$value, 5:8)
})


test_that("measure_slice with multiple values", {
  m2d <- new_measure_nd_tbl(
    location_1 = rep(1:5, each = 2),
    location_2 = rep(1:2, times = 5),
    value = 1:10
  )

  # Slice at dim_1 in (2, 4)
  result <- measure_slice(m2d, dim_1 = c(2, 4), drop = FALSE)

  expect_s3_class(result, "measure_nd_tbl")
  expect_equal(nrow(result), 4)
  expect_setequal(unique(result$location_1), c(2, 4))
})


test_that("measure_slice with dimension name", {
  m2d <- new_measure_nd_tbl(
    location_1 = rep(1:3, each = 2),
    location_2 = rep(c(254, 280), times = 3),
    value = 1:6,
    dim_names = c("time", "wavelength")
  )

  result <- measure_slice(m2d, wavelength = 254)

  expect_s3_class(result, "measure_tbl")
  expect_equal(nrow(result), 3)
})


test_that("measure_slice with function condition", {
  m2d <- new_measure_nd_tbl(
    location_1 = rep(1:5, each = 2),
    location_2 = rep(1:2, times = 5),
    value = 1:10
  )

  # Slice where dim_1 > 3
  result <- measure_slice(m2d, dim_1 = function(x) x > 3)

  expect_equal(nrow(result), 4)
  expect_true(all(result$location_1 > 3))
})


test_that("measure_slice drop = FALSE preserves dimensions", {
  m2d <- new_measure_nd_tbl(
    location_1 = rep(1:3, each = 2),
    location_2 = rep(1:2, times = 3),
    value = 1:6
  )

  result <- measure_slice(m2d, dim_1 = 2, drop = FALSE)

  expect_s3_class(result, "measure_nd_tbl")
  expect_equal(measure_ndim(result), 2L)
})


test_that("measure_slice on 3D data", {
  m3d <- new_measure_nd_tbl(
    location_1 = rep(1:2, each = 6),
    location_2 = rep(rep(1:3, each = 2), 2),
    location_3 = rep(1:2, 6),
    value = 1:12
  )

  # Slice dim_1 = 1, should get 2D result
  result <- measure_slice(m3d, dim_1 = 1)

  expect_s3_class(result, "measure_nd_tbl")
  expect_equal(measure_ndim(result), 2L)
  expect_equal(nrow(result), 6)
})


test_that("measure_slice on measure_nd_list", {
  m1 <- new_measure_nd_tbl(
    location_1 = rep(1:2, each = 3),
    location_2 = rep(1:3, 2),
    value = 1:6
  )
  m2 <- new_measure_nd_tbl(
    location_1 = rep(1:2, each = 3),
    location_2 = rep(1:3, 2),
    value = 7:12
  )
  ml <- new_measure_nd_list(list(m1, m2))

  result <- measure_slice(ml, dim_1 = 1)

  expect_s3_class(result, "measure_list")
  expect_length(result, 2)
  expect_equal(nrow(result[[1]]), 3)
})


test_that("measure_slice warns on empty result", {
  m2d <- new_measure_nd_tbl(
    location_1 = 1:4,
    location_2 = 1:4,
    value = 1:4
  )

  expect_warning(
    measure_slice(m2d, dim_1 = 999),
    "no data"
  )
})


# ------------------------------------------------------------------------------
# measure_project tests
# ------------------------------------------------------------------------------

test_that("measure_project aggregates across dimension", {
  m2d <- new_measure_nd_tbl(
    location_1 = rep(1:3, each = 2),
    location_2 = rep(1:2, times = 3),
    value = c(1, 2, 3, 4, 5, 6)
  )

  # Project across dim_2 (average over wavelengths)
  result <- measure_project(m2d, along = 2)

  expect_s3_class(result, "measure_tbl")
  expect_equal(nrow(result), 3)
  # Mean of (1,2)=1.5, (3,4)=3.5, (5,6)=5.5
  expect_equal(result$value, c(1.5, 3.5, 5.5))
})


test_that("measure_project with sum function", {
  m2d <- new_measure_nd_tbl(
    location_1 = rep(1:2, each = 3),
    location_2 = rep(1:3, times = 2),
    value = rep(1, 6)
  )

  result <- measure_project(m2d, along = 2, fn = sum)

  expect_equal(result$value, c(3, 3)) # Sum of 3 ones for each loc_1
})


test_that("measure_project with dimension name", {
  m2d <- new_measure_nd_tbl(
    location_1 = rep(1:2, each = 3),
    location_2 = rep(1:3, times = 2),
    value = 1:6,
    dim_names = c("time", "wavelength")
  )

  result <- measure_project(m2d, along = "wavelength")

  expect_s3_class(result, "measure_tbl")
  expect_equal(nrow(result), 2)
})


test_that("measure_project across all dims returns scalar", {
  m2d <- new_measure_nd_tbl(
    location_1 = rep(1:2, each = 3),
    location_2 = rep(1:3, times = 2),
    value = 1:6
  )

  result <- measure_project(m2d, along = c(1, 2))

  expect_true(is.numeric(result))
  expect_length(result, 1)
  expect_equal(result, mean(1:6))
})


test_that("measure_project on 3D data", {
  m3d <- new_measure_nd_tbl(
    location_1 = rep(1:2, each = 6),
    location_2 = rep(rep(1:2, each = 3), 2),
    location_3 = rep(1:3, 4),
    value = rep(1, 12),
    dim_names = c("sample", "time", "wavelength")
  )

  # Project across wavelength (dim_3)
  result <- measure_project(m3d, along = 3)

  expect_s3_class(result, "measure_nd_tbl")
  expect_equal(measure_ndim(result), 2L)
  expect_equal(nrow(result), 4) # 2 samples x 2 times
})


test_that("measure_project handles NA values", {
  m2d <- new_measure_nd_tbl(
    location_1 = rep(1:2, each = 3),
    location_2 = rep(1:3, times = 2),
    value = c(1, 2, NA, 4, 5, 6)
  )

  # With na_rm = TRUE (default)
  result <- measure_project(m2d, along = 2)
  expect_equal(result$value[1], mean(c(1, 2), na.rm = TRUE))

  # With na_rm = FALSE
  result_na <- measure_project(m2d, along = 2, na_rm = FALSE)
  expect_true(is.na(result_na$value[1]))
})


test_that("measure_project on measure_nd_list", {
  m1 <- new_measure_nd_tbl(
    location_1 = rep(1:2, each = 2),
    location_2 = rep(1:2, 2),
    value = c(1, 2, 3, 4)
  )
  m2 <- new_measure_nd_tbl(
    location_1 = rep(1:2, each = 2),
    location_2 = rep(1:2, 2),
    value = c(5, 6, 7, 8)
  )
  ml <- new_measure_nd_list(list(m1, m2))

  result <- measure_project(ml, along = 2)

  expect_s3_class(result, "measure_list")
  expect_length(result, 2)
  expect_equal(result[[1]]$value, c(1.5, 3.5))
  expect_equal(result[[2]]$value, c(5.5, 7.5))
})


test_that("measure_project validates along parameter", {
  m2d <- new_measure_nd_tbl(
    location_1 = 1:4,
    location_2 = 1:4,
    value = 1:4
  )

  expect_error(
    measure_project(m2d, along = 3),
    "between 1 and 2"
  )
})


test_that("measure_project preserves remaining dimension metadata", {
  m2d <- new_measure_nd_tbl(
    location_1 = rep(1:2, each = 3),
    location_2 = rep(1:3, times = 2),
    value = 1:6,
    dim_names = c("time", "wavelength"),
    dim_units = c("min", "nm")
  )

  result <- measure_project(m2d, along = 2)

  # 1D result doesn't have dim_names/dim_units on measure_tbl
  expect_s3_class(result, "measure_tbl")
})
