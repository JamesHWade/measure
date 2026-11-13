test_that("slice selectors are strict, unambiguous, and preserve row order", {
  x <- new_measure_nd_tbl(
    location_1 = c(2, 1, 2, 1),
    location_2 = c(280, 254, 254, 280),
    value = c(20, 10, 21, 11),
    dim_names = c("time", "wavelength"),
    dim_units = c("min", "nm")
  )

  selected <- measure_slice(x, wavelength = c(254, 280), drop = FALSE)
  expect_equal(selected$value, c(20, 10, 21, 11))
  expect_equal(selected$location_1, x$location_1)
  expect_equal(selected$location_2, x$location_2)

  one_point <- measure_slice(x, dim_1 = 1, dim_2 = 280)
  expect_type(one_point, "double")
  expect_length(one_point, 1L)
  expect_equal(one_point, 11)

  integer_string <- measure_slice(x, `1` = 1, drop = FALSE)
  expect_equal(integer_string$value, c(10, 11))
  expect_error(
    measure_slice(x, 1),
    "Every slice condition must have a nonempty dimension name"
  )
  expect_error(measure_slice(x, `1.5` = 1), "Unknown dimension reference")
  expect_error(measure_slice(x, dim_1 = 1, time = 1), "same dimension")
  expect_error(measure_slice(x, dim_1 = "1"), "numeric vector")
  expect_error(measure_slice(x, dim_1 = matrix(1)), "numeric vector")
  expect_error(
    measure_slice(x, dim_1 = 1, drop = NA),
    "plain, non-missing logical"
  )
  expect_error(
    measure_slice(x, dim_1 = 1, drop = c(TRUE, FALSE)),
    "plain, non-missing logical"
  )
})


test_that("slice predicate masks cannot recycle, contain NA, or carry dimensions", {
  x <- new_measure_nd_tbl(
    location_1 = 1:4,
    location_2 = rep(1, 4),
    value = 1:4,
    dim_names = c("time", "wavelength")
  )

  expect_error(
    measure_slice(x, time = function(coord) c(TRUE, FALSE)),
    "length 4 with no missing values"
  )
  expect_error(
    measure_slice(x, time = function(coord) c(TRUE, NA, TRUE, FALSE)),
    "length 4 with no missing values"
  )
  expect_error(
    measure_slice(x, time = function(coord) matrix(TRUE, nrow = 4)),
    "length 4 with no missing values"
  )

  selected <- measure_slice(x, time = function(coord) coord > 2)
  expect_equal(selected$location, 3:4)
  expect_equal(selected$value, 3:4)
})


test_that("slice exact membership can select unknown coordinates", {
  x <- new_measure_nd_tbl(
    location_1 = c(NA_real_, 1, NA_real_),
    location_2 = c(2, 2, 3),
    value = c(10, 20, 30),
    dim_names = c("time", "wavelength")
  )

  selected <- measure_slice(x, time = NA_real_, drop = FALSE)
  expect_equal(selected$location_1, c(NA_real_, NA_real_))
  expect_equal(selected$value, c(10, 30))

  expect_warning(empty <- measure_slice(x, wavelength = 999), "no data")
  expect_s3_class(empty, "measure_nd_tbl")
  expect_equal(measure_ndim(empty), 2L)
  expect_equal(nrow(empty), 0L)
  expect_identical(measure_dim_names(empty), c("time", "wavelength"))
})


test_that("slice selectors cannot collide with semantic axis names", {
  x <- new_measure_nd_tbl(
    location_1 = 1:2,
    location_2 = c(10, 20),
    value = 1:2,
    dim_names = c("dim_2", "wavelength")
  )

  expect_error(
    measure_slice(x, dim_2 = 10),
    "ambiguous between a reserved dimension and a semantic axis name"
  )
})


test_that("nD list slices preserve a shared schema or report mixed results", {
  dad_a <- new_measure_nd_tbl(
    location_1 = rep(0:3, each = 2),
    location_2 = rep(c(254, 280), times = 4),
    value = 1:8,
    dim_names = c("time", "wavelength"),
    dim_units = c("min", "nm")
  )
  dad_b <- new_measure_nd_tbl(
    location_1 = c(0, 0, 2, 2),
    location_2 = c(280, 300, 280, 300),
    value = 11:14,
    dim_names = c("time", "wavelength"),
    dim_units = c("min", "nm")
  )
  dads <- new_measure_nd_list(list(dad_a, dad_b))

  expect_warning(
    kept <- measure_slice(dads, wavelength = 254, drop = FALSE),
    "no data"
  )
  expect_s3_class(kept, "measure_nd_list")
  expect_equal(nrow(kept[[1]]), 4L)
  expect_equal(nrow(kept[[2]]), 0L)
  expect_identical(measure_dim_names(kept[[2]]), c("time", "wavelength"))
  expect_identical(measure_dim_units(kept[[2]]), c("min", "nm"))

  expect_snapshot(error = TRUE, measure_slice(dads, wavelength = 254))
})


test_that("projection references are whole, nonempty, unique plain vectors", {
  x <- new_measure_nd_tbl(
    location_1 = rep(1:2, each = 2),
    location_2 = rep(1:2, 2),
    value = 1:4,
    dim_names = c("time", "wavelength")
  )

  expect_error(measure_project(x, along = 1.5), "whole-number dimensions")
  expect_error(measure_project(x, along = c(1, 1)), "duplicate dimensions")
  expect_error(measure_project(x, along = integer()), "whole-number dimensions")
  expect_error(measure_project(x, along = NA_real_), "whole-number dimensions")
  expect_error(measure_project(x, along = Inf), "whole-number dimensions")
  expect_error(
    measure_project(x, along = matrix(1)),
    "plain numeric or character vector"
  )
  expect_error(
    measure_project(x, along = TRUE),
    "plain numeric or character vector"
  )
  expect_error(measure_project(x, along = "dim_1"), "Unknown dimension name")
  expect_error(measure_project(x, along = 3), "between 1 and 2")
  expect_error(measure_project(x, along = 2, fn = 1), "fn.*function")
  expect_error(
    measure_project(x, along = 2, na_rm = NA),
    "plain, non-missing logical"
  )
  expect_error(
    measure_project(x, along = 2, na_rm = c(TRUE, FALSE)),
    "plain, non-missing logical"
  )
})


test_that("projection validates one plain numeric scalar for each group", {
  x <- new_measure_nd_tbl(
    location_1 = rep(1:2, each = 2),
    location_2 = rep(1:2, 2),
    value = 1:4
  )

  expect_error(
    measure_project(x, along = 2, fn = function(x) c(1, 2)),
    "one plain numeric scalar"
  )
  expect_error(
    measure_project(x, along = 2, fn = function(x) matrix(1)),
    "one plain numeric scalar"
  )
  expect_error(
    measure_project(x, along = 2, fn = function(x) list(1)),
    "one plain numeric scalar"
  )
  expect_error(
    measure_project(x, along = 2, fn = function(x) TRUE),
    "one plain numeric scalar"
  )
  expect_error(
    measure_project(
      new_measure_nd_tbl(
        location_1 = c(1, 1, 2),
        location_2 = 1:3,
        value = 1:3
      ),
      along = 2,
      fn = function(x) {
        if (length(x) == 2L) mean(x) else c(1, 2)
      }
    ),
    "one plain numeric scalar"
  )
})


test_that("projection preserves ordinary missingness and coordinate grouping", {
  x <- new_measure_nd_tbl(
    location_1 = c(2, 1, 2, 1, NA_real_),
    location_2 = c(1, 1, 2, 2, 1),
    value = c(20, 10, 21, 11, 30)
  )

  sorted <- measure_project(x, along = 2)
  expect_equal(sorted$location, c(1, 2, NA_real_))
  expect_equal(sorted$value, c(10.5, 20.5, 30))

  missing <- new_measure_nd_tbl(
    location_1 = rep(1:2, each = 2),
    location_2 = 1:4,
    value = c(NA_real_, NA_real_, NA_real_, 2)
  )
  means <- measure_project(missing, along = 2)
  sums <- measure_project(missing, along = 2, fn = sum)
  expect_true(is.nan(means$value[[1]]))
  expect_equal(means$value[[2]], 2)
  expect_equal(sums$value, c(0, 2))
  all_missing <- new_measure_nd_tbl(
    location_1 = 1:2,
    location_2 = 1:2,
    value = c(NA_real_, NA_real_)
  )
  expect_true(is.nan(measure_project(all_missing, along = c(1, 2))))
  expect_equal(measure_project(all_missing, along = c(1, 2), fn = sum), 0)
  expect_equal(measure_project(missing, along = c(1, 2), fn = sum), 2)
})


test_that("nD list projections retain homogeneous result types and schemas", {
  first <- new_measure_nd_tbl(
    location_1 = rep(1:2, each = 2),
    location_2 = rep(254:255, 2),
    value = 1:4,
    dim_names = c("time", "wavelength"),
    dim_units = c("min", "nm")
  )
  second <- new_measure_nd_tbl(
    location_1 = rep(1:2, each = 2),
    location_2 = rep(254:255, 2),
    value = 5:8,
    dim_names = c("time", "wavelength"),
    dim_units = c("min", "nm")
  )
  projected <- measure_project(
    new_measure_nd_list(list(first, second)),
    along = "wavelength"
  )
  expect_s3_class(projected, "measure_list")
  expect_equal(projected[[1]]$value, c(1.5, 3.5))
  expect_equal(projected[[2]]$value, c(5.5, 7.5))

  cube_a <- new_measure_nd_tbl(
    location_1 = rep(1:2, each = 4),
    location_2 = rep(rep(1:2, each = 2), 2),
    location_3 = rep(1:2, 4),
    value = 1:8,
    dim_names = c("time", "wavelength", "temperature"),
    dim_units = c("min", "nm", "C")
  )
  cube_b <- new_measure_nd_tbl(
    location_1 = rep(1:2, each = 4),
    location_2 = rep(rep(1:2, each = 2), 2),
    location_3 = rep(1:2, 4),
    value = 11:18,
    dim_names = c("time", "mass_to_charge", "temperature"),
    dim_units = c("min", "Da", "C")
  )
  expect_snapshot(
    error = TRUE,
    measure_project(new_measure_nd_list(list(cube_a, cube_b)), along = 1)
  )
})


test_that("nD list projections validate retained 1D axis schemas", {
  different_traces <- list(
    new_measure_nd_tbl(
      location_1 = rep(1:2, each = 2),
      location_2 = rep(1:2, 2),
      value = 1:4,
      dim_names = c("time", "wavelength"),
      dim_units = c("min", "nm")
    ),
    new_measure_nd_tbl(
      location_1 = rep(1:2, each = 2),
      location_2 = rep(1:2, 2),
      value = 5:8,
      dim_names = c("sample", "wavelength"),
      dim_units = c("replicate", "nm")
    )
  )
  expect_snapshot(
    error = TRUE,
    measure_project(new_measure_nd_list(different_traces), along = 2)
  )
})
test_that("reserved dimension tokens match the entire selector", {
  x <- new_measure_nd_tbl(
    location_1 = 1:2,
    location_2 = 3:4,
    value = 5:6,
    dim_names = c("time", "wavelength")
  )
  bad_reserved <- stats::setNames(list(1), "dim_1\n")
  expect_snapshot(
    error = TRUE,
    do.call(measure_slice, c(list(x), bad_reserved))
  )
  bad_numeric <- stats::setNames(list(1), "1\n")
  expect_snapshot(
    error = TRUE,
    do.call(measure_slice, c(list(x), bad_numeric))
  )

  attr(x, "dim_names") <- c("time", "dim_1\n")
  exact_semantic <- stats::setNames(list(3), "dim_1\n")
  expect_identical(do.call(measure_slice, c(list(x), exact_semantic)), 5L)
})

test_that("list slices do not flatten repeated observations into scalar results", {
  repeated <- new_measure_nd_tbl(
    location_1 = c(1, 1),
    location_2 = c(254, 254),
    value = c(2, 3)
  )
  single <- new_measure_nd_tbl(location_1 = 2, location_2 = 254, value = 4)
  pair <- new_measure_nd_list(list(repeated, single))
  expect_identical(measure_slice(repeated, dim_2 = 254), c(2, 3))
  expect_snapshot(error = TRUE, measure_slice(pair, dim_2 = 254))
  retained <- measure_slice(pair, dim_2 = 254, drop = FALSE)
  expect_identical(retained[[1]]$value, c(2, 3))
  expect_identical(retained[[2]]$value, 4)
})
