test_that("nD constructors canonicalize dimensions and preserve numeric payloads", {
  m2d <- new_measure_nd_tbl(
    location_2 = c(2L, 2L, 3L),
    location_1 = c(7L, 7L, 8L),
    value = c(10L, NA_integer_, 30L),
    dim_names = c("time", "wavelength"),
    dim_units = c("min", "nm")
  )

  expect_identical(names(m2d), c("location_1", "location_2", "value"))
  expect_identical(m2d$location_1, c(7L, 7L, 8L))
  expect_identical(m2d$location_2, c(2L, 2L, 3L))
  expect_identical(m2d$value, c(10L, NA_integer_, 30L))
  expect_identical(attr(m2d, "dim_order"), c(1L, 2L))
  expect_identical(
    withVisible(new_measure_nd_tbl(
      location_1 = integer(),
      location_2 = double(),
      value = double()
    ))$visible,
    TRUE
  )

  m3d <- new_measure_nd_tbl(
    location_3 = c(Inf, NA_real_),
    location_1 = c(4L, 2L),
    location_2 = c(5L, 1L),
    value = c(NA_real_, 8),
    dim_names = c("sample", NA_character_, "wavelength"),
    dim_units = c("min", "min", NA_character_)
  )

  expect_identical(
    names(m3d),
    c("location_1", "location_2", "location_3", "value")
  )
  expect_identical(m3d$location_1, c(4L, 2L))
  expect_identical(m3d$location_2, c(5L, 1L))
  expect_identical(m3d$location_3, c(Inf, NA_real_))
  expect_identical(m3d$value, c(NA_real_, 8))
  expect_identical(
    measure_dim_names(m3d),
    c("sample", NA_character_, "wavelength")
  )
  expect_identical(measure_dim_units(m3d), c("min", "min", NA_character_))

  empty <- new_measure_nd_tbl(
    location_2 = integer(),
    location_1 = double(),
    value = integer()
  )
  expect_identical(names(empty), c("location_1", "location_2", "value"))
  expect_identical(empty$location_1, double())
  expect_identical(empty$location_2, integer())
  expect_identical(empty$value, integer())
  expect_identical(nrow(empty), 0L)

  ml <- new_measure_nd_list(list(m2d, m2d))
  expect_s3_class(ml, "measure_nd_list")
  expect_identical(length(ml), 2L)
  expect_identical(ml[[1]], m2d)
  expect_identical(measure_ndim(ml), 2L)

  ml3d <- new_measure_nd_list(list(m3d, m3d))
  ptype3d <- attr(ml3d, "ptype")
  expect_identical(
    names(ptype3d),
    c(
      "location_1",
      "location_2",
      "location_3",
      "value"
    )
  )
  expect_identical(attr(ptype3d, "ndim"), 3L)
  expect_identical(attr(ptype3d, "dim_order"), c(1L, 2L, 3L))
  expect_false(withVisible(.nd_validate_tbl(ptype3d))$visible)

  empty_list <- new_measure_nd_list()
  expect_s3_class(empty_list, "measure_nd_list")
  expect_identical(length(empty_list), 0L)
  expect_identical(measure_ndim(empty_list), 2L)
})

test_that("nD metadata accepts unknown names and repeated units", {
  x <- new_measure_nd_tbl(
    location_1 = 1:2,
    location_2 = c(10, 20),
    value = c(3, 4),
    dim_names = c("time", NA_character_),
    dim_units = c("min", "min")
  )

  expect_identical(measure_dim_names(x), c("time", NA_character_))
  expect_identical(measure_dim_units(x), c("min", "min"))
  expect_identical(.nd_validate_tbl(x), x)
  expect_false(withVisible(.nd_validate_tbl(x))$visible)
})

test_that("nD constructors reject noncanonical location suffixes", {
  expect_snapshot(
    error = TRUE,
    new_measure_nd_tbl(
      location_1 = 1:2,
      location_3 = 1:2,
      value = 1:2
    )
  )
  expect_snapshot(
    error = TRUE,
    new_measure_nd_tbl(
      location_0 = 1:2,
      location_1 = 1:2,
      value = 1:2
    )
  )
  expect_snapshot(
    error = TRUE,
    new_measure_nd_tbl(
      location_1 = 1:2,
      location_999999999999999999999999999999999999 = 1:2,
      value = 1:2
    )
  )

  duplicate_names <- structure(
    list(1:2, 1:2, 1:2),
    names = c("location_1", "location_1", "value")
  )
  expect_snapshot(error = TRUE, do.call(new_measure_nd_tbl, duplicate_names))
})

test_that("nD constructors require plain numeric vectors and dimension metadata", {
  expect_snapshot(
    error = TRUE,
    new_measure_nd_tbl(
      location_1 = matrix(1:2),
      location_2 = 1:2,
      value = 1:2
    )
  )
  expect_snapshot(
    error = TRUE,
    new_measure_nd_tbl(
      location_1 = as.Date(c("2020-01-01", "2020-01-02")),
      location_2 = 1:2,
      value = 1:2
    )
  )
  expect_snapshot(
    error = TRUE,
    new_measure_nd_tbl(
      location_1 = 1:2,
      location_2 = 1:2,
      value = 1:2,
      dim_names = c("time", "time")
    )
  )
  expect_snapshot(
    error = TRUE,
    new_measure_nd_tbl(
      location_1 = 1:2,
      location_2 = 1:2,
      value = 1:2,
      dim_names = c("time", " ")
    )
  )
  expect_snapshot(
    error = TRUE,
    new_measure_nd_tbl(
      location_1 = 1:2,
      location_2 = 1:2,
      value = 1:2,
      dim_units = 1:2
    )
  )
  expect_snapshot(
    error = TRUE,
    new_measure_nd_tbl(
      location_1 = 1:2,
      location_2 = 1:2,
      value = 1:2,
      dim_units = matrix(c("min", "nm"), nrow = 1)
    )
  )
})

test_that("nD mutable structures are checked at list and grid entry points", {
  x <- new_measure_nd_tbl(
    location_1 = c(1L, 1L, 2L),
    location_2 = c(1L, 2L, 1L),
    value = c(1, NA_real_, 3),
    dim_names = c("time", NA_character_),
    dim_units = c("min", "min")
  )

  bad_columns <- x
  names(bad_columns)[2] <- "location_3"
  expect_true(is_measure_nd_tbl(bad_columns))
  expect_snapshot(error = TRUE, .nd_validate_tbl(bad_columns))
  expect_snapshot(error = TRUE, measure_grid_info(bad_columns))

  bad_list <- structure(
    list(bad_columns),
    class = c("measure_nd_list", "measure_list")
  )
  bad_ndim <- x
  attr(bad_ndim, "ndim") <- .Machine$integer.max

  bad_order <- x
  attr(bad_order, "dim_order") <- c(2L, 1L)
  expect_snapshot(error = TRUE, new_measure_nd_list(list(bad_order)))

  bad_value <- x
  bad_value$value <- factor(c("a", "b", "c"))
  expect_snapshot(error = TRUE, new_measure_nd_list(list(bad_value)))

  bad_names <- x
  attr(bad_names, "dim_names") <- c("time", "time")
  expect_snapshot(error = TRUE, measure_grid_info(bad_names))

  bad_units <- x
  attr(bad_units, "dim_units") <- c(1, 2)
  expect_snapshot(error = TRUE, measure_grid_info(bad_units))

  expect_snapshot(error = TRUE, measure_grid_info(bad_list))
  expect_snapshot(error = TRUE, .nd_validate_tbl(bad_ndim))
})

test_that("nD semantic metadata is unclassed character data", {
  expect_snapshot(
    error = TRUE,
    new_measure_nd_tbl(
      location_1 = 1:2,
      location_2 = 3:4,
      value = 5:6,
      dim_names = I(c("time", "wavelength"))
    )
  )
})

test_that("nD grid inspection keeps unknown coordinates visible", {
  with_unknown_coordinate <- new_measure_nd_tbl(
    location_1 = c(1, NA_real_),
    location_2 = c(10, 10),
    value = c(2, 4)
  )

  expect_false(measure_is_regular(with_unknown_coordinate))
  info <- measure_grid_info(with_unknown_coordinate)
  expect_identical(info$unique_values[[1]], c(1, NA_real_))
  expect_identical(unname(info$shape), c(2L, 1L))
  expect_identical(info$n_points, 2L)
  expect_false(info$is_regular)
  expect_false(info$has_na)

  with_missing_signal <- new_measure_nd_tbl(
    location_1 = rep(1:2, each = 2),
    location_2 = rep(1:2, times = 2),
    value = c(1, NA_real_, 3, 4)
  )
  expect_true(measure_is_regular(with_missing_signal))
  expect_true(measure_grid_info(with_missing_signal)$has_na)
})

test_that("nD metadata rejects Unicode whitespace without rewriting labels", {
  for (blank in c("\u00a0", "\u2003", "\f", "\v", "\u0085")) {
    expect_error(
      new_measure_nd_tbl(
        location_1 = 1:2,
        location_2 = 3:4,
        value = 5:6,
        dim_names = c("time", blank)
      ),
      "nonblank"
    )
  }
  named <- new_measure_nd_tbl(
    location_1 = 1:2,
    location_2 = 3:4,
    value = 5:6,
    dim_names = c("time\u00a0", "wavelength")
  )
  expect_identical(measure_dim_names(named), c("time\u00a0", "wavelength"))
})

test_that("nD grid inspection rejects forged mixed-dimension collections", {
  two <- new_measure_nd_tbl(location_1 = 1:2, location_2 = 3:4, value = 5:6)
  three <- new_measure_nd_tbl(
    location_1 = 1:2,
    location_2 = 3:4,
    location_3 = 7:8,
    value = 5:6
  )
  mixed <- structure(
    list(two, three),
    class = c("measure_nd_list", "measure_list")
  )
  expect_snapshot(error = TRUE, measure_grid_info(mixed))
  expect_snapshot(error = TRUE, measure_is_regular(mixed))
})
