test_that("unfold requires an exact integral dimension permutation", {
  x <- new_measure_nd_tbl(
    location_1 = 1:2,
    location_2 = 1:2,
    value = 1:2
  )

  invalid_orders <- list(
    c(1, 1),
    c(1, 2.5),
    c(1, NA_real_),
    c(1, Inf),
    c(TRUE, FALSE),
    matrix(1:2)
  )

  for (order in invalid_orders) {
    expect_error(
      measure_unfold(x, order = order),
      "complete permutation"
    )
  }
})


test_that("fold restores canonical unfold order through point indices", {
  x <- new_measure_nd_tbl(
    location_1 = c(2, 1, 2, 1),
    location_2 = c(20, 10, 10, 20),
    value = c(20, 11, 21, 10),
    dim_names = c("time", "wavelength"),
    dim_units = c("min", "nm")
  )

  unfolded <- measure_unfold(x, order = c(2, 1))
  expect_identical(unfolded$location, seq_len(nrow(x)))
  expect_null(attr(new_measure_list(list(unfolded)), "fold_info"))

  edited_values <- unfolded$value + 100
  row_order <- c(3, 1, 4, 2)
  permuted <- new_measure_tbl(
    location = row_order,
    value = edited_values[row_order]
  )
  attr(permuted, "fold_info") <- attr(unfolded, "fold_info")

  restored <- measure_fold(permuted)

  expect_equal(restored$location_1, c(1, 1, 2, 2))
  expect_equal(restored$location_2, c(10, 20, 10, 20))
  expect_equal(restored$value, edited_values)
  expect_equal(measure_dim_names(restored), c("time", "wavelength"))
  expect_equal(measure_dim_units(restored), c("min", "nm"))
})


test_that("fold rejects invalid point indices", {
  x <- new_measure_nd_tbl(
    location_1 = c(1, 1, 2, 2),
    location_2 = c(1, 2, 1, 2),
    value = 1:4
  )
  unfolded <- measure_unfold(x)
  info <- attr(unfolded, "fold_info")

  invalid_indices <- list(
    c(1, 1, 3, 4),
    c(1, 2, 3, 5),
    c(1, 2, 3, 3.5),
    c(1, 2, 3, NA_real_),
    c(1, 2, 3, Inf),
    c(0, 2, 3, 4),
    c(TRUE, FALSE, TRUE, FALSE),
    c("1", "2", "3", "4")
  )

  for (point_index in invalid_indices) {
    invalid <- new_measure_tbl(location = 1:4, value = 1:4)
    invalid$location <- point_index
    attr(invalid, "fold_info") <- info

    expect_error(measure_fold(invalid), "complete point index")
  }
})


test_that("fold validates metadata fields before rebuilding an nD object", {
  x <- new_measure_nd_tbl(
    location_1 = c(1, 1, 2, 2),
    location_2 = c(1, 2, 1, 2),
    value = 1:4,
    dim_names = c("time", "wavelength"),
    dim_units = c("min", "nm")
  )
  unfolded <- measure_unfold(x)
  valid_info <- attr(unfolded, "fold_info")

  alter_info <- function(info, ...) {
    replacements <- list(...)
    for (name in names(replacements)) {
      info[[name]] <- replacements[[name]]
    }
    info
  }

  with_info <- function(info) {
    out <- new_measure_tbl(location = 1:4, value = 1:4)
    attr(out, "fold_info") <- info
    out
  }

  invalid_info <- list(
    alter_info(valid_info, ndim = 2.5),
    alter_info(valid_info, order = c(1, 1)),
    alter_info(valid_info, dim_names = "time"),
    alter_info(valid_info, dim_names = c("same", "same")),
    alter_info(valid_info, dim_units = c("", "nm")),
    alter_info(valid_info, ndim = .Machine$integer.max),
    alter_info(
      valid_info,
      coordinates = list(dim_1 = letters[1:4], dim_2 = 1:4)
    ),
    alter_info(
      valid_info,
      coordinates = list(dim_1 = matrix(1:4), dim_2 = 1:4)
    ),
    alter_info(valid_info, coordinates = list(dim_1 = 1:4, dim_2 = 1:3)),
    alter_info(valid_info, coordinates = list(dim_2 = 1:4, dim_1 = 1:4)),
    alter_info(
      valid_info,
      coordinates = list(dim_1 = 1:4, dim_2 = 1:4, dim_3 = 1:4)
    )
  )

  for (info in invalid_info) {
    expect_error(measure_fold(with_info(info)))
  }

  expect_snapshot(
    error = TRUE,
    measure_fold(with_info(alter_info(
      valid_info,
      coordinates = list(dim_1 = 1:4)
    )))
  )

  mutated <- x
  attr(mutated, "ndim") <- .Machine$integer.max
  expect_error(measure_unfold(mutated))
})


test_that("per-member metadata survives heterogeneous, reordered lists", {
  first <- new_measure_nd_tbl(
    location_1 = c(2, 1, 2, 1),
    location_2 = c(20, 10, 10, 20),
    value = c(20, 11, 21, 10),
    dim_names = c("time", "wavelength"),
    dim_units = c("min", "nm")
  )
  second <- new_measure_nd_tbl(
    location_1 = c(4, 2, 4),
    location_2 = c(80, 90, 90),
    value = c(4, 2, 5),
    dim_names = c("sample", "mass"),
    dim_units = c("s", "Da")
  )

  unfolded <- measure_unfold(new_measure_nd_list(list(first, second)))
  expect_null(attr(unfolded, "fold_info"))
  expect_false(is.null(attr(unfolded[[1]], "fold_info")))
  expect_false(is.null(attr(unfolded[[2]], "fold_info")))

  # A stale list-level grid must not replace either member's own grid.
  reordered <- unfolded[c(2, 1)]
  attr(reordered, "fold_info") <- attr(unfolded[[1]], "fold_info")
  restored <- measure_fold(reordered)

  expect_equal(restored[[1]]$location_1, c(4, 2, 4))
  expect_equal(restored[[1]]$location_2, c(80, 90, 90))
  expect_equal(restored[[1]]$value, c(4, 2, 5))
  expect_equal(measure_dim_names(restored[[1]]), c("sample", "mass"))
  expect_equal(measure_dim_units(restored[[1]]), c("s", "Da"))
  expect_equal(restored[[2]]$location_1, c(1, 2, 1, 2))
  expect_equal(restored[[2]]$location_2, c(10, 10, 20, 20))
  expect_equal(restored[[2]]$value, c(11, 21, 10, 20))
  expect_equal(measure_dim_names(restored[[2]]), c("time", "wavelength"))

  subsetted <- unfolded[2]
  attr(subsetted, "fold_info") <- attr(unfolded[[1]], "fold_info")
  restored_subset <- measure_fold(subsetted)
  expect_equal(restored_subset[[1]], restored[[1]])
})


test_that("partial member metadata never falls back to a shared grid", {
  x <- new_measure_nd_tbl(
    location_1 = c(1, 1, 2, 2),
    location_2 = c(1, 2, 1, 2),
    value = 1:4
  )
  unfolded <- measure_unfold(new_measure_nd_list(list(x, x)))
  attr(unfolded[[2]], "fold_info") <- NULL
  attr(unfolded, "fold_info") <- attr(measure_unfold(x), "fold_info")

  expect_snapshot(error = TRUE, measure_fold(unfolded))
})


test_that("legacy list-level shared metadata is accepted only when valid", {
  x <- new_measure_nd_tbl(
    location_1 = c(1, 1, 2, 2),
    location_2 = c(1, 2, 1, 2),
    value = 1:4,
    dim_names = c("time", "wavelength")
  )
  unfolded <- measure_unfold(x)
  shared_info <- attr(unfolded, "fold_info")
  first <- new_measure_tbl(location = 1:4, value = 1:4)
  second <- new_measure_tbl(location = 1:4, value = 5:8)
  legacy <- new_measure_list(list(first, second))
  attr(legacy, "fold_info") <- shared_info

  restored <- measure_fold(legacy)
  expect_equal(restored[[1]]$value, 1:4)
  expect_equal(restored[[2]]$value, 5:8)
  expect_equal(measure_dim_names(restored[[2]]), c("time", "wavelength"))

  shared_info$order <- c(1, 1)
  attr(legacy, "fold_info") <- shared_info
  expect_snapshot(error = TRUE, measure_fold(legacy))
})


test_that("fold handles empty points, 3D data, and empty lists", {
  empty <- new_measure_nd_tbl(
    location_1 = double(),
    location_2 = integer(),
    location_3 = double(),
    value = double(),
    dim_names = c("sample", "time", "wavelength"),
    dim_units = c(NA_character_, "min", "nm")
  )
  nonempty <- new_measure_nd_tbl(
    location_1 = c(2, 1),
    location_2 = c(20, 10),
    location_3 = c(300, 280),
    value = c(2, 1),
    dim_names = c("sample", "time", "wavelength"),
    dim_units = c(NA_character_, "min", "nm")
  )

  unfolded <- measure_unfold(new_measure_nd_list(list(empty, nonempty)))
  expect_equal(nrow(unfolded[[1]]), 0L)
  expect_null(attr(unfolded, "fold_info"))
  restored <- measure_fold(unfolded)

  expect_equal(nrow(restored[[1]]), 0L)
  expect_equal(measure_ndim(restored[[1]]), 3L)
  expect_equal(restored[[2]]$location_1, c(1, 2))
  expect_equal(restored[[2]]$location_2, c(10, 20))
  expect_equal(restored[[2]]$location_3, c(280, 300))
  expect_equal(restored[[2]]$value, c(1, 2))

  expect_length(measure_unfold(new_measure_nd_list(list())), 0L)
  expect_length(measure_fold(new_measure_list(list())), 0L)
})


test_that("valid older single-object fold metadata remains supported", {
  old_metadata <- list(
    ndim = 2L,
    dim_names = c("time", "wavelength"),
    dim_units = c("min", "nm"),
    order = c(1, 2),
    coordinates = list(dim_1 = c(1, 2), dim_2 = c(280, 300))
  )
  unfolded <- new_measure_tbl(location = c(1, 2), value = c(3, 4))
  attr(unfolded, "fold_info") <- old_metadata

  restored <- measure_fold(unfolded)
  expect_equal(restored$location_1, c(1, 2))
  expect_equal(restored$location_2, c(280, 300))
  expect_equal(restored$value, c(3, 4))
  expect_equal(measure_dim_names(restored), c("time", "wavelength"))
})


test_that("fold preserves non-finite coordinates and signal values", {
  unfolded <- new_measure_tbl(
    location = 1:2,
    value = c(NA_real_, Inf)
  )
  attr(unfolded, "fold_info") <- list(
    ndim = 2L,
    dim_names = NULL,
    dim_units = NULL,
    order = 1:2,
    coordinates = list(
      dim_1 = c(NA_real_, Inf),
      dim_2 = c(20, 10)
    )
  )

  restored <- measure_fold(unfolded)
  expect_true(is.na(restored$location_1[[1]]))
  expect_true(is.infinite(restored$location_1[[2]]))
  expect_equal(restored$location_2, c(20, 10))
  expect_true(is.na(restored$value[[1]]))
  expect_true(is.infinite(restored$value[[2]]))
})

test_that("fold metadata rejects Unicode-only unit labels", {
  x <- new_measure_nd_tbl(location_1 = 1:2, location_2 = 3:4, value = 5:6)
  unfolded <- measure_unfold(x)
  attr(unfolded, "fold_info")$dim_units <- c("min", "\u00a0")
  expect_snapshot(error = TRUE, measure_fold(unfolded))
})

test_that("fold never flattens a malformed matrix-valued signal", {
  x <- new_measure_nd_tbl(location_1 = 1:2, location_2 = 3:4, value = 5:6)
  unfolded <- measure_unfold(x)
  unfolded$value <- matrix(1:4, nrow = 2)
  expect_snapshot(error = TRUE, measure_fold(unfolded))
})

test_that("fold rejects dimensional list containers before reconstruction", {
  x <- new_measure_nd_tbl(location_1 = 1:2, location_2 = 3:4, value = 5:6)
  unfolded <- measure_unfold(x)
  bad_list <- structure(
    list(unfolded, unfolded),
    dim = c(1L, 2L),
    class = "measure_list"
  )
  expect_snapshot(error = TRUE, measure_fold(bad_list))

  bad_info <- unfolded
  attr(bad_info, "fold_info") <- structure(
    attr(unfolded, "fold_info"),
    dim = c(5L, 1L)
  )
  expect_snapshot(error = TRUE, measure_fold(bad_info))

  bad_coordinates <- unfolded
  attr(bad_coordinates, "fold_info")$coordinates <- structure(
    attr(unfolded, "fold_info")$coordinates,
    dim = c(1L, 2L)
  )
  expect_snapshot(error = TRUE, measure_fold(bad_coordinates))
})

test_that("fold mappings require the exact fold_info attribute", {
  x <- new_measure_nd_tbl(location_1 = 1:2, location_2 = 3:4, value = 5:6)
  unfolded <- measure_unfold(x)
  attr(unfolded, "fold_info_backup") <- attr(unfolded, "fold_info")
  attr(unfolded, "fold_info") <- NULL
  expect_snapshot(error = TRUE, measure_fold(unfolded))

  pair <- new_measure_list(list(unfolded, unfolded))
  attr(pair, "fold_info_backup") <- attr(unfolded, "fold_info_backup")
  expect_snapshot(error = TRUE, measure_fold(pair))
})
