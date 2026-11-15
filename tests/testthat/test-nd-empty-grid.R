test_that("empty grid summaries use the retained dimensional prototype", {
  for (ndim in c(2L, 3L, 5L)) {
    coordinates <- stats::setNames(
      rep(list(c(1, 2)), ndim),
      paste0("location_", seq_len(ndim))
    )
    measurement <- do.call(
      new_measure_nd_tbl,
      c(coordinates, list(value = c(10, 20)))
    )
    collection <- new_measure_nd_list(list(measurement))
    empty_versions <- list(
      collection[0],
      vctrs::vec_slice(collection, integer()),
      vctrs::vec_restore(list(), collection)
    )

    for (empty in empty_versions) {
      expect_identical(
        measure_grid_info(empty),
        list(n_samples = 0L, ndim = ndim, sample_info = list())
      )
    }
  }

  expect_identical(
    measure_grid_info(new_measure_nd_list()),
    list(n_samples = 0L, ndim = 2L, sample_info = list())
  )
})

test_that("empty grid inspection validates its structural prototype", {
  measurement <- new_measure_nd_tbl(
    location_1 = c(1, 2),
    location_2 = c(3, 4),
    location_3 = c(5, 6),
    value = c(10, 20)
  )
  empty <- new_measure_nd_list(list(measurement))[0]

  missing <- empty
  attr(missing, "ptype") <- NULL
  expect_snapshot(error = TRUE, measure_grid_info(missing))

  populated <- empty
  attr(populated, "ptype") <- measurement
  expect_snapshot(error = TRUE, measure_grid_info(populated))

  malformed <- empty
  prototype <- attr(malformed, "ptype", exact = TRUE)
  attr(prototype, "ndim") <- 2L
  attr(malformed, "ptype") <- prototype
  expect_snapshot(error = TRUE, measure_grid_info(malformed))
})

test_that("nonempty grid summaries retain per-member observations", {
  first <- new_measure_nd_tbl(
    location_1 = c(0, 1),
    location_2 = c(254, 280),
    location_3 = c(1, 1),
    value = c(1, NA_real_),
    dim_names = c("time", "wavelength", "scan"),
    dim_units = c("min", "nm", NA_character_)
  )
  second <- new_measure_nd_tbl(
    location_1 = c(3, 2, 1),
    location_2 = c(300, 280, 254),
    location_3 = c(2, 2, 2),
    value = c(5, 6, 7)
  )
  information <- measure_grid_info(new_measure_nd_list(list(first, second)))

  expect_identical(information$n_samples, 2L)
  expect_identical(information$ndim, 3L)
  expect_identical(
    information$sample_info,
    list(measure_grid_info(first), measure_grid_info(second))
  )
})
