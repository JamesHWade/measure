make_nd_list_measurement <- function(ndim, offset = 0) {
  location_columns <- stats::setNames(
    lapply(seq_len(ndim), function(i) as.double(c(1, 2) + offset + i)),
    paste0("location_", seq_len(ndim))
  )

  do.call(
    new_measure_nd_tbl,
    c(
      location_columns,
      list(
        value = as.double(c(10, 20) + offset),
        dim_names = paste0("axis_", seq_len(ndim)),
        dim_units = rep("unit", ndim)
      )
    )
  )
}

test_that("empty nD list subsets retain their declared dimensionality", {
  for (ndim in c(2L, 3L)) {
    measurements <- new_measure_nd_list(list(
      make_nd_list_measurement(ndim),
      make_nd_list_measurement(ndim, offset = 100)
    ))
    attr(measurements, "unrelated") <- "do not copy"

    empty <- measurements[0]
    expect_s3_class(empty, "measure_nd_list")
    expect_identical(length(empty), 0L)
    expect_identical(measure_ndim(empty), ndim)
    expect_identical(attr(attr(empty, "ptype"), "ndim"), ndim)
    expect_null(attr(empty, "unrelated", exact = TRUE))
    expect_null(measure_dim_names(empty))
    expect_null(measure_dim_units(empty))
    if (ndim == 3L) {
      printed <- paste(capture.output(print(empty)), collapse = " ")
      expect_match(printed, "0 measurements \\(3D\\)")
    }

    kept <- measurements[2]
    expect_identical(measure_ndim(kept), ndim)
    expect_identical(kept[[1]], measurements[[2]])

    sliced <- vctrs::vec_slice(measurements, integer())
    expect_identical(measure_ndim(sliced), ndim)

    restored <- vctrs::vec_restore(list(), measurements)
    expect_identical(measure_ndim(restored), ndim)
  }

  expect_identical(measure_ndim(new_measure_nd_list()), 2L)
})

test_that("nD list subsetting keeps strict vctrs index semantics", {
  measurements <- new_measure_nd_list(list(
    first = make_nd_list_measurement(3L),
    second = make_nd_list_measurement(3L, offset = 100)
  ))

  expect_snapshot(error = TRUE, measurements[1.5])
  expect_snapshot(error = TRUE, measurements[c(TRUE, FALSE, TRUE)])
  expect_snapshot(error = TRUE, measurements[3L])
  expect_snapshot(error = TRUE, measurements["missing"])
  expect_snapshot(error = TRUE, measurements[NA_integer_])
})

test_that("nD list subsetting rejects extra and missing dimensions", {
  measurements <- new_measure_nd_list(list(
    make_nd_list_measurement(3L),
    make_nd_list_measurement(3L, offset = 100)
  ))

  expect_snapshot(error = TRUE, measurements[, 1])
  expect_snapshot(error = TRUE, measurements[,])
  expect_snapshot(error = TRUE, measurements[,,])
  expect_snapshot(error = TRUE, measurements[, 1, drop = FALSE])
  expect_snapshot(error = TRUE, measurements[foo = 1])
})

test_that("nD list subsetting preserves names, element metadata, values, and order", {
  first <- make_nd_list_measurement(3L)
  second <- make_nd_list_measurement(3L, offset = 100)
  measurements <- new_measure_nd_list(list(first = first, second = second))

  selected <- measurements[c("second", "first"), drop = FALSE]

  expect_s3_class(selected, "measure_nd_list")
  expect_identical(names(selected), c("second", "first"))
  expect_identical(selected[[1]], second)
  expect_identical(selected[[2]], first)
  expect_identical(
    measure_dim_names(selected[[1]]),
    c("axis_1", "axis_2", "axis_3")
  )
  expect_identical(measure_dim_units(selected[[1]]), c("unit", "unit", "unit"))
  expect_identical(selected[[1]]$value, c(110, 120))
})

test_that("tibble filtering and recipe baking preserve an empty nD list prototype", {
  measurements <- new_measure_nd_list(list(
    make_nd_list_measurement(3L),
    make_nd_list_measurement(3L, offset = 100)
  ))
  data <- tibble::tibble(
    id = 1:2,
    response = c(0, 1),
    spectrum = measurements
  )

  filtered <- dplyr::filter(data, id > 2)
  expect_identical(nrow(filtered), 0L)
  expect_identical(measure_ndim(filtered$spectrum), 3L)
  expect_identical(attr(attr(filtered$spectrum, "ptype"), "ndim"), 3L)
  expect_null(measure_dim_names(filtered$spectrum))
  expect_null(measure_dim_units(filtered$spectrum))

  trained <- recipes::recipe(response ~ ., data = data) |>
    recipes::prep(training = data, verbose = FALSE)
  baked <- recipes::bake(trained, new_data = filtered)
  expect_identical(nrow(baked), 0L)
  expect_identical(measure_ndim(baked$spectrum), 3L)
})

test_that("empty nD lists require a valid dimensional prototype", {
  missing_ptype <- structure(
    list(),
    class = c("measure_nd_list", "measure_list")
  )
  expect_snapshot(error = TRUE, measure_ndim(missing_ptype))

  malformed_ptype <- attr(new_measure_nd_list(), "ptype")
  attr(malformed_ptype, "ndim") <- .Machine$integer.max
  malformed <- vctrs::new_list_of(
    list(),
    ptype = malformed_ptype,
    class = c("measure_nd_list", "measure_list")
  )
  expect_snapshot(error = TRUE, measure_ndim(malformed))

  nonempty_ptype <- make_nd_list_measurement(3L)
  malformed_empty <- structure(
    list(),
    ptype = nonempty_ptype,
    class = class(new_measure_nd_list())
  )
  expect_snapshot(error = TRUE, measure_ndim(malformed_empty))

  source <- new_measure_nd_list(list(make_nd_list_measurement(3L)))
  attr(source, "ptype") <- nonempty_ptype
  expect_snapshot(error = TRUE, source[0])
})
