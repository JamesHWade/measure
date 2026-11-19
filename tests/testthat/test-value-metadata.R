make_value_metadata_measures <- function(n = 3L) {
  new_measure_list(lapply(seq_len(n), function(i) {
    new_measure_tbl(location = 1:3, value = i + 0:2)
  }))
}

test_that("value metadata is a validated column declaration", {
  measures <- make_value_metadata_measures()

  expect_null(measure_value_metadata(measures))
  expect_null(measure_value_metadata(tibble::tibble(value = 1:3)))

  tagged <- set_measure_value_metadata(
    measures,
    quantity = "signal",
    unit = "AU"
  )
  expect_identical(
    measure_value_metadata(tagged),
    list(quantity = "signal", unit = "AU")
  )
  expect_null(attr(tagged[[1]]$value, "measure_value_metadata", exact = TRUE))

  unitted <- set_measure_value_metadata(measures, quantity = "signal")
  expect_identical(
    measure_value_metadata(unitted),
    list(quantity = "signal", unit = NULL)
  )
})

test_that("setting metadata validates the object and scalar strings", {
  measures <- make_value_metadata_measures()
  bad_strings <- list(
    character(),
    c("signal", "other"),
    NA_character_,
    "",
    "   ",
    structure("signal", class = "declared"),
    structure("signal", dim = c(1L, 1L))
  )

  for (bad in bad_strings) {
    expect_error(
      set_measure_value_metadata(measures, quantity = bad),
      "quantity"
    )
    expect_error(
      set_measure_value_metadata(measures, quantity = "signal", unit = bad),
      "unit"
    )
  }

  expect_error(
    set_measure_value_metadata(tibble::tibble(value = 1:3), "signal"),
    "measure_list"
  )
  expect_error(
    set_measure_value_metadata(
      new_measure_nd_list(list(
        new_measure_nd_tbl(
          location_1 = 1:2,
          location_2 = 1:2,
          value = 1:2
        )
      )),
      "ordinary 1D"
    )
  )
})

test_that("named scalar declarations are canonicalized", {
  measures <- make_value_metadata_measures()
  tagged <- set_measure_value_metadata(
    measures,
    quantity = stats::setNames("signal", "quantity"),
    unit = stats::setNames("AU", "unit")
  )

  expect_identical(
    measure_value_metadata(tagged),
    list(quantity = "signal", unit = "AU")
  )
  expect_null(names(measure_value_metadata(tagged)$quantity))
  expect_null(names(measure_value_metadata(tagged)$unit))
})

test_that("malformed external declarations are rejected", {
  measures <- make_value_metadata_measures()

  attr(measures, "measure_value_metadata") <- list(quantity = "signal")
  expect_error(measure_value_metadata(measures), "measure_value_metadata")

  attr(measures, "measure_value_metadata") <- list(
    quantity = structure("signal", class = "declared"),
    unit = "AU"
  )
  expect_error(measure_value_metadata(measures), "measure_value_metadata")
})

test_that("base and vctrs row slicing preserve a declaration", {
  tagged <- set_measure_value_metadata(
    make_value_metadata_measures(),
    quantity = "signal",
    unit = "AU"
  )
  expected <- measure_value_metadata(tagged)

  expect_identical(measure_value_metadata(tagged[2]), expected)
  expect_identical(measure_value_metadata(tagged[0]), expected)
  expect_identical(
    measure_value_metadata(vctrs::vec_slice(tagged, c(3L, 1L))),
    expected
  )

  data <- tibble::tibble(id = 1:3, signal = tagged)
  expect_identical(
    measure_value_metadata(dplyr::filter(data, id > 1)$signal),
    expected
  )
  expect_identical(
    measure_value_metadata(dplyr::slice(data, c(3L, 1L))$signal),
    expected
  )
})

test_that("rebuilding a measure column does not carry its declaration", {
  tagged <- set_measure_value_metadata(
    make_value_metadata_measures(),
    quantity = "signal",
    unit = "AU"
  )
  rebuilt <- new_measure_list(lapply(tagged, identity))

  expect_null(measure_value_metadata(rebuilt))
  expect_null(measure_value_metadata(
    new_measure_list(tagged)
  ))
})

test_that("compatible concatenation and binding preserve declarations", {
  first <- set_measure_value_metadata(
    make_value_metadata_measures(2L),
    quantity = "signal",
    unit = "AU"
  )
  second <- set_measure_value_metadata(
    make_value_metadata_measures(1L),
    quantity = "signal",
    unit = "AU"
  )
  expected <- measure_value_metadata(first)

  expect_identical(measure_value_metadata(c(first, second)), expected)
  expect_identical(
    measure_value_metadata(vctrs::vec_c(first, second)),
    expected
  )
  expect_identical(
    measure_value_metadata(
      dplyr::bind_rows(
        tibble::tibble(signal = first),
        tibble::tibble(signal = second)
      )$signal
    ),
    expected
  )
})

test_that("incompatible or mixed declarations cannot be combined", {
  tagged <- set_measure_value_metadata(
    make_value_metadata_measures(),
    quantity = "signal",
    unit = "AU"
  )
  different_quantity <- set_measure_value_metadata(
    make_value_metadata_measures(),
    quantity = "concentration",
    unit = "AU"
  )
  different_unit <- set_measure_value_metadata(
    make_value_metadata_measures(),
    quantity = "signal",
    unit = "mAU"
  )
  untagged <- make_value_metadata_measures()

  expect_error(c(tagged, different_quantity), "incompatible")
  expect_error(c(different_quantity, tagged), "incompatible")
  expect_error(c(tagged, different_unit), "incompatible")
  expect_error(vctrs::vec_c(tagged, different_quantity), "incompatible")
  expect_error(vctrs::vec_c(different_quantity, tagged), "incompatible")
  expect_error(vctrs::vec_c(tagged, untagged), "incompatible")
  expect_error(vctrs::vec_c(untagged, tagged), "incompatible")
  expect_error(
    dplyr::bind_rows(
      tibble::tibble(signal = tagged),
      tibble::tibble(signal = different_quantity)
    ),
    "incompatible"
  )
  expect_error(
    dplyr::bind_rows(
      tibble::tibble(signal = untagged),
      tibble::tibble(signal = tagged)
    ),
    "incompatible"
  )
})

test_that("empty prototypes and all-untyped columns retain old behavior", {
  empty <- make_value_metadata_measures(0L)
  tagged_empty <- set_measure_value_metadata(
    empty,
    quantity = "signal",
    unit = "AU"
  )

  expect_identical(
    measure_value_metadata(tagged_empty[0]),
    measure_value_metadata(tagged_empty)
  )
  expect_identical(
    measure_value_metadata(vctrs::vec_slice(tagged_empty, integer())),
    measure_value_metadata(tagged_empty)
  )
  expect_s3_class(c(empty, empty), "measure_list")
  expect_s3_class(
    dplyr::bind_rows(
      tibble::tibble(signal = empty),
      tibble::tibble(signal = empty)
    )$signal,
    "measure_list"
  )

  # An empty untyped input contributes no values and is neutral for base c().
  expect_identical(
    measure_value_metadata(c(tagged_empty, empty)),
    measure_value_metadata(tagged_empty)
  )
})
