measure_results_test_data <- function(tagged = TRUE) {
  signal <- new_measure_list(list(
    new_measure_tbl(c(100, 200, 300), c(1, 2, NA_real_)),
    new_measure_tbl(c(100, 200, 300), c(2, 4, 6))
  ))
  if (tagged) {
    signal <- set_measure_value_metadata(
      signal,
      quantity = "absorbance",
      unit = "AU"
    )
  }

  tibble::tibble(
    sample_id = c("A", "B"),
    batch = c("one", "two"),
    signal = signal,
    row_metadata = list(c("a", "b"), c("c", "d"))
  )
}

test_that("measure_results retains the original data and selections", {
  data <- measure_results_test_data()

  result <- measure_results(
    data,
    measures = "signal",
    id_cols = c("sample_id", "batch")
  )

  expect_s3_class(result, "measure_results")
  expect_identical(result$data, data)
  expect_identical(result$measures, "signal")
  expect_identical(result$id_cols, c("sample_id", "batch"))
  expect_identical(names(result), c("data", "measures", "id_cols"))
})

test_that("measure_results validates the same selections as table summaries", {
  data <- measure_results_test_data(tagged = FALSE)
  data$other <- data$signal

  expect_error(measure_results(data), "Multiple measure columns")
  expect_error(
    measure_results(data, measures = "other", id_cols = "signal"),
    "id_cols"
  )
  expect_error(
    measure_results(data, measures = "missing"),
    "measurement column"
  )
})

test_that("summary.measure_results delegates with the stored selections", {
  data <- measure_results_test_data()
  result <- measure_results(data, measures = "signal", id_cols = "sample_id")

  expect_equal(
    summary(result),
    measure_summary_table(data, measures = "signal", id_cols = "sample_id")
  )
  expect_equal(
    summary(result, na.rm = FALSE),
    measure_summary_table(
      data,
      measures = "signal",
      id_cols = "sample_id",
      na.rm = FALSE
    )
  )
})

test_that("measure_results preserves n-dimensional data and metadata", {
  cube <- new_measure_nd_tbl(
    location_1 = c(1, 1, 2),
    location_2 = c(10, 20, 10),
    value = c(1, NA, 3),
    dim_names = c("time", "wavelength"),
    dim_units = c("min", "nm")
  )
  data <- tibble::tibble(
    sample_id = "A",
    cube = new_measure_nd_list(list(cube))
  )

  result <- measure_results(data, id_cols = "sample_id")

  expect_identical(result$data, data)
  expect_equal(measure_ndim(result$data$cube), 2L)
  expect_identical(
    attr(result$data$cube[[1]], "dim_names", exact = TRUE),
    c("time", "wavelength")
  )
  expect_identical(
    attr(result$data$cube[[1]], "dim_units", exact = TRUE),
    c("min", "nm")
  )
  expect_equal(summary(result)$n_dimensions, 2L)
})

test_that("print.measure_results is concise and reports known structure", {
  data <- measure_results_test_data()
  result <- measure_results(data, measures = "signal", id_cols = "sample_id")

  output <- paste(capture.output(print(result)), collapse = "\n")

  expect_match(output, "<measure_results>")
  expect_match(output, "Members: 2")
  expect_match(output, "Dimensions: 1D")
  expect_match(output, "Measure column: signal")
  expect_false(any(grepl("sample_id|absorbance|0.12", output)))
})

test_that("measure_results methods support prefixed S3 subclasses", {
  data <- measure_results_test_data()
  result <- measure_results(data, measures = "signal")
  class(result) <- c("my_measure_results", class(result))

  expect_equal(nrow(summary(result)), 2L)
  expect_s3_class(result, "my_measure_results")
})
