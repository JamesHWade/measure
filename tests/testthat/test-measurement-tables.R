test_measurement_tables_1d <- function(tagged = TRUE) {
  location <- c(100, 200, 300)
  signal <- new_measure_list(list(
    new_measure_tbl(location, c(1, 2, NA_real_)),
    new_measure_tbl(location, c(2, 4, 6))
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

test_that("measure_summary_table summarizes one-dimensional measurements", {
  data <- test_measurement_tables_1d()

  result <- measure_summary_table(data, id_cols = "sample_id")

  expect_s3_class(result, "tbl_df")
  expect_equal(result$sample_id, c("A", "B"))
  expect_equal(result$row_id, c(1L, 2L))
  expect_equal(result$n_points, c(3L, 3L))
  expect_equal(result$n_missing, c(1L, 0L))
  expect_equal(result$n_observed, c(2L, 3L))
  expect_equal(result$mean, c(1.5, 4))
  expect_equal(result$sd, c(stats::sd(c(1, 2)), 2))
  expect_equal(result$min, c(1, 2))
  expect_equal(result$max, c(2, 6))
  expect_equal(result$value_quantity, c("absorbance", "absorbance"))
  expect_equal(result$value_unit, c("AU", "AU"))
  expect_false("row_metadata" %in% names(result))
})

test_that("measure_slice_table preserves one-dimensional axes and missing values", {
  data <- test_measurement_tables_1d()

  result <- measure_slice_table(data, id_cols = c("sample_id", "batch"))

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 6L)
  expect_equal(result$sample_id, rep(c("A", "B"), each = 3))
  expect_equal(result$batch, rep(c("one", "two"), each = 3))
  expect_equal(result$row_id, rep(1:2, each = 3))
  expect_equal(result$point, rep(seq_len(3), 2))
  expect_equal(result$location, rep(c(100, 200, 300), 2))
  expect_equal(result$value, c(1, 2, NA, 2, 4, 6))
  expect_equal(is.na(result$value), c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE))
  expect_equal(result$value_quantity, rep("absorbance", 6))
  expect_equal(result$value_unit, rep("AU", 6))
})

test_that("table exports preserve recipe-baked IDs and omit nested helper columns", {
  raw <- tibble::tibble(
    sample_id = rep(c("A", "B"), each = 3),
    location = rep(c(1, 2, 3), 2),
    signal = c(1, 2, NA, 2, 4, 6)
  )

  recipe <- recipes::recipe(signal ~ ., data = raw) |>
    recipes::update_role(sample_id, new_role = "id") |>
    step_measure_input_long(signal, location = dplyr::vars(location)) |>
    recipes::prep()
  baked <- recipes::bake(recipe, new_data = NULL)

  summary <- measure_summary_table(baked)
  slices <- measure_slice_table(baked)

  expect_equal(summary$sample_id, c("A", "B"))
  expect_equal(summary$n_missing, c(1L, 0L))
  expect_equal(nrow(slices), 6L)
  expect_equal(slices$sample_id, rep(c("A", "B"), each = 3))
  expect_false("location" %in% names(summary))
})

test_that("n-dimensional exports preserve axes and native labels without encoding", {
  first <- new_measure_nd_tbl(
    location_1 = c(1, 1, 2),
    location_2 = c(10, 20, 10),
    value = c(1, NA, 3),
    dim_names = c("time", "wavelength"),
    dim_units = c("min", "nm")
  )
  second <- new_measure_nd_tbl(
    location_1 = c(1, 2, 2),
    location_2 = c(10, 10, 20),
    value = c(4, 5, NA),
    dim_names = c("retention", "lambda"),
    dim_units = c("s", "nm")
  )
  data <- tibble::tibble(
    sample_id = c("A", "B"),
    cube = new_measure_nd_list(list(first, second))
  )

  summary <- measure_summary_table(data)
  slices <- measure_slice_table(data)

  expect_equal(summary$n_missing, c(1L, 1L))
  expect_equal(summary$axis_names[[1]], c("time", "wavelength"))
  expect_equal(summary$axis_names[[2]], c("retention", "lambda"))
  expect_equal(summary$axis_units[[1]], c("min", "nm"))
  expect_equal(summary$axis_units[[2]], c("s", "nm"))
  expect_equal(nrow(slices), 6L)
  expect_equal(slices$location_1, c(1, 1, 2, 1, 2, 2))
  expect_equal(slices$location_2, c(10, 20, 10, 10, 10, 20))
  expect_equal(slices$value, c(1, NA, 3, 4, 5, NA))
  expect_equal(slices$axis_names[[1]], c("time", "wavelength"))
  expect_equal(slices$axis_names[[4]], c("retention", "lambda"))
})

test_that("empty members and empty prototypes retain typed output schemas", {
  empty_1d <- new_measure_list(list(new_measure_tbl(double(), double())))
  one_empty <- tibble::tibble(sample_id = "empty", signal = empty_1d)

  summary <- measure_summary_table(one_empty)
  slices <- measure_slice_table(one_empty)

  expect_equal(summary$n_points, 0L)
  expect_equal(summary$n_missing, 0L)
  expect_true(is.na(summary$mean))
  expect_equal(nrow(slices), 0L)
  expect_true(all(c("row_id", "point", "location", "value") %in% names(slices)))

  empty_nd <- new_measure_nd_list()
  no_rows <- tibble::tibble(
    sample_id = character(),
    cube = empty_nd
  )
  empty_summary <- measure_summary_table(no_rows)
  empty_slices <- measure_slice_table(no_rows)

  expect_equal(nrow(empty_summary), 0L)
  expect_equal(nrow(empty_slices), 0L)
  expect_true(all(
    c("location_1", "location_2", "value") %in% names(empty_slices)
  ))
})

test_that("ambiguous measure columns require explicit selection", {
  data <- test_measurement_tables_1d(tagged = FALSE)
  data$other <- data$signal

  expect_error(measure_summary_table(data), "Multiple measure columns")
  expect_error(measure_slice_table(data), "Multiple measure columns")

  selected <- measure_slice_table(data, measures = "other")
  expect_equal(nrow(selected), 6L)
})

test_that("named samples retain missing counts when statistics propagate NA", {
  x <- new_measure_list(list(
    blank = new_measure_tbl(1:2, c(NaN, NA_real_)),
    sample = new_measure_tbl(1:2, c(4, NA_real_))
  ))

  summary <- measure_summary_table(x, na.rm = FALSE)
  expect_identical(summary$member_name, c("blank", "sample"))
  expect_equal(summary$n_missing, c(2L, 1L))
  expect_equal(summary$mean, c(NA_real_, NA_real_))
  expect_equal(measure_summary_table(x)$mean, c(NA_real_, 4))
  expect_identical(measure_slice_table(x)$value, c(NaN, NA, 4, NA))
})

test_that("generated fields do not overwrite colliding metadata", {
  signal <- new_measure_list(list(new_measure_tbl(1:2, c(3, 4))))
  data <- tibble::tibble(
    row_id = "source-row",
    point = 99L,
    location = "source-location",
    value = "source-value",
    signal = signal
  )

  result <- measure_slice_table(data)

  expect_equal(result$row_id, c(1L, 1L))
  expect_equal(result$point, c(1L, 2L))
  expect_equal(result$location, c(1, 2))
  expect_equal(result$value, c(3, 4))
  expect_equal(result$row_id.data, c("source-row", "source-row"))
  expect_equal(result$point.data, c(99L, 99L))
  expect_equal(result$location.data, c("source-location", "source-location"))
  expect_equal(result$value.data, c("source-value", "source-value"))
})
