test_that("transform measure to matrix", {
  rec <-
    recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    prep()

  spect_mat <- measure_to_matrix(rec$template$.measures)

  expect_equal(nrow(spect_mat), length(unique(meats_small$id)))
  expect_equal(ncol(spect_mat), length(unique(meats_small$channel)))

  ids <- unique(meats_small$id)
  for (i in seq_along(ids)) {
    expect_equal(
      spect_mat[i, ],
      meats_small[meats_small$id == ids[i], "transmittance"][[1]]
    )
  }
})

test_that("transform matrix to measure", {
  rec <-
    recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    prep()

  spect_mat <- measure_to_matrix(rec$template$.measures)

  locs <- unique(meats_small$channel)

  spect_list <- matrix_to_measure(spect_mat, locs)

  for (i in seq_along(spect_list)) {
    expect_equal(
      spect_list[[i]],
      rec$template$.measures[[i]]
    )
  }
})

test_that("transform tidy format", {
  rec <-
    recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    prep()

  spect_df <- measure_to_tibble(rec$template$.measures)
  exp_df <- meats_small[, c("channel", "transmittance", "id")]
  names(exp_df) <- c("location", "value", "sample_num")

  expect_equal(spect_df, exp_df)
})

test_that("matrix conversion rejects ambiguous shapes and mismatched axes", {
  expect_error(matrix_to_measure(1:4, c(12, 10)), "matrix")
  expect_error(matrix_to_measure(matrix(1:4, nrow = 2), 12), "locations")
})

test_that("column validation prefers typed measures and accepts legacy .measures", {
  member <- new_measure_tbl(c(12, 10), c(0.1, 0.3))
  legacy <- tibble::tibble(.measures = list(member))
  mixed <- legacy
  mixed$signal <- new_measure_list(list(member))
  expect_identical(check_for_measure(mixed), "signal")
  expect_identical(check_for_measure(legacy), ".measures")
  expect_identical(
    check_has_measure(legacy, quote(prep.step_measure_output_long())),
    ".measures"
  )
})

test_that("one-dimensional padding appends missing points only to short samples", {
  data <- tibble::tibble(
    id = rep(c("a", "b", "c"), c(3, 2, 3)),
    axis = c(3, 2, 1, 3, 1, 3, 2, 1),
    signal = c(11, NA, 13, 21, 23, 31, 32, 33)
  )
  rec <- recipe(~., data = data) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(
      signal,
      location = vars(axis),
      col_name = "spectrum",
      pad = TRUE
    )
  baked <- bake(prep(rec), new_data = data)
  expect_identical(baked$id, c("a", "b", "c"))
  expect_equal(baked$spectrum[[1]], new_measure_tbl(c(3, 2, 1), c(11, NA, 13)))
  expect_equal(baked$spectrum[[2]], new_measure_tbl(c(3, 1, NA), c(21, 23, NA)))
  expect_equal(baked$spectrum[[3]], new_measure_tbl(c(3, 2, 1), 31:33))
})

test_that("matrix-based centering rejects ragged spectra before recycling signal", {
  data <- tibble::tibble(
    signal = new_measure_list(list(
      new_measure_tbl(1:4, c(1, 2, 3, 4)),
      new_measure_tbl(1:2, c(7, 9))
    ))
  )
  rec <- recipe(~., data = data) |> step_measure_center()
  expect_error(prep(rec), "different point counts")
  trained <- prep(rec, training = data[1, ])
  expect_error(bake(trained, new_data = data), "different point counts")
})
