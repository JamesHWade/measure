test_that("transform measure to matrix", {

  rec <-
    recipe(water + fat + protein ~ ., data = meats_long) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    prep()

  spect_mat <- measure:::measure_to_matrix(rec$template$.measures)

  expect_equal(nrow(spect_mat), length(unique(meats_long$id)))
  expect_equal(ncol(spect_mat), length(unique(meats_long$channel)))

  ids <- unique(meats_long$id)
  for (i in seq_along(ids)) {
    expect_equal(
      spect_mat[i, ],
      meats_long[meats_long$id == ids[i], "transmittance"][[1]]
    )
  }

})

test_that("transform matrix to measure", {

  rec <-
    recipe(water + fat + protein ~ ., data = meats_long) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    prep()

  spect_mat <- measure:::measure_to_matrix(rec$template$.measures)

  locs <- unique(meats_long$channel)

  spect_list <- measure:::matrix_to_measure(spect_mat, locs)

  for (i in seq_along(spect_list)) {
    expect_equal(
      spect_list[[i]],
      rec$template$.measures[[i]]
    )
  }

})

test_that("transform tidy format", {

  rec <-
    recipe(water + fat + protein ~ ., data = meats_long) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    prep()

  spect_df <- measure:::measure_to_tibble(rec$template$.measures)
  exp_df <- meats_long[, c("channel", "transmittance", "id")]
  names(exp_df) <- c("location", "value", "sample_num")

  expect_equal(spect_df, exp_df)

})
