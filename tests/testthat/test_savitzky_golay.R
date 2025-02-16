test_that("savitzky-golay computations", {
  skip_if_not_installed("prospectr")

  # ------------------------------------------------------------------------------

  rec <-
    recipe(water + fat + protein ~ ., data = meats_long) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    prep()

  spect_start <- measure:::measure_to_matrix(rec$template$.measures)

  # ------------------------------------------------------------------------------

  grid <- expand.grid(diffs = 0:3, deg = 4:6, wn = c(15, 21, 25))

  for (i in 1:nrow(grid)) {
    meas_res <-
      measure:::.comp_savitzky_golay(
        rec$template$.measures,
        diffs = grid$diffs[i],
        degree = grid$deg[i],
        window = grid$wn[i]
      ) %>%
      measure:::measure_to_matrix()
    prosp_res <-
      prospectr::savitzkyGolay(spect_start,
                               m = grid$diffs[i],
                               p = grid$deg[i],
                               w = grid$wn[i]
      )
    expect_equal(meas_res, prosp_res)
  }
})

test_that("savitzky-golay inputs", {
  skip_if_not_installed("prospectr")

  # ------------------------------------------------------------------------------

  rec <-
    recipe(water + fat + protein ~ ., data = meats_long) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    prep()

  spect_start <- measure:::measure_to_matrix(rec$template$.measures)

  # ------------------------------------------------------------------------------

  arg_inputs <-
    tibble::tribble(
      ~win_side,          ~order, ~degree,
      1L,                     1L,      4L,
      1L,                     3L,      3L,
      1L,                     1L,      2L,
      1L,                     3L,      5L,
      1L,                     2L,      1L,
      1L,                     4L,      2L,
      2L,                     4L,      4L,
      2L,                     0L,      4L,
      2L,                     1L,      5L,
      2L,                     1L,      3L,
      2L,                     0L,      2L,
      2L,                     1L,      1L,
      3L,                     3L,      5L,
      3L,                     3L,      1L,
      3L,                     3L,      3L,
      3L,                     4L,      2L,
      3L,                     0L,      5L,
      3L,                     2L,      2L,
      4L,                     2L,      4L,
      4L,                     0L,      3L,
      4L,                     0L,      1L,
      4L,                     2L,      5L,
      4L,                     4L,      5L,
      4L,                     4L,      1L,
      5L,                     4L,      3L,
      5L,                     2L,      1L,
      5L,                     0L,      4L,
      5L,                     1L,      3L,
      5L,                     3L,      2L,
      5L,                     2L,      4L
    )

  for (i in 1:nrow(arg_inputs)) {

    window_length <- 2 * arg_inputs$win_side[i] + 1

    warn_val <-
      arg_inputs$degree[i] <  arg_inputs$order[i] |
      arg_inputs$degree[i] >= window_length

    if (warn_val) {
      expect_snapshot(
        rec %>%
          step_measure_savitzky_golay(
            differentiation_order = arg_inputs$order[i],
            window_side = arg_inputs$win_side[i],
            degree = arg_inputs$degree[i]
          ) %>%
          prep()
      )
    } else {
      expect_silent(
        rec %>%
          step_measure_savitzky_golay(
            differentiation_order = arg_inputs$order[i],
            window_side = arg_inputs$win_side[i],
            degree = arg_inputs$degree[i]
          ) %>%
          prep()
      )
    }
  }
})


test_that("savitzky-golay tuning parameters", {
  expect_snapshot(window_side())
  expect_snapshot(window_side(c(2, 10)))
  expect_snapshot(differentiation_order())

  expect_snapshot(
    recipe(water + fat + protein ~ ., data = meats_long) %>%
      update_role(id, new_role = "id") %>%
      step_measure_input_long(transmittance, location = vars(channel)) %>%
      step_measure_savitzky_golay() %>%
      tunable()
  )
})
