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

  # ------------------------------------------------------------------------------

  bad_inputs <-
    tibble::tribble(
      ~diffs, ~deg, ~wn,
      0L,   5L,  1L,
      1L,   5L,  1L,
      2L,   5L,  1L,
      3L,   5L,  1L,
      0L,   6L,  1L,
      1L,   6L,  1L,
      2L,   6L,  1L,
      3L,   6L,  1L,
      1L,   1L, 10L,
      2L,   1L, 10L,
      3L,   1L, 10L,
      2L,   2L, 10L,
      3L,   2L, 10L,
      3L,   3L, 10L,
     -1L,   1L, 21L,
      3L,   0L, 21L,
      3L,   2L,  0L
    )


  for (i in 1:nrow(bad_inputs)) {
    bad_rng <- bad_inputs$diffs[i] < 0 | bad_inputs$deg[i] < 1 | bad_inputs$wn[i] < 1

    if (bad_rng) {
      expect_snapshot(
        {
          rec %>%
            step_measure_savitzky_golay(
              differentiation_order = bad_inputs$diffs[i],
              window_side = bad_inputs$wn[i],
              degree = bad_inputs$deg[i]
            ) %>%
            prep()
        },
        error = TRUE
      )
    } else {
      expect_snapshot_warning({
        rec %>%
          step_measure_savitzky_golay(
            differentiation_order = bad_inputs$diffs[i],
            window_side = bad_inputs$wn[i],
            degree = bad_inputs$deg[i]
          ) %>%
          prep()
      })
    }
  }
})
