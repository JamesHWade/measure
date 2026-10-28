test_that("savitzky-golay computations", {
  skip_if_not_installed("prospectr")

  # ------------------------------------------------------------------------------

  rec <-
    recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    prep()

  spect_start <- measure_to_matrix(rec$template$.measures)

  # ------------------------------------------------------------------------------

  grid <- expand.grid(diffs = 0:3, deg = 4:6, wn = c(15, 21, 25))

  for (i in seq_len(nrow(grid))) {
    meas_res <-
      .comp_savitzky_golay(
        rec$template$.measures,
        diffs = grid$diffs[i],
        degree = grid$deg[i],
        window = grid$wn[i]
      ) |>
      measure_to_matrix()
    prosp_res <-
      prospectr::savitzkyGolay(
        spect_start,
        m = grid$diffs[i],
        p = grid$deg[i],
        w = grid$wn[i]
      )
    expect_equal(meas_res, prosp_res)
  }
})

test_that("Savitzky-Golay keeps centered locations for each window", {
  skip_if_not_installed("prospectr")

  locations <- seq(100, 112, by = 2)
  index <- seq_along(locations)
  measures <- new_measure_list(list(
    new_measure_tbl(location = locations, value = index),
    new_measure_tbl(location = locations, value = index^2)
  ))

  for (window in c(3L, 5L)) {
    result <- .comp_savitzky_golay(
      measures,
      diffs = 1,
      degree = 2,
      window = window
    )
    edge_points <- (window - 1L) / 2L
    expected_locations <- locations[
      seq_len(length(locations) - 2L * edge_points) + edge_points
    ]
    expected_values <- prospectr::savitzkyGolay(
      measure_to_matrix(measures),
      m = 1,
      p = 2,
      w = window
    )

    expect_equal(result[[1]]$location, expected_locations)
    expect_equal(result[[2]]$location, expected_locations)
    expect_equal(measure_to_matrix(result), expected_values)
  }

  smoothed <- .comp_savitzky_golay(
    measures,
    diffs = 0,
    degree = 2,
    window = 5
  )

  expect_equal(smoothed[[1]]$location, locations[3:5])
  expect_equal(smoothed[[1]]$value, index[3:5])
  expect_equal(smoothed[[2]]$value, index[3:5]^2)
})

test_that("Savitzky-Golay centers labels on descending non-unit axes", {
  skip_if_not_installed("prospectr")

  locations <- seq(32, 8, by = -3)
  index <- seq_along(locations)
  measures <- new_measure_list(list(
    new_measure_tbl(location = locations, value = index^2),
    new_measure_tbl(location = locations, value = sin(index))
  ))

  result <- .comp_savitzky_golay(
    measures,
    diffs = 1,
    degree = 2,
    window = 5
  )
  expected_values <- prospectr::savitzkyGolay(
    measure_to_matrix(measures),
    m = 1,
    p = 2,
    w = 5
  )

  expect_equal(result[[1]]$location, locations[3:7])
  expect_equal(result[[2]]$location, locations[3:7])
  expect_equal(measure_to_matrix(result), expected_values)
})

test_that("Savitzky-Golay rejects measurements with different axes", {
  skip_if_not_installed("prospectr")

  measures <- new_measure_list(list(
    new_measure_tbl(location = seq_len(7), value = seq_len(7)),
    new_measure_tbl(location = seq_len(7) + 1, value = seq_len(7))
  ))

  expect_snapshot(
    error = TRUE,
    .comp_savitzky_golay(measures, diffs = 0, degree = 2, window = 3)
  )
})

test_that("Savitzky-Golay preserves locations through long recipe steps", {
  skip_if_not_installed("prospectr")
  locations <- seq(30, 14, by = -2)
  index <- seq_along(locations)
  data <- tibble::tibble(
    sample_id = rep(c("a", "b"), each = length(locations)),
    concentration = rep(c(1, 2), each = length(locations)),
    channel = rep(locations, times = 2),
    signal = c(index, index^2)
  )

  result <- recipe(concentration ~ ., data = data) |>
    update_role(sample_id, new_role = "id") |>
    step_measure_input_long(signal, location = vars(channel)) |>
    step_measure_savitzky_golay(window_side = 2, degree = 2) |>
    step_measure_output_long(values_to = "signal", location_to = "channel") |>
    prep() |>
    bake(new_data = NULL)

  expect_equal(result$channel, rep(locations[3:7], times = 2))
  expect_equal(nrow(result), 10L)
})

test_that("savitzky-golay inputs", {
  skip_if_not_installed("prospectr")

  # ------------------------------------------------------------------------------

  rec <-
    recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    prep()

  spect_start <- measure_to_matrix(rec$template$.measures)

  # ------------------------------------------------------------------------------

  arg_inputs <-
    tibble::tribble(
      ~win_side , ~order , ~degree ,
      1L        , 1L     , 4L      ,
      1L        , 3L     , 3L      ,
      1L        , 1L     , 2L      ,
      1L        , 3L     , 5L      ,
      1L        , 2L     , 1L      ,
      1L        , 4L     , 2L      ,
      2L        , 4L     , 4L      ,
      2L        , 0L     , 4L      ,
      2L        , 1L     , 5L      ,
      2L        , 1L     , 3L      ,
      2L        , 0L     , 2L      ,
      2L        , 1L     , 1L      ,
      3L        , 3L     , 5L      ,
      3L        , 3L     , 1L      ,
      3L        , 3L     , 3L      ,
      3L        , 4L     , 2L      ,
      3L        , 0L     , 5L      ,
      3L        , 2L     , 2L      ,
      4L        , 2L     , 4L      ,
      4L        , 0L     , 3L      ,
      4L        , 0L     , 1L      ,
      4L        , 2L     , 5L      ,
      4L        , 4L     , 5L      ,
      4L        , 4L     , 1L      ,
      5L        , 4L     , 3L      ,
      5L        , 2L     , 1L      ,
      5L        , 0L     , 4L      ,
      5L        , 1L     , 3L      ,
      5L        , 3L     , 2L      ,
      5L        , 2L     , 4L
    )

  for (i in seq_len(nrow(arg_inputs))) {
    window_length <- 2 * arg_inputs$win_side[i] + 1

    warn_val <-
      arg_inputs$degree[i] < arg_inputs$order[i] |
      arg_inputs$degree[i] >= window_length

    if (warn_val) {
      expect_snapshot(
        rec |>
          step_measure_savitzky_golay(
            differentiation_order = arg_inputs$order[i],
            window_side = arg_inputs$win_side[i],
            degree = arg_inputs$degree[i]
          ) |>
          prep()
      )
    } else {
      expect_silent(
        rec |>
          step_measure_savitzky_golay(
            differentiation_order = arg_inputs$order[i],
            window_side = arg_inputs$win_side[i],
            degree = arg_inputs$degree[i]
          ) |>
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
    recipe(water + fat + protein ~ ., data = meats_small) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_measure_savitzky_golay() |>
      tunable()
  )
})
