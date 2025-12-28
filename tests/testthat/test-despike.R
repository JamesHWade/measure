# ==============================================================================
# Tests for Despike Step
# ==============================================================================

test_that("step_measure_despike runs in recipe workflow", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_despike(threshold = 5) |>
    prep()

  result <- bake(rec, new_data = NULL)

  expect_s3_class(result, "tbl_df")
  expect_true(".measures" %in% names(result))
  expect_true(is_measure_list(result$.measures))
})

test_that("step_measure_despike removes spikes", {
  # Create data with spikes
  n_points <- 100
  values <- rep(10, n_points)
  values[30] <- 100  # Large positive spike
  values[70] <- -80  # Large negative spike

  synthetic_data <- tibble::tibble(
    id = rep(1, n_points),
    outcome = rep(1, n_points),
    value = values,
    location = seq_len(n_points)
  )

  rec <- recipe(outcome ~ ., data = synthetic_data) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(value, location = vars(location)) |>
    step_measure_despike(threshold = 3, window = 5) |>
    prep()

  result <- bake(rec, new_data = NULL)
  despiked <- result$.measures[[1]]$value

  # Spikes should be replaced with values close to background
  expect_lt(abs(despiked[30] - 10), 5)
  expect_lt(abs(despiked[70] - 10), 5)
})

test_that("step_measure_despike respects max_width", {
  # Create data with wide anomaly (not a spike)
  n_points <- 100
  values <- rep(10, n_points)
  values[45:55] <- 50  # Wide peak, not a spike

  synthetic_data <- tibble::tibble(
    id = rep(1, n_points),
    outcome = rep(1, n_points),
    value = values,
    location = seq_len(n_points)
  )

  rec <- recipe(outcome ~ ., data = synthetic_data) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(value, location = vars(location)) |>
    step_measure_despike(threshold = 3, max_width = 3) |>
    prep()

  result <- bake(rec, new_data = NULL)
  despiked <- result$.measures[[1]]$value

  # Wide peak should not be removed (width > max_width)
  expect_equal(despiked[50], 50)
})

test_that("step_measure_despike methods work", {
  # Create data with a single spike
  n_points <- 50
  values <- rep(10, n_points)
  values[25] <- 100

  synthetic_data <- tibble::tibble(
    id = rep(1, n_points),
    outcome = rep(1, n_points),
    value = values,
    location = seq_len(n_points)
  )

  for (method in c("interpolate", "median", "mean")) {
    rec <- recipe(outcome ~ ., data = synthetic_data) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(value, location = vars(location)) |>
      step_measure_despike(method = method, threshold = 3) |>
      prep()

    result <- bake(rec, new_data = NULL)
    despiked <- result$.measures[[1]]$value

    # All methods should reduce the spike
    expect_lt(despiked[25], 50)
  }
})

test_that("step_measure_despike validates parameters", {
  expect_error(
    recipe(water + fat + protein ~ ., data = meats_long) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_measure_despike(window = 2) |>
      prep(),
    "window"
  )

  expect_error(
    recipe(water + fat + protein ~ ., data = meats_long) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_measure_despike(threshold = -1) |>
      prep(),
    "threshold"
  )
})

test_that("step_measure_despike tidy method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_despike(window = 7, threshold = 4, method = "median")

  tidy_before <- tidy(rec, number = 2)
  expect_equal(tidy_before$window, 7)
  expect_equal(tidy_before$threshold, 4)
  expect_equal(tidy_before$method, "median")
})

test_that("step_measure_despike leaves clean data unchanged", {
  # Create clean data (no spikes)
  n_points <- 50
  values <- seq(1, 50)

  synthetic_data <- tibble::tibble(
    id = rep(1, n_points),
    outcome = rep(1, n_points),
    value = values,
    location = seq_len(n_points)
  )

  rec <- recipe(outcome ~ ., data = synthetic_data) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(value, location = vars(location)) |>
    step_measure_despike(threshold = 5) |>
    prep()

  result <- bake(rec, new_data = NULL)
  despiked <- result$.measures[[1]]$value

  # Clean data should be unchanged
  expect_equal(despiked, values)
})
