test_that("step_measure_detrend runs in recipe workflow", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_detrend(degree = 1) |>
    prep()

  result <- bake(rec, new_data = NULL)

  expect_s3_class(result, "tbl_df")
  expect_true(".measures" %in% names(result))
  expect_true(is_measure_list(result$.measures))
})

test_that("step_measure_detrend modifies values", {
  rec_original <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    prep()

  rec_detrend <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_detrend(degree = 1) |>
    prep()

  original <- bake(rec_original, new_data = NULL)
  corrected <- bake(rec_detrend, new_data = NULL)

  orig_vals <- original$.measures[[1]]$value
  corr_vals <- corrected$.measures[[1]]$value

  expect_false(all(orig_vals == corr_vals))
})

test_that("step_measure_detrend validates degree", {
  expect_error(
    recipe(water + fat + protein ~ ., data = meats_long) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_measure_detrend(degree = -1) |>
      prep(),
    "degree"
  )
})

test_that("step_measure_detrend degree 0 centers data", {
  # Create synthetic data
  n_points <- 50
  location <- seq(1, 50)
  value <- rep(10, n_points) # Constant offset

  synthetic_data <- tibble::tibble(
    id = rep(1, n_points),
    outcome = rep(1, n_points),
    value = value,
    location = location
  )

  rec <- recipe(outcome ~ ., data = synthetic_data) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(value, location = vars(location)) |>
    step_measure_detrend(degree = 0) |>
    prep()

  result <- bake(rec, new_data = NULL)
  detrended <- result$.measures[[1]]$value

  # Mean should be ~0 after centering
  expect_lt(abs(mean(detrended)), 1e-10)
})

test_that("step_measure_detrend removes linear trend", {
  # Create synthetic data with linear trend
  n_points <- 100
  location <- seq(1, 100)
  trend <- 2 * location + 5 # Linear trend: y = 2x + 5
  signal <- rep(0, n_points)
  value <- signal + trend

  synthetic_data <- tibble::tibble(
    id = rep(1, n_points),
    outcome = rep(1, n_points),
    value = value,
    location = location
  )

  rec <- recipe(outcome ~ ., data = synthetic_data) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(value, location = vars(location)) |>
    step_measure_detrend(degree = 1) |>
    prep()

  result <- bake(rec, new_data = NULL)
  detrended <- result$.measures[[1]]$value

  # After removing linear trend, values should be near zero
  expect_lt(max(abs(detrended)), 1e-10)
})

test_that("step_measure_detrend tidy method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_detrend(degree = 2)

  # Before prep
  tidy_before <- tidy(rec, number = 2)
  expect_equal(tidy_before$terms, "<all measure columns>")
  expect_equal(tidy_before$degree, 2)

  # After prep
  rec_prepped <- prep(rec)
  tidy_after <- tidy(rec_prepped, number = 2)
  expect_equal(tidy_after$terms, ".measures")
  expect_equal(tidy_after$degree, 2)
})

test_that("step_measure_detrend print method shows correct description", {
  rec_linear <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_detrend(degree = 1)

  expect_output(print(rec_linear), "Linear detrending")

  rec_center <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_detrend(degree = 0)

  expect_output(print(rec_center), "Mean centering")

  rec_poly <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_detrend(degree = 3)

  expect_output(print(rec_poly), "Polynomial.*degree 3.*detrending")
})

test_that("step_measure_detrend works with different degrees", {
  for (deg in 0:3) {
    rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_measure_detrend(degree = deg) |>
      prep()

    result <- bake(rec, new_data = NULL)
    expect_true(is_measure_list(result$.measures))
  }
})

test_that("required_pkgs includes measure", {
  step <- step_measure_detrend(
    recipe(water + fat + protein ~ ., data = meats_long)
  )
  expect_true("measure" %in% required_pkgs(step))
})
