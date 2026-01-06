test_that("step_measure_baseline_als runs in recipe workflow", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_baseline_als(lambda = 1e5, p = 0.01) |>
    prep()

  result <- bake(rec, new_data = NULL)

  expect_s3_class(result, "tbl_df")
  expect_true(".measures" %in% names(result))
  expect_true(is_measure_list(result$.measures))
})

test_that("step_measure_baseline_als modifies values", {
  rec_original <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    prep()

  rec_als <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_baseline_als(lambda = 1e5, p = 0.01) |>
    prep()

  original <- bake(rec_original, new_data = NULL)
  corrected <- bake(rec_als, new_data = NULL)

  # Values should differ after baseline correction
  orig_vals <- original$.measures[[1]]$value
  corr_vals <- corrected$.measures[[1]]$value

  expect_false(all(orig_vals == corr_vals))
})

test_that("step_measure_baseline_als validates lambda", {
  expect_error(
    recipe(water + fat + protein ~ ., data = meats_small) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_measure_baseline_als(lambda = -1) |>
      prep(),
    "lambda"
  )

  expect_error(
    recipe(water + fat + protein ~ ., data = meats_small) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_measure_baseline_als(lambda = 0) |>
      prep(),
    "lambda"
  )
})

test_that("step_measure_baseline_als validates p", {
  expect_error(
    recipe(water + fat + protein ~ ., data = meats_small) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_measure_baseline_als(p = 0) |>
      prep(),
    "p"
  )

  expect_error(
    recipe(water + fat + protein ~ ., data = meats_small) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_measure_baseline_als(p = 1) |>
      prep(),
    "p"
  )

  expect_error(
    recipe(water + fat + protein ~ ., data = meats_small) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_measure_baseline_als(p = 1.5) |>
      prep(),
    "p"
  )
})

test_that("step_measure_baseline_als tidy method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_baseline_als(lambda = 1e6, p = 0.02)

  # Before prep
  tidy_before <- tidy(rec, number = 2)
  expect_equal(tidy_before$terms, "<all measure columns>")
  expect_equal(tidy_before$lambda, 1e6)
  expect_equal(tidy_before$p, 0.02)

  # After prep
  rec_prepped <- prep(rec)
  tidy_after <- tidy(rec_prepped, number = 2)
  expect_equal(tidy_after$terms, ".measures")
  expect_equal(tidy_after$lambda, 1e6)
  expect_equal(tidy_after$p, 0.02)
})

test_that("step_measure_baseline_als print method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_baseline_als()

  expect_output(print(rec), "ALS baseline correction")
})

test_that("step_measure_baseline_als works with synthetic baseline", {
  # Create synthetic data with known polynomial baseline
  n_points <- 100
  location <- seq(1, 100, length.out = n_points)

  # True signal: peaks
  true_signal <- exp(-((location - 30)^2) / 50) +
    0.5 * exp(-((location - 70)^2) / 30)

  # Baseline: quadratic
  baseline <- 0.5 + 0.01 * location + 0.0001 * location^2

  # Observed = signal + baseline
  observed <- true_signal + baseline

  # Create data in long format
  synthetic_data <- tibble::tibble(
    id = rep(1, n_points),
    outcome = rep(1, n_points),
    value = observed,
    location = location
  )

  rec <- recipe(outcome ~ ., data = synthetic_data) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(value, location = vars(location)) |>
    step_measure_baseline_als(lambda = 1e4, p = 0.001, max_iter = 10) |>
    prep()

  result <- bake(rec, new_data = NULL)
  corrected <- result$.measures[[1]]$value

  # The corrected signal should be closer to the true signal than the observed
  # (baseline should be largely removed)
  error_before <- mean((observed - true_signal)^2)
  error_after <- mean((corrected - true_signal)^2)

  # After correction, error should be reduced
  expect_lt(error_after, error_before)
})

test_that("required_pkgs includes measure", {
  step <- step_measure_baseline_als(
    recipe(water + fat + protein ~ ., data = meats_small)
  )
  expect_true("measure" %in% required_pkgs(step))
})
