test_that("step_measure_baseline_gpc runs in recipe workflow", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_baseline_gpc(left_frac = 0.1, right_frac = 0.1) |>
    prep()

  result <- bake(rec, new_data = NULL)

  expect_s3_class(result, "tbl_df")
  expect_true(".measures" %in% names(result))
  expect_true(is_measure_list(result$.measures))
})

test_that("step_measure_baseline_gpc modifies values", {
  rec_original <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    prep()

  rec_gpc <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_baseline_gpc() |>
    prep()

  original <- bake(rec_original, new_data = NULL)
  corrected <- bake(rec_gpc, new_data = NULL)

  orig_vals <- original$.measures[[1]]$value
  corr_vals <- corrected$.measures[[1]]$value

  expect_false(all(orig_vals == corr_vals))
})

test_that("step_measure_baseline_gpc validates left_frac", {
  expect_error(
    recipe(water + fat + protein ~ ., data = meats_small) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_measure_baseline_gpc(left_frac = 0) |>
      prep(),
    "left_frac"
  )

  expect_error(
    recipe(water + fat + protein ~ ., data = meats_small) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_measure_baseline_gpc(left_frac = 0.6) |>
      prep(),
    "left_frac"
  )

  expect_error(
    recipe(water + fat + protein ~ ., data = meats_small) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_measure_baseline_gpc(left_frac = -0.1) |>
      prep(),
    "left_frac"
  )
})

test_that("step_measure_baseline_gpc validates right_frac", {
  expect_error(
    recipe(water + fat + protein ~ ., data = meats_small) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_measure_baseline_gpc(right_frac = 0) |>
      prep(),
    "right_frac"
  )

  expect_error(
    recipe(water + fat + protein ~ ., data = meats_small) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_measure_baseline_gpc(right_frac = 0.6) |>
      prep(),
    "right_frac"
  )
})

test_that("step_measure_baseline_gpc validates combined fractions", {
  expect_error(
    recipe(water + fat + protein ~ ., data = meats_small) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_measure_baseline_gpc(left_frac = 0.4, right_frac = 0.4) |>
      prep(),
    "left_frac.*right_frac"
  )
})

test_that("step_measure_baseline_gpc validates method", {
  expect_error(
    recipe(water + fat + protein ~ ., data = meats_small) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_measure_baseline_gpc(method = "invalid") |>
      prep(),
    "method"
  )
})

test_that("step_measure_baseline_gpc removes linear baseline", {
  # Create synthetic data with linear baseline
  n_points <- 100
  location <- seq(1, 100)

  # True signal: Gaussian peak
  signal <- 10 * exp(-((location - 50)^2) / (2 * 10^2))

  # Linear baseline
  baseline <- 0.1 * location + 5

  # Observed = signal + baseline
  observed <- signal + baseline

  synthetic_data <- tibble::tibble(
    id = rep(1, n_points),
    outcome = rep(1, n_points),
    value = observed,
    location = location
  )

  rec <- recipe(outcome ~ ., data = synthetic_data) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(value, location = vars(location)) |>
    step_measure_baseline_gpc(left_frac = 0.1, right_frac = 0.1) |>
    prep()

  result <- bake(rec, new_data = NULL)
  corrected <- result$.measures[[1]]$value

  # In the baseline regions (first and last 10%), values should be near zero
  left_region <- corrected[1:10]
  right_region <- corrected[91:100]

  # These should be close to zero (allowing some tolerance for the peak tails)
  expect_lt(abs(mean(left_region)), 1)
  expect_lt(abs(mean(right_region)), 1)
})

test_that("step_measure_baseline_gpc linear method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_baseline_gpc(method = "linear") |>
    prep()

  result <- bake(rec, new_data = NULL)
  expect_true(is_measure_list(result$.measures))
})

test_that("step_measure_baseline_gpc median method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_baseline_gpc(method = "median") |>
    prep()

  result <- bake(rec, new_data = NULL)
  expect_true(is_measure_list(result$.measures))
})

test_that("step_measure_baseline_gpc spline method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_baseline_gpc(method = "spline") |>
    prep()

  result <- bake(rec, new_data = NULL)
  expect_true(is_measure_list(result$.measures))
})

test_that("step_measure_baseline_gpc tidy method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_baseline_gpc(
      left_frac = 0.1,
      right_frac = 0.15,
      method = "median"
    )

  # Before prep
  tidy_before <- tidy(rec, number = 2)
  expect_equal(tidy_before$terms, "<all measure columns>")
  expect_equal(tidy_before$left_frac, 0.1)
  expect_equal(tidy_before$right_frac, 0.15)
  expect_equal(tidy_before$method, "median")

  # After prep
  rec_prepped <- prep(rec)
  tidy_after <- tidy(rec_prepped, number = 2)
  expect_equal(tidy_after$terms, ".measures")
  expect_equal(tidy_after$left_frac, 0.1)
  expect_equal(tidy_after$right_frac, 0.15)
  expect_equal(tidy_after$method, "median")
})

test_that("step_measure_baseline_gpc print method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_baseline_gpc()

  expect_output(print(rec), "GPC/SEC baseline correction")
})

test_that("step_measure_baseline_gpc works with different fraction values", {
  for (frac in c(0.05, 0.1, 0.2)) {
    rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_measure_baseline_gpc(left_frac = frac, right_frac = frac) |>
      prep()

    result <- bake(rec, new_data = NULL)
    expect_true(is_measure_list(result$.measures))
  }
})

test_that("required_pkgs includes measure", {
  step <- step_measure_baseline_gpc(
    recipe(water + fat + protein ~ ., data = meats_small)
  )
  expect_true("measure" %in% required_pkgs(step))
})
