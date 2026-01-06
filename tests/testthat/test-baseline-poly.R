test_that("step_measure_baseline_poly runs in recipe workflow", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_baseline_poly(degree = 2) |>
    prep()

  result <- bake(rec, new_data = NULL)

  expect_s3_class(result, "tbl_df")
  expect_true(".measures" %in% names(result))
  expect_true(is_measure_list(result$.measures))
})

test_that("step_measure_baseline_poly modifies values", {
  rec_original <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    prep()

  rec_poly <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_baseline_poly(degree = 2) |>
    prep()

  original <- bake(rec_original, new_data = NULL)
  corrected <- bake(rec_poly, new_data = NULL)

  orig_vals <- original$.measures[[1]]$value
  corr_vals <- corrected$.measures[[1]]$value

  expect_false(all(orig_vals == corr_vals))
})

test_that("step_measure_baseline_poly validates degree", {
  expect_error(
    recipe(water + fat + protein ~ ., data = meats_small) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_measure_baseline_poly(degree = 0) |>
      prep(),
    "degree"
  )

  expect_error(
    recipe(water + fat + protein ~ ., data = meats_small) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_measure_baseline_poly(degree = -1) |>
      prep(),
    "degree"
  )
})

test_that("step_measure_baseline_poly validates max_iter", {
  expect_error(
    recipe(water + fat + protein ~ ., data = meats_small) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_measure_baseline_poly(max_iter = -1) |>
      prep(),
    "max_iter"
  )
})

test_that("step_measure_baseline_poly tidy method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_baseline_poly(degree = 3)

  # Before prep
  tidy_before <- tidy(rec, number = 2)
  expect_equal(tidy_before$terms, "<all measure columns>")
  expect_equal(tidy_before$degree, 3)

  # After prep
  rec_prepped <- prep(rec)
  tidy_after <- tidy(rec_prepped, number = 2)
  expect_equal(tidy_after$terms, ".measures")
  expect_equal(tidy_after$degree, 3)
})

test_that("step_measure_baseline_poly print method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_baseline_poly()

  expect_output(print(rec), "Polynomial baseline correction")
})

test_that("step_measure_baseline_poly works with iterative fitting", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_baseline_poly(degree = 2, max_iter = 5, threshold = 1.5) |>
    prep()

  result <- bake(rec, new_data = NULL)

  expect_s3_class(result, "tbl_df")
  expect_true(is_measure_list(result$.measures))
})

test_that("step_measure_baseline_poly removes known polynomial baseline", {
  # Create synthetic data with known polynomial baseline
  n_points <- 100
  location <- seq(1, 100, length.out = n_points)

  # True signal: constant (or small peaks)
  true_signal <- rep(0, n_points)

  # Baseline: quadratic
  baseline <- 0.5 + 0.01 * location + 0.0002 * location^2

  # Observed = signal + baseline
  observed <- true_signal + baseline

  synthetic_data <- tibble::tibble(
    id = rep(1, n_points),
    outcome = rep(1, n_points),
    value = observed,
    location = location
  )

  rec <- recipe(outcome ~ ., data = synthetic_data) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(value, location = vars(location)) |>
    step_measure_baseline_poly(degree = 2) |>
    prep()

  result <- bake(rec, new_data = NULL)
  corrected <- result$.measures[[1]]$value

  # After removing quadratic baseline, values should be near zero
  expect_lt(max(abs(corrected)), 1e-6)
})

test_that("different polynomial degrees work", {
  for (deg in 1:4) {
    rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_measure_baseline_poly(degree = deg) |>
      prep()

    result <- bake(rec, new_data = NULL)
    expect_true(is_measure_list(result$.measures))
  }
})

test_that("required_pkgs includes measure", {
  step <- step_measure_baseline_poly(
    recipe(water + fat + protein ~ ., data = meats_small)
  )
  expect_true("measure" %in% required_pkgs(step))
})
