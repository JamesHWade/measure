# ==============================================================================
# Tests for Wavelet Denoising Step
# ==============================================================================

test_that("step_measure_smooth_wavelet runs in recipe workflow", {
  skip_if_not_installed("wavethresh")

  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_smooth_wavelet() |>
    prep()

  result <- bake(rec, new_data = NULL)

  expect_s3_class(result, "tbl_df")
  expect_true(".measures" %in% names(result))
  expect_true(is_measure_list(result$.measures))
})

test_that("step_measure_smooth_wavelet processes synthetic data", {
  skip_if_not_installed("wavethresh")

  # Create noisy synthetic data
  set.seed(123)
  n_points <- 128 # Power of 2 for wavelet
  signal <- sin(seq(0, 4 * pi, length.out = n_points))
  noisy <- signal + rnorm(n_points, sd = 0.3)

  synthetic_data <- tibble::tibble(
    id = rep(1, n_points),
    outcome = rep(1, n_points),
    value = noisy,
    location = seq_len(n_points)
  )

  rec <- recipe(outcome ~ ., data = synthetic_data) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(value, location = vars(location)) |>
    step_measure_smooth_wavelet() |>
    prep()

  result <- bake(rec, new_data = NULL)

  # Should produce valid output
  expect_s3_class(result, "tbl_df")
  expect_true(is_measure_list(result$.measures))
  expect_equal(length(result$.measures[[1]]$value), n_points)
})

test_that("step_measure_smooth_wavelet threshold types work", {
  skip_if_not_installed("wavethresh")

  for (type in c("soft", "hard")) {
    rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_measure_smooth_wavelet(threshold_type = type) |>
      prep()

    result <- bake(rec, new_data = NULL)
    expect_true(is_measure_list(result$.measures))
  }
})

test_that("step_measure_smooth_wavelet threshold policies work", {
  skip_if_not_installed("wavethresh")

  for (policy in c("universal", "sure")) {
    rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_measure_smooth_wavelet(threshold_policy = policy) |>
      prep()

    result <- bake(rec, new_data = NULL)
    expect_true(is_measure_list(result$.measures))
  }
})

test_that("step_measure_smooth_wavelet requires wavethresh package", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_smooth_wavelet()

  pkgs <- required_pkgs(rec$steps[[2]])
  expect_true("wavethresh" %in% pkgs)
})

test_that("step_measure_smooth_wavelet tidy method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_smooth_wavelet(wavelet = "DaubExPhase", filter_number = 8)

  tidy_result <- tidy(rec, number = 2)
  expect_equal(tidy_result$wavelet, "DaubExPhase")
  expect_equal(tidy_result$filter_number, 8)
})

test_that("step_measure_smooth_wavelet handles short signals", {
  skip_if_not_installed("wavethresh")

  # Create short signal
  n_points <- 6 # Very short
  synthetic_data <- tibble::tibble(
    id = rep(1, n_points),
    outcome = rep(1, n_points),
    value = rnorm(n_points),
    location = seq_len(n_points)
  )

  # Should warn about short signal and return unchanged
  expect_warning(
    rec <- recipe(outcome ~ ., data = synthetic_data) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(value, location = vars(location)) |>
      step_measure_smooth_wavelet() |>
      prep(),
    "too short"
  )

  result <- bake(rec, new_data = NULL)
  expect_s3_class(result, "tbl_df")
})

test_that("step_measure_smooth_wavelet different wavelets work", {
  skip_if_not_installed("wavethresh")

  for (wavelet in c("DaubExPhase", "DaubLeAsymm")) {
    rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_measure_smooth_wavelet(wavelet = wavelet) |>
      prep()

    result <- bake(rec, new_data = NULL)
    expect_true(is_measure_list(result$.measures))
  }
})
