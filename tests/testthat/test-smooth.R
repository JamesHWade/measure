# ==============================================================================
# Tests for Smoothing Steps
# ==============================================================================

# ------------------------------------------------------------------------------
# step_measure_smooth_ma
# ------------------------------------------------------------------------------

test_that("step_measure_smooth_ma runs in recipe workflow", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_smooth_ma(window = 5) |>
    prep()

  result <- bake(rec, new_data = NULL)

  expect_s3_class(result, "tbl_df")
  expect_true(".measures" %in% names(result))
  expect_true(is_measure_list(result$.measures))
})

test_that("step_measure_smooth_ma reduces noise", {
  rec_original <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    prep()

  rec_smooth <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_smooth_ma(window = 11) |>
    prep()

  original <- bake(rec_original, new_data = NULL)
  smoothed <- bake(rec_smooth, new_data = NULL)

  orig_vals <- original$.measures[[1]]$value
  smooth_vals <- smoothed$.measures[[1]]$value

  # Smoothing should reduce variance of differences (noise)
  orig_diff_var <- var(diff(orig_vals))
  smooth_diff_var <- var(diff(smooth_vals))

  expect_lt(smooth_diff_var, orig_diff_var)
})

test_that("step_measure_smooth_ma errors on even window", {
  expect_error(
    recipe(water + fat + protein ~ ., data = meats_long) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_measure_smooth_ma(window = 6) |>
      prep(),
    "odd"
  )
})

test_that("step_measure_smooth_ma validates window", {
  expect_error(
    recipe(water + fat + protein ~ ., data = meats_long) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_measure_smooth_ma(window = 2) |>
      prep(),
    "window"
  )
})

test_that("step_measure_smooth_ma tidy method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_smooth_ma(window = 7)

  tidy_before <- tidy(rec, number = 2)
  expect_equal(tidy_before$window, 7)

  rec_prepped <- prep(rec)
  tidy_after <- tidy(rec_prepped, number = 2)
  expect_equal(tidy_after$window, 7)
})


# ------------------------------------------------------------------------------
# step_measure_smooth_median
# ------------------------------------------------------------------------------

test_that("step_measure_smooth_median runs in recipe workflow", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_smooth_median(window = 5) |>
    prep()

  result <- bake(rec, new_data = NULL)

  expect_s3_class(result, "tbl_df")
  expect_true(is_measure_list(result$.measures))
})

test_that("step_measure_smooth_median removes spikes", {
  # Create data with a spike
  n_points <- 100
  values <- rep(10, n_points)
  values[50] <- 100 # Spike

  synthetic_data <- tibble::tibble(
    id = rep(1, n_points),
    outcome = rep(1, n_points),
    value = values,
    location = seq_len(n_points)
  )

  rec <- recipe(outcome ~ ., data = synthetic_data) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(value, location = vars(location)) |>
    step_measure_smooth_median(window = 5) |>
    prep()

  result <- bake(rec, new_data = NULL)
  filtered <- result$.measures[[1]]$value

  # Spike should be greatly reduced
  expect_lt(filtered[50], 50)
})


# ------------------------------------------------------------------------------
# step_measure_smooth_gaussian
# ------------------------------------------------------------------------------

test_that("step_measure_smooth_gaussian runs in recipe workflow", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_smooth_gaussian(sigma = 2) |>
    prep()

  result <- bake(rec, new_data = NULL)

  expect_s3_class(result, "tbl_df")
  expect_true(is_measure_list(result$.measures))
})

test_that("step_measure_smooth_gaussian validates sigma", {
  expect_error(
    recipe(water + fat + protein ~ ., data = meats_long) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_measure_smooth_gaussian(sigma = -1) |>
      prep(),
    "sigma"
  )
})

test_that("step_measure_smooth_gaussian auto-calculates window", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_smooth_gaussian(sigma = 3) |>
    prep()

  # Window should be set based on 6-sigma rule
  tidy_result <- tidy(rec, number = 2)
  expect_true(tidy_result$window >= 17) # 6*3 = 18, rounded to odd
})


# ------------------------------------------------------------------------------
# step_measure_filter_fourier
# ------------------------------------------------------------------------------

test_that("step_measure_filter_fourier runs in recipe workflow", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_filter_fourier(cutoff = 0.1) |>
    prep()

  result <- bake(rec, new_data = NULL)

  expect_s3_class(result, "tbl_df")
  expect_true(is_measure_list(result$.measures))
})

test_that("step_measure_filter_fourier validates cutoff", {
  expect_error(
    recipe(water + fat + protein ~ ., data = meats_long) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_measure_filter_fourier(cutoff = 0.6) |>
      prep(),
    "cutoff"
  )
})

test_that("step_measure_filter_fourier removes high frequencies", {
  rec_original <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    prep()

  rec_filtered <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_filter_fourier(cutoff = 0.2, type = "lowpass") |>
    prep()

  original <- bake(rec_original, new_data = NULL)
  filtered <- bake(rec_filtered, new_data = NULL)

  # Values should be different after filtering
  orig_vals <- original$.measures[[1]]$value
  filt_vals <- filtered$.measures[[1]]$value

  expect_false(all(orig_vals == filt_vals))
})

test_that("step_measure_filter_fourier tidy method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_filter_fourier(cutoff = 0.2, type = "lowpass")

  tidy_before <- tidy(rec, number = 2)
  expect_equal(tidy_before$cutoff, 0.2)
  expect_equal(tidy_before$type, "lowpass")
})
