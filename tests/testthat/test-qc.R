# ==============================================================================
# Tests for Quality Control Steps
# ==============================================================================

# ------------------------------------------------------------------------------
# step_measure_qc_snr
# ------------------------------------------------------------------------------

test_that("step_measure_qc_snr runs in recipe workflow", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_qc_snr() |>
    prep()

  result <- bake(rec, new_data = NULL)

  expect_s3_class(result, "tbl_df")
  expect_true(".snr" %in% names(result))
  expect_type(result$.snr, "double")
})

test_that("step_measure_qc_snr computes positive SNR for real data", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_qc_snr() |>
    prep()

  result <- bake(rec, new_data = NULL)

  # SNR should be positive for real spectral data
  expect_true(all(result$.snr > 0, na.rm = TRUE))
})

test_that("step_measure_qc_snr different methods work", {
  for (signal in c("max", "range", "rms")) {
    for (noise in c("diff", "mad", "residual")) {
      rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
        update_role(id, new_role = "id") |>
        step_measure_input_long(transmittance, location = vars(channel)) |>
        step_measure_qc_snr(signal_method = signal, noise_method = noise) |>
        prep()

      result <- bake(rec, new_data = NULL)
      expect_true(".snr" %in% names(result))
    }
  }
})

test_that("step_measure_qc_snr custom column name works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_qc_snr(new_col = "my_snr") |>
    prep()

  result <- bake(rec, new_data = NULL)
  expect_true("my_snr" %in% names(result))
})


# ------------------------------------------------------------------------------
# step_measure_qc_saturated
# ------------------------------------------------------------------------------

test_that("step_measure_qc_saturated runs in recipe workflow", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_qc_saturated() |>
    prep()

  result <- bake(rec, new_data = NULL)

  expect_s3_class(result, "tbl_df")
  expect_true(".saturated" %in% names(result))
  expect_true(".sat_pct" %in% names(result))
  expect_type(result$.saturated, "logical")
  expect_type(result$.sat_pct, "double")
})

test_that("step_measure_qc_saturated detects saturation", {
  # Create data with saturation
  n_points <- 100
  values <- seq(1, 100)
  values[90:100] <- 100  # Saturated at max

  synthetic_data <- tibble::tibble(
    id = rep(1, n_points),
    outcome = rep(1, n_points),
    value = values,
    location = seq_len(n_points)
  )

  rec <- recipe(outcome ~ ., data = synthetic_data) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(value, location = vars(location)) |>
    step_measure_qc_saturated() |>
    prep()

  result <- bake(rec, new_data = NULL)

  expect_true(result$.saturated[1])
  expect_gt(result$.sat_pct[1], 0)
})

test_that("step_measure_qc_saturated with manual limits", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_qc_saturated(upper_limit = 3, lower_limit = 2) |>
    prep()

  result <- bake(rec, new_data = NULL)
  expect_true(".saturated" %in% names(result))
})


# ------------------------------------------------------------------------------
# step_measure_impute
# ------------------------------------------------------------------------------

test_that("step_measure_impute runs in recipe workflow", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_impute() |>
    prep()

  result <- bake(rec, new_data = NULL)

  expect_s3_class(result, "tbl_df")
  expect_true(is_measure_list(result$.measures))
})

test_that("step_measure_impute fills NA values", {
  # Create data with NAs
  n_points <- 50
  values <- seq(1, 50, length.out = n_points)
  values[c(10, 20, 30)] <- NA

  synthetic_data <- tibble::tibble(
    id = rep(1, n_points),
    outcome = rep(1, n_points),
    value = values,
    location = seq_len(n_points)
  )

  rec <- recipe(outcome ~ ., data = synthetic_data) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(value, location = vars(location)) |>
    step_measure_impute(method = "linear") |>
    prep()

  result <- bake(rec, new_data = NULL)
  imputed <- result$.measures[[1]]$value

  # No NAs should remain
 expect_false(anyNA(imputed))

  # Imputed values should be reasonable (near linear trend)
  expect_lt(abs(imputed[10] - 10), 1)
  expect_lt(abs(imputed[20] - 20), 1)
  expect_lt(abs(imputed[30] - 30), 1)
})

test_that("step_measure_impute methods work", {
  # Create data with NAs
  n_points <- 50
  values <- rep(10, n_points)
  values[25] <- NA

  synthetic_data <- tibble::tibble(
    id = rep(1, n_points),
    outcome = rep(1, n_points),
    value = values,
    location = seq_len(n_points)
  )

  for (method in c("linear", "spline", "constant", "mean")) {
    rec <- recipe(outcome ~ ., data = synthetic_data) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(value, location = vars(location)) |>
      step_measure_impute(method = method) |>
      prep()

    result <- bake(rec, new_data = NULL)
    imputed <- result$.measures[[1]]$value

    expect_false(is.na(imputed[25]))
    expect_equal(imputed[25], 10)  # All methods should give ~10
  }
})

test_that("step_measure_impute respects max_gap", {
  # Create data with large gap
  n_points <- 50
  values <- rep(10, n_points)
  values[20:30] <- NA  # 11-point gap

  synthetic_data <- tibble::tibble(
    id = rep(1, n_points),
    outcome = rep(1, n_points),
    value = values,
    location = seq_len(n_points)
  )

  rec <- recipe(outcome ~ ., data = synthetic_data) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(value, location = vars(location)) |>
    step_measure_impute(max_gap = 5) |>
    prep()

  result <- bake(rec, new_data = NULL)
  imputed <- result$.measures[[1]]$value

  # Large gap should not be imputed
  expect_true(all(is.na(imputed[20:30])))
})


# ------------------------------------------------------------------------------
# step_measure_qc_outlier
# ------------------------------------------------------------------------------

test_that("step_measure_qc_outlier runs in recipe workflow", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_qc_outlier(threshold = 3) |>
    prep()

  result <- bake(rec, new_data = NULL)

  expect_s3_class(result, "tbl_df")
  expect_true(".outlier" %in% names(result))
  expect_true(".outlier_score" %in% names(result))
  expect_type(result$.outlier, "logical")
  expect_type(result$.outlier_score, "double")
})

test_that("step_measure_qc_outlier both methods work", {
  for (method in c("mahalanobis", "pca")) {
    rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_measure_qc_outlier(method = method, threshold = 3) |>
      prep()

    result <- bake(rec, new_data = NULL)
    expect_true(".outlier" %in% names(result))
    expect_true(".outlier_score" %in% names(result))
  }
})

test_that("step_measure_qc_outlier scores are non-negative", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_qc_outlier() |>
    prep()

  result <- bake(rec, new_data = NULL)

  expect_true(all(result$.outlier_score >= 0))
})

test_that("step_measure_qc_outlier custom column names work", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_qc_outlier(new_col = "is_outlier", new_col_score = "outlier_dist") |>
    prep()

  result <- bake(rec, new_data = NULL)
  expect_true("is_outlier" %in% names(result))
  expect_true("outlier_dist" %in% names(result))
})
