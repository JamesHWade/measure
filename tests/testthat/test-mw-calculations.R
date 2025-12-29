# ==============================================================================
# Tests for molecular weight calculations
# ==============================================================================

# ==============================================================================
# Test Helpers
# ==============================================================================

# Create synthetic GPC data with known properties
create_gpc_data <- function() {
  # Simulate a GPC chromatogram
  # X-axis: log10(MW) from 6 to 2 (high MW elutes first)
  log_mw <- seq(6, 2, length.out = 100)

  # Single Gaussian peak centered at log10(MW) = 4 (MW = 10000)
  # This simulates a monodisperse polymer
  signal <- exp(-((log_mw - 4)^2) / 0.5)

  tibble::tibble(
    id = "sample1",
    log_mw = log_mw,
    signal = signal
  )
}

# Create bimodal GPC data
create_bimodal_gpc_data <- function() {
  log_mw <- seq(6, 2, length.out = 100)

  # Two peaks at log10(MW) = 5 and 3
  signal <- 0.6 * exp(-((log_mw - 5)^2) / 0.3) +
            0.4 * exp(-((log_mw - 3)^2) / 0.2)

  tibble::tibble(
    id = "sample1",
    log_mw = log_mw,
    signal = signal
  )
}

# ==============================================================================
# step_measure_mw_averages tests
# ==============================================================================

test_that("step_measure_mw_averages calculates all metrics", {
  data <- create_gpc_data()

  rec <- recipe(~., data = data) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(signal, location = vars(log_mw)) |>
    step_measure_mw_averages() |>
    prep()

  result <- bake(rec, new_data = NULL)

  expect_true("mw_mn" %in% names(result))
  expect_true("mw_mw" %in% names(result))
  expect_true("mw_mz" %in% names(result))
  expect_true("mw_mp" %in% names(result))
  expect_true("mw_dispersity" %in% names(result))
})

test_that("step_measure_mw_averages produces reasonable values for Gaussian peak", {
  data <- create_gpc_data()

  rec <- recipe(~., data = data) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(signal, location = vars(log_mw)) |>
    step_measure_mw_averages() |>
    prep()

  result <- bake(rec, new_data = NULL)

  # For a Gaussian peak centered at log10(MW) = 4, Mp should be ~10000
  expect_gt(result$mw_mp, 5000)
  expect_lt(result$mw_mp, 20000)

  # Dispersity should be > 1 (always true for polymers)
  expect_gt(result$mw_dispersity, 1)

  # Mz >= Mw >= Mn (by definition)
  expect_gte(result$mw_mz, result$mw_mw)
  expect_gte(result$mw_mw, result$mw_mn)
})

test_that("step_measure_mw_averages respects output_cols", {
  data <- create_gpc_data()

  rec <- recipe(~., data = data) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(signal, location = vars(log_mw)) |>
    step_measure_mw_averages(output_cols = c("mn", "mw")) |>
    prep()

  result <- bake(rec, new_data = NULL)

  expect_true("mw_mn" %in% names(result))
  expect_true("mw_mw" %in% names(result))
  expect_false("mw_mz" %in% names(result))
  expect_false("mw_mp" %in% names(result))
})

test_that("step_measure_mw_averages respects prefix", {
  data <- create_gpc_data()

  rec <- recipe(~., data = data) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(signal, location = vars(log_mw)) |>
    step_measure_mw_averages(prefix = "sec_") |>
    prep()

  result <- bake(rec, new_data = NULL)

  expect_true("sec_mn" %in% names(result))
  expect_false("mw_mn" %in% names(result))
})

test_that("step_measure_mw_averages with linear calibration works", {
  # Create data with retention time on x-axis
  rt <- seq(5, 15, length.out = 100)
  signal <- exp(-((rt - 10)^2) / 2)

  data <- tibble::tibble(id = "sample1", rt = rt, signal = signal)

  # Calibration: log10(MW) = -0.5 * RT + 9
  # At RT = 10, log10(MW) = 4, so MW = 10000
  rec <- recipe(~., data = data) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(signal, location = vars(rt)) |>
    step_measure_mw_averages(calibration = c(-0.5, 9)) |>
    prep()

  result <- bake(rec, new_data = NULL)

  # Mp should be around 10000
  expect_gt(result$mw_mp, 5000)
  expect_lt(result$mw_mp, 20000)
})

test_that("step_measure_mw_averages with integration_range works", {
  data <- create_bimodal_gpc_data()

  # Only integrate the high MW peak (log_mw 4.5-6)
  rec <- recipe(~., data = data) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(signal, location = vars(log_mw)) |>
    step_measure_mw_averages(integration_range = c(4.5, 6)) |>
    prep()

  result <- bake(rec, new_data = NULL)

  # Mp should be around 10^5 = 100000 (high MW peak)
  expect_gt(result$mw_mp, 50000)
})

test_that("step_measure_mw_averages errors with invalid output_cols", {
  expect_error(
    recipe(~., data = create_gpc_data()) |>
      step_measure_mw_averages(output_cols = c("invalid")),
    "Invalid output columns"
  )
})

test_that("step_measure_mw_averages print method works", {
  rec <- recipe(~., data = create_gpc_data()) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(signal, location = vars(log_mw)) |>
    step_measure_mw_averages()

  expect_output(print(rec$steps[[2]]), "MW averages")
})

test_that("step_measure_mw_averages tidy method works", {
  data <- create_gpc_data()

  rec <- recipe(~., data = data) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(signal, location = vars(log_mw)) |>
    step_measure_mw_averages() |>
    prep()

  tidy_result <- tidy(rec, number = 2)
  expect_s3_class(tidy_result, "tbl_df")
  expect_true("prefix" %in% names(tidy_result))
})

test_that("step_measure_mw_averages handles multiple samples", {
  # Create data with 3 samples
  data <- tibble::tibble(
    id = rep(c("s1", "s2", "s3"), each = 100),
    log_mw = rep(seq(6, 2, length.out = 100), 3),
    signal = c(
      exp(-((seq(6, 2, length.out = 100) - 4)^2) / 0.5),    # centered at 4
      exp(-((seq(6, 2, length.out = 100) - 4.5)^2) / 0.5),  # centered at 4.5
      exp(-((seq(6, 2, length.out = 100) - 3.5)^2) / 0.5)   # centered at 3.5
    )
  )

  rec <- recipe(~., data = data) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(signal, location = vars(log_mw)) |>
    step_measure_mw_averages(output_cols = c("mp")) |>
    prep()

  result <- bake(rec, new_data = NULL)

  expect_equal(nrow(result), 3)
  # Mp values should differ between samples
  expect_true(length(unique(result$mw_mp)) > 1)
})

# ==============================================================================
# Integration tests
# ==============================================================================

test_that("MW calculations work with baseline correction", {
  data <- create_gpc_data()
  # Add a baseline offset
  data$signal <- data$signal + 0.1

  rec <- recipe(~., data = data) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(signal, location = vars(log_mw)) |>
    step_measure_baseline_rolling(window_size = 20) |>
    step_measure_mw_averages() |>
    prep()

  result <- bake(rec, new_data = NULL)

  expect_true("mw_mn" %in% names(result))
  expect_false(is.na(result$mw_mn))
})
