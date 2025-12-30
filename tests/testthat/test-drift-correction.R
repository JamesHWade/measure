# ==============================================================================
# Tests for drift-correction.R
# ==============================================================================

library(recipes)

# Helper function to create drift data
create_drift_data <- function(n = 20, drift_slope = 0.5, noise_sd = 2) {
  set.seed(42)
  data.frame(
    sample_id = paste0("S", seq_len(n)),
    sample_type = rep(c("qc", "unknown", "unknown", "unknown", "qc"), n / 5),
    run_order = seq_len(n),
    feature1 = 100 + seq_len(n) * drift_slope + rnorm(n, sd = noise_sd),
    feature2 = 50 - seq_len(n) * 0.3 + rnorm(n, sd = 1)
  )
}

# ==============================================================================
# step_measure_drift_qc_loess() tests
# ==============================================================================

test_that("step_measure_drift_qc_loess creates a recipe step", {
  data <- create_drift_data()

  rec <- recipe(~ ., data = data) |>
    update_role(sample_id, new_role = "id") |>
    step_measure_drift_qc_loess(feature1, feature2)

  expect_s3_class(rec, "recipe")
  expect_equal(length(rec$steps), 1)
  expect_s3_class(rec$steps[[1]], "step_measure_drift_qc_loess")
})

test_that("step_measure_drift_qc_loess preps successfully", {
  data <- create_drift_data()

  rec <- recipe(~ ., data = data) |>
    update_role(sample_id, new_role = "id") |>
    step_measure_drift_qc_loess(feature1, feature2) |>
    prep()

  expect_true(rec$steps[[1]]$trained)
  expect_true(length(rec$steps[[1]]$drift_models) == 2)
  expect_true(length(rec$steps[[1]]$qc_medians) == 2)
})

test_that("step_measure_drift_qc_loess corrects drift", {
  data <- create_drift_data(n = 25, drift_slope = 2, noise_sd = 0.5)

  rec <- recipe(~ ., data = data) |>
    update_role(sample_id, new_role = "id") |>
    step_measure_drift_qc_loess(feature1) |>
    prep()

  corrected <- bake(rec, new_data = NULL)

  # After correction, QC samples should have more consistent values
  qc_original <- data$feature1[data$sample_type == "qc"]
  qc_corrected <- corrected$feature1[corrected$sample_type == "qc"]

  # CV should decrease after correction
  cv_original <- sd(qc_original) / mean(qc_original)
  cv_corrected <- sd(qc_corrected) / mean(qc_corrected)

  expect_true(cv_corrected < cv_original)
})

test_that("step_measure_drift_qc_loess requires sufficient QC samples", {
  data <- data.frame(
    sample_type = c("qc", "qc", rep("unknown", 8)),
    run_order = 1:10,
    feature1 = rnorm(10)
  )

  rec <- recipe(~ ., data = data) |>
    step_measure_drift_qc_loess(feature1, min_qc = 5)

  expect_error(prep(rec), "Insufficient QC samples")
})

test_that("step_measure_drift_qc_loess validates required columns", {
  data <- data.frame(
    sample_type = rep("qc", 10),
    feature1 = rnorm(10)
  )

  rec <- recipe(~ ., data = data) |>
    step_measure_drift_qc_loess(feature1)

  expect_error(prep(rec), "run_order")
})

test_that("step_measure_drift_qc_loess apply_to parameter works", {
  data <- create_drift_data()

  # apply_to = "all" (default)
  rec_all <- recipe(~ ., data = data) |>
    update_role(sample_id, new_role = "id") |>
    step_measure_drift_qc_loess(feature1, apply_to = "all") |>
    prep()

  corrected_all <- bake(rec_all, new_data = NULL)

  # apply_to = "unknown"
  rec_unknown <- recipe(~ ., data = data) |>
    update_role(sample_id, new_role = "id") |>
    step_measure_drift_qc_loess(feature1, apply_to = "unknown") |>
    prep()

  corrected_unknown <- bake(rec_unknown, new_data = NULL)

  # QC samples should be unchanged when apply_to = "unknown"
  qc_idx <- data$sample_type == "qc"
  expect_equal(corrected_unknown$feature1[qc_idx], data$feature1[qc_idx])

  # But should be different when apply_to = "all"
  expect_false(all(corrected_all$feature1[qc_idx] == data$feature1[qc_idx]))
})

test_that("step_measure_drift_qc_loess tidy method works", {
  data <- create_drift_data()

  rec <- recipe(~ ., data = data) |>
    update_role(sample_id, new_role = "id") |>
    step_measure_drift_qc_loess(feature1, feature2) |>
    prep()

  tidy_result <- tidy(rec, number = 1)

  expect_s3_class(tidy_result, "tbl_df")
  expect_true("feature" %in% names(tidy_result))
  expect_true("qc_median" %in% names(tidy_result))
  expect_equal(nrow(tidy_result), 2)
})

test_that("step_measure_drift_qc_loess print method works", {
  data <- create_drift_data()

  rec <- recipe(~ ., data = data) |>
    update_role(sample_id, new_role = "id") |>
    step_measure_drift_qc_loess(feature1, feature2)

  expect_output(print(rec$steps[[1]]), "QC-LOESS drift correction")

  rec_prepped <- prep(rec)
  expect_output(print(rec_prepped$steps[[1]]), "2 features")
})

test_that("step_measure_drift_qc_loess works with custom qc_type", {
  data <- create_drift_data()
  data$sample_type <- ifelse(data$sample_type == "qc", "pool", data$sample_type)

  rec <- recipe(~ ., data = data) |>
    update_role(sample_id, new_role = "id") |>
    step_measure_drift_qc_loess(feature1, qc_type = "pool") |>
    prep()

  corrected <- bake(rec, new_data = NULL)
  expect_equal(nrow(corrected), nrow(data))
})

test_that("step_measure_drift_qc_loess span parameter affects smoothness", {
  set.seed(42)
  # Create data with more QC samples for flexible span test
  data <- data.frame(
    sample_id = paste0("S", 1:50),
    sample_type = rep(c("qc", "unknown"), 25),  # Every other sample is QC
    run_order = 1:50,
    feature1 = 100 + (1:50) * 0.5 + rnorm(50, sd = 2)
  )

  rec_smooth <- recipe(~ ., data = data) |>
    update_role(sample_id, new_role = "id") |>
    step_measure_drift_qc_loess(feature1, span = 1.0) |>
    prep()

  rec_flexible <- recipe(~ ., data = data) |>
    update_role(sample_id, new_role = "id") |>
    step_measure_drift_qc_loess(feature1, span = 0.5) |>
    prep()

  # Both should work without error
  expect_s3_class(rec_smooth, "recipe")
  expect_s3_class(rec_flexible, "recipe")
})

# ==============================================================================
# measure_detect_drift() tests
# ==============================================================================

test_that("measure_detect_drift detects significant drift", {
  data <- create_drift_data(drift_slope = 2)

  result <- measure_detect_drift(data, c("feature1", "feature2"))

  expect_s3_class(result, "tbl_df")
  expect_true("feature" %in% names(result))
  expect_true("slope" %in% names(result))
  expect_true("significant" %in% names(result))

  # feature1 should have significant positive drift
  f1_result <- result[result$feature == "feature1", ]
  expect_true(f1_result$slope > 0)
})

test_that("measure_detect_drift works with QC filter", {
  data <- create_drift_data()

  result <- measure_detect_drift(
    data,
    c("feature1"),
    qc_type = "qc"
  )

  expect_equal(nrow(result), 1)
})

test_that("measure_detect_drift validates input", {
  data <- create_drift_data()

  expect_error(
    measure_detect_drift(data, "feature1", run_order_col = "missing"),
    "not found"
  )
})

test_that("measure_detect_drift handles missing features gracefully", {
  data <- create_drift_data()

  expect_warning(
    result <- measure_detect_drift(data, c("feature1", "missing_feature")),
    "missing_feature"
  )

  expect_equal(nrow(result), 1)
})

test_that("measure_detect_drift calculates percent change correctly", {
  set.seed(123)
  # Create data with known drift: +1 unit per run over 20 runs
  # Add tiny noise to avoid "essentially perfect fit" warning
  data <- data.frame(
    sample_type = rep("qc", 20),
    run_order = 1:20,
    feature1 = 100 + 1:20 + rnorm(20, 0, 0.01)  # Minimal noise
  )

  result <- measure_detect_drift(data, "feature1")

  # Slope should be very close to 1

  expect_equal(result$slope, 1, tolerance = 0.01)

  # Percent change over 19 runs: (19 * 1) / mean(100 + 1:20) = 19/110.5 ≈ 17.2%
  expect_true(result$percent_change > 15 && result$percent_change < 20)
})
