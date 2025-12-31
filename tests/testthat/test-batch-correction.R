# ==============================================================================
# Tests for batch-correction.R
# ==============================================================================

library(recipes)

# Helper function to create batch data
create_batch_data <- function() {
  set.seed(42)
  data.frame(
    sample_id = paste0("S", 1:20),
    sample_type = rep(
      c("reference", "unknown", "unknown", "unknown", "reference"),
      4
    ),
    batch_id = rep(c("B1", "B1", "B2", "B2"), 5),
    feature1 = c(rep(100, 10), rep(120, 10)) + rnorm(20, sd = 3),
    feature2 = c(rep(50, 10), rep(45, 10)) + rnorm(20, sd = 2)
  )
}

# ==============================================================================
# step_measure_batch_reference() tests
# ==============================================================================

test_that("step_measure_batch_reference creates a recipe step", {
  data <- create_batch_data()

  rec <- recipe(~., data = data) |>
    update_role(sample_id, new_role = "id") |>
    step_measure_batch_reference(feature1, feature2, batch_col = "batch_id")

  expect_s3_class(rec, "recipe")
  expect_equal(length(rec$steps), 1)
  expect_s3_class(rec$steps[[1]], "step_measure_batch_reference")
})

test_that("step_measure_batch_reference preps successfully", {
  data <- create_batch_data()

  rec <- recipe(~., data = data) |>
    update_role(sample_id, new_role = "id") |>
    step_measure_batch_reference(feature1, feature2, batch_col = "batch_id") |>
    prep()

  expect_true(rec$steps[[1]]$trained)
  expect_true(length(rec$steps[[1]]$correction_factors) == 2)
})

test_that("step_measure_batch_reference median_ratio corrects batch effects", {
  data <- create_batch_data()

  rec <- recipe(~., data = data) |>
    update_role(sample_id, new_role = "id") |>
    step_measure_batch_reference(
      feature1,
      batch_col = "batch_id",
      method = "median_ratio"
    ) |>
    prep()

  corrected <- bake(rec, new_data = NULL)

  # Reference samples should now have similar values across batches
  ref_b1 <- corrected$feature1[
    corrected$sample_type == "reference" & corrected$batch_id == "B1"
  ]
  ref_b2 <- corrected$feature1[
    corrected$sample_type == "reference" & corrected$batch_id == "B2"
  ]

  # Medians should be much closer after correction
  expect_true(abs(median(ref_b1) - median(ref_b2)) < 10)
})

test_that("step_measure_batch_reference mean_center method works", {
  data <- create_batch_data()

  rec <- recipe(~., data = data) |>
    update_role(sample_id, new_role = "id") |>
    step_measure_batch_reference(
      feature1,
      batch_col = "batch_id",
      method = "mean_center"
    ) |>
    prep()

  corrected <- bake(rec, new_data = NULL)

  # Should complete without error
  expect_equal(nrow(corrected), nrow(data))
})

test_that("step_measure_batch_reference requires sufficient reference samples", {
  set.seed(42)
  # Create data with reference samples in both batches, but only 1 in B2
  # batch_id positions: B1=1,2,5,6,9,10,13,14,17,18 and B2=3,4,7,8,11,12,15,16,19,20
  sample_types <- rep("unknown", 20)
  sample_types[c(1, 5, 9)] <- "reference" # 3 refs in B1
  sample_types[c(3)] <- "reference" # Only 1 ref in B2

  data <- data.frame(
    sample_id = paste0("S", 1:20),
    sample_type = sample_types,
    batch_id = rep(c("B1", "B1", "B2", "B2"), 5),
    feature1 = rnorm(20, 100, sd = 5)
  )

  rec <- recipe(~., data = data) |>
    update_role(sample_id, new_role = "id") |>
    step_measure_batch_reference(feature1, batch_col = "batch_id", min_ref = 2)

  expect_error(prep(rec), "only 1 reference")
})

test_that("step_measure_batch_reference validates required columns", {
  data <- create_batch_data()
  data$batch_id <- NULL

  rec <- recipe(~., data = data) |>
    step_measure_batch_reference(feature1, batch_col = "batch_id")

  expect_error(prep(rec), "not found")
})

test_that("step_measure_batch_reference target_batch parameter works", {
  data <- create_batch_data()

  # Target B2 instead of default B1
  rec <- recipe(~., data = data) |>
    update_role(sample_id, new_role = "id") |>
    step_measure_batch_reference(
      feature1,
      batch_col = "batch_id",
      target_batch = "B2"
    ) |>
    prep()

  # Should work and have different correction factors
  expect_equal(rec$steps[[1]]$target_batch, "B2")
})

test_that("step_measure_batch_reference global target works", {
  data <- create_batch_data()

  rec <- recipe(~., data = data) |>
    update_role(sample_id, new_role = "id") |>
    step_measure_batch_reference(
      feature1,
      batch_col = "batch_id",
      target_batch = "global"
    ) |>
    prep()

  corrected <- bake(rec, new_data = NULL)
  expect_equal(nrow(corrected), nrow(data))
})

test_that("step_measure_batch_reference tidy method works", {
  data <- create_batch_data()

  rec <- recipe(~., data = data) |>
    update_role(sample_id, new_role = "id") |>
    step_measure_batch_reference(feature1, feature2, batch_col = "batch_id") |>
    prep()

  tidy_result <- tidy(rec, number = 1)

  expect_s3_class(tidy_result, "tbl_df")
  expect_true("feature" %in% names(tidy_result))
  expect_true("batch" %in% names(tidy_result))
  expect_true("correction_factor" %in% names(tidy_result))
  # 2 features * 2 batches = 4 rows
  expect_equal(nrow(tidy_result), 4)
})

test_that("step_measure_batch_reference print method works", {
  data <- create_batch_data()

  rec <- recipe(~., data = data) |>
    update_role(sample_id, new_role = "id") |>
    step_measure_batch_reference(feature1, batch_col = "batch_id")

  expect_output(print(rec$steps[[1]]), "batch correction")

  rec_prepped <- prep(rec)
  expect_output(print(rec_prepped$steps[[1]]), "1 features")
  expect_output(print(rec_prepped$steps[[1]]), "2 batches")
})

test_that("step_measure_batch_reference handles custom reference_type", {
  data <- create_batch_data()
  data$sample_type <- ifelse(
    data$sample_type == "reference",
    "pool",
    data$sample_type
  )

  rec <- recipe(~., data = data) |>
    update_role(sample_id, new_role = "id") |>
    step_measure_batch_reference(
      feature1,
      batch_col = "batch_id",
      reference_type = "pool"
    ) |>
    prep()

  corrected <- bake(rec, new_data = NULL)
  expect_equal(nrow(corrected), nrow(data))
})
