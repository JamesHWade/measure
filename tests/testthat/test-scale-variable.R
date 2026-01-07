# Tests for variable-wise scaling steps
# These steps learn statistics from training data and apply them during baking

# ==============================================================================
# step_measure_center
# ==============================================================================

test_that("step_measure_center runs in recipe workflow", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_center() |>
    prep()

  result <- bake(rec, new_data = NULL)

  expect_s3_class(result, "tbl_df")
  expect_true(".measures" %in% names(result))
  expect_true(is_measure_list(result$.measures))
})

test_that("step_measure_center makes column means zero", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_center() |>
    prep()

  result <- bake(rec, new_data = NULL)

  # Convert to matrix and check column means
  mat <- measure_to_matrix(result$.measures)
  col_means <- colMeans(mat)

  # Column means should be very close to zero
  expect_true(all(abs(col_means) < 1e-10))
})

test_that("step_measure_center stores learned parameters", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_center() |>
    prep()

  center_step <- rec$steps[[2]]
  expect_true(!is.null(center_step$learned_params))
  expect_true(".measures" %in% names(center_step$learned_params))
  expect_equal(length(center_step$learned_params[[".measures"]]$means), 100)
})

test_that("step_measure_center applies same parameters to new data", {
  # Split data - use first 2 for training, last 1 for test
  all_ids <- unique(meats_small$id)
  train_ids <- all_ids[1:2]
  test_ids <- all_ids[3]

  train_data <- meats_small[meats_small$id %in% train_ids, ]
  test_data <- meats_small[meats_small$id %in% test_ids, ]

  rec <- recipe(water + fat + protein ~ ., data = train_data) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_center() |>
    prep()

  # Means from training should be stored
  train_means <- rec$steps[[2]]$learned_params[[".measures"]]$means

  # Bake on new data
  result <- bake(rec, new_data = test_data)
  expect_true(is_measure_list(result$.measures))

  # Means should still be the same (from training)
  expect_equal(rec$steps[[2]]$learned_params[[".measures"]]$means, train_means)
})

test_that("step_measure_center tidy method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_center()

  tidy_before <- tidy(rec, number = 2)
  expect_equal(tidy_before$terms, "<all measure columns>")
  expect_true(is.na(tidy_before$mean))

  rec_prepped <- prep(rec)
  tidy_after <- tidy(rec_prepped, number = 2)
  expect_equal(unique(tidy_after$terms), ".measures")
  expect_equal(nrow(tidy_after), 100) # 100 locations
  expect_true(!anyNA(tidy_after$mean))
})

test_that("step_measure_center print method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_center()

  expect_output(print(rec), "Mean centering")
})

# ==============================================================================
# step_measure_scale_auto
# ==============================================================================

test_that("step_measure_scale_auto runs in recipe workflow", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_scale_auto() |>
    prep()

  result <- bake(rec, new_data = NULL)

  expect_s3_class(result, "tbl_df")
  expect_true(is_measure_list(result$.measures))
})

test_that("step_measure_scale_auto makes column means zero and SDs 1", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_scale_auto() |>
    prep()

  result <- bake(rec, new_data = NULL)

  mat <- measure_to_matrix(result$.measures)
  col_means <- colMeans(mat)
  col_sds <- apply(mat, 2, sd)

  # Column means should be ~0 and SDs should be ~1
  expect_true(all(abs(col_means) < 1e-10))
  expect_true(all(abs(col_sds - 1) < 1e-10))
})

test_that("step_measure_scale_auto stores learned parameters", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_scale_auto() |>
    prep()

  scale_step <- rec$steps[[2]]
  expect_true(!is.null(scale_step$learned_params))
  expect_equal(length(scale_step$learned_params[[".measures"]]$means), 100)
  expect_equal(length(scale_step$learned_params[[".measures"]]$sds), 100)
})

test_that("step_measure_scale_auto tidy method includes sd", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_scale_auto() |>
    prep()

  tidy_result <- tidy(rec, number = 2)
  expect_true("mean" %in% names(tidy_result))
  expect_true("sd" %in% names(tidy_result))
  expect_equal(nrow(tidy_result), 100)
})

test_that("step_measure_scale_auto print method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_scale_auto()

  expect_output(print(rec), "Auto-scaling")
})

# ==============================================================================
# step_measure_scale_pareto
# ==============================================================================

test_that("step_measure_scale_pareto runs in recipe workflow", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_scale_pareto() |>
    prep()

  result <- bake(rec, new_data = NULL)

  expect_s3_class(result, "tbl_df")
  expect_true(is_measure_list(result$.measures))
})

test_that("step_measure_scale_pareto scales by sqrt(SD)", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_scale_pareto() |>
    prep()

  result <- bake(rec, new_data = NULL)

  mat <- measure_to_matrix(result$.measures)
  col_means <- colMeans(mat)

  # Column means should be ~0
  expect_true(all(abs(col_means) < 1e-10))

  # SDs should be close to sqrt(original_sd) / sqrt(original_sd) = 1
  # But pareto uses sqrt(sd) divisor, so result sd = original_sd / sqrt(original_sd) = sqrt(original_sd)
  col_sds <- apply(mat, 2, sd)
  expect_true(all(col_sds > 0))
})

test_that("step_measure_scale_pareto stores learned parameters", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_scale_pareto() |>
    prep()

  scale_step <- rec$steps[[2]]
  expect_true(!is.null(scale_step$learned_params))
  expect_equal(length(scale_step$learned_params[[".measures"]]$means), 100)
  expect_equal(length(scale_step$learned_params[[".measures"]]$sds), 100)
})

test_that("step_measure_scale_pareto print method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_scale_pareto()

  expect_output(print(rec), "Pareto scaling")
})

# ==============================================================================
# step_measure_scale_range
# ==============================================================================

test_that("step_measure_scale_range runs in recipe workflow", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_scale_range() |>
    prep()

  result <- bake(rec, new_data = NULL)

  expect_s3_class(result, "tbl_df")
  expect_true(is_measure_list(result$.measures))
})

test_that("step_measure_scale_range centers and scales by range", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_scale_range() |>
    prep()

  result <- bake(rec, new_data = NULL)

  mat <- measure_to_matrix(result$.measures)
  col_means <- colMeans(mat)

  # Column means should be ~0
  expect_true(all(abs(col_means) < 1e-10))
})

test_that("step_measure_scale_range stores learned parameters", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_scale_range() |>
    prep()

  scale_step <- rec$steps[[2]]
  expect_true(!is.null(scale_step$learned_params))
  expect_equal(length(scale_step$learned_params[[".measures"]]$means), 100)
  expect_equal(length(scale_step$learned_params[[".measures"]]$ranges), 100)
})

test_that("step_measure_scale_range tidy method includes range", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_scale_range() |>
    prep()

  tidy_result <- tidy(rec, number = 2)
  expect_true("mean" %in% names(tidy_result))
  expect_true("range" %in% names(tidy_result))
})

test_that("step_measure_scale_range print method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_scale_range()

  expect_output(print(rec), "Range scaling")
})

# ==============================================================================
# step_measure_scale_vast
# ==============================================================================

test_that("step_measure_scale_vast runs in recipe workflow", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_scale_vast() |>
    prep()

  result <- bake(rec, new_data = NULL)

  expect_s3_class(result, "tbl_df")
  expect_true(is_measure_list(result$.measures))
})

test_that("step_measure_scale_vast stores learned parameters including CV", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_scale_vast() |>
    prep()

  scale_step <- rec$steps[[2]]
  expect_true(!is.null(scale_step$learned_params))
  expect_equal(length(scale_step$learned_params[[".measures"]]$means), 100)
  expect_equal(length(scale_step$learned_params[[".measures"]]$sds), 100)
  expect_equal(length(scale_step$learned_params[[".measures"]]$cvs), 100)
})

test_that("step_measure_scale_vast tidy method includes cv", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_scale_vast() |>
    prep()

  tidy_result <- tidy(rec, number = 2)
  expect_true("mean" %in% names(tidy_result))
  expect_true("sd" %in% names(tidy_result))
  expect_true("cv" %in% names(tidy_result))
})

test_that("step_measure_scale_vast print method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_scale_vast()

  expect_output(print(rec), "VAST scaling")
})

# ==============================================================================
# Common tests for all scaling steps
# ==============================================================================

test_that("all scaling steps preserve locations", {
  steps <- list(
    step_measure_center,
    step_measure_scale_auto,
    step_measure_scale_pareto,
    step_measure_scale_range,
    step_measure_scale_vast
  )

  for (step_fn in steps) {
    rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_fn() |>
      prep()

    result <- bake(rec, new_data = NULL)

    locs <- result$.measures[[1]]$location
    expect_equal(length(locs), 100)
    expect_equal(locs, 1:100)
  }
})

test_that("all scaling steps fail without measure input step", {
  skip_if_not_installed("modeldata")
  data(meats, package = "modeldata")

  steps <- list(
    step_measure_center,
    step_measure_scale_auto,
    step_measure_scale_pareto,
    step_measure_scale_range,
    step_measure_scale_vast
  )

  for (step_fn in steps) {
    expect_error(
      recipe(water + fat + protein ~ ., data = meats) |>
        step_fn() |>
        prep(),
      "should be in the data"
    )
  }
})

test_that("required_pkgs includes measure", {
  step <- step_measure_center(
    recipe(water + fat + protein ~ ., data = meats_small)
  )
  pkgs <- required_pkgs(step)
  expect_true("measure" %in% pkgs)
})

# ==============================================================================
# Works with wide format
# ==============================================================================

test_that("scaling steps work with wide format", {
  skip_if_not_installed("modeldata")
  data(meats, package = "modeldata")

  rec <- recipe(water + fat + protein ~ ., data = meats) |>
    step_measure_input_wide(dplyr::starts_with("x_")) |>
    step_measure_scale_auto() |>
    prep()

  result <- bake(rec, new_data = NULL)
  expect_true(is_measure_list(result$.measures))

  # Verify means are 0 and SDs are 1
  mat <- measure_to_matrix(result$.measures)
  col_means <- colMeans(mat)
  col_sds <- apply(mat, 2, sd)

  expect_true(all(abs(col_means) < 1e-10))
  expect_true(all(abs(col_sds - 1) < 1e-10))
})

# ==============================================================================
# Difference between scaling methods
# ==============================================================================

test_that("different scaling methods produce different results", {
  rec_auto <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_scale_auto() |>
    prep()

  rec_pareto <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_scale_pareto() |>
    prep()

  result_auto <- bake(rec_auto, new_data = NULL)
  result_pareto <- bake(rec_pareto, new_data = NULL)

  # Values should be different
  expect_false(all(
    result_auto$.measures[[1]]$value == result_pareto$.measures[[1]]$value
  ))
})

test_that("centering is different from auto-scaling", {
  rec_center <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_center() |>
    prep()

  rec_auto <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_scale_auto() |>
    prep()

  result_center <- bake(rec_center, new_data = NULL)
  result_auto <- bake(rec_auto, new_data = NULL)

  # Both should have mean 0
  mat_center <- measure_to_matrix(result_center$.measures)
  mat_auto <- measure_to_matrix(result_auto$.measures)

  expect_true(all(abs(colMeans(mat_center)) < 1e-10))
  expect_true(all(abs(colMeans(mat_auto)) < 1e-10))

  # But auto-scaled should have SD of 1, centered should not
  center_sds <- apply(mat_center, 2, sd)
  auto_sds <- apply(mat_auto, 2, sd)

  expect_false(all(abs(center_sds - 1) < 1e-10))
  expect_true(all(abs(auto_sds - 1) < 1e-10))
})
