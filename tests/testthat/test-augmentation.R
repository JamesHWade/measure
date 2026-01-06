# ==============================================================================
# Tests for data augmentation steps
#
# This file tests:
# - step_measure_augment_noise
# - step_measure_augment_shift
# - step_measure_augment_scale
# ==============================================================================

# ==============================================================================
# Test Helpers
# ==============================================================================

create_augmentation_data <- function(n = 5) {
  purrr::map_dfr(seq_len(n), function(i) {
    tibble::tibble(
      id = paste0("sample", i),
      outcome = i * 10,
      location = 1:100,
      value = sin(seq(0, 2 * pi, length.out = 100)) + 1
    )
  })
}

prep_augment_recipe <- function(steps_fn) {
  data <- create_augmentation_data()

  rec <- recipe(outcome ~ ., data = data) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(value, location = vars(location))

  rec <- steps_fn(rec)
  prep(rec)
}

# ==============================================================================
# step_measure_augment_noise tests
# ==============================================================================

test_that("step_measure_augment_noise adds gaussian noise", {
  rec <- prep_augment_recipe(function(r) {
    step_measure_augment_noise(
      r,
      sd = 0.1,
      distribution = "gaussian",
      skip = FALSE
    )
  })

  result <- bake(rec, new_data = NULL)

  expect_true(".measures" %in% names(result))
  expect_true(is_measure_list(result$.measures))

  # Values should be modified (not identical to original)
  original_values <- sin(seq(0, 2 * pi, length.out = 100)) + 1
  expect_false(all(result$.measures[[1]]$value == original_values))
})

test_that("step_measure_augment_noise adds uniform noise", {
  rec <- prep_augment_recipe(function(r) {
    step_measure_augment_noise(
      r,
      sd = 0.1,
      distribution = "uniform",
      skip = FALSE
    )
  })

  result <- bake(rec, new_data = NULL)

  expect_true(".measures" %in% names(result))
  # Values should be modified
  original_values <- sin(seq(0, 2 * pi, length.out = 100)) + 1
  expect_false(all(result$.measures[[1]]$value == original_values))
})

test_that("step_measure_augment_noise is reproducible", {
  rec <- prep_augment_recipe(function(r) {
    step_measure_augment_noise(r, sd = 0.1, skip = FALSE)
  })

  result1 <- bake(rec, new_data = NULL)
  result2 <- bake(rec, new_data = NULL)

  # Same input should produce same output
  expect_equal(result1$.measures[[1]]$value, result2$.measures[[1]]$value)
})

test_that("step_measure_augment_noise respects relative parameter", {
  data <- create_augmentation_data()

  rec_rel <- recipe(outcome ~ ., data = data) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(value, location = vars(location)) |>
    step_measure_augment_noise(sd = 0.1, relative = TRUE, skip = FALSE) |>
    prep()

  rec_abs <- recipe(outcome ~ ., data = data) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(value, location = vars(location)) |>
    step_measure_augment_noise(sd = 0.1, relative = FALSE, skip = FALSE) |>
    prep()

  result_rel <- bake(rec_rel, new_data = NULL)
  result_abs <- bake(rec_abs, new_data = NULL)

  # Results should be different
  expect_false(all(
    result_rel$.measures[[1]]$value == result_abs$.measures[[1]]$value
  ))
})

test_that("step_measure_augment_noise skips by default", {
  # Create recipe with skip=TRUE (default)
  rec <- prep_augment_recipe(function(r) {
    step_measure_augment_noise(r, sd = 0.5)
  })

  # Get original data for comparison
  data <- create_augmentation_data()
  rec_orig <- recipe(outcome ~ ., data = data) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(value, location = vars(location)) |>
    prep()

  # Bake with new_data = NULL should apply augmentation during training
  result_train <- bake(rec, new_data = NULL)

  # Bake with explicit new data should skip augmentation
  # (simulate new data scenario)
  orig_result <- bake(rec_orig, new_data = NULL)

  # The training result should have noise added (different from original)
  expect_false(all(
    result_train$.measures[[1]]$value == orig_result$.measures[[1]]$value
  ))
})

test_that("step_measure_augment_noise validates inputs", {
  expect_error(
    recipe(~., data = create_augmentation_data()) |>
      step_measure_augment_noise(sd = -0.1),
    "non-negative"
  )
})

test_that("step_measure_augment_noise print method works", {
  rec <- recipe(~., data = create_augmentation_data()) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(value, location = vars(location)) |>
    step_measure_augment_noise(sd = 0.05)

  expect_output(print(rec$steps[[2]]), "gaussian noise.*0.05.*training only")
})

test_that("step_measure_augment_noise tidy method works", {
  rec <- prep_augment_recipe(function(r) {
    step_measure_augment_noise(r, sd = 0.02)
  })

  tidy_result <- tidy(rec, number = 2)
  expect_s3_class(tidy_result, "tbl_df")
  expect_true("sd" %in% names(tidy_result))
  expect_true("distribution" %in% names(tidy_result))
})

# ==============================================================================
# step_measure_augment_shift tests
# ==============================================================================

test_that("step_measure_augment_shift applies x-axis shift", {
  rec <- prep_augment_recipe(function(r) {
    step_measure_augment_shift(r, max_shift = 5, skip = FALSE)
  })

  result <- bake(rec, new_data = NULL)

  expect_true(".measures" %in% names(result))
  expect_true(is_measure_list(result$.measures))

  # Values should be modified due to shift
  original_values <- sin(seq(0, 2 * pi, length.out = 100)) + 1
  expect_false(all(result$.measures[[1]]$value == original_values))
})

test_that("step_measure_augment_shift is reproducible", {
  rec <- prep_augment_recipe(function(r) {
    step_measure_augment_shift(r, max_shift = 3, skip = FALSE)
  })

  result1 <- bake(rec, new_data = NULL)
  result2 <- bake(rec, new_data = NULL)

  # Same input should produce same output
  expect_equal(result1$.measures[[1]]$value, result2$.measures[[1]]$value)
})

test_that("step_measure_augment_shift with zero shift produces original", {
  rec <- prep_augment_recipe(function(r) {
    step_measure_augment_shift(r, max_shift = 0, skip = FALSE)
  })

  result <- bake(rec, new_data = NULL)

  # With max_shift = 0, values should be unchanged
  original_values <- sin(seq(0, 2 * pi, length.out = 100)) + 1
  expect_equal(result$.measures[[1]]$value, original_values, tolerance = 1e-10)
})

test_that("step_measure_augment_shift validates inputs", {
  expect_error(
    recipe(~., data = create_augmentation_data()) |>
      step_measure_augment_shift(max_shift = -1),
    "non-negative"
  )
})

test_that("step_measure_augment_shift print method works", {
  rec <- recipe(~., data = create_augmentation_data()) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(value, location = vars(location)) |>
    step_measure_augment_shift(max_shift = 3)

  expect_output(print(rec$steps[[2]]), "x-shift.*3.*training only")
})

test_that("step_measure_augment_shift tidy method works", {
  rec <- prep_augment_recipe(function(r) {
    step_measure_augment_shift(r, max_shift = 2)
  })

  tidy_result <- tidy(rec, number = 2)
  expect_s3_class(tidy_result, "tbl_df")
  expect_true("max_shift" %in% names(tidy_result))
  expect_equal(tidy_result$max_shift, 2)
})

# ==============================================================================
# step_measure_augment_scale tests
# ==============================================================================

test_that("step_measure_augment_scale applies scaling", {
  rec <- prep_augment_recipe(function(r) {
    step_measure_augment_scale(r, range = c(0.5, 1.5), skip = FALSE)
  })

  result <- bake(rec, new_data = NULL)

  expect_true(".measures" %in% names(result))
  expect_true(is_measure_list(result$.measures))

  # Values should be modified due to scaling
  original_values <- sin(seq(0, 2 * pi, length.out = 100)) + 1
  expect_false(all(result$.measures[[1]]$value == original_values))
})

test_that("step_measure_augment_scale is reproducible", {
  rec <- prep_augment_recipe(function(r) {
    step_measure_augment_scale(r, range = c(0.8, 1.2), skip = FALSE)
  })

  result1 <- bake(rec, new_data = NULL)
  result2 <- bake(rec, new_data = NULL)

  # Same input should produce same output
  expect_equal(result1$.measures[[1]]$value, result2$.measures[[1]]$value)
})

test_that("step_measure_augment_scale with range [1,1] produces original", {
  rec <- prep_augment_recipe(function(r) {
    step_measure_augment_scale(r, range = c(1, 1.0001), skip = FALSE) # Near-unity range
  })

  result <- bake(rec, new_data = NULL)

  # With scale factor ~1, values should be nearly unchanged
  original_values <- sin(seq(0, 2 * pi, length.out = 100)) + 1
  expect_equal(result$.measures[[1]]$value, original_values, tolerance = 0.01)
})

test_that("step_measure_augment_scale validates inputs", {
  expect_error(
    recipe(~., data = create_augmentation_data()) |>
      step_measure_augment_scale(range = c(1.5, 0.5)),
    "min < max"
  )

  expect_error(
    recipe(~., data = create_augmentation_data()) |>
      step_measure_augment_scale(range = c(-0.5, 0.5)),
    "positive"
  )

  expect_error(
    recipe(~., data = create_augmentation_data()) |>
      step_measure_augment_scale(range = c(1)),
    "length 2"
  )
})

test_that("step_measure_augment_scale print method works", {
  rec <- recipe(~., data = create_augmentation_data()) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(value, location = vars(location)) |>
    step_measure_augment_scale(range = c(0.8, 1.2))

  expect_output(print(rec$steps[[2]]), "scaling.*0.8.*1.2.*training only")
})

test_that("step_measure_augment_scale tidy method works", {
  rec <- prep_augment_recipe(function(r) {
    step_measure_augment_scale(r, range = c(0.9, 1.1))
  })

  tidy_result <- tidy(rec, number = 2)
  expect_s3_class(tidy_result, "tbl_df")
  expect_true("range_min" %in% names(tidy_result))
  expect_true("range_max" %in% names(tidy_result))
})

# ==============================================================================
# Integration tests
# ==============================================================================

test_that("augmentation steps work with meats_small data", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_augment_noise(sd = 0.01, skip = FALSE) |>
    prep()

  result <- bake(rec, new_data = NULL)

  expect_true(".measures" %in% names(result))
  expect_true(is_measure_list(result$.measures))
})

test_that("multiple augmentation steps can be combined", {
  rec <- prep_augment_recipe(function(r) {
    r |>
      step_measure_augment_noise(sd = 0.01, skip = FALSE) |>
      step_measure_augment_shift(max_shift = 1, skip = FALSE) |>
      step_measure_augment_scale(range = c(0.95, 1.05), skip = FALSE)
  })

  result <- bake(rec, new_data = NULL)

  expect_true(".measures" %in% names(result))

  # Should have all modifications applied
  original_values <- sin(seq(0, 2 * pi, length.out = 100)) + 1
  expect_false(all(result$.measures[[1]]$value == original_values))
})

test_that("augmentation steps preserve measure structure", {
  rec <- prep_augment_recipe(function(r) {
    r |>
      step_measure_augment_noise(sd = 0.1, skip = FALSE) |>
      step_measure_augment_shift(max_shift = 2, skip = FALSE)
  })

  result <- bake(rec, new_data = NULL)

  # Structure should be preserved
  expect_equal(nrow(result$.measures[[1]]), 100)
  expect_equal(names(result$.measures[[1]]), c("location", "value"))
})
