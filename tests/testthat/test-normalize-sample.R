# Tests for sample-wise normalization steps
# Each step normalizes each spectrum independently

# ==============================================================================
# step_measure_normalize_sum
# ==============================================================================

test_that("step_measure_normalize_sum runs in recipe workflow", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_normalize_sum() |>
    prep()

  result <- bake(rec, new_data = NULL)

  expect_s3_class(result, "tbl_df")
  expect_true(".measures" %in% names(result))
  expect_true(is_measure_list(result$.measures))
})

test_that("step_measure_normalize_sum makes values sum to 1", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_normalize_sum() |>
    prep()

  result <- bake(rec, new_data = NULL)

  # Check that each spectrum sums to 1
  for (i in 1:5) {
    total <- sum(result$.measures[[i]]$value)
    expect_equal(total, 1, tolerance = 1e-10)
  }
})

test_that("step_measure_normalize_sum tidy method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_normalize_sum()

  tidy_before <- tidy(rec, number = 2)
  expect_equal(tidy_before$terms, "<all measure columns>")

  rec_prepped <- prep(rec)
  tidy_after <- tidy(rec_prepped, number = 2)
  expect_equal(tidy_after$terms, ".measures")
})

test_that("step_measure_normalize_sum print method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_normalize_sum()

  expect_output(print(rec), "Sum normalization")
})

test_that("step_measure_normalize_sum warns on zero sum", {
  # Create data with all zeros for one sample
  zero_data <- tibble::tibble(
    id = c(rep(1, 5), rep(2, 5)),
    outcome = c(rep(1, 5), rep(2, 5)),
    value = c(rep(0, 5), c(1, 2, 3, 4, 5)),
    location = rep(1:5, 2)
  )

  rec <- recipe(outcome ~ ., data = zero_data) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(value, location = vars(location)) |>
    step_measure_normalize_sum()

  expect_warning(prep(rec), "zero or NA")
})

# ==============================================================================
# step_measure_normalize_max
# ==============================================================================

test_that("step_measure_normalize_max runs in recipe workflow", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_normalize_max() |>
    prep()

  result <- bake(rec, new_data = NULL)

  expect_s3_class(result, "tbl_df")
  expect_true(is_measure_list(result$.measures))
})

test_that("step_measure_normalize_max makes max value 1", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_normalize_max() |>
    prep()

  result <- bake(rec, new_data = NULL)

  # Check that each spectrum has max = 1
  for (i in 1:5) {
    max_val <- max(result$.measures[[i]]$value)
    expect_equal(max_val, 1, tolerance = 1e-10)
  }
})

test_that("step_measure_normalize_max tidy method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_normalize_max()

  tidy_before <- tidy(rec, number = 2)
  expect_equal(tidy_before$terms, "<all measure columns>")

  rec_prepped <- prep(rec)
  tidy_after <- tidy(rec_prepped, number = 2)
  expect_equal(tidy_after$terms, ".measures")
})

test_that("step_measure_normalize_max print method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_normalize_max()

  expect_output(print(rec), "Max normalization")
})

# ==============================================================================
# step_measure_normalize_range
# ==============================================================================

test_that("step_measure_normalize_range runs in recipe workflow", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_normalize_range() |>
    prep()

  result <- bake(rec, new_data = NULL)

  expect_s3_class(result, "tbl_df")
  expect_true(is_measure_list(result$.measures))
})

test_that("step_measure_normalize_range scales to [0, 1]", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_normalize_range() |>
    prep()

  result <- bake(rec, new_data = NULL)

  # Check that each spectrum has min = 0 and max = 1
  for (i in 1:5) {
    min_val <- min(result$.measures[[i]]$value)
    max_val <- max(result$.measures[[i]]$value)
    expect_equal(min_val, 0, tolerance = 1e-10)
    expect_equal(max_val, 1, tolerance = 1e-10)
  }
})

test_that("step_measure_normalize_range tidy method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_normalize_range()

  tidy_before <- tidy(rec, number = 2)
  expect_equal(tidy_before$terms, "<all measure columns>")

  rec_prepped <- prep(rec)
  tidy_after <- tidy(rec_prepped, number = 2)
  expect_equal(tidy_after$terms, ".measures")
})

test_that("step_measure_normalize_range print method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_normalize_range()

  expect_output(print(rec), "Range.*normalization")
})

test_that("step_measure_normalize_range warns on constant spectrum", {
  # Create data with constant values
  const_data <- tibble::tibble(
    id = c(rep(1, 5), rep(2, 5)),
    outcome = c(rep(1, 5), rep(2, 5)),
    value = c(rep(5, 5), c(1, 2, 3, 4, 5)),
    location = rep(1:5, 2)
  )

  rec <- recipe(outcome ~ ., data = const_data) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(value, location = vars(location)) |>
    step_measure_normalize_range()

  expect_warning(prep(rec), "zero or NA")
})

# ==============================================================================
# step_measure_normalize_vector
# ==============================================================================

test_that("step_measure_normalize_vector runs in recipe workflow", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_normalize_vector() |>
    prep()

  result <- bake(rec, new_data = NULL)

  expect_s3_class(result, "tbl_df")
  expect_true(is_measure_list(result$.measures))
})

test_that("step_measure_normalize_vector makes L2 norm 1", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_normalize_vector() |>
    prep()

  result <- bake(rec, new_data = NULL)

  # Check that each spectrum has L2 norm = 1
  for (i in 1:5) {
    l2_norm <- sqrt(sum(result$.measures[[i]]$value^2))
    expect_equal(l2_norm, 1, tolerance = 1e-10)
  }
})

test_that("step_measure_normalize_vector tidy method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_normalize_vector()

  tidy_before <- tidy(rec, number = 2)
  expect_equal(tidy_before$terms, "<all measure columns>")

  rec_prepped <- prep(rec)
  tidy_after <- tidy(rec_prepped, number = 2)
  expect_equal(tidy_after$terms, ".measures")
})

test_that("step_measure_normalize_vector print method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_normalize_vector()

  expect_output(print(rec), "L2.*normalization")
})

# ==============================================================================
# step_measure_normalize_auc
# ==============================================================================

test_that("step_measure_normalize_auc runs in recipe workflow", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_normalize_auc() |>
    prep()

  result <- bake(rec, new_data = NULL)

  expect_s3_class(result, "tbl_df")
  expect_true(is_measure_list(result$.measures))
})

test_that("step_measure_normalize_auc modifies values", {
  rec_original <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    prep()

  rec_auc <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_normalize_auc() |>
    prep()

  original <- bake(rec_original, new_data = NULL)
  normalized <- bake(rec_auc, new_data = NULL)

  expect_false(all(original$.measures[[1]]$value == normalized$.measures[[1]]$value))
})

test_that("step_measure_normalize_auc tidy method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_normalize_auc()

  tidy_before <- tidy(rec, number = 2)
  expect_equal(tidy_before$terms, "<all measure columns>")

  rec_prepped <- prep(rec)
  tidy_after <- tidy(rec_prepped, number = 2)
  expect_equal(tidy_after$terms, ".measures")
})

test_that("step_measure_normalize_auc print method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_normalize_auc()

  expect_output(print(rec), "AUC normalization")
})

# ==============================================================================
# step_measure_normalize_peak
# ==============================================================================

test_that("step_measure_normalize_peak runs in recipe workflow", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_normalize_peak(location_min = 40, location_max = 60) |>
    prep()

  result <- bake(rec, new_data = NULL)

  expect_s3_class(result, "tbl_df")
  expect_true(is_measure_list(result$.measures))
})

test_that("step_measure_normalize_peak requires location bounds", {
  expect_error(
    recipe(water + fat + protein ~ ., data = meats_long) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_measure_normalize_peak() |>
      prep(),
    "location_min.*location_max"
  )
})

test_that("step_measure_normalize_peak validates location order", {
  expect_error(
    recipe(water + fat + protein ~ ., data = meats_long) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_measure_normalize_peak(location_min = 60, location_max = 40) |>
      prep(),
    "less than"
  )
})

test_that("step_measure_normalize_peak errors on empty region", {
  expect_error(
    recipe(water + fat + protein ~ ., data = meats_long) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_measure_normalize_peak(location_min = 1000, location_max = 2000) |>
      prep(),
    "no measurement locations"
  )
})

test_that("step_measure_normalize_peak works with different methods", {
  methods <- c("mean", "max", "integral")

  for (m in methods) {
    rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_measure_normalize_peak(
        location_min = 40, location_max = 60, method = m
      ) |>
      prep()

    result <- bake(rec, new_data = NULL)
    expect_true(is_measure_list(result$.measures))
  }
})

test_that("step_measure_normalize_peak tidy method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_normalize_peak(location_min = 40, location_max = 60, method = "max")

  tidy_before <- tidy(rec, number = 2)
  expect_equal(tidy_before$terms, "<all measure columns>")
  expect_equal(tidy_before$location_min, 40)
  expect_equal(tidy_before$location_max, 60)
  expect_equal(tidy_before$method, "max")

  rec_prepped <- prep(rec)
  tidy_after <- tidy(rec_prepped, number = 2)
  expect_equal(tidy_after$terms, ".measures")
  expect_equal(tidy_after$location_min, 40)
  expect_equal(tidy_after$location_max, 60)
})

test_that("step_measure_normalize_peak print method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_normalize_peak(location_min = 40, location_max = 60)

  expect_output(print(rec), "Peak region normalization")
})

test_that("step_measure_normalize_peak tunable returns correct params", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_normalize_peak(location_min = 40, location_max = 60)

  tunable_params <- tunable(rec$steps[[2]])
  expect_equal(nrow(tunable_params), 2)
  expect_true("location_min" %in% tunable_params$name)
  expect_true("location_max" %in% tunable_params$name)
})

# ==============================================================================
# Location preservation and required packages
# ==============================================================================

test_that("all normalization steps preserve locations", {
  steps <- list(
    step_measure_normalize_sum,
    step_measure_normalize_max,
    step_measure_normalize_range,
    step_measure_normalize_vector,
    step_measure_normalize_auc
  )

  for (step_fn in steps) {
    rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_fn() |>
      prep()

    result <- bake(rec, new_data = NULL)

    # Check locations are preserved
    locs <- result$.measures[[1]]$location
    expect_equal(length(locs), 100)  # meats has 100 channels
    expect_equal(locs, 1:100)
  }
})

test_that("required_pkgs includes measure and purrr", {
  step <- step_measure_normalize_sum(
    recipe(water + fat + protein ~ ., data = meats_long)
  )
  pkgs <- required_pkgs(step)
  expect_true("measure" %in% pkgs)
  expect_true("purrr" %in% pkgs)
})

# ==============================================================================
# Works with wide format
# ==============================================================================

test_that("normalization steps work with wide format", {
  skip_if_not_installed("modeldata")
  data(meats, package = "modeldata")

  rec <- recipe(water + fat + protein ~ ., data = meats) |>
    step_measure_input_wide(dplyr::starts_with("x_")) |>
    step_measure_normalize_sum() |>
    prep()

  result <- bake(rec, new_data = NULL)
  expect_true(is_measure_list(result$.measures))

  # Verify sum is 1
  total <- sum(result$.measures[[1]]$value)
  expect_equal(total, 1, tolerance = 1e-10)
})
