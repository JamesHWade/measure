# ==============================================================================
# Tests for extended baseline correction methods
# ==============================================================================

# ==============================================================================
# step_measure_baseline_rolling tests
# ==============================================================================

test_that("step_measure_baseline_rolling removes baseline", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    step_measure_baseline_rolling(window_size = 50) %>%
    prep()

  result <- bake(rec, new_data = NULL)

  expect_true(is_measure_list(result$.measures))
  # Baseline correction should change values
  original <- recipe(water + fat + protein ~ ., data = meats_long) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    prep() %>%
    bake(new_data = NULL)

  expect_false(identical(result$.measures[[1]]$value, original$.measures[[1]]$value))
})

test_that("step_measure_baseline_rolling preserves locations", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    step_measure_baseline_rolling(window_size = 30) %>%
    prep()

  result <- bake(rec, new_data = NULL)
  original_locs <- seq_len(100)  # meats_long has 100 channels

  expect_equal(result$.measures[[1]]$location, original_locs)
})

test_that("step_measure_baseline_rolling errors with invalid window_size", {
  expect_error(
    recipe(water + fat + protein ~ ., data = meats_long) %>%
      step_measure_baseline_rolling(window_size = 1),
    "must be a number >= 3"
  )
})

test_that("step_measure_baseline_rolling print method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    step_measure_baseline_rolling(window_size = 50)

  expect_output(print(rec$steps[[2]]), "Rolling ball baseline")
})

test_that("step_measure_baseline_rolling tidy method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    step_measure_baseline_rolling(window_size = 50) %>%
    prep()

  tidy_result <- tidy(rec, number = 2)
  expect_s3_class(tidy_result, "tbl_df")
  expect_true("window_size" %in% names(tidy_result))
})

# ==============================================================================
# step_measure_baseline_airpls tests
# ==============================================================================

test_that("step_measure_baseline_airpls removes baseline", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    step_measure_baseline_airpls(lambda = 1e4, max_iter = 10) %>%
    prep()

  result <- bake(rec, new_data = NULL)

  expect_true(is_measure_list(result$.measures))
})

test_that("step_measure_baseline_airpls preserves locations", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    step_measure_baseline_airpls(lambda = 1e4) %>%
    prep()

  result <- bake(rec, new_data = NULL)

  expect_equal(length(result$.measures[[1]]$location),
               length(result$.measures[[1]]$value))
})

test_that("step_measure_baseline_airpls errors with invalid lambda", {
  expect_error(
    recipe(water + fat + protein ~ ., data = meats_long) %>%
      step_measure_baseline_airpls(lambda = -1),
    "must be a positive number"
  )
})

test_that("step_measure_baseline_airpls print method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    step_measure_baseline_airpls(lambda = 1e5)

  expect_output(print(rec$steps[[2]]), "airPLS baseline")
})

test_that("step_measure_baseline_airpls is tunable", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    step_measure_baseline_airpls(lambda = 1e5)

  tunable_params <- tunable(rec$steps[[2]])
  expect_true("lambda" %in% tunable_params$name)
})

# ==============================================================================
# step_measure_baseline_snip tests
# ==============================================================================

test_that("step_measure_baseline_snip removes baseline", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    step_measure_baseline_snip(iterations = 20) %>%
    prep()

  result <- bake(rec, new_data = NULL)

  expect_true(is_measure_list(result$.measures))
})

test_that("step_measure_baseline_snip with decreasing works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    step_measure_baseline_snip(iterations = 20, decreasing = TRUE) %>%
    prep()

  result <- bake(rec, new_data = NULL)
  expect_true(is_measure_list(result$.measures))
})

test_that("step_measure_baseline_snip with fixed window works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    step_measure_baseline_snip(iterations = 10, decreasing = FALSE) %>%
    prep()

  result <- bake(rec, new_data = NULL)
  expect_true(is_measure_list(result$.measures))
})

test_that("step_measure_baseline_snip errors with invalid iterations", {
  expect_error(
    recipe(water + fat + protein ~ ., data = meats_long) %>%
      step_measure_baseline_snip(iterations = 0),
    "must be a positive integer"
  )
})

test_that("step_measure_baseline_snip print method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    step_measure_baseline_snip(iterations = 30)

  expect_output(print(rec$steps[[2]]), "SNIP baseline")
})

test_that("step_measure_baseline_snip tidy method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    step_measure_baseline_snip(iterations = 30) %>%
    prep()

  tidy_result <- tidy(rec, number = 2)
  expect_s3_class(tidy_result, "tbl_df")
  expect_true("iterations" %in% names(tidy_result))
})

# ==============================================================================
# Integration tests
# ==============================================================================

test_that("extended baseline methods can be combined with other steps", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    step_measure_baseline_rolling(window_size = 30) %>%
    step_measure_snv() %>%
    prep()

  result <- bake(rec, new_data = NULL)
  expect_true(is_measure_list(result$.measures))
})
