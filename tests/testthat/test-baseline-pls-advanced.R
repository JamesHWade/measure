# ==============================================================================
# Tests for step_measure_baseline_aspls
# ==============================================================================

test_that("step_measure_baseline_aspls runs in recipe workflow", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_baseline_aspls(lambda = 1e5, alpha = 0.5) |>
    prep()

  result <- bake(rec, new_data = NULL)

  expect_s3_class(result, "tbl_df")
  expect_true(".measures" %in% names(result))
  expect_true(is_measure_list(result$.measures))
})

test_that("step_measure_baseline_aspls modifies values", {
  rec_original <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    prep()

  rec_aspls <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_baseline_aspls(lambda = 1e5) |>
    prep()

  original <- bake(rec_original, new_data = NULL)
  corrected <- bake(rec_aspls, new_data = NULL)

  orig_vals <- original$.measures[[1]]$value
  corr_vals <- corrected$.measures[[1]]$value

  expect_false(all(orig_vals == corr_vals))
})

test_that("step_measure_baseline_aspls validates lambda", {
  expect_error(
    recipe(water + fat + protein ~ ., data = meats_small) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_measure_baseline_aspls(lambda = -1) |>
      prep(),
    "lambda"
  )

  expect_error(
    recipe(water + fat + protein ~ ., data = meats_small) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_measure_baseline_aspls(lambda = 0) |>
      prep(),
    "lambda"
  )
})

test_that("step_measure_baseline_aspls validates alpha", {
  expect_error(
    recipe(water + fat + protein ~ ., data = meats_small) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_measure_baseline_aspls(alpha = -0.1) |>
      prep(),
    "alpha"
  )

  expect_error(
    recipe(water + fat + protein ~ ., data = meats_small) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_measure_baseline_aspls(alpha = 1.5) |>
      prep(),
    "alpha"
  )
})

test_that("step_measure_baseline_aspls tidy method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_baseline_aspls(lambda = 1e6, alpha = 0.3)

  tidy_before <- tidy(rec, number = 2)
  expect_equal(tidy_before$terms, "<all measure columns>")
  expect_equal(tidy_before$lambda, 1e6)
  expect_equal(tidy_before$alpha, 0.3)

  rec_prepped <- prep(rec)
  tidy_after <- tidy(rec_prepped, number = 2)
  expect_equal(tidy_after$terms, ".measures")
})

test_that("step_measure_baseline_aspls print method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_baseline_aspls()

  expect_output(print(rec), "aspls baseline")
})

test_that("step_measure_baseline_aspls is tunable", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_baseline_aspls()

  tunable_params <- tunable(rec$steps[[2]])
  expect_true("lambda" %in% tunable_params$name)
  expect_true("alpha" %in% tunable_params$name)
})

# ==============================================================================
# Tests for step_measure_baseline_iarpls
# ==============================================================================

test_that("step_measure_baseline_iarpls runs in recipe workflow", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_baseline_iarpls(lambda = 1e6, lambda_1 = 1e4) |>
    prep()

  result <- bake(rec, new_data = NULL)

  expect_s3_class(result, "tbl_df")
  expect_true(".measures" %in% names(result))
  expect_true(is_measure_list(result$.measures))
})

test_that("step_measure_baseline_iarpls modifies values", {
  rec_original <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    prep()

  rec_iarpls <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_baseline_iarpls() |>
    prep()

  original <- bake(rec_original, new_data = NULL)
  corrected <- bake(rec_iarpls, new_data = NULL)

  orig_vals <- original$.measures[[1]]$value
  corr_vals <- corrected$.measures[[1]]$value

  expect_false(all(orig_vals == corr_vals))
})

test_that("step_measure_baseline_iarpls validates lambda parameters", {
  expect_error(
    recipe(water + fat + protein ~ ., data = meats_small) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_measure_baseline_iarpls(lambda = -1) |>
      prep(),
    "lambda"
  )

  expect_error(
    recipe(water + fat + protein ~ ., data = meats_small) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_measure_baseline_iarpls(lambda_1 = 0) |>
      prep(),
    "lambda_1"
  )
})

test_that("step_measure_baseline_iarpls tidy method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_baseline_iarpls(lambda = 1e6, lambda_1 = 1e4)

  tidy_before <- tidy(rec, number = 2)
  expect_equal(tidy_before$terms, "<all measure columns>")
  expect_equal(tidy_before$lambda, 1e6)
  expect_equal(tidy_before$lambda_1, 1e4)

  rec_prepped <- prep(rec)
  tidy_after <- tidy(rec_prepped, number = 2)
  expect_equal(tidy_after$terms, ".measures")
})

test_that("step_measure_baseline_iarpls print method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_baseline_iarpls()

  expect_output(print(rec), "iarpls baseline")
})

test_that("step_measure_baseline_iarpls is tunable", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_baseline_iarpls()

  tunable_params <- tunable(rec$steps[[2]])
  expect_true("lambda" %in% tunable_params$name)
  expect_true("lambda_1" %in% tunable_params$name)
})

# ==============================================================================
# Tests for step_measure_baseline_fastchrom
# ==============================================================================

test_that("step_measure_baseline_fastchrom runs in recipe workflow", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_baseline_fastchrom(lambda = 1e5, window = 30) |>
    prep()

  result <- bake(rec, new_data = NULL)

  expect_s3_class(result, "tbl_df")
  expect_true(".measures" %in% names(result))
  expect_true(is_measure_list(result$.measures))
})

test_that("step_measure_baseline_fastchrom modifies values", {
  rec_original <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    prep()

  rec_fastchrom <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_baseline_fastchrom() |>
    prep()

  original <- bake(rec_original, new_data = NULL)
  corrected <- bake(rec_fastchrom, new_data = NULL)

  orig_vals <- original$.measures[[1]]$value
  corr_vals <- corrected$.measures[[1]]$value

  expect_false(all(orig_vals == corr_vals))
})

test_that("step_measure_baseline_fastchrom validates lambda", {
  expect_error(
    recipe(water + fat + protein ~ ., data = meats_small) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_measure_baseline_fastchrom(lambda = -1) |>
      prep(),
    "lambda"
  )

  expect_error(
    recipe(water + fat + protein ~ ., data = meats_small) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_measure_baseline_fastchrom(lambda = 0) |>
      prep(),
    "lambda"
  )
})

test_that("step_measure_baseline_fastchrom validates window", {
  expect_error(
    recipe(water + fat + protein ~ ., data = meats_small) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_measure_baseline_fastchrom(window = 2) |>
      prep(),
    "window"
  )

  expect_error(
    recipe(water + fat + protein ~ ., data = meats_small) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_measure_baseline_fastchrom(window = -1) |>
      prep(),
    "window"
  )
})

test_that("step_measure_baseline_fastchrom tidy method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_baseline_fastchrom(lambda = 1e6, window = 60)

  tidy_before <- tidy(rec, number = 2)
  expect_equal(tidy_before$terms, "<all measure columns>")
  expect_equal(tidy_before$lambda, 1e6)
  expect_equal(tidy_before$window, 60)

  rec_prepped <- prep(rec)
  tidy_after <- tidy(rec_prepped, number = 2)
  expect_equal(tidy_after$terms, ".measures")
})

test_that("step_measure_baseline_fastchrom print method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_baseline_fastchrom()

  expect_output(print(rec), "FastChrom baseline")
})

test_that("step_measure_baseline_fastchrom is tunable", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_baseline_fastchrom()

  tunable_params <- tunable(rec$steps[[2]])
  expect_true("lambda" %in% tunable_params$name)
  expect_true("window" %in% tunable_params$name)
})

# ==============================================================================
# Edge case tests
# ==============================================================================

test_that("baseline algorithms handle constant signals gracefully", {
  # Create data with constant values
  constant_data <- tibble::tibble(
    id = rep(1, 50),
    outcome = 1,
    value = rep(5, 50),
    location = 1:50
  )

  # All three methods should work without error
  expect_no_error({
    recipe(outcome ~ ., data = constant_data) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(value, location = vars(location)) |>
      step_measure_baseline_aspls(max_iter = 5) |>
      prep() |>
      bake(new_data = NULL)
  })

  expect_no_error({
    recipe(outcome ~ ., data = constant_data) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(value, location = vars(location)) |>
      step_measure_baseline_iarpls(max_iter = 5) |>
      prep() |>
      bake(new_data = NULL)
  })

  expect_no_error({
    recipe(outcome ~ ., data = constant_data) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(value, location = vars(location)) |>
      step_measure_baseline_fastchrom(max_iter = 5) |>
      prep() |>
      bake(new_data = NULL)
  })
})

test_that("baseline algorithms can be combined with other steps", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_baseline_aspls(lambda = 1e5) |>
    step_measure_snv() |>
    prep()

  result <- bake(rec, new_data = NULL)
  expect_true(is_measure_list(result$.measures))
})
