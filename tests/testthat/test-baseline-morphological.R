test_that("step_measure_baseline_morphological runs in recipe workflow", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_baseline_morphological(window_size = 50) |>
    prep()

  result <- bake(rec, new_data = NULL)

  expect_s3_class(result, "tbl_df")
  expect_true(".measures" %in% names(result))
  expect_true(is_measure_list(result$.measures))
})

test_that("step_measure_baseline_morphological modifies values", {
  rec_original <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    prep()

  rec_morph <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_baseline_morphological(window_size = 50) |>
    prep()

  original <- bake(rec_original, new_data = NULL)
  corrected <- bake(rec_morph, new_data = NULL)

  orig_vals <- original$.measures[[1]]$value
  corr_vals <- corrected$.measures[[1]]$value

  expect_false(all(orig_vals == corr_vals))
})

test_that("step_measure_baseline_morphological validates window_size", {
  expect_error(
    recipe(water + fat + protein ~ ., data = meats_small) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_measure_baseline_morphological(window_size = 2) |>
      prep(),
    "window_size"
  )

  expect_error(
    recipe(water + fat + protein ~ ., data = meats_small) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_measure_baseline_morphological(window_size = -1) |>
      prep(),
    "window_size"
  )
})

test_that("step_measure_baseline_morphological validates iterations", {
  expect_error(
    recipe(water + fat + protein ~ ., data = meats_small) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_measure_baseline_morphological(iterations = 0) |>
      prep(),
    "iterations"
  )

  expect_error(
    recipe(water + fat + protein ~ ., data = meats_small) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_measure_baseline_morphological(iterations = -1) |>
      prep(),
    "iterations"
  )
})

test_that("step_measure_baseline_morphological tidy method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_baseline_morphological(window_size = 40, iterations = 2)

  # Before prep
  tidy_before <- tidy(rec, number = 2)
  expect_equal(tidy_before$terms, "<all measure columns>")
  expect_equal(tidy_before$window_size, 40)
  expect_equal(tidy_before$iterations, 2)

  # After prep
  rec_prepped <- prep(rec)
  tidy_after <- tidy(rec_prepped, number = 2)
  expect_equal(tidy_after$terms, ".measures")
  expect_equal(tidy_after$window_size, 40)
  expect_equal(tidy_after$iterations, 2)
})

test_that("step_measure_baseline_morphological print method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_baseline_morphological()

  expect_output(print(rec), "Morphological baseline")
})

test_that("step_measure_baseline_morphological works with multiple iterations", {
  rec1 <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_baseline_morphological(window_size = 50, iterations = 1) |>
    prep()

  rec2 <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_baseline_morphological(window_size = 50, iterations = 3) |>
    prep()

  result1 <- bake(rec1, new_data = NULL)
  result2 <- bake(rec2, new_data = NULL)

  vals1 <- result1$.measures[[1]]$value
  vals2 <- result2$.measures[[1]]$value

  # Multiple iterations should give different results
  expect_false(all(vals1 == vals2))
})
