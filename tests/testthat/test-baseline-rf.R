test_that("step_measure_baseline_rf runs in recipe workflow", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_baseline_rf(span = 0.5) |>
    prep()

  result <- bake(rec, new_data = NULL)

  expect_s3_class(result, "tbl_df")
  expect_true(".measures" %in% names(result))
  expect_true(is_measure_list(result$.measures))
})

test_that("step_measure_baseline_rf modifies values", {
  rec_original <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    prep()

  rec_rf <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_baseline_rf() |>
    prep()

  original <- bake(rec_original, new_data = NULL)
  corrected <- bake(rec_rf, new_data = NULL)

  orig_vals <- original$.measures[[1]]$value
  corr_vals <- corrected$.measures[[1]]$value

  expect_false(all(orig_vals == corr_vals))
})

test_that("step_measure_baseline_rf validates span", {
  expect_error(
    recipe(water + fat + protein ~ ., data = meats_long) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_measure_baseline_rf(span = 0) |>
      prep(),
    "span"
  )

  expect_error(
    recipe(water + fat + protein ~ ., data = meats_long) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_measure_baseline_rf(span = 1.5) |>
      prep(),
    "span"
  )

  expect_error(
    recipe(water + fat + protein ~ ., data = meats_long) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_measure_baseline_rf(span = -0.1) |>
      prep(),
    "span"
  )
})

test_that("step_measure_baseline_rf validates maxit", {
  expect_error(
    recipe(water + fat + protein ~ ., data = meats_long) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_measure_baseline_rf(maxit = c(5)) |>
      prep(),
    "maxit"
  )

  expect_error(
    recipe(water + fat + protein ~ ., data = meats_long) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_measure_baseline_rf(maxit = c(0, 5)) |>
      prep(),
    "maxit"
  )
})

test_that("step_measure_baseline_rf tidy method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_baseline_rf(span = 0.4)

  # Before prep
  tidy_before <- tidy(rec, number = 2)
  expect_equal(tidy_before$terms, "<all measure columns>")
  expect_equal(tidy_before$span, 0.4)

  # After prep
  rec_prepped <- prep(rec)
  tidy_after <- tidy(rec_prepped, number = 2)
  expect_equal(tidy_after$terms, ".measures")
  expect_equal(tidy_after$span, 0.4)
})

test_that("step_measure_baseline_rf print method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_baseline_rf()

  expect_output(print(rec), "Robust fitting baseline correction")
})

test_that("step_measure_baseline_rf works with different span values", {
  for (sp in c(0.3, 0.5, 0.7)) {
    rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_measure_baseline_rf(span = sp) |>
      prep()

    result <- bake(rec, new_data = NULL)
    expect_true(is_measure_list(result$.measures))
  }
})

test_that("required_pkgs includes measure and IDPmisc", {
  step <- step_measure_baseline_rf(
    recipe(water + fat + protein ~ ., data = meats_long)
  )
  pkgs <- required_pkgs(step)
  expect_true("measure" %in% pkgs)
  expect_true("IDPmisc" %in% pkgs)
})
