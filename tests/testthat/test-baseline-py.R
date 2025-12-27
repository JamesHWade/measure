# Skip all tests if pybaselines is not available
skip_if_no_pybaselines <- function() {
  skip_if_not(
    .pybaselines_available(),
    "pybaselines Python package not available"
  )
}

test_that("step_measure_baseline_py runs with asls method", {
  skip_if_no_pybaselines()

  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_baseline_py(method = "asls", lam = 1e6, p = 0.01) |>
    prep()

  result <- bake(rec, new_data = NULL)

  expect_s3_class(result, "tbl_df")
  expect_true(".measures" %in% names(result))
  expect_true(is_measure_list(result$.measures))
})

test_that("step_measure_baseline_py modifies values", {
  skip_if_no_pybaselines()

  rec_original <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    prep()

  rec_py <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_baseline_py(method = "asls") |>
    prep()

  original <- bake(rec_original, new_data = NULL)
  corrected <- bake(rec_py, new_data = NULL)

  orig_vals <- original$.measures[[1]]$value
  corr_vals <- corrected$.measures[[1]]$value

  expect_false(all(orig_vals == corr_vals))
})

test_that("step_measure_baseline_py works with different methods", {
  skip_if_no_pybaselines()

  # Test polynomial method
  rec_poly <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_baseline_py(method = "modpoly", poly_order = 3L) |>
    prep()

  result_poly <- bake(rec_poly, new_data = NULL)
  expect_true(is_measure_list(result_poly$.measures))

  # Test snip method
  rec_snip <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_baseline_py(method = "snip", max_half_window = 30L) |>
    prep()

  result_snip <- bake(rec_snip, new_data = NULL)
  expect_true(is_measure_list(result_snip$.measures))
})

test_that("step_measure_baseline_py subtract = FALSE returns baseline", {
  skip_if_no_pybaselines()

  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_baseline_py(method = "asls", subtract = FALSE) |>
    prep()

  result <- bake(rec, new_data = NULL)
  baseline_vals <- result$.measures[[1]]$value

  # Baseline should be smoother than original signal
  # (lower variance of differences)
  rec_orig <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    prep()

  orig <- bake(rec_orig, new_data = NULL)
  orig_vals <- orig$.measures[[1]]$value

  # Baseline should have lower local variance (be smoother)
  orig_diff_var <- var(diff(orig_vals))
  baseline_diff_var <- var(diff(baseline_vals))
  expect_lt(baseline_diff_var, orig_diff_var)
})

test_that("step_measure_baseline_py validates method argument", {
  skip_if_no_pybaselines()

  expect_error(
    recipe(water + fat + protein ~ ., data = meats_long) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_measure_baseline_py(method = 123) |>
      prep(),
    "method"
  )
})

test_that("step_measure_baseline_py validates subtract argument", {
  skip_if_no_pybaselines()

  expect_error(
    recipe(water + fat + protein ~ ., data = meats_long) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_measure_baseline_py(subtract = "yes") |>
      prep(),
    "subtract"
  )
})

test_that("step_measure_baseline_py handles unknown method gracefully", {
  skip_if_no_pybaselines()

  # Use minimal data to reduce warnings (one per sample)
  minimal_data <- meats_long[meats_long$id == unique(meats_long$id)[1], ]

  # Warning fires during prep() because recipes bakes training data internally
  expect_warning(
    recipe(water + fat + protein ~ ., data = minimal_data) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_measure_baseline_py(method = "nonexistent_method") |>
      prep(),
    "pybaselines.*failed"
  )
})

test_that("step_measure_baseline_py tidy method works", {
  skip_if_no_pybaselines()

  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_baseline_py(method = "airpls", subtract = FALSE)

  # Before prep
  tidy_before <- tidy(rec, number = 2)
  expect_equal(tidy_before$terms, "<all measure columns>")
  expect_equal(tidy_before$method, "airpls")
  expect_equal(tidy_before$subtract, FALSE)

  # After prep
  rec_prepped <- prep(rec)
  tidy_after <- tidy(rec_prepped, number = 2)
  expect_equal(tidy_after$terms, ".measures")
  expect_equal(tidy_after$method, "airpls")
})

test_that("step_measure_baseline_py print method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_baseline_py(method = "asls")

  expect_output(print(rec), "pybaselines.*asls")
})

test_that("step_measure_baseline_py tunable returns correct params for asls", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_baseline_py(method = "asls")

  tunable_params <- tunable(rec$steps[[2]])
  expect_equal(nrow(tunable_params), 2)
  expect_true("lam" %in% tunable_params$name)
  expect_true("p" %in% tunable_params$name)
})

test_that("step_measure_baseline_py tunable returns correct params for modpoly", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_baseline_py(method = "modpoly")

  tunable_params <- tunable(rec$steps[[2]])
  expect_equal(nrow(tunable_params), 1)
  expect_equal(tunable_params$name, "poly_order")
})

test_that("step_measure_baseline_py tunable returns empty for unknown method params", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_baseline_py(method = "loess")  # No tunable params defined

  tunable_params <- tunable(rec$steps[[2]])
  expect_equal(nrow(tunable_params), 0)
})

test_that("required_pkgs includes measure and reticulate", {
  step <- step_measure_baseline_py(
    recipe(water + fat + protein ~ ., data = meats_long),
    method = "asls"
  )
  pkgs <- required_pkgs(step)
  expect_true("measure" %in% pkgs)
  expect_true("reticulate" %in% pkgs)
})
