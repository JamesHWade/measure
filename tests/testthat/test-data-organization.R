# Tests for data-organization.R

test_that("measure_identify_columns detects column types", {
  # Create test data with various column patterns
  df <- data.frame(
    sample_id = 1:5,
    concentration = rnorm(5),
    wn_1000 = rnorm(5),
    wn_1001 = rnorm(5),
    nm_500 = rnorm(5),
    rt_2.5 = rnorm(5),
    mz_100 = rnorm(5),
    x_001 = rnorm(5)
  )

  result <- measure_identify_columns(df)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), ncol(df))
  expect_named(
    result,
    c("column", "type", "suggested_role", "n_values", "class")
  )

  # Check type detection
  expect_equal(result$type[result$column == "wn_1000"], "wavenumber")
  expect_equal(result$type[result$column == "nm_500"], "wavelength")
  expect_equal(result$type[result$column == "rt_2.5"], "retention_time")
  expect_equal(result$type[result$column == "mz_100"], "mz")
  expect_equal(result$type[result$column == "x_001"], "generic")
  expect_equal(result$type[result$column == "sample_id"], "other")
})

test_that("measure_identify_columns suggests appropriate roles", {
  df <- data.frame(
    id = letters[1:5],
    outcome_y = 1:5,
    concentration = rnorm(5),
    wn_1000 = rnorm(5),
    wn_1001 = rnorm(5),
    batch = c("A", "A", "B", "B", "C")
  )

  result <- measure_identify_columns(df)

  # ID columns should be detected (unique character values)
  expect_equal(result$suggested_role[result$column == "id"], "id")

  # Outcome hints should work
  expect_equal(result$suggested_role[result$column == "outcome_y"], "outcome")
  expect_equal(
    result$suggested_role[result$column == "concentration"],
    "outcome"
  )

  # Measurement columns should be predictors
  expect_equal(result$suggested_role[result$column == "wn_1000"], "predictor")
  expect_equal(result$suggested_role[result$column == "wn_1001"], "predictor")
})

test_that("measure_identify_columns handles custom patterns", {
  df <- data.frame(
    spec_1 = rnorm(5),
    spec_2 = rnorm(5),
    other = 1:5
  )

  custom_patterns <- list(
    spectrum = "^spec_"
  )

  result <- measure_identify_columns(df, patterns = custom_patterns)

  expect_equal(result$type[result$column == "spec_1"], "spectrum")
  expect_equal(result$type[result$column == "spec_2"], "spectrum")
  expect_equal(result$type[result$column == "other"], "other")
})

test_that("measure_identify_columns errors on non-data.frame", {
  expect_error(
    measure_identify_columns("not a data frame"),
    "must be a data frame"
  )
})

test_that("measure_column_summary works correctly", {
  df <- data.frame(
    id = 1:5,
    wn_1000 = rnorm(5),
    wn_1001 = rnorm(5),
    wn_1002 = rnorm(5),
    nm_500 = rnorm(5),
    outcome = rnorm(5)
  )

  result <- measure_column_summary(df)

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("type", "n_columns", "example_cols"))

  # Should have wavenumber as most common
  expect_equal(result$type[1], "wavenumber")
  expect_equal(result$n_columns[result$type == "wavenumber"], 3)
})

test_that("set_measure_roles updates recipe roles", {
  skip_if_not_installed("recipes")

  df <- data.frame(
    sample_id = 1:10,
    batch = rep(c("A", "B"), 5),
    outcome = rnorm(10),
    x_001 = rnorm(10),
    x_002 = rnorm(10)
  )

  rec <- recipes::recipe(outcome ~ ., data = df) |>
    set_measure_roles(
      id_cols = sample_id,
      metadata_cols = batch
    )

  var_info <- rec$var_info

  expect_equal(var_info$role[var_info$variable == "sample_id"], "id")
  expect_equal(var_info$role[var_info$variable == "batch"], "metadata")
})

test_that("set_measure_roles errors on non-recipe", {
  expect_error(
    set_measure_roles("not a recipe"),
    "must be a recipe"
  )
})

test_that("check_measure_recipe detects missing input step", {
  skip_if_not_installed("recipes")

  df <- data.frame(
    outcome = rnorm(10),
    x_001 = rnorm(10)
  )

  # Recipe with no input step
  rec <- recipes::recipe(outcome ~ ., data = df)

  issues <- check_measure_recipe(rec)

  expect_true(any(issues$level == "error"))
  expect_true(any(grepl("no input step", issues$message, ignore.case = TRUE)))
})

test_that("check_measure_recipe detects missing output step", {
  skip_if_not_installed("recipes")

  data(meats_long, package = "measure")

  # Recipe with input but no output
  rec <- recipes::recipe(water ~ ., data = meats_long) |>
    recipes::update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel))

  issues <- check_measure_recipe(rec)

  expect_true(any(issues$level == "warning"))
  expect_true(any(grepl("no output step", issues$message, ignore.case = TRUE)))
})

test_that("check_measure_recipe non-strict mode returns recipe", {
  skip_if_not_installed("recipes")

  df <- data.frame(
    outcome = rnorm(10),
    x_001 = rnorm(10)
  )

  rec <- recipes::recipe(outcome ~ ., data = df)

  # Should return recipe invisibly (messages are expected)
  result <- suppressMessages(check_measure_recipe(rec, strict = FALSE))

  expect_s3_class(result, "recipe")
})

test_that("check_measure_recipe detects multiple input steps", {
  skip_if_not_installed("recipes")

  data(meats_long, package = "measure")

  # Create a recipe with two input steps (unusual but possible)
  rec <- recipes::recipe(water ~ ., data = meats_long) |>
    recipes::update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel))

  # Manually add another input step to simulate the issue
  rec$steps <- c(rec$steps, rec$steps[1])

  issues <- check_measure_recipe(rec)

  expect_true(any(issues$level == "error"))
  # Check for message about multiple input steps
  expect_true(any(grepl("2 input steps", issues$message, ignore.case = TRUE)))
})
