# ==============================================================================
# Tests for extended scatter correction steps
#
# This file tests:
# - step_measure_emsc (Extended MSC)
# - step_measure_osc (Orthogonal Signal Correction)
# ==============================================================================

# ==============================================================================
# Test Helpers
# ==============================================================================

# Create test data with scatter effects
create_scatter_data <- function(n = 10) {
  purrr::map_dfr(seq_len(n), function(i) {
    x <- 1:100
    # Base spectrum
    base <- sin(seq(0, 2 * pi, length.out = 100))
    # Add multiplicative scatter
    mult <- 0.8 + 0.4 * runif(1)
    # Add additive scatter
    add <- runif(1, -0.2, 0.2)
    # Add wavelength-dependent scatter (for EMSC)
    wavelength_effect <- 0.1 * (x / 100)^2 * runif(1)

    tibble::tibble(
      id = paste0("sample", i),
      outcome = 10 + 5 * mult + rnorm(1, sd = 0.5),
      location = x,
      value = mult * base + add + wavelength_effect + rnorm(100, sd = 0.01)
    )
  })
}

prep_scatter_recipe <- function(steps_fn, data = NULL) {
  if (is.null(data)) {
    data <- create_scatter_data()
  }

  rec <- recipe(outcome ~ ., data = data) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(value, location = vars(location))

  rec <- steps_fn(rec)
  prep(rec)
}

# ==============================================================================
# step_measure_emsc tests
# ==============================================================================

test_that("step_measure_emsc applies correction with default settings", {
  rec <- prep_scatter_recipe(function(r) {
    step_measure_emsc(r)
  })

  result <- bake(rec, new_data = NULL)

  expect_true(".measures" %in% names(result))
  expect_true(is_measure_list(result$.measures))

  # Check values are reasonable (not all NA, not extreme)
  values <- result$.measures[[1]]$value
  expect_false(anyNA(values))
  expect_true(all(is.finite(values)))
})

test_that("step_measure_emsc works with different degrees", {
  rec_0 <- prep_scatter_recipe(function(r) step_measure_emsc(r, degree = 0))
  rec_1 <- prep_scatter_recipe(function(r) step_measure_emsc(r, degree = 1))
  rec_3 <- prep_scatter_recipe(function(r) step_measure_emsc(r, degree = 3))

  result_0 <- bake(rec_0, new_data = NULL)
  result_1 <- bake(rec_1, new_data = NULL)
  result_3 <- bake(rec_3, new_data = NULL)

  # All should produce valid results
  expect_false(anyNA(result_0$.measures[[1]]$value))
  expect_false(anyNA(result_1$.measures[[1]]$value))
  expect_false(anyNA(result_3$.measures[[1]]$value))

  # Different degrees should produce different results
  expect_false(all(result_0$.measures[[1]]$value == result_1$.measures[[1]]$value))
})

test_that("step_measure_emsc works with median reference", {
  rec <- prep_scatter_recipe(function(r) {
    step_measure_emsc(r, reference = "median")
  })

  result <- bake(rec, new_data = NULL)

  expect_true(".measures" %in% names(result))
  expect_false(anyNA(result$.measures[[1]]$value))
})

test_that("step_measure_emsc works with custom reference", {
  data <- create_scatter_data()
  custom_ref <- sin(seq(0, 2 * pi, length.out = 100))

  rec <- prep_scatter_recipe(function(r) {
    step_measure_emsc(r, reference = custom_ref)
  }, data = data)

  result <- bake(rec, new_data = NULL)

  expect_true(".measures" %in% names(result))
})

test_that("step_measure_emsc validates inputs", {
  expect_error(
    recipe(~., data = create_scatter_data()) |>
      step_measure_emsc(degree = -1),
    "non-negative integer"
  )

  expect_error(
    recipe(~., data = create_scatter_data()) |>
      step_measure_emsc(reference = "invalid"),
    "'mean', 'median', or a numeric"
  )
})

test_that("step_measure_emsc print method works", {
  rec <- recipe(~., data = create_scatter_data()) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(value, location = vars(location)) |>
    step_measure_emsc(degree = 3)

  expect_output(print(rec$steps[[2]]), "EMSC.*degree = 3")
})

test_that("step_measure_emsc tidy method works", {
  rec <- prep_scatter_recipe(function(r) {
    step_measure_emsc(r, degree = 2)
  })

  tidy_result <- tidy(rec, number = 2)
  expect_s3_class(tidy_result, "tbl_df")
  expect_true("degree" %in% names(tidy_result))
  expect_equal(tidy_result$degree, 2)
})

test_that("step_measure_emsc is tunable", {
  step <- step_measure_emsc(recipe(~., data = create_scatter_data()), degree = 2)
  tunable_result <- tunable(step$steps[[1]])
  expect_s3_class(tunable_result, "tbl_df")
  expect_true("degree" %in% tunable_result$name)
})

# ==============================================================================
# step_measure_osc tests
# ==============================================================================

test_that("step_measure_osc removes orthogonal variation", {
  rec <- prep_scatter_recipe(function(r) {
    step_measure_osc(r, n_components = 1)
  })

  result <- bake(rec, new_data = NULL)

  expect_true(".measures" %in% names(result))
  expect_true(is_measure_list(result$.measures))

  # Check values are reasonable
  values <- result$.measures[[1]]$value
  expect_false(anyNA(values))
  expect_true(all(is.finite(values)))
})

test_that("step_measure_osc works with multiple components", {
  rec <- prep_scatter_recipe(function(r) {
    step_measure_osc(r, n_components = 3)
  })

  result <- bake(rec, new_data = NULL)

  expect_true(".measures" %in% names(result))
  expect_false(anyNA(result$.measures[[1]]$value))
})

test_that("step_measure_osc validates inputs", {
  expect_error(
    recipe(~., data = create_scatter_data()) |>
      step_measure_osc(n_components = 0),
    "positive integer"
  )

  expect_error(
    recipe(~., data = create_scatter_data()) |>
      step_measure_osc(tolerance = -1),
    "positive number"
  )
})

test_that("step_measure_osc errors without outcomes", {
  data <- create_scatter_data()

  expect_error(
    recipe(~ ., data = data) |>
      update_role(id, new_role = "id") |>
      update_role(outcome, new_role = "other") |>
      step_measure_input_long(value, location = vars(location)) |>
      step_measure_osc() |>
      prep(),
    "No outcome variables"
  )
})

test_that("step_measure_osc print method works", {
  rec <- recipe(outcome ~ ., data = create_scatter_data()) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(value, location = vars(location)) |>
    step_measure_osc(n_components = 2)

  expect_output(print(rec$steps[[2]]), "OSC.*2 components")
})

test_that("step_measure_osc tidy method works", {
  rec <- prep_scatter_recipe(function(r) {
    step_measure_osc(r, n_components = 2)
  })

  tidy_result <- tidy(rec, number = 2)
  expect_s3_class(tidy_result, "tbl_df")
  expect_true("n_components" %in% names(tidy_result))
  expect_equal(tidy_result$n_components, 2)
})

test_that("step_measure_osc is tunable", {
  step <- step_measure_osc(
    recipe(outcome ~ ., data = create_scatter_data()),
    n_components = 1
  )
  tunable_result <- tunable(step$steps[[1]])
  expect_s3_class(tunable_result, "tbl_df")
  expect_true("n_components" %in% tunable_result$name)
})

# ==============================================================================
# Integration tests
# ==============================================================================

test_that("EMSC works with meats_long data", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_emsc(degree = 2) |>
    prep()

  result <- bake(rec, new_data = NULL)

  expect_true(".measures" %in% names(result))
  expect_false(anyNA(result$.measures[[1]]$value))
})

test_that("OSC works with meats_long data", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_osc(n_components = 2) |>
    prep()

  result <- bake(rec, new_data = NULL)

  expect_true(".measures" %in% names(result))
  expect_false(anyNA(result$.measures[[1]]$value))
})

test_that("EMSC and OSC can be combined", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_emsc(degree = 1) |>
    step_measure_osc(n_components = 1) |>
    prep()

  result <- bake(rec, new_data = NULL)

  expect_true(".measures" %in% names(result))
  expect_false(anyNA(result$.measures[[1]]$value))
})
