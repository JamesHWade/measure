test_that("step_measure_baseline_custom runs in recipe workflow", {
  # Simple baseline function that returns mean
  mean_baseline <- function(x) {
    rep(mean(x$value, na.rm = TRUE), nrow(x))
  }

  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_baseline_custom(.fn = mean_baseline) |>
    prep()

  result <- bake(rec, new_data = NULL)

  expect_s3_class(result, "tbl_df")
  expect_true(".measures" %in% names(result))
  expect_true(is_measure_list(result$.measures))
})

test_that("step_measure_baseline_custom modifies values", {
  poly_baseline <- function(x) {
    fit <- lm(x$value ~ poly(x$location, 2))
    predict(fit)
  }

  rec_original <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    prep()

  rec_custom <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_baseline_custom(.fn = poly_baseline) |>
    prep()

  original <- bake(rec_original, new_data = NULL)
  corrected <- bake(rec_custom, new_data = NULL)

  orig_vals <- original$.measures[[1]]$value
  corr_vals <- corrected$.measures[[1]]$value

  expect_false(all(orig_vals == corr_vals))
})

test_that("step_measure_baseline_custom works with formula interface", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_baseline_custom(.fn = ~ rep(mean(.x$value), nrow(.x))) |>
    prep()

  result <- bake(rec, new_data = NULL)
  expect_true(is_measure_list(result$.measures))
})

test_that("step_measure_baseline_custom passes additional arguments", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_baseline_custom(
      .fn = ~ {
        fit <- lm(.x$value ~ poly(.x$location, degree))
        predict(fit)
      },
      degree = 2
    ) |>
    prep()

  result <- bake(rec, new_data = NULL)
  expect_true(is_measure_list(result$.measures))
})

test_that("step_measure_baseline_custom subtract = FALSE returns baseline", {
  mean_baseline <- function(x) {
    rep(mean(x$value, na.rm = TRUE), nrow(x))
  }

  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_baseline_custom(.fn = mean_baseline, subtract = FALSE) |>
    prep()

  result <- bake(rec, new_data = NULL)
  baseline_vals <- result$.measures[[1]]$value

  # All values should be the same (the mean)
  expect_equal(length(unique(baseline_vals)), 1)
})

test_that("step_measure_baseline_custom validates subtract argument", {
  expect_error(
    recipe(water + fat + protein ~ ., data = meats_small) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_measure_baseline_custom(.fn = ~ .x$value, subtract = "yes") |>
      prep(),
    "subtract"
  )
})

test_that("step_measure_baseline_custom handles function errors gracefully", {
  bad_fn <- function(x) {
    stop("intentional error")
  }

  # Use minimal data (1 sample) to avoid multiple warnings

  minimal_data <- meats_small[meats_small$id == 1, ]

  # Should warn during prep but not error
  expect_warning(
    rec <- recipe(water + fat + protein ~ ., data = minimal_data) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_measure_baseline_custom(.fn = bad_fn) |>
      prep(),
    "Custom baseline function failed"
  )

  # Values should be unchanged (returned as-is when function fails)
  result <- bake(rec, new_data = NULL)
  expect_true(is_measure_list(result$.measures))
})

test_that("step_measure_baseline_custom validates return length", {
  wrong_length_fn <- function(x) {
    rep(0, 5) # Always returns 5 values
  }

  # Use minimal data (1 sample) to avoid multiple warnings
  minimal_data <- meats_small[meats_small$id == 1, ]

  # Should warn during prep
  expect_warning(
    recipe(water + fat + protein ~ ., data = minimal_data) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_measure_baseline_custom(.fn = wrong_length_fn) |>
      prep(),
    "returned.*values"
  )
})

test_that("step_measure_baseline_custom validates return type", {
  non_numeric_fn <- function(x) {
    rep("baseline", nrow(x))
  }

  # Use minimal data (1 sample) to avoid multiple warnings
  minimal_data <- meats_small[meats_small$id == 1, ]

  # Should warn during prep
  expect_warning(
    recipe(water + fat + protein ~ ., data = minimal_data) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_measure_baseline_custom(.fn = non_numeric_fn) |>
      prep(),
    "must return numeric"
  )
})

test_that("step_measure_baseline_custom removes known baseline correctly", {
  # Create synthetic data with known linear baseline only (no peak)

  # This tests that a linear baseline function correctly removes a linear trend
  set.seed(123)
  n_points <- 100
  location <- seq(1, 100)
  true_baseline <- 0.05 * location + 2 # Linear baseline
  noise <- rnorm(n_points, sd = 0.01) # Small noise
  observed <- true_baseline + noise

  synthetic_data <- tibble::tibble(
    id = rep(1, n_points),
    outcome = rep(1, n_points),
    value = observed,
    location = location
  )

  # Use linear baseline function
  linear_baseline <- function(x) {
    fit <- lm(x$value ~ x$location)
    predict(fit)
  }

  rec <- recipe(outcome ~ ., data = synthetic_data) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(value, location = vars(location)) |>
    step_measure_baseline_custom(.fn = linear_baseline) |>
    prep()

  result <- bake(rec, new_data = NULL)
  corrected <- result$.measures[[1]]$value

  # After removing a linear baseline from linear data, values should be ~0
  # (just noise remaining)
  expect_lt(abs(mean(corrected)), 0.1)
  expect_lt(sd(corrected), 0.1)
})

test_that("step_measure_baseline_custom tidy method works", {
  mean_fn <- function(x) rep(mean(x$value), nrow(x))

  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_baseline_custom(.fn = mean_fn, subtract = FALSE)

  # Before prep
  tidy_before <- tidy(rec, number = 2)
  expect_equal(tidy_before$terms, "<all measure columns>")
  expect_equal(tidy_before$subtract, FALSE)

  # After prep
  rec_prepped <- prep(rec)
  tidy_after <- tidy(rec_prepped, number = 2)
  expect_equal(tidy_after$terms, ".measures")
  expect_equal(tidy_after$subtract, FALSE)
})

test_that("step_measure_baseline_custom print method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_baseline_custom(.fn = ~ .x$value)

  expect_output(print(rec), "Custom baseline correction")
})

test_that("step_measure_baseline_custom tunable returns empty for no tunable params", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_baseline_custom(.fn = ~ .x$value)

  tunable_params <- tunable(rec$steps[[2]])
  expect_equal(nrow(tunable_params), 0)
})

test_that("step_measure_baseline_custom tunable works with declared params", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_baseline_custom(
      .fn = ~ {
        fit <- lm(.x$value ~ poly(.x$location, degree))
        predict(fit)
      },
      degree = 2,
      tunable = list(
        degree = list(pkg = "dials", fun = "degree_int", range = c(1L, 5L))
      )
    )

  tunable_params <- tunable(rec$steps[[2]])
  expect_equal(nrow(tunable_params), 1)
  expect_equal(tunable_params$name, "degree")
  expect_equal(tunable_params$call_info[[1]]$pkg, "dials")
  expect_equal(tunable_params$call_info[[1]]$fun, "degree_int")
})

test_that("required_pkgs includes measure", {
  step <- step_measure_baseline_custom(
    recipe(water + fat + protein ~ ., data = meats_small),
    .fn = ~ .x$value
  )
  expect_true("measure" %in% required_pkgs(step))
})
