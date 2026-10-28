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

test_that("step_measure_baseline_custom preserves callback lexical bindings", {
  offset <- 99
  callback <- function(x, offset) {
    rep(offset, nrow(x))
  }

  rec_input <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    prep()
  original <- bake(rec_input, new_data = NULL)

  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_baseline_custom(.fn = callback, offset = 2) |>
    prep()

  result <- bake(rec, new_data = NULL)
  result_again <- bake(rec, new_data = NULL)

  expect_identical(get("offset", envir = environment(callback)), 99)
  expect_equal(
    result$.measures[[1]]$value,
    original$.measures[[1]]$value - 2
  )
  expect_equal(result_again, result)
})

test_that("formula callback arguments use a private invocation environment", {
  scale <- 99
  baseline_formula <- ~ rep(scale, nrow(.x))

  rec_input <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    prep()
  original <- bake(rec_input, new_data = NULL)

  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_baseline_custom(.fn = baseline_formula, scale = 2) |>
    prep()

  result <- bake(rec, new_data = NULL)
  result_again <- bake(rec, new_data = NULL)

  expect_identical(get("scale", envir = environment(baseline_formula)), 99)
  expect_equal(
    result$.measures[[1]]$value,
    original$.measures[[1]]$value - 2
  )
  expect_equal(result_again, result)
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

test_that("custom baseline parameters are discovered and finalized by tune", {
  rec_tuned <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_baseline_custom(
      .fn = ~ {
        fit <- lm(.x$value ~ poly(.x$location, degree))
        predict(fit)
      },
      degree = tune::tune(id = "baseline_degree"),
      tunable = list(
        degree = list(
          pkg = "dials",
          fun = "degree_int",
          range = c(1L, 4L)
        )
      )
    )

  tune_args <- generics::tune_args(rec_tuned, full = TRUE)
  degree_arg <- tune_args[tune_args$name == "degree", ]
  parameters <- tune::extract_parameter_set_dials(rec_tuned)

  expect_equal(nrow(degree_arg), 1)
  expect_true(degree_arg$tunable)
  expect_identical(degree_arg$id, "baseline_degree")
  expect_identical(parameters$id, "baseline_degree")

  rec_final <- tune::finalize_recipe(
    rec_tuned,
    list(baseline_degree = 2L)
  )
  finalized_args <- generics::tune_args(rec_final, full = TRUE)
  rec_fixed <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_baseline_custom(
      .fn = ~ {
        fit <- lm(.x$value ~ poly(.x$location, degree))
        predict(fit)
      },
      degree = 2L
    )

  result_final <- bake(prep(rec_final), new_data = NULL)
  result_fixed <- bake(prep(rec_fixed), new_data = NULL)
  result_serialized <- bake(
    prep(unserialize(serialize(rec_final, NULL))),
    new_data = NULL
  )

  expect_equal(result_final$.measures, result_fixed$.measures)
  expect_equal(result_serialized$.measures, result_fixed$.measures)
  expect_false(
    finalized_args$tunable[finalized_args$name == "degree"]
  )
})

test_that("multiple custom baseline parameters retain their tune IDs", {
  param_spec <- list(
    pkg = "dials",
    fun = "degree_int",
    range = c(1L, 4L)
  )
  rec_tuned <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_baseline_custom(
      .fn = ~ rep(a + b, nrow(.x)),
      a = tune::tune(id = "custom_a"),
      b = tune::tune(id = "custom_b"),
      subtract = FALSE,
      tunable = list(a = param_spec, b = param_spec)
    )

  args <- generics::tune_args(rec_tuned, full = TRUE)
  parameters <- tune::extract_parameter_set_dials(rec_tuned)
  rec_final <- tune::finalize_recipe(
    rec_tuned,
    list(custom_a = 1L, custom_b = 3L)
  )
  result <- bake(prep(rec_final), new_data = NULL)

  expect_setequal(args$name[args$tunable], c("a", "b"))
  expect_setequal(args$id[args$tunable], c("custom_a", "custom_b"))
  expect_setequal(parameters$id, c("custom_a", "custom_b"))
  expect_equal(unique(result$.measures[[1]]$value), 4)
})

test_that("custom baseline tuning supports callback names matching step fields", {
  rec_tuned <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_baseline_custom(
      .fn = ~ rep(tunable_params, nrow(.x)),
      tunable_params = tune::tune(id = "custom_value"),
      subtract = FALSE,
      tunable = list(
        tunable_params = list(
          pkg = "dials",
          fun = "degree_int",
          range = c(1L, 4L)
        )
      )
    )

  args <- generics::tune_args(rec_tuned, full = TRUE)
  custom_arg <- args[args$name == "tunable_params", ]
  rec_final <- tune::finalize_recipe(
    rec_tuned,
    list(custom_value = 2L)
  )
  result <- bake(prep(rec_final), new_data = NULL)

  expect_true(custom_arg$tunable)
  expect_identical(custom_arg$id, "custom_value")
  expect_equal(unique(result$.measures[[1]]$value), 2)
})

test_that("custom IDs survive a collision with a proxy field name", {
  rec_tuned <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_baseline_custom(
      .fn = ~ rep(fn_args, nrow(.x)),
      fn_args = tune::tune(id = ".measure_custom_arg_fn_args"),
      subtract = FALSE,
      tunable = list(
        fn_args = list(
          pkg = "dials",
          fun = "degree_int",
          range = c(1L, 4L)
        )
      )
    )

  args <- generics::tune_args(rec_tuned)
  rec_final <- tune::finalize_recipe(
    rec_tuned,
    list(.measure_custom_arg_fn_args = 2L)
  )
  result <- bake(prep(rec_final), new_data = NULL)

  expect_identical(args$id, ".measure_custom_arg_fn_args")
  expect_true(args$tunable)
  expect_equal(unique(result$.measures[[1]]$value), 2)
})

test_that("empty tunable metadata permits fixed unnamed callback arguments", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_baseline_custom(
      .fn = ~ rep(1, nrow(.x)),
      42L,
      subtract = FALSE,
      tunable = list()
    ) |>
    prep()

  result <- bake(rec, new_data = NULL)

  expect_equal(unique(result$.measures[[1]]$value), 1)
})

test_that("fixed nested callback arguments preserve NULL list elements", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_baseline_custom(
      .fn = ~ rep(length(settings), nrow(.x)),
      settings = list(NULL, 2L),
      subtract = FALSE
    )

  args <- generics::tune_args(rec, full = TRUE)
  result <- bake(prep(rec), new_data = NULL)

  expect_false(args$tunable[args$name == "settings"])
  expect_equal(unique(result$.measures[[1]]$value), 2)
})

test_that("tunable declarations require names", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small)

  expect_snapshot(
    error = TRUE,
    step_measure_baseline_custom(
      rec,
      .fn = ~ .x$value,
      degree = 2L,
      tunable = list(list(pkg = "dials", fun = "degree_int"))
    )
  )
})

test_that("tunable declarations require unique names", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small)
  spec <- list(pkg = "dials", fun = "degree_int")
  duplicate_specs <- structure(
    list(spec, spec),
    names = c("degree", "degree")
  )

  expect_snapshot(
    error = TRUE,
    step_measure_baseline_custom(
      rec,
      .fn = ~ .x$value,
      degree = 2L,
      tunable = duplicate_specs
    )
  )
})

test_that("declared tunable arguments must be provided exactly once", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small)
  spec <- list(pkg = "dials", fun = "degree_int")
  duplicate_args <- structure(
    list(1L, 2L),
    names = c("degree", "degree")
  )

  expect_snapshot(
    error = TRUE,
    do.call(
      step_measure_baseline_custom,
      c(
        list(recipe = rec, .fn = ~ .x$value),
        duplicate_args,
        list(tunable = list(degree = spec))
      )
    )
  )
})

test_that("tunable declarations must match callback argument names", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small)

  expect_snapshot(
    error = TRUE,
    step_measure_baseline_custom(
      rec,
      .fn = ~ .x$value,
      degree = 2L,
      tunable = list(span = list(pkg = "dials", fun = "degree_int"))
    )
  )
})

test_that("tunable declarations require package and function strings", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small)

  expect_snapshot(
    error = TRUE,
    step_measure_baseline_custom(
      rec,
      .fn = ~ .x$value,
      degree = 2L,
      tunable = list(degree = list(pkg = "dials"))
    )
  )

  expect_snapshot(
    error = TRUE,
    step_measure_baseline_custom(
      rec,
      .fn = ~ .x$value,
      degree = 2L,
      tunable = list(degree = list(fun = "degree_int"))
    )
  )
})

test_that("unresolved custom baseline tuning markers abort during prep", {
  rec <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_baseline_custom(
      .fn = ~ rep(degree, nrow(.x)),
      degree = tune::tune(),
      tunable = list(
        degree = list(
          pkg = "dials",
          fun = "degree_int",
          range = c(1L, 4L)
        )
      )
    )

  expect_error(prep(rec), "unresolved tuning markers")

  rec_unnamed <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_baseline_custom(
      .fn = ~ .x$value,
      tune::tune()
    )

  expect_error(prep(rec_unnamed), "unresolved tuning markers")

  rec_nested <- recipe(water + fat + protein ~ ., data = meats_small) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_baseline_custom(
      .fn = ~ .x$value,
      settings = list(tune::tune())
    )

  expect_error(prep(rec_nested), "unresolved tuning markers")
})

test_that("required_pkgs includes measure", {
  step <- step_measure_baseline_custom(
    recipe(water + fat + protein ~ ., data = meats_small),
    .fn = ~ .x$value
  )
  expect_true("measure" %in% required_pkgs(step))
})
