test_that("derivative and baseline tuning markers keep their identifiers", {
  skip_if_not_installed("tune")

  data <- tibble::tibble(x = 1:3)
  recipe_specs <- list(
    recipes::recipe(~., data = data) |>
      step_measure_derivative(order = tune::tune("derivative_order")),
    recipes::recipe(~., data = data) |>
      step_measure_derivative_gap(
        gap = tune::tune("derivative_gap"),
        segment = tune::tune("derivative_segment")
      ),
    recipes::recipe(~., data = data) |>
      step_measure_baseline_airpls(lambda = tune::tune("airpls_lambda")),
    recipes::recipe(~., data = data) |>
      step_measure_baseline_arpls(lambda = tune::tune("arpls_lambda"))
  )

  expected <- list(
    c("derivative_order"),
    c("derivative_gap", "derivative_segment"),
    c("airpls_lambda"),
    c("arpls_lambda")
  )
  marker_fields <- list(
    "order",
    c("gap", "segment"),
    "lambda",
    "lambda"
  )

  for (i in seq_along(recipe_specs)) {
    step <- recipe_specs[[i]]$steps[[1L]]
    parameters <- tune::extract_parameter_set_dials(recipe_specs[[i]])

    expect_setequal(parameters$id, expected[[i]])
    expect_output(print(step), "tune")

    tidy_step <- recipes::tidy(step)
    for (name in marker_fields[[i]]) {
      expect_true(
        is.list(tidy_step[[name]]) &&
          rlang::is_call(tidy_step[[name]][[1L]], "tune"),
        info = name
      )
    }
  }
})

test_that("derivative and baseline controls reject invalid concrete values", {
  expect_error(
    step_measure_derivative(order = 1.5),
    "must be 1 or 2"
  )
  expect_error(
    step_measure_derivative_gap(gap = 1.5),
    "must be .*integer"
  )
  expect_error(
    step_measure_derivative_gap(segment = 1.5),
    "must be .*integer"
  )
  expect_error(
    step_measure_baseline_airpls(lambda = Inf),
    "must be a positive number"
  )
  expect_error(
    step_measure_baseline_arpls(lambda = NA_real_),
    "must be a positive number"
  )
})

test_that("finalized derivative and baseline recipes prep and bake", {
  skip_if_not_installed("tune")

  data <- tibble::tibble(
    id = rep(1:2, each = 20),
    outcome = rep(c(0, 1), each = 20),
    location = rep(seq_len(20), 2),
    signal = rep(sin(seq_len(20) / 3), 2) + rep(c(0, 0.2), each = 20)
  )
  input <- function() {
    recipes::recipe(outcome ~ ., data = data) |>
      recipes::update_role(id, new_role = "id") |>
      step_measure_input_long(signal, location = dplyr::vars(location))
  }

  derivative <- input() |>
    step_measure_derivative(order = tune::tune("order")) |>
    tune::finalize_recipe(tibble::tibble(order = 2L)) |>
    recipes::prep()
  derivative_result <- recipes::bake(derivative, new_data = NULL)
  expect_equal(length(derivative_result$.measures[[1]]$value), 18L)
  expect_identical(derivative$steps[[2]]$order, 2L)

  gap <- input() |>
    step_measure_derivative_gap(
      gap = tune::tune("gap"),
      segment = tune::tune("segment")
    ) |>
    tune::finalize_recipe(tibble::tibble(gap = 2L, segment = 2L)) |>
    recipes::prep()
  gap_result <- recipes::bake(gap, new_data = NULL)
  expect_equal(length(gap_result$.measures[[1]]$value), 14L)
  expect_identical(gap$steps[[2]]$gap, 2L)
  expect_identical(gap$steps[[2]]$segment, 2L)

  airpls <- input() |>
    step_measure_baseline_airpls(
      lambda = tune::tune("lambda"),
      max_iter = 3L
    ) |>
    tune::finalize_recipe(tibble::tibble(lambda = 10)) |>
    recipes::prep()
  airpls_result <- recipes::bake(airpls, new_data = NULL)
  expect_true(is_measure_list(airpls_result$.measures))
  expect_identical(airpls$steps[[2]]$lambda, 10)

  arpls <- input() |>
    step_measure_baseline_arpls(
      lambda = tune::tune("lambda"),
      max_iter = 3L
    ) |>
    tune::finalize_recipe(tibble::tibble(lambda = 10)) |>
    recipes::prep()
  arpls_result <- recipes::bake(arpls, new_data = NULL)
  expect_true(is_measure_list(arpls_result$.measures))
  expect_identical(arpls$steps[[2]]$lambda, 10)
})

test_that("prep revalidates finalized integer derivative controls", {
  skip_if_not_installed("tune")

  data <- tibble::tibble(
    outcome = 1,
    location = seq_len(8),
    signal = seq_len(8)
  )
  rec <- recipes::recipe(outcome ~ ., data = data) |>
    step_measure_input_long(signal, location = dplyr::vars(location)) |>
    step_measure_derivative(order = tune::tune("order")) |>
    tune::finalize_recipe(tibble::tibble(order = 1.5))

  expect_error(recipes::prep(rec), "must be 1 or 2")
})
