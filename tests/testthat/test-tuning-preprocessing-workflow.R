test_that("resampled preprocessing matches the selected concrete recipe", {
  skip_if_not_installed(c("parsnip", "rsample", "workflows"))
  withr::local_seed(45)
  axis <- seq(0, 8, length.out = 41)
  data <- tibble::tibble(outcome = seq_len(12))
  data$.measures <- new_measure_list(lapply(data$outcome, function(i) {
    new_measure_tbl(
      axis,
      0.1 * axis^2 + i * exp(-((axis - 3) / 0.7)^2) + sin(axis * 3)
    )
  }))

  recipe_for <- function(width, penalty, order) {
    recipes::recipe(outcome ~ ., data = data) |>
      step_measure_smooth_ma(window = width) |>
      step_measure_baseline_airpls(lambda = penalty) |>
      step_measure_derivative(order = order) |>
      recipes::step_mutate(
        energy = purrr::map_dbl(.measures, function(x) sum(x$value^2))
      ) |>
      recipes::step_rm(.measures)
  }
  wf <- workflows::workflow() |>
    workflows::add_recipe(recipe_for(
      tune::tune("width"),
      tune::tune("penalty"),
      tune::tune("difference")
    )) |>
    workflows::add_model(parsnip::set_engine(parsnip::linear_reg(), "lm"))
  grid <- tibble::tibble(
    width = c(3L, 7L, 7L),
    penalty = c(100, 10000, 100),
    difference = c(1L, 2L, 1L)
  )
  expect_setequal(tune::extract_parameter_set_dials(wf)$id, names(grid))
  probe <- data[1, ]
  energy_at_probe <- function(trained) {
    recipes::bake(trained, new_data = probe)$energy
  }
  expected <- purrr::pmap_dbl(grid, function(width, penalty, difference) {
    energy_at_probe(recipes::prep(recipe_for(width, penalty, difference)))
  })
  expect_true(all(is.finite(expected)))
  expect_equal(length(unique(signif(expected, 6))), nrow(grid))

  results <- tune::tune_grid(
    wf,
    resamples = rsample::vfold_cv(data, v = 2),
    grid = grid,
    metrics = yardstick::metric_set(yardstick::rmse),
    control = tune::control_grid(extract = function(fit) {
      energy_at_probe(workflows::extract_recipe(fit, estimated = TRUE))
    })
  )
  actual <- tune::collect_extracts(results)
  comparison <- dplyr::left_join(
    actual,
    dplyr::mutate(grid, expected = expected),
    by = names(grid)
  )
  expect_equal(nrow(comparison), nrow(grid) * 2L)
  expect_equal(unlist(comparison$.extracts), comparison$expected)
  metrics <- tune::collect_metrics(results)
  expect_equal(nrow(metrics), nrow(grid))
  expect_true(all(is.finite(metrics$mean)))
})
