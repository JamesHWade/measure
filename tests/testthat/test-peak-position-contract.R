test_that("constrained fits preserve initial component rank beyond start midpoints", {
  x <- seq(0, 16, by = 0.025)
  models <- list(gaussian_peak_model(), lorentzian_peak_model())
  truth <- list(
    list(height = 1.2, center = 10, width = 0.65),
    list(height = 0.8, center = 8, gamma = 0.5)
  )
  y <- sum_peak_models(x, models, truth)
  init_params <- list(
    list(height = 1.1, center = 8, width = 0.7),
    list(height = 0.75, center = 6.8, gamma = 0.55)
  )
  initial_midpoint <- mean(c(
    init_params[[1]]$center,
    init_params[[2]]$center
  ))

  # Seed 3 makes the second multistart's center jitters cross the original
  # component order. Its rank anchor must still come from init_params.
  jittered <- withr::with_seed(
    3,
    .perturb_params(init_params, scale = 0.4)
  )
  expect_lt(jittered[[1]]$center, jittered[[2]]$center)
  fit_context <- .new_deconv_optimizer_context(
    models,
    init_params,
    range(x),
    range(y),
    constrain_positions = TRUE
  )
  jittered_start <- .prepare_deconv_start(
    models,
    jittered,
    fit_context
  )
  jittered_fit <- .unflatten_params(
    .transform_deconv_params(jittered_start, fit_context),
    models
  )
  expect_true(
    jittered_fit[[2]]$center < jittered_fit[[1]]$center
  )

  for (optimizer in c("lbfgsb", "multistart", "nelder_mead")) {
    result <- withr::with_seed(
      3,
      optimize_deconvolution(
        x,
        y,
        models,
        init_params,
        optimizer = optimizer,
        max_iter = 10000L,
        tol = 1e-9,
        n_starts = 3L,
        constrain_positions = TRUE
      )
    )
    params <- result$parameters

    expect_true(params[[2]]$center < params[[1]]$center, info = optimizer)
    expect_true(params[[2]]$center > initial_midpoint, info = optimizer)
    expect_true(params[[1]]$center > initial_midpoint, info = optimizer)
    expect_equal(params[[1]]$center, truth[[1]]$center, tolerance = 0.03)
    expect_equal(params[[2]]$center, truth[[2]]$center, tolerance = 0.03)
    expect_named(params[[1]], c("height", "center", "width"))
    expect_named(params[[2]], c("height", "center", "gamma"))

    expected_fitted <- sum_peak_models(x, models, params)
    expected_residuals <- y - expected_fitted
    expect_equal(result$fitted_values, expected_fitted, tolerance = 1e-10)
    expect_equal(result$residuals, expected_residuals, tolerance = 1e-10)
    expect_equal(
      result$final_value,
      sum(expected_residuals^2),
      tolerance = 1e-10
    )
  }
})

test_that("unconstrained fits allow centers to remain in descending order", {
  x <- seq(0, 16, by = 0.025)
  models <- list(gaussian_peak_model(), lorentzian_peak_model())
  truth <- list(
    list(height = 1.2, center = 10, width = 0.65),
    list(height = 0.8, center = 8, gamma = 0.5)
  )
  y <- sum_peak_models(x, models, truth)

  result <- optimize_deconvolution(
    x,
    y,
    models,
    truth,
    optimizer = "lbfgsb",
    constrain_positions = FALSE
  )

  expect_gt(result$parameters[[1]]$center, result$parameters[[2]]$center)
  expect_equal(result$parameters, truth, tolerance = 1e-5)
  expect_lt(result$final_value, 1e-12)
})

test_that("constrained fitting rejects duplicate starting centers", {
  x <- seq(0, 10, by = 0.1)
  models <- list(gaussian_peak_model(), gaussian_peak_model())
  init_params <- list(
    list(height = 1, center = 5, width = 1),
    list(height = 0.5, center = 5, width = 1.5)
  )

  expect_error(
    optimize_deconvolution(
      x,
      rep(0, length(x)),
      models,
      init_params,
      optimizer = "lbfgsb",
      constrain_positions = TRUE
    ),
    "duplicate starting centers"
  )
})


test_that("constrained fits honor component-specific center bounds", {
  original_bounds <- getS3method(
    "peak_model_bounds",
    "gaussian_peak_model",
    envir = asNamespace("measure")
  )
  testthat::local_mocked_bindings(
    peak_model_bounds.gaussian_peak_model = function(
      model,
      x_range,
      y_range
    ) {
      bounds <- original_bounds(model, x_range, y_range)
      if (!is.null(model$center_bounds)) {
        bounds$lower[["center"]] <- model$center_bounds[[1]]
        bounds$upper[["center"]] <- model$center_bounds[[2]]
      }
      bounds
    },
    .package = "measure"
  )

  x <- seq(0, 16, by = 0.025)
  models <- list(gaussian_peak_model(), gaussian_peak_model())
  models[[1]]$center_bounds <- c(8, 11)
  models[[2]]$center_bounds <- c(4, 8.5)
  truth <- list(
    list(height = 1.2, center = 10, width = 0.65),
    list(height = 0.8, center = 6, width = 0.7)
  )
  y <- sum_peak_models(x, models, truth)
  init_params <- list(
    list(height = 1.1, center = 8.5, width = 0.7),
    list(height = 0.75, center = 5.5, width = 0.75)
  )

  result <- optimize_deconvolution(
    x,
    y,
    models,
    init_params,
    optimizer = "lbfgsb",
    max_iter = 10000L,
    tol = 1e-9,
    constrain_positions = TRUE
  )

  expect_true(result$parameters[[2]]$center < result$parameters[[1]]$center)
  expect_gte(result$parameters[[1]]$center, 8)
  expect_lte(result$parameters[[1]]$center, 11)
  expect_gte(result$parameters[[2]]$center, 4)
  expect_lte(result$parameters[[2]]$center, 8.5)
  expect_equal(
    result$parameters[[1]]$center,
    truth[[1]]$center,
    tolerance = 0.03
  )
  expect_equal(
    result$parameters[[2]]$center,
    truth[[2]]$center,
    tolerance = 0.03
  )

  models[[1]]$center_bounds <- c(9, 12)
  models[[2]]$center_bounds <- c(2, 8)
  init_params[[1]]$center <- 7
  init_params[[2]]$center <- 10
  expect_error(
    optimize_deconvolution(
      x,
      y,
      models,
      init_params,
      optimizer = "lbfgsb",
      constrain_positions = TRUE
    ),
    "cannot satisfy"
  )
})
