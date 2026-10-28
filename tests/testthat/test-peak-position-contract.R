test_that("constrained fits preserve initial component rank beyond start midpoints", {
  x <- seq(0, 16, by = 0.025)
  models <- list(gaussian_peak_model(), lorentzian_peak_model())
  truth <- list(
    list(height = 1.2, center = 10, width = 0.65),
    list(height = 0.8, center = 8, gamma = 0.5)
  )
  y <- sum_peak_models(x, models, truth)
  init_params <- list(
    list(height = 1.1, center = 8.3, width = 0.7),
    list(height = 0.75, center = 7.3, gamma = 0.55)
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
  expect_lte(
    jittered_fit[[2]]$center,
    jittered_fit[[1]]$center
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


test_that("constrained fits honor component-specific and fixed bounds", {
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
      if (!is.null(model$fixed_params)) {
        for (name in names(model$fixed_params)) {
          bounds$lower[[name]] <- model$fixed_params[[name]]
          bounds$upper[[name]] <- model$fixed_params[[name]]
        }
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

  x <- seq(0, 10, by = 0.05)
  fixed_center_model <- gaussian_peak_model()
  fixed_center_model$center_bounds <- c(3, 3)
  fixed_center_truth <- list(list(height = 1, center = 3, width = 0.8))
  fixed_center_y <- sum_peak_models(
    x,
    list(fixed_center_model),
    fixed_center_truth
  )
  fixed_center_start <- list(list(height = 0.9, center = 3, width = 1.1))

  tied_models <- list(gaussian_peak_model(), gaussian_peak_model())
  tied_models[[1]]$center_bounds <- c(3, 3)
  tied_models[[2]]$center_bounds <- c(3, 3)
  tied_truth <- list(
    list(height = 1, center = 3, width = 0.8),
    list(height = 0.6, center = 3, width = 1.1)
  )
  tied_y <- sum_peak_models(x, tied_models, tied_truth)
  tied_start <- list(
    list(height = 1, center = 2, width = 0.8),
    list(height = 0.6, center = 4, width = 1.1)
  )
  tied_result <- optimize_deconvolution(
    x,
    tied_y,
    tied_models,
    tied_start,
    optimizer = "lbfgsb",
    constrain_positions = TRUE
  )
  expect_equal(
    vapply(tied_result$parameters, function(params) params$center, numeric(1)),
    c(3, 3)
  )

  for (optimizer in c("lbfgsb", "multistart", "nelder_mead")) {
    result <- optimize_deconvolution(
      x,
      fixed_center_y,
      list(fixed_center_model),
      fixed_center_start,
      optimizer = optimizer,
      max_iter = 10000L,
      tol = 1e-9,
      n_starts = 3L,
      constrain_positions = TRUE
    )

    expect_true(result$convergence, info = optimizer)
    expect_equal(result$parameters[[1]]$center, 3)
    expect_equal(
      result$parameters[[1]]$height,
      fixed_center_truth[[1]]$height,
      tolerance = 0.01
    )
    expect_equal(
      result$parameters[[1]]$width,
      fixed_center_truth[[1]]$width,
      tolerance = 0.01
    )
    expect_lt(result$final_value, 1e-6)
  }

  fixed_model <- gaussian_peak_model()
  fixed_model$fixed_params <- c(height = 1, center = 3, width = 0.8)
  for (optimizer in c("lbfgsb", "multistart", "nelder_mead")) {
    result <- optimize_deconvolution(
      x,
      fixed_center_y,
      list(fixed_model),
      list(list(height = 0.9, center = 2, width = 1.1)),
      optimizer = optimizer,
      max_iter = 10000L,
      n_starts = 3L,
      constrain_positions = TRUE
    )

    expect_true(result$convergence, info = optimizer)
    expect_identical(result$n_iterations, 0L)
    expect_equal(result$parameters, fixed_center_truth)
    expect_lt(result$final_value, 1e-12)
  }
})


test_that("bounded isotonic projection returns least-squares centers and allows ties", {
  expect_equal(
    .bounded_isotonic_projection(
      centers = c(3, 1, 4),
      lower = rep(-Inf, 3),
      upper = rep(Inf, 3)
    ),
    c(2, 2, 4)
  )
  expect_equal(
    .bounded_isotonic_projection(
      centers = c(10, 0, 9),
      lower = c(0, 1, 2),
      upper = c(4, 8, 7)
    ),
    c(4, 4, 7)
  )
  expect_equal(
    .bounded_isotonic_projection(
      centers = c(9, 8),
      lower = c(0, 0),
      upper = c(10, 5)
    ),
    c(5, 5)
  )
  expect_equal(
    .bounded_isotonic_projection(
      centers = c(8, 2),
      lower = c(0, 5),
      upper = c(10, 10)
    ),
    c(5, 5)
  )
  expect_error(
    .bounded_isotonic_projection(
      centers = c(8, 2),
      lower = c(6, 0),
      upper = c(10, 5)
    ),
    "cannot satisfy"
  )
})

test_that("a constrained single peak can move from the axis boundary", {
  x <- seq(0, 10, by = 0.05)
  model <- gaussian_peak_model()
  truth <- list(list(height = 1, center = 3, width = 1))
  y <- sum_peak_models(x, list(model), truth)
  init_params <- list(list(height = 1, center = 0, width = 2))

  result <- optimize_deconvolution(
    x,
    y,
    list(model),
    init_params,
    optimizer = "lbfgsb",
    max_iter = 10000L,
    tol = 1e-10,
    constrain_positions = TRUE
  )

  expect_true(result$convergence)
  expect_equal(result$parameters[[1]]$center, 3, tolerance = 1e-4)
  expect_lt(result$final_value, 1e-8)
  expect_equal(
    result$final_value,
    sum(result$residuals^2),
    tolerance = 1e-12
  )
})

test_that("near-overlapping peaks separate from nearly coincident starts", {
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
      if (!is.null(model$fixed_params)) {
        for (name in names(model$fixed_params)) {
          bounds$lower[[name]] <- model$fixed_params[[name]]
          bounds$upper[[name]] <- model$fixed_params[[name]]
        }
      }
      bounds
    },
    .package = "measure"
  )

  x <- seq(0, 10, by = 0.025)
  models <- list(gaussian_peak_model(), gaussian_peak_model())
  models[[1]]$fixed_params <- c(height = 1, width = 0.5)
  models[[2]]$fixed_params <- c(height = 0.8, width = 0.5)
  truth <- list(
    list(height = 1, center = 3, width = 0.5),
    list(height = 0.8, center = 5, width = 0.5)
  )
  y <- sum_peak_models(x, models, truth)
  init_params <- list(
    list(height = 1, center = 4, width = 0.5),
    list(height = 0.8, center = 4.001, width = 0.5)
  )

  result <- optimize_deconvolution(
    x,
    y,
    models,
    init_params,
    optimizer = "lbfgsb",
    max_iter = 10000L,
    tol = 1e-10,
    constrain_positions = TRUE
  )

  expect_true(result$convergence)
  expect_equal(result$parameters[[1]]$center, 3, tolerance = 1e-4)
  expect_equal(result$parameters[[2]]$center, 5, tolerance = 1e-4)
  expect_lt(result$final_value, 1e-8)
})

test_that("projected restarts escape tied centers before reporting convergence", {
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
      if (!is.null(model$fixed_params)) {
        for (name in names(model$fixed_params)) {
          bounds$lower[[name]] <- model$fixed_params[[name]]
          bounds$upper[[name]] <- model$fixed_params[[name]]
        }
      }
      bounds
    },
    .package = "measure"
  )

  x <- seq(0, 10, by = 0.025)
  models <- list(gaussian_peak_model(), gaussian_peak_model())
  models[[1]]$fixed_params <- c(height = 1, width = 0.4)
  models[[2]]$fixed_params <- c(height = 0.8, width = 1)
  truth <- list(
    list(height = 1, center = 3, width = 0.4),
    list(height = 0.8, center = 5, width = 1)
  )
  y <- sum_peak_models(x, models, truth)
  starts <- lapply(
    list(c(6.5, 6.501), c(6.5, 6.6)),
    function(centers) {
      list(
        list(height = 1, center = centers[[1]], width = 0.4),
        list(height = 0.8, center = centers[[2]], width = 1)
      )
    }
  )

  for (init_params in starts) {
    for (optimizer in c("lbfgsb", "multistart", "nelder_mead")) {
      result <- optimize_deconvolution(
        x,
        y,
        models,
        init_params,
        optimizer = optimizer,
        max_iter = 10000L,
        tol = 1e-10,
        n_starts = 1L,
        constrain_positions = TRUE
      )

      centers <- vapply(
        result$parameters,
        function(params) params$center,
        numeric(1)
      )
      expect_true(result$convergence, info = paste(optimizer, centers))
      expect_equal(centers, c(3, 5), tolerance = 1e-4)
      expect_lt(result$final_value, 1e-6)
      expect_equal(
        result$final_value,
        sum(result$residuals^2),
        tolerance = 1e-12
      )
    }
  }

  budget_limited <- optimize_deconvolution(
    x,
    y,
    models,
    starts[[1]],
    optimizer = "lbfgsb",
    max_iter = 10L,
    tol = 1e-10,
    constrain_positions = TRUE
  )
  expect_false(budget_limited$convergence)
  expect_lte(
    budget_limited$parameters[[1]]$center,
    budget_limited$parameters[[2]]$center
  )
})

test_that("constrained multi-peak fits can move both centers off axis bounds", {
  x <- seq(0, 10, by = 0.05)
  models <- list(gaussian_peak_model(), gaussian_peak_model())
  truth <- list(
    list(height = 1, center = 3, width = 1),
    list(height = 0.8, center = 7, width = 1)
  )
  y <- sum_peak_models(x, models, truth)
  init_params <- list(
    list(height = 1, center = 0, width = 2),
    list(height = 0.8, center = 10, width = 2)
  )

  result <- optimize_deconvolution(
    x,
    y,
    models,
    init_params,
    optimizer = "lbfgsb",
    max_iter = 10000L,
    tol = 1e-10,
    constrain_positions = TRUE
  )

  expect_true(result$convergence)
  expect_equal(result$parameters[[1]]$center, 3, tolerance = 1e-4)
  expect_equal(result$parameters[[2]]$center, 7, tolerance = 1e-4)
  expect_lt(result$final_value, 1e-8)
})

test_that("position constraints require one center per model", {
  x <- seq(0, 5, by = 0.1)
  model <- new_peak_model(
    name = "centerless",
    n_params = 1,
    param_names = "area"
  )

  expect_error(
    optimize_deconvolution(
      x,
      rep(0, length(x)),
      list(model),
      list(list(area = 1)),
      optimizer = "lbfgsb",
      constrain_positions = TRUE
    ),
    "exactly one center"
  )
})
