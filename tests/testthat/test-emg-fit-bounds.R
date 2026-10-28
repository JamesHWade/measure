test_that("EMG bounds apply to precursor height, not observed baseline", {
  model <- emg_peak_model()
  bounds <- peak_model_bounds(
    model,
    x_range = c(0, 20),
    y_range = c(3, 10)
  )

  expect_equal(bounds$lower[["height"]], 0)
  expect_true(is.infinite(bounds$upper[["height"]]))
  expect_equal(bounds$upper[["height"]], Inf)
})

test_that("EMG model retains Gaussian limit and precursor Gaussian area", {
  model <- emg_peak_model()
  params <- list(height = 1, center = 10, width = 1, tau = 10)
  x <- seq(0, 120, by = 0.25)

  expected_area <- params$height * params$width * sqrt(2 * pi)
  expect_equal(
    peak_model_area(model, params, x_range = range(x)),
    expected_area,
    tolerance = 1e-4
  )

  gaussian_params <- params
  gaussian_params$tau <- 0
  gaussian <- params$height *
    exp(-0.5 * ((x - params$center) / params$width)^2)

  expect_equal(peak_model_value(model, x, gaussian_params), gaussian)
  expect_equal(
    peak_model_area(model, gaussian_params, x_range = c(0, 20)),
    expected_area,
    tolerance = 1e-4
  )
})

test_that("EMG deconvolution fits a long-tailed precursor above its apex", {
  model <- emg_peak_model()
  x <- seq(0, 120, by = 0.25)

  for (tau in c(5, 10)) {
    truth <- list(height = 1, center = 10, width = 1, tau = tau)
    y <- peak_model_value(model, x, truth)
    peak_idx <- which.max(y)
    init_params <- list(peak_model_initial_guess(model, x, y, peak_idx))
    bounds <- peak_model_bounds(model, range(x), range(y))
    expected_area <- truth$height * truth$width * sqrt(2 * pi)

    for (optimizer in c("lbfgsb", "multistart", "nelder_mead")) {
      result <- withr::with_seed(
        42,
        optimize_deconvolution(
          x,
          y,
          list(model),
          init_params,
          optimizer = optimizer,
          tol = 1e-8,
          max_iter = 5000L,
          n_starts = 3L
        )
      )

      params <- result$parameters[[1]]
      residuals <- y - result$fitted_values
      r_squared <- 1 - sum(residuals^2) / sum((y - mean(y))^2)
      fitted_area <- peak_model_area(model, params, x_range = range(x))
      info <- paste(optimizer, "tau", tau)

      expect_true(result$convergence, info = info)
      expect_true(r_squared > 0.99999, info = info)
      expect_true(sqrt(mean(residuals^2)) < 1e-4, info = info)
      expect_true(params$height > max(y), info = info)
      expect_equal(fitted_area, expected_area, tolerance = 5e-4)

      for (parameter in names(params)) {
        expect_true(
          params[[parameter]] >= bounds$lower[[parameter]],
          info = paste(info, parameter)
        )
        expect_true(
          params[[parameter]] <= bounds$upper[[parameter]],
          info = paste(info, parameter)
        )
      }
    }
  }
})
