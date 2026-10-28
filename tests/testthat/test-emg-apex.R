test_that("fitted EMG properties report the component apex", {
  model <- emg_peak_model()
  params <- list(height = 1, center = 10, width = 1, tau = 5)
  x <- seq(0, 50, by = 0.4)
  apex <- .fitted_emg_apex(model, params, x)

  # The derivative of the independent EMG density is zero when the inverse
  # normal Mills ratio equals width / tau.
  k <- params$width / params$tau
  z <- stats::uniroot(
    function(z) stats::dnorm(z - k) / stats::pnorm(z - k) - k,
    c(0, 5),
    tol = 1e-12
  )$root
  expected_location <- params$center + params$width * z

  expect_equal(apex$location, expected_location, tolerance = 1e-7)
  expect_equal(
    apex$height,
    peak_model_value(model, expected_location, params),
    tolerance = 1e-10
  )
  expect_gt(apex$location, params$center)
  expect_lt(apex$height, params$height)
  expect_equal(.fitted_emg_apex(model, params, rev(x)), apex)
})

test_that("fitted EMG apex stays within the measured domain", {
  model <- emg_peak_model()
  params <- list(height = 1, center = 10, width = 1, tau = 5)
  x <- seq(20, 30, by = 0.4)
  apex <- .fitted_emg_apex(model, params, x)

  expect_equal(apex$location, min(x))
  expect_equal(apex$height, peak_model_value(model, min(x), params))
  expect_equal(.fitted_emg_apex(model, params, 20), apex)

  params$tau <- 0
  apex <- .fitted_emg_apex(model, params, seq(0, 20, by = 0.4))
  expect_equal(apex$location, params$center, tolerance = 1e-7)
  expect_equal(apex$height, params$height, tolerance = 1e-10)
})

test_that("EMG recipe outputs keep actual fitted apex properties", {
  model <- emg_peak_model()
  params <- list(height = 1, center = 10, width = 1, tau = 5)
  x <- seq(0, 60, by = 0.1)
  data <- tibble::tibble(
    id = "sample",
    location = x,
    value = peak_model_value(model, x, params)
  )
  rec <- recipes::recipe(~., data = data) |>
    recipes::update_role(id, new_role = "id") |>
    step_measure_input_long(value, location = vars(location)) |>
    step_measure_peaks_detect(min_height = 0.01) |>
    step_measure_peaks_deconvolve(
      model = "emg",
      max_iter = 3000L,
      tol = 1e-10,
      quality_threshold = 0,
      store_components = TRUE
    ) |>
    recipes::prep()
  result <- recipes::bake(rec, new_data = NULL)$.peaks[[1]]
  expected_apex <- .fitted_emg_apex(model, params, x)

  expect_equal(nrow(result), 1L)
  expect_gt(result$fit_r_squared, 0.9999)
  expect_equal(result$location, expected_apex$location, tolerance = 0.01)
  expect_equal(result$height, expected_apex$height, tolerance = 0.01)
  expect_gt(result$location, params$center + 0.5)
  expect_lt(result$height, params$height / 2)
  component <- attr(result, "fitted_components")[[1]]
  expect_gte(result$height + 1e-10, max(component))
})

test_that("continuous EMG apex does not depend on axis sampling gaps", {
  model <- emg_peak_model()
  params <- list(height = 1, center = 10, width = 1, tau = 5)
  dense <- .fitted_emg_apex(model, params, seq(0, 50, by = 0.1))
  sparse <- .fitted_emg_apex(model, params, c(0, 10, 1e6))

  expect_equal(sparse, dense)
  expect_gt(sparse$location, 11)
  expect_gt(sparse$height, 0.3)
})

test_that("EMG mode retains physical shifts near the Gaussian limit", {
  params <- list(height = 1, center = 0, width = 1e20, tau = 1e5)
  # The exponential displacement remains 1e5 in physical units even though
  # its standardized displacement 1e-15 is below an ordinary root tolerance.
  expect_equal(.emg_peak_mode(params), params$tau, tolerance = 1e-12)

  params$width <- 1e300
  params$tau <- 1e-300
  expect_equal(.emg_peak_mode(params), params$center)
})

test_that("EMG mode solves the independent density derivative across regimes", {
  for (k in c(1e-3, 1, 20.99, 21.01)) {
    params <- list(height = 1, center = 10, width = 2, tau = 2 / k)
    z <- (.emg_peak_mode(params) - params$center) / params$width
    inverse_mills <- exp(
      stats::dnorm(z - k, log = TRUE) - stats::pnorm(z - k, log.p = TRUE)
    )
    expect_equal(inverse_mills, k, tolerance = 1e-10)
    expect_gt(z, 0)
  }
})

test_that("near-Gaussian mode bracketing survives endpoint roundoff", {
  for (k in c(1.1e8, 1.2e8, 1.3e8)) {
    params <- list(height = 1, center = 0, width = 1, tau = 1 / k)
    expect_equal(.emg_peak_mode(params), params$tau, tolerance = 1e-12)
  }
})
