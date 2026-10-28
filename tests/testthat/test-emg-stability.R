test_that("EMG predictions match a Gaussian convolved with an exponential", {
  model <- emg_peak_model()
  params <- list(height = 1.3, center = 0.4, width = 0.8, tau = 0.4)
  x <- seq(-1.2, 6, length.out = 33)

  predicted <- peak_model_value(model, x, params)
  convolved <- vapply(
    x,
    function(at) {
      stats::integrate(
        function(u) {
          gaussian <- params$height *
            exp(-0.5 * ((u - params$center) / params$width)^2)
          decay <- stats::dexp(at - u, rate = 1 / params$tau)
          gaussian * decay
        },
        lower = -Inf,
        upper = at,
        rel.tol = 1e-10
      )$value
    },
    numeric(1)
  )

  expect_equal(predicted, convolved, tolerance = 1e-8)
})

test_that("EMG predictions approach the precursor Gaussian as tau shrinks", {
  model <- emg_peak_model()
  params <- list(height = 1.3, center = 0.4, width = 0.8, tau = 1e-4)
  x <- seq(-2, 3, length.out = 101)
  gaussian <- params$height *
    exp(-0.5 * ((x - params$center) / params$width)^2)

  expect_equal(peak_model_value(model, x, params), gaussian, tolerance = 1e-3)
})

test_that("EMG area preserves the precursor Gaussian area", {
  model <- emg_peak_model()
  params <- list(height = 1.3, center = 0.4, width = 0.8, tau = 0.4)
  x <- seq(
    params$center - 8 * params$width,
    params$center + 30 * params$tau,
    by = 0.005
  )
  y <- peak_model_value(model, x, params)
  area <- sum(diff(x) * (head(y, -1) + tail(y, -1)) / 2)

  expect_equal(
    area,
    params$height * params$width * sqrt(2 * pi),
    tolerance = 1e-5
  )
})

test_that("EMG evaluation preserves finite axis differences that overflow", {
  model <- emg_peak_model()
  params <- list(
    height = 1,
    center = -1e308,
    width = 1e308,
    tau = 1e308
  )
  x <- 1e308
  z <- 2
  k <- params$width / params$tau
  reference <- stats::integrate(
    function(v) {
      exp(-0.5 * v^2) * stats::dexp(z - v, rate = k)
    },
    lower = -Inf,
    upper = z,
    rel.tol = 1e-10
  )$value

  expect_equal(peak_model_value(model, x, params), reference, tolerance = 1e-8)
})

test_that("EMG retains representable right tails after z overflows", {
  model <- emg_peak_model()
  params <- list(height = 1e300, center = 0, width = 1e-100, tau = 1e250)
  x <- c(1e250, 1.1e250)
  convolution_integral <- stats::integrate(
    function(u) {
      exp(-0.5 * u^2 + (params$width / params$tau) * u)
    },
    lower = -10,
    upper = 10,
    rel.tol = 1e-10
  )$value
  reference <- params$height *
    params$width /
    params$tau *
    exp(-(x - params$center) / params$tau) *
    convolution_integral
  predicted <- peak_model_value(model, x, params)

  expect_true(all(predicted > 0))
  expect_equal(predicted, reference, tolerance = 1e-10)
})

test_that("Nelder-Mead returns its bounded fit and matching objective", {
  withr::local_seed(42)
  x <- seq(8, 20, by = 0.05)
  y <- 100 *
    stats::dnorm(x, mean = 12, sd = 1) +
    60 * stats::dnorm(x, mean = 14, sd = 1.2) +
    rnorm(length(x), sd = 0.3)
  models <- list(emg_peak_model(), emg_peak_model())
  peak_locations <- c(12, 14)
  peak_indices <- vapply(
    peak_locations,
    function(location) {
      which.min(abs(x - location))
    },
    integer(1)
  )
  init_params <- initialize_peak_params(
    x,
    y,
    n_peaks = length(models),
    models = models,
    peak_indices = peak_indices
  )

  result <- optimize_deconvolution(
    x,
    y,
    models,
    init_params,
    optimizer = "nelder_mead",
    max_iter = 500L
  )
  bounds <- peak_model_bounds(models[[1]], range(x), range(y))
  expected_fitted <- sum_peak_models(x, models, result$parameters)

  for (params in result$parameters) {
    expect_gte(params$height, bounds$lower[["height"]])
    expect_gte(params$center, bounds$lower[["center"]])
    expect_gte(params$width, bounds$lower[["width"]])
    expect_gte(params$tau, bounds$lower[["tau"]])
    expect_lte(params$height, bounds$upper[["height"]])
    expect_lte(params$center, bounds$upper[["center"]])
    expect_lte(params$width, bounds$upper[["width"]])
    expect_lte(params$tau, bounds$upper[["tau"]])
  }
  expect_true(all(is.finite(result$fitted_values)))
  expect_equal(result$fitted_values, expected_fitted)
  expect_equal(result$residuals, y - expected_fitted)
  expect_equal(result$final_value, sum((y - expected_fitted)^2))
})

test_that("quality assessment rejects non-finite component predictions", {
  x <- seq(-1, 1, length.out = 21)
  y <- rep(0, length(x))
  model <- emg_peak_model()
  result <- list(
    fitted_values = y,
    residuals = y,
    parameters = list(list(height = 1, center = 0, width = 0, tau = 0.5)),
    convergence = TRUE,
    n_iterations = 1L,
    final_value = 0,
    optimizer = "lbfgsb",
    elapsed_time = 0
  )

  expect_error(
    assess_deconv_quality(x, y, result, list(model)),
    "non-finite predictions"
  )
})
