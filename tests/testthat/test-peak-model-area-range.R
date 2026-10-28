test_that("finite Gaussian areas are integrated over the requested interval", {
  model <- gaussian_peak_model()
  params <- list(height = 2, center = 0.5, width = 1.2)
  probability <- stats::pnorm(1) - stats::pnorm(-1)
  expected <- params$height * params$width * sqrt(2 * pi) * probability

  expect_equal(
    peak_model_area(model, params, x_range = c(-0.7, 1.7)),
    expected,
    tolerance = 1e-12
  )
  expect_equal(
    peak_model_area(model, params, x_range = c(1.7, -0.7)),
    expected,
    tolerance = 1e-12
  )
  expect_equal(
    peak_model_area(model, params),
    params$height * params$width * sqrt(2 * pi),
    tolerance = 1e-12
  )
})

test_that("finite bi-Gaussian areas use the width on each side", {
  model <- bigaussian_peak_model()
  params <- list(
    height = 2,
    center = 0,
    width_left = 1,
    width_right = 2
  )
  expected <- params$height *
    sqrt(2 * pi) *
    (params$width_left *
      (stats::pnorm(0) - stats::pnorm(-1)) +
      params$width_right * (stats::pnorm(1) - stats::pnorm(0)))

  expect_equal(
    peak_model_area(model, params, x_range = c(-1, 2)),
    expected,
    tolerance = 1e-12
  )
  expect_equal(
    peak_model_area(model, params),
    params$height * sqrt(pi / 2) * (params$width_left + params$width_right),
    tolerance = 1e-12
  )
})

test_that("finite Lorentzian areas are integrated over the requested interval", {
  model <- lorentzian_peak_model()
  params <- list(height = 2, center = 0, gamma = 1)
  expected <- params$height * params$gamma * (atan(2) - atan(-1))

  expect_equal(
    peak_model_area(model, params, x_range = c(-1, 2)),
    expected,
    tolerance = 1e-12
  )
  expect_equal(
    peak_model_area(model, params, x_range = c(2, -1)),
    expected,
    tolerance = 1e-12
  )
  expect_equal(
    peak_model_area(model, params),
    params$height * params$gamma * pi,
    tolerance = 1e-12
  )
})

test_that("finite EMG areas match an independent convolution integral", {
  model <- emg_peak_model()
  params <- list(height = 1.7, center = -0.2, width = 0.7, tau = 1.2)
  x_range <- c(-1.5, 2.3)

  expected <- params$height *
    params$width *
    sqrt(2 * pi) *
    stats::integrate(
      function(v) {
        stats::dexp(v) *
          (stats::pnorm(
            (x_range[2] - params$center - params$tau * v) / params$width
          ) -
            stats::pnorm(
              (x_range[1] - params$center - params$tau * v) / params$width
            ))
      },
      lower = 0,
      upper = Inf,
      rel.tol = 1e-11,
      abs.tol = 0
    )$value

  expect_equal(
    peak_model_area(model, params, x_range = x_range),
    expected,
    tolerance = 1e-8
  )
})

test_that("finite EMG areas resolve narrow peaks on wide intervals", {
  model <- emg_peak_model()
  params <- list(height = 3, center = 0, width = 1e-5, tau = 2e-5)

  expect_equal(
    peak_model_area(model, params, x_range = c(-0.01, 0.01)),
    peak_model_area(model, params),
    tolerance = 1e-8
  )
})

test_that("finite EMG areas scale with width and tail time", {
  model <- emg_peak_model()

  areas <- vapply(
    c(1e-6, 1, 1e6),
    function(scale) {
      params <- list(
        height = 1.3,
        center = 0,
        width = scale,
        tau = 2 * scale
      )
      x_range <- c(-2 * scale, 4 * scale)
      expected <- params$height *
        params$width *
        sqrt(2 * pi) *
        stats::integrate(
          function(v) {
            stats::dexp(v) *
              (stats::pnorm(
                (x_range[2] - params$tau * v) / params$width
              ) -
                stats::pnorm(
                  (x_range[1] - params$tau * v) / params$width
                ))
          },
          lower = 0,
          upper = Inf,
          rel.tol = 1e-11,
          abs.tol = 0
        )$value

      actual <- peak_model_area(model, params, x_range = x_range)
      expect_equal(actual, expected, tolerance = 1e-8)
      actual / scale
    },
    numeric(1)
  )

  expect_equal(areas[1], areas[2], tolerance = 1e-8)
  expect_equal(areas[2], areas[3], tolerance = 1e-8)
})

test_that("zero-height built-in models have zero finite interval area", {
  expect_equal(
    peak_model_area(
      gaussian_peak_model(),
      list(height = 0, center = 0, width = 1),
      x_range = c(-1, 1)
    ),
    0
  )
  expect_equal(
    peak_model_area(
      emg_peak_model(),
      list(height = 0, center = 0, width = 1, tau = 3),
      x_range = c(-1, 1)
    ),
    0
  )
})

test_that("finite EMG areas stay within intervals outside the component center", {
  model <- emg_peak_model()
  params <- list(height = 1.2, center = 0, width = 0.8, tau = 2)

  for (x_range in list(c(-5, -2), c(4, 9))) {
    expected <- params$height *
      params$width *
      sqrt(2 * pi) *
      stats::integrate(
        function(v) {
          stats::dexp(v) *
            (stats::pnorm(
              (x_range[2] - params$center - params$tau * v) / params$width
            ) -
              stats::pnorm(
                (x_range[1] - params$center - params$tau * v) / params$width
              ))
        },
        lower = 0,
        upper = Inf,
        rel.tol = 1e-10,
        abs.tol = 0
      )$value

    expect_equal(
      peak_model_area(model, params, x_range = x_range),
      expected,
      tolerance = 1e-9
    )
  }
})
