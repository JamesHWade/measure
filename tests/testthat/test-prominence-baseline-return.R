baseline_return_trace <- function(slope = 0, descending = FALSE) {
  x <- seq(0, 12, by = 0.01)
  if (descending) {
    x <- rev(x)
  }
  gaussian <- function(center, sigma, height) {
    height * exp(-0.5 * ((x - center) / sigma)^2)
  }
  signal <- 0.4 +
    slope * x +
    gaussian(3.2, 0.08, 80) +
    gaussian(4.2, 0.06, 3) +
    gaussian(7.5, 0.12, 35)
  noise <- 0.008 * sin(13 * x) + 0.004 * cos(31 * x)
  list(
    data = tibble::tibble(id = "sample", location = x, value = signal + noise),
    noiseless = signal,
    areas = c(80 * 0.08, 35 * 0.12) * sqrt(2 * pi)
  )
}

baseline_return_peaks <- function(trace, params = list(), baseline = "local") {
  rec <- recipes::recipe(~., data = trace$data) |>
    recipes::update_role(id, new_role = "id") |>
    step_measure_input_long(value, location = dplyr::vars(location)) |>
    step_measure_peaks_detect(min_prominence = 5, algorithm_params = params) |>
    step_measure_peaks_integrate(method = "trapezoid", baseline = baseline) |>
    step_measure_peaks_properties("prominence")
  recipes::bake(recipes::prep(rec), new_data = NULL)$.peaks[[1]]
}

return_params <- list(boundary_baseline = 0.4, boundary_tolerance = 0.05)

test_that("sampled returns exclude a filtered neighbor and preserve its neighbors' contours", {
  trace <- baseline_return_trace()
  default <- baseline_return_peaks(trace)
  bounded <- baseline_return_peaks(trace, return_params)

  expect_equal(bounded$location, c(3.2, 7.5))
  expect_named(bounded, names(default))
  preserved <- c(
    "peak_id",
    "location",
    "height",
    "prominence",
    "prominence_left_base",
    "prominence_right_base"
  )
  expect_identical(bounded[, preserved], default[, preserved])

  # At 4.2 the small peak is absent from both peak tables, but lies inside
  # the default integration window for the peak at 7.5.
  expect_lt(default$left_base[2], 4.2)
  expect_lt(bounded$right_base[1], 4.2)
  expect_gt(bounded$left_base[2], 4.2)
  expect_true(all(bounded$left_base >= default$left_base))
  expect_true(all(bounded$right_base <= default$right_base))

  x <- trace$data$location
  y <- trace$data$value
  for (i in seq_len(nrow(bounded))) {
    left <- match(bounded$left_base[i], x)
    right <- match(bounded$right_base[i], x)
    expect_true(all(y[c(left, right)] <= 0.45))
    expect_true(all(y[(left + 1L):(right - 1L)] > 0.45))
  }
})

test_that("raw area includes the known background inside the sampled window", {
  trace <- baseline_return_trace()
  peaks <- baseline_return_peaks(trace, return_params, baseline = "none")

  for (i in seq_len(nrow(peaks))) {
    bounds <- c(peaks$left_base[i], peaks$right_base[i])
    in_window <- trace$data$location >= bounds[1] &
      trace$data$location <= bounds[2]
    dx <- diff(trace$data$location[in_window])
    integrate <- function(values) {
      y <- values[in_window]
      sum(dx * (y[-1L] + y[-length(y)]) / 2)
    }
    expect_equal(peaks$area[i], integrate(trace$data$value), tolerance = 1e-12)
    # Noise is bounded by 0.012 signal units at every point.
    expect_lt(
      abs(peaks$area[i] - integrate(trace$noiseless)),
      0.012 * diff(bounds) + 1e-8
    )
    expect_gt(peaks$area[i], trace$areas[i])
  }
})

test_that("local areas recover Gaussian areas on flat and sloping backgrounds", {
  for (slope in c(0, 0.02)) {
    trace <- baseline_return_trace(slope)
    params <- list(
      boundary_baseline = function(x) 0.4 + slope * x,
      boundary_tolerance = 0.05
    )
    bounded <- baseline_return_peaks(trace, params)
    expect_equal(bounded$location, c(3.2, 7.5))
    expect_lt(max(abs(bounded$area / trace$areas - 1)), 0.01)
  }
})

test_that("reversing the acquisition axis reverses bounds and preserves area", {
  ascending <- baseline_return_peaks(baseline_return_trace(), return_params)
  descending <- baseline_return_peaks(
    baseline_return_trace(descending = TRUE),
    return_params
  )
  descending <- descending[match(ascending$location, descending$location), ]
  expect_equal(descending$left_base, ascending$right_base)
  expect_equal(descending$right_base, ascending$left_base)
  expect_equal(descending$area, ascending$area)
  expect_equal(descending$prominence, ascending$prominence)
})

test_that("an unresolved return cannot borrow a sample beyond a shared valley", {
  detect <- function(y, baseline, tolerance) {
    .detect_peaks_prominence(
      seq_along(y),
      y,
      boundary_baseline = baseline,
      boundary_tolerance = tolerance
    )
  }
  expect_error(detect(c(0, 3, 1, 4, 0), 0, 0.5), "no sampled return")
  expect_error(detect(c(1, 3, 1), 3, 0.1), "does not rise above")
  expect_error(detect(c(0, 3, 0), 0, 0), "positive")
  expect_error(
    detect(c(0, 3, 0), function(x) c(1, 1), 0.5),
    "one finite value per location"
  )
  expect_error(detect(c(0, 3, NA_real_), 0, 0.5), "finite signal")
  expect_error(
    .detect_peaks_prominence(
      c(1, 2, 2),
      c(0, 3, 0),
      boundary_baseline = 0,
      boundary_tolerance = 0.5
    ),
    "strictly monotonic"
  )
})
