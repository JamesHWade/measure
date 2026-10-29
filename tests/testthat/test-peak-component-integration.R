test_that("fitted component metadata stays associated after reordering and serialization", {
  model_gaussian <- gaussian_peak_model()
  model_emg <- emg_peak_model()
  params_gaussian <- list(height = 2, center = 8, width = 1)
  params_emg <- list(height = 2, center = 11, width = 0.7, tau = 6)
  location <- seq(0, 30, by = 0.03)^1.02
  location <- location * 30 / max(location)
  location <- rev(location)
  value <- peak_model_value(model_gaussian, location, params_gaussian) +
    peak_model_value(model_emg, location, params_emg)
  measured <- new_measure_tbl(location = location, value = value)
  interval <- range(location)

  peaks <- new_peaks_tbl(
    peak_id = c(1L, 2L),
    location = c(
      params_gaussian$center,
      .fitted_emg_apex(model_emg, params_emg, location)$location
    ),
    height = c(
      params_gaussian$height,
      .fitted_emg_apex(model_emg, params_emg, location)$height
    ),
    left_base = rep(interval[1], 2),
    right_base = rep(interval[2], 2),
    area = rep(NA_real_, 2),
    fit_model = list(model_gaussian, model_emg),
    fit_params = list(params_gaussian, params_emg),
    detected_left_base = c(6, 9),
    detected_right_base = c(10, 16),
    area_basis = rep("fitted_component", 2)
  )
  peaks <- unserialize(serialize(peaks[c(2, 1), ], NULL))
  filtered_peaks <- dplyr::slice(peaks, 1)
  expect_equal(filtered_peaks$fit_params[[1]], params_emg)
  expect_s3_class(filtered_peaks$fit_model[[1]], "emg_peak_model")
  measures <- new_measure_list(list(measured))
  input <- tibble::tibble(
    .peaks = new_peaks_list(list(peaks)),
    .measures = measures
  )
  step <- step_measure_peaks_integrate_new(
    method = "trapezoid",
    baseline = "local",
    measures = ".measures",
    role = NA,
    trained = TRUE,
    skip = FALSE,
    id = "component-integration"
  )

  result <- bake.step_measure_peaks_integrate(step, input)
  result_peaks <- result$.peaks[[1]]
  expected <- c(
    peak_model_area(model_emg, params_emg, interval),
    peak_model_area(model_gaussian, params_gaussian, interval)
  )
  raw_area <- .integrate_peak(
    location,
    value,
    interval[1],
    interval[2],
    method = "trapezoid",
    baseline_type = "none"
  )

  expect_equal(result_peaks$peak_id, c(2L, 1L))
  expect_s3_class(result_peaks$fit_model[[1]], "emg_peak_model")
  expect_equal(result_peaks$fit_params[[1]], params_emg)
  expect_equal(result_peaks$area, expected, tolerance = 1e-8)
  expect_equal(
    result_peaks$area_basis,
    rep("fitted_component", 2)
  )
  expect_gt(raw_area, max(result_peaks$area))
})

test_that("fitted integration metadata is retained when sampled curves are disabled", {
  model <- emg_peak_model()
  params <- list(height = 1, center = 10, width = 1, tau = 5)
  location <- seq(0, 60, by = 0.1)
  value <- peak_model_value(model, location, params)
  data <- tibble::tibble(
    id = "sample",
    location = location,
    value = value
  )
  recipe <- recipes::recipe(~., data = data) |>
    recipes::update_role(id, new_role = "id") |>
    step_measure_input_long(value, location = vars(location)) |>
    step_measure_peaks_detect(min_height = 0.01) |>
    step_measure_peaks_deconvolve(
      model = "emg",
      max_iter = 3000L,
      tol = 1e-10,
      quality_threshold = 0,
      store_components = FALSE
    ) |>
    step_measure_peaks_integrate() |>
    recipes::prep()

  peaks <- recipes::bake(recipe, new_data = NULL)$.peaks[[1]]
  component_area <- peak_model_area(
    peaks$fit_model[[1]],
    peaks$fit_params[[1]],
    x_range = c(peaks$left_base[[1]], peaks$right_base[[1]])
  )

  expect_equal(nrow(peaks), 1L)
  expect_s3_class(peaks$fit_model[[1]], "emg_peak_model")
  expect_named(peaks$fit_params[[1]], c("height", "center", "width", "tau"))
  expect_true(all(
    c("detected_left_base", "detected_right_base") %in% names(peaks)
  ))
  expect_gte(peaks$location[[1]], peaks$left_base[[1]])
  expect_lte(peaks$location[[1]], peaks$right_base[[1]])
  expect_equal(peaks$area[[1]], component_area, tolerance = 1e-10)
  expect_equal(peaks$area_basis[[1]], "fitted_component")
})

test_that("fit integration windows contain long-tail EMG modes within the measured domain", {
  model <- emg_peak_model()
  params <- list(height = 1, center = 10, width = 1, tau = 10000)
  measured_domain <- seq(0, 400, by = 1)
  apex <- .fitted_emg_apex(model, params, measured_domain)
  window <- .fitted_component_window(
    detected_left_base = 8,
    detected_right_base = 12,
    apex_location = apex$location,
    observed_range = range(measured_domain)
  )

  expect_gt(apex$location, params$center + 3 * params$width)
  expect_gte(window[1], min(measured_domain))
  expect_lte(window[2], max(measured_domain))
  expect_lte(window[1], apex$location)
  expect_gte(window[2], apex$location)
  expect_equal(window[1], 8)
})

test_that("fitted FWHM uses the stored component without a signal baseline", {
  model <- gaussian_peak_model()
  params <- list(height = 2, center = 5, width = 1)
  location <- seq(0, 10, by = 0.4)
  value <- peak_model_value(model, location, params) + 0.3
  peaks <- new_peaks_tbl(
    peak_id = 1L,
    location = params$center,
    height = params$height,
    left_base = 0,
    right_base = 10,
    area = NA_real_,
    fit_model = list(model),
    fit_params = list(params),
    detected_left_base = 4,
    detected_right_base = 6
  )

  properties <- .add_peak_properties(
    peaks,
    new_measure_tbl(location = location, value = value),
    properties = c("prominence", "fwhm")
  )

  expect_equal(
    properties$fwhm,
    2 * sqrt(2 * log(2)) * params$width,
    tolerance = 1e-8
  )
  observed_at_peak_and_bases <- stats::approx(
    location,
    value,
    xout = c(params$center, 4, 6)
  )$y
  expect_equal(
    properties$prominence,
    observed_at_peak_and_bases[1] - max(observed_at_peak_and_bases[-1]),
    tolerance = 1e-8
  )
})

test_that("observed integration handles descending irregular axes", {
  location <- c(4, 2.5, 1, 0)
  value <- 4 - location

  trapezoid <- .integrate_peak(
    location,
    value,
    left_base = 4,
    right_base = 0,
    method = "trapezoid",
    baseline_type = "none"
  )
  simpson <- .integrate_peak(
    location,
    value,
    left_base = 4,
    right_base = 0,
    method = "simpson",
    baseline_type = "none"
  )

  expect_equal(trapezoid, 8)
  expect_equal(simpson, trapezoid)
})

test_that("zero fitted components integrate to zero", {
  peaks <- new_peaks_tbl(
    peak_id = 1L,
    location = 0.5,
    height = 0,
    left_base = 0,
    right_base = 1,
    area = NA_real_,
    fit_model = list(gaussian_peak_model()),
    fit_params = list(list(height = 0, center = 0.5, width = 1))
  )

  expect_equal(.integrate_fitted_peak(peaks, 1), 0)
})

test_that("unsupported custom fitted models report an integration diagnostic", {
  peaks <- new_peaks_tbl(
    peak_id = 7L,
    location = 0.5,
    height = 1,
    left_base = 0,
    right_base = 1,
    area = NA_real_,
    fit_model = list(new_peak_model("unsupported", 1, "height")),
    fit_params = list(list(height = 1))
  )
  error <- tryCatch(
    .integrate_fitted_peak(peaks, 1),
    error = function(error) error
  )

  expect_s3_class(error, "rlang_error")
  expect_match(
    conditionMessage(error),
    "Unable to integrate fitted component for peak 7"
  )
  expect_match(conditionMessage(error), "unsupported")
})

test_that("finite EMG integration is stable across width-to-tail ratios", {
  model <- emg_peak_model()

  for (ratio in c(1e-4, 1e4)) {
    params <- list(height = 1.2, center = 0, width = 1, tau = ratio)
    x_range <- c(-10, 12 * max(params$width, params$tau))
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
        rel.tol = 1e-10,
        abs.tol = 0
      )$value

    expect_equal(
      peak_model_area(model, params, x_range = x_range),
      expected,
      tolerance = 1e-8
    )
  }
})

test_that("large-offset narrow EMG fits produce finite component areas", {
  center <- 1e8 + 0.2
  params <- list(height = 2, center = center, width = 0.015, tau = 0.025)
  location <- center + seq(-200, 600, by = 1) * 0.001
  value <- peak_model_value(emg_peak_model(), location, params)
  data <- tibble::tibble(id = "offset", location = location, value = value)
  recipe <- recipes::recipe(~., data = data) |>
    recipes::update_role(id, new_role = "id") |>
    step_measure_input_long(value, location = vars(location)) |>
    step_measure_peaks_detect(min_height = 0.01) |>
    step_measure_peaks_deconvolve(
      model = "emg",
      max_iter = 3000L,
      tol = 1e-10,
      quality_threshold = 0,
      store_components = FALSE
    ) |>
    step_measure_peaks_integrate() |>
    recipes::prep()
  peaks <- recipes::bake(recipe, new_data = NULL)$.peaks[[1]]

  expect_equal(nrow(peaks), 1L)
  expect_s3_class(peaks$fit_model[[1]], "emg_peak_model")
  expect_true(all(is.finite(unlist(peaks$fit_params[[1]], use.names = FALSE))))
  expect_true(is.finite(peaks$area[[1]]))
  expect_gt(peaks$area[[1]], 0)
  expect_equal(peaks$area_basis[[1]], "fitted_component")
})
