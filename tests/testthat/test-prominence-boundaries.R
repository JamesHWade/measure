prominence_bounds_trace <- function(floor = c("rising", "falling")) {
  floor <- match.arg(floor)
  location <- seq(0, 12, by = 0.01)
  gaussian <- function(center, sigma, height) {
    height * exp(-0.5 * ((location - center) / sigma)^2)
  }
  baseline <- if (floor == "rising") {
    0.02 * location
  } else {
    0.02 * (12 - location)
  }

  list(
    location = location,
    value = gaussian(3, 0.1, 80) +
      gaussian(6, 0.1, 5) +
      gaussian(9, 0.1, 35) +
      baseline,
    baseline = baseline
  )
}

prominence_bounds_recipe <- function(
  location,
  value,
  algorithm = "prominence",
  baseline = NULL,
  properties = NULL,
  min_prominence = 1
) {
  data <- tibble::tibble(
    id = 1L,
    location = location,
    value = value
  )
  rec <- recipes::recipe(~., data = data) |>
    recipes::update_role(id, new_role = "id") |>
    step_measure_input_long(value, location = dplyr::vars(location)) |>
    step_measure_peaks_detect(
      algorithm = algorithm,
      min_height = 0.8,
      min_distance = 0.05,
      min_prominence = min_prominence
    )

  if (!is.null(properties)) {
    rec <- rec |> step_measure_peaks_properties(properties)
  }
  if (!is.null(baseline)) {
    rec <- rec |>
      step_measure_peaks_integrate(
        method = "trapezoid",
        baseline = baseline
      )
  }

  rec
}

trapezoid_area <- function(location, value) {
  sum(diff(location) * (value[-1] + value[-length(value)]) / 2)
}

test_that("prominence bounds separate adjacent peaks in sample order", {
  trace <- prominence_bounds_trace("rising")
  cases <- list(
    list(
      location = trace$location,
      min_distance = 0,
      expected = c(3, 6, 9)
    ),
    list(
      location = rev(trace$location),
      min_distance = 0,
      expected = c(9, 6, 3)
    ),
    list(
      location = rev(trace$location),
      min_distance = 0.05,
      expected = c(3, 6, 9)
    )
  )

  for (case in cases) {
    location <- case$location
    peaks <- .detect_peaks_prominence(
      location,
      trace$value,
      min_prominence = 1,
      min_distance = case$min_distance
    )
    apex_index <- match(peaks$location, location)
    left_index <- match(peaks$left_base, location)
    right_index <- match(peaks$right_base, location)
    by_sample <- order(apex_index)

    expect_equal(peaks$peak_id, 1:3)
    expect_equal(peaks$location, case$expected)
    expect_equal(peaks$height, trace$value[apex_index])
    expect_lte(max(left_index - apex_index), 0L)
    expect_lte(max(apex_index - right_index), 0L)
    expect_lte(
      max(
        right_index[by_sample][-nrow(peaks)] -
          left_index[by_sample][-1]
      ),
      0L
    )
    expect_named(
      peaks,
      c(
        "peak_id",
        "location",
        "height",
        "left_base",
        "right_base",
        "area",
        "prominence_left_base",
        "prominence_right_base"
      )
    )
  }
})

test_that("prominence bounds resolve tied valleys at their first sample", {
  value <- c(0, 5, 1, 1, 4, 0)
  clipped <- .clip_peak_bases_to_valleys(
    value,
    peaks_idx = c(2L, 5L),
    left_bases = c(1L, 3L),
    right_bases = c(3L, 6L)
  )

  expect_equal(clipped$right, c(3L, 6L))
  expect_equal(clipped$left, c(1L, 3L))

  narrow <- .clip_peak_bases_to_valleys(
    value,
    peaks_idx = c(2L, 5L),
    left_bases = c(1L, 4L),
    right_bases = c(2L, 6L)
  )
  expect_equal(narrow$right, c(2L, 6L))
  expect_equal(narrow$left, c(1L, 4L))
})

test_that("prominence clipping handles empty and single-peak results", {
  location <- 0:10
  empty <- .detect_peaks_prominence(location, rep(0, length(location)))
  single <- .detect_peaks_prominence(
    location,
    exp(-0.5 * ((location - 5) / 1)^2)
  )

  expect_equal(nrow(empty), 0L)
  expect_equal(nrow(single), 1L)
  expect_equal(single$left_base, single$prominence_left_base)
  expect_equal(single$right_base, single$prominence_right_base)
})

test_that("local-baseline areas match the three Gaussian areas for all detectors", {
  expected <- c(80, 5, 35) * 0.1 * sqrt(2 * pi)

  for (floor in c("rising", "falling")) {
    trace <- prominence_bounds_trace(floor)
    for (algorithm in c("prominence", "derivative", "local_maxima")) {
      peaks <- recipes::bake(
        recipes::prep(prominence_bounds_recipe(
          trace$location,
          trace$value,
          algorithm = algorithm,
          baseline = "local"
        )),
        new_data = NULL
      )$.peaks[[1]]

      expect_equal(peaks$location, c(3, 6, 9))
      expect_lte(max(abs(peaks$area - expected)), 0.001)
      if (algorithm == "prominence") {
        expect_named(
          peaks,
          c(
            "peak_id",
            "location",
            "height",
            "left_base",
            "right_base",
            "area",
            "prominence_left_base",
            "prominence_right_base",
            "area_basis"
          )
        )
      } else {
        expect_named(
          peaks,
          c(
            "peak_id",
            "location",
            "height",
            "left_base",
            "right_base",
            "area",
            "area_basis"
          )
        )
      }
    }
  }
})

test_that("none baseline includes the falling floor within each window", {
  trace <- prominence_bounds_trace("falling")
  peaks <- recipes::bake(
    recipes::prep(prominence_bounds_recipe(
      trace$location,
      trace$value,
      baseline = "none"
    )),
    new_data = NULL
  )$.peaks[[1]]

  for (i in seq_len(nrow(peaks))) {
    bounds <- sort(c(peaks$left_base[i], peaks$right_base[i]))
    index <- which(
      trace$location >= bounds[1] & trace$location <= bounds[2]
    )
    expected <- trapezoid_area(
      trace$location[index],
      trace$value[index]
    )
    peak_only <- trapezoid_area(
      trace$location[index],
      trace$value[index] - trace$baseline[index]
    )

    expect_equal(peaks$area[i], expected)
    expect_gt(peaks$area[i], peak_only)
  }
})

test_that("prominence properties use contours after recipe serialization", {
  trace <- prominence_bounds_trace("rising")
  after_apex <- trace$location > 9
  trace$value[after_apex] <- trace$value[after_apex] -
    0.12 * (trace$location[after_apex] - 9)
  trained <- recipes::prep(prominence_bounds_recipe(
    trace$location,
    trace$value,
    properties = "prominence"
  ))
  trained <- unserialize(serialize(trained, NULL))
  peaks <- recipes::bake(trained, new_data = NULL)$.peaks[[1]]
  contour_signal <- stats::approx(
    trace$location,
    trace$value,
    xout = c(
      peaks$location[3],
      peaks$prominence_left_base[3],
      peaks$prominence_right_base[3]
    ),
    rule = 2
  )$y
  clipped_signal <- stats::approx(
    trace$location,
    trace$value,
    xout = c(peaks$location[3], peaks$left_base[3], peaks$right_base[3]),
    rule = 2
  )$y

  expect_lt(peaks$prominence_left_base[3], peaks$location[2])
  expect_gt(peaks$left_base[3], peaks$location[2])
  expect_equal(
    peaks$prominence[3],
    contour_signal[1] - max(contour_signal[2:3])
  )
  expect_gt(
    abs(
      peaks$prominence[3] -
        (clipped_signal[1] - max(clipped_signal[2:3]))
    ),
    1e-6
  )
})

test_that("filtered-out peaks do not create clipping boundaries", {
  trace <- prominence_bounds_trace("rising")
  peaks <- .detect_peaks_prominence(
    trace$location,
    trace$value,
    min_prominence = 10
  )

  expect_equal(peaks$location, c(3, 9))
  expect_lt(peaks$left_base[2], 6)
  expect_gt(peaks$right_base[2], 6)
})

test_that("deconvolution keeps refined integration windows and prominence contours", {
  location <- seq(0, 12, by = 0.01)
  value <- 4 * exp(-0.5 * ((location - 6) / 0.3)^2) + 0.02 * location
  peaks <- .detect_peaks_prominence(location, value, min_prominence = 1)
  original_left <- peaks$left_base
  original_right <- peaks$right_base
  peaks$original_left_base <- original_left
  peaks$original_right_base <- original_right
  peaks$left_base <- peaks$location - 0.5
  peaks$right_base <- peaks$location + 0.5
  refined_left <- peaks$left_base
  refined_right <- peaks$right_base
  input <- tibble::tibble(
    .peaks = new_peaks_list(list(peaks)),
    .measures = new_measure_list(list(new_measure_tbl(location, value)))
  )
  step <- step_measure_peaks_deconvolve_new(
    model = "gaussian",
    optimizer = "lbfgsb",
    max_iter = 1000L,
    tol = 1e-8,
    n_starts = 1L,
    constrain_positions = FALSE,
    quality_threshold = 0,
    store_components = FALSE,
    smart_init = TRUE,
    peaks_col = ".peaks",
    measures_col = ".measures",
    role = NA,
    trained = TRUE,
    skip = FALSE,
    id = "prominence-boundaries"
  )
  result <- bake.step_measure_peaks_deconvolve(step, input)
  fitted <- unserialize(serialize(result$.peaks[[1]], NULL))
  properties <- .add_peak_properties(
    fitted,
    new_measure_tbl(location, value),
    "prominence"
  )
  expected_prominence <- .calculate_peak_prominence(
    location,
    value,
    fitted$location[1],
    fitted$prominence_left_base[1],
    fitted$prominence_right_base[1]
  )
  refined_prominence <- .calculate_peak_prominence(
    location,
    value,
    fitted$location[1],
    fitted$detected_left_base[1],
    fitted$detected_right_base[1]
  )

  expect_equal(fitted$original_left_base, original_left)
  expect_equal(fitted$original_right_base, original_right)
  expect_equal(fitted$detected_left_base, refined_left)
  expect_equal(fitted$detected_right_base, refined_right)
  expect_equal(fitted$left_base, refined_left)
  expect_equal(fitted$right_base, refined_right)
  expect_equal(fitted$prominence_left_base, original_left)
  expect_equal(fitted$prominence_right_base, original_right)
  expect_equal(properties$prominence, expected_prominence)
  expect_gt(abs(properties$prominence - refined_prominence), 1e-6)
})

test_that("prominence deconvolution areas use finite fitted-component windows", {
  location <- seq(0, 12, by = 0.01)
  model <- gaussian_peak_model()
  components <- list(
    list(height = 1, center = 5, width = 0.5),
    list(height = 1, center = 6.25, width = 0.5)
  )
  value <- peak_model_value(model, location, components[[1]]) +
    peak_model_value(model, location, components[[2]])
  data <- tibble::tibble(
    id = 1L,
    location = location,
    value = value
  )
  recipe <- recipes::recipe(~., data = data) |>
    recipes::update_role(id, new_role = "id") |>
    step_measure_input_long(value, location = dplyr::vars(location)) |>
    step_measure_peaks_detect(
      algorithm = "prominence",
      min_prominence = 0.1
    ) |>
    step_measure_peaks_deconvolve(
      model = "gaussian",
      optimizer = "lbfgsb",
      max_iter = 2000L,
      tol = 1e-10,
      n_starts = 1L,
      quality_threshold = 0
    ) |>
    step_measure_peaks_integrate()
  peaks <- recipes::bake(recipes::prep(recipe), new_data = NULL)$.peaks[[1]]
  fitted_window_areas <- vapply(
    seq_len(nrow(peaks)),
    function(i) {
      peak_model_area(
        peaks$fit_model[[i]],
        peaks$fit_params[[i]],
        x_range = sort(c(peaks$left_base[i], peaks$right_base[i]))
      )
    },
    numeric(1)
  )
  full_component_areas <- vapply(
    seq_len(nrow(peaks)),
    function(i) {
      peak_model_area(peaks$fit_model[[i]], peaks$fit_params[[i]])
    },
    numeric(1)
  )

  expect_equal(nrow(peaks), 2L)
  expect_equal(peaks$location, c(5, 6.25), tolerance = 0.005)
  expect_equal(peaks$right_base[1], peaks$left_base[2])
  expect_equal(peaks$area_basis, rep("fitted_component", 2))
  expect_equal(peaks$area, fitted_window_areas, tolerance = 1e-10)
  expect_equal(
    full_component_areas,
    rep(0.5 * sqrt(2 * pi), 2),
    tolerance = 1e-5
  )
  expect_equal(peaks$area, c(1.1186, 1.1232), tolerance = 0.005)
  expect_lt(max(peaks$area - full_component_areas), -0.1)
})
