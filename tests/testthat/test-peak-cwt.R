test_that("CWT is discoverable as a built-in peak algorithm", {
  expect_true(has_peak_algorithm("cwt"))
  expect_true("cwt" %in% peak_algorithms(packs = "measure")$name)

  algorithm <- get_peak_algorithm("cwt")
  expect_equal(algorithm$pack_name, "measure")
  expect_true(is.function(algorithm$algorithm_fn))
  expect_equal(algorithm$default_params$scales, c(1, 2, 3, 4, 6, 8, 12))
  expect_equal(algorithm$default_params$min_snr, 4.5)
})

test_that("CWT ridges find narrow and broad Gaussian peaks in sample units", {
  location <- seq(0, 100, by = 1)
  deterministic_noise <- withr::with_seed(
    2718,
    stats::rnorm(length(location), sd = 0.005)
  )
  value <-
    exp(-0.5 * ((location - 20) / 1.5)^2) +
    0.7 * exp(-0.5 * ((location - 54) / 5)^2) +
    deterministic_noise

  peaks <- .run_peak_algorithm("cwt", location, value)

  expect_s3_class(peaks, "peaks_tbl")
  expect_equal(peaks$location, c(20, 54))
  expect_equal(peaks$height, value[peaks$location + 1], tolerance = 1e-12)
  expect_true(all(peaks$left_base < peaks$location))
  expect_true(all(peaks$right_base > peaks$location))
  expect_true(all(peaks$algorithm_score >= 4.5))
  expect_true(all(peaks$algorithm_score <= 1e6))
  expect_true(all(peaks$ridge_length >= 3L))
  expect_equal(.run_peak_algorithm("cwt", location, value), peaks)
})

test_that("CWT scores are invariant to baseline and separated peak heights", {
  location <- seq_len(201)
  signal <- exp(-0.5 * ((location - 101) / 3)^2)
  unshifted <- .run_peak_algorithm("cwt", location, signal)
  shifted <- .run_peak_algorithm("cwt", location, signal + 1e8)

  expect_equal(shifted$location, 101)
  expect_lt(
    abs(shifted$algorithm_score - unshifted$algorithm_score),
    1e-5
  )

  location <- seq_len(501)
  dynamic_range_signal <-
    1e8 *
    exp(-0.5 * ((location - 101) / 3)^2) +
    exp(-0.5 * ((location - 401) / 3)^2)
  peaks <- .run_peak_algorithm("cwt", location, dynamic_range_signal)

  expect_equal(peaks$location, c(101, 401))
  expect_equal(peaks$height, c(1e8, 1), tolerance = 1e-8)
})

test_that("CWT ridge matching includes equal distance at either edge", {
  coefficients <- matrix(0, nrow = 2, ncol = 10)
  coefficients[1, 5] <- 1
  coefficients[2, 3] <- 1

  ridge <- .cwt_link_ridges(coefficients, scales = c(1, 2), gap_thresh = 1)[[1]]
  mirrored <- .cwt_link_ridges(
    coefficients[, 10:1],
    scales = c(1, 2),
    gap_thresh = 1
  )[[1]]

  expect_equal(ridge$points, c(5L, 3L))
  expect_equal(mirrored$points, c(6L, 8L))
})

test_that("CWT localizes mixed-width peaks under seeded Gaussian noise", {
  location <- seq_len(1001)
  signal <-
    exp(-0.5 * ((location - 200) / 2)^2) +
    exp(-0.5 * ((location - 500) / 8)^2) +
    exp(-0.5 * ((location - 800) / 20)^2)

  for (noise_sd in c(0.05, 0.1)) {
    noisy_signal <- withr::with_seed(
      1,
      signal + stats::rnorm(length(signal), sd = noise_sd)
    )
    peaks <- .run_peak_algorithm(
      "cwt",
      location,
      noisy_signal,
      min_distance = 10
    )

    expect_equal(nrow(peaks), 3L)
    expect_true(all(abs(peaks$location - c(200, 500, 800)) <= 10))
  }
})

test_that("CWT honors raw height and physical min-distance thresholds", {
  location <- seq(0, 100, by = 1)
  value <- exp(-0.5 * ((location - 20) / 1.5)^2) +
    0.7 * exp(-0.5 * ((location - 54) / 5)^2)

  by_height <- .run_peak_algorithm("cwt", location, value, min_height = 0.8)
  by_distance <- .run_peak_algorithm("cwt", location, value, min_distance = 40)

  by_snr <- .run_peak_algorithm("cwt", location, value, min_snr = 100)

  expect_equal(by_height$location, 20)
  expect_equal(by_distance$location, 20)
  expect_equal(nrow(by_snr), 0L)
})

test_that("CWT detection is identical for ascending and descending axes", {
  location <- seq(0, 100, by = 1)
  value <- exp(-0.5 * ((location - 21) / 2)^2) +
    0.8 * exp(-0.5 * ((location - 76) / 4)^2)

  ascending <- .run_peak_algorithm("cwt", location, value)
  descending <- .run_peak_algorithm("cwt", rev(location), rev(value))

  expect_equal(descending, ascending)
  expect_equal(ascending$location, c(21, 76))
})

test_that("CWT ridge matching is symmetric for a mirrored noisy signal", {
  location <- seq_len(101)
  signal <- exp(-0.5 * ((location - 30) / 3)^2) +
    0.8 * exp(-0.5 * ((location - 65) / 5)^2)
  noisy_signal <- withr::with_seed(
    1,
    signal + stats::rnorm(length(signal), sd = 0.1)
  )

  forward <- .run_peak_algorithm("cwt", location, noisy_signal)
  reflected <- .run_peak_algorithm("cwt", location, rev(noisy_signal))

  expect_equal(
    forward$location,
    sort(length(location) + 1 - reflected$location)
  )
})

test_that("CWT handles edge peaks, negative values, missing runs, and blanks", {
  location <- 0:100
  edge_signal <- exp(-0.5 * ((location - 3) / 1.5)^2) +
    0.8 * exp(-0.5 * ((location - 96) / 2)^2)
  edge_peaks <- .run_peak_algorithm("cwt", location, edge_signal)
  expect_equal(edge_peaks$location, c(3, 96))

  negative_signal <- -abs(sin(location / 5))
  expect_equal(nrow(.run_peak_algorithm("cwt", location, negative_signal)), 0L)

  separated_signal <- exp(-0.5 * ((location - 20) / 2)^2) +
    0.7 * exp(-0.5 * ((location - 80) / 4)^2)
  separated_signal[51] <- NA_real_
  separated <- .run_peak_algorithm("cwt", location, separated_signal)
  expect_equal(separated$location, c(20, 80))
  expect_true(separated$right_base[1] < location[51])
  expect_true(separated$left_base[2] > location[51])

  blank_location <- seq_len(1001)
  for (seed in c(1, 11, 2026)) {
    blank_noise <- withr::with_seed(
      seed,
      stats::rnorm(length(blank_location))
    )
    expect_equal(
      nrow(.run_peak_algorithm("cwt", blank_location, blank_noise)),
      0L
    )
  }
  longer_blank <- withr::with_seed(
    2718,
    stats::rnorm(10001)
  )
  expect_equal(
    nrow(.run_peak_algorithm("cwt", seq_len(10001), longer_blank)),
    0L
  )
  expect_equal(
    nrow(.run_peak_algorithm("cwt", location, rep(0, length(location)))),
    0L
  )
  expect_equal(
    nrow(.run_peak_algorithm("cwt", location, rep(NA_real_, length(location)))),
    0L
  )
  expect_named(
    .run_peak_algorithm("cwt", location, rep(0, length(location))),
    c("peak_id", "location", "height", "left_base", "right_base", "area")
  )
})

test_that("CWT rejects irregular or non-monotone location axes", {
  location <- 0:100
  value <- exp(-0.5 * ((location - 50) / 3)^2)
  irregular <- location
  irregular[50] <- irregular[50] + 0.25

  expect_error(
    .run_peak_algorithm("cwt", irregular, value),
    "uniformly spaced"
  )
  expect_error(
    .run_peak_algorithm("cwt", c(location[1:50], rev(location[51:101])), value),
    "strictly monotone"
  )
})

test_that("CWT validates scale, ridge, gap, noise, and SNR parameters", {
  location <- 0:50
  value <- exp(-0.5 * ((location - 25) / 2)^2)

  expect_error(
    .run_peak_algorithm("cwt", location, value, scales = c(2, 1)),
    "scales"
  )
  expect_error(
    .run_peak_algorithm("cwt", location, value, scales = c(1, 1)),
    "scales"
  )
  expect_error(
    .run_peak_algorithm("cwt", location, value, scales = c(0, 1)),
    "scales"
  )
  expect_error(
    .run_peak_algorithm("cwt", location, value, scales = c(0.5, 1)),
    "1 through 10,000"
  )
  expect_error(
    .run_peak_algorithm("cwt", location, value, scales = c(1, 2, 1e20)),
    "1 through 10,000"
  )
  expect_error(
    .run_peak_algorithm("cwt", location, value, scales = c(1, NA_real_)),
    "scales"
  )
  expect_error(
    .run_peak_algorithm("cwt", location, value, scales = seq_len(257)),
    "2 to 256"
  )
  expect_error(
    .run_peak_algorithm("cwt", location, value, scales = c(1, 2, 26)),
    "half the number of finite samples"
  )
  expect_error(
    .cwt_check_resource_budget(1000000, c(1, 2, 3, 4, 6, 8, 12)),
    "computation budget"
  )
  expect_error(
    .run_peak_algorithm("cwt", location, value, min_ridge_length = 0),
    "min_ridge_length"
  )
  expect_error(
    .run_peak_algorithm("cwt", location, value, min_ridge_length = 8),
    "min_ridge_length"
  )
  expect_error(
    .run_peak_algorithm("cwt", location, value, gap_thresh = -1),
    "gap_thresh"
  )
  expect_error(
    .run_peak_algorithm("cwt", location, value, gap_thresh = 7),
    "gap_thresh"
  )
  expect_error(
    .run_peak_algorithm("cwt", location, value, min_snr = NA_real_),
    "min_snr"
  )
  expect_error(
    .run_peak_algorithm("cwt", location, value, min_snr = -1),
    "min_snr"
  )
  expect_error(
    .run_peak_algorithm("cwt", location, value, noise_window = 2),
    "noise_window"
  )
  expect_error(
    .run_peak_algorithm("cwt", location, value, noise_quantile = 0),
    "noise_quantile"
  )
  expect_error(
    .run_peak_algorithm("cwt", location, value, noise_quantile = 1),
    "noise_quantile"
  )
  expect_error(
    .run_peak_algorithm("cwt", location, value, min_height = -1),
    "min_height"
  )
  expect_error(
    .run_peak_algorithm("cwt", location, value, min_distance = -1),
    "min_distance"
  )
})

test_that("CWT works through recipe prep, bake, and algorithm-specific parameters", {
  location <- seq(0, 100, by = 1)
  value <- exp(-0.5 * ((location - 24) / 2)^2) +
    0.75 * exp(-0.5 * ((location - 72) / 5)^2)
  data <- tibble::tibble(
    id = "sample-1",
    location = location,
    intensity = value
  )

  recipe <- recipes::recipe(~., data = data) |>
    recipes::update_role(id, new_role = "id") |>
    step_measure_input_long(intensity, location = vars(location)) |>
    step_measure_peaks_detect(
      algorithm = "cwt",
      algorithm_params = list(
        scales = c(1, 2, 3, 4, 6, 8),
        min_ridge_length = 3L,
        min_snr = 3
      )
    )

  trained <- recipes::prep(recipe)
  baked <- recipes::bake(trained, new_data = NULL)
  peaks <- baked$.peaks[[1]]

  expect_s3_class(baked$.peaks, "peaks_list")
  expect_equal(peaks$location, c(24, 72))
  expect_equal(peaks$height, value[peaks$location + 1], tolerance = 1e-12)
})

test_that("CWT returns empty schema for short signals", {
  peaks <- .run_peak_algorithm("cwt", c(1, 2), c(0, 1))

  expect_s3_class(peaks, "peaks_tbl")
  expect_equal(nrow(peaks), 0L)
  expect_named(
    peaks,
    c("peak_id", "location", "height", "left_base", "right_base", "area")
  )

  short_location <- seq_len(10)
  short_signal <- exp(-0.5 * ((short_location - 5) / 1)^2)
  short_peaks <- .run_peak_algorithm("cwt", short_location, short_signal)
  expect_equal(short_peaks$location, 5)

  below_ridge_length <- .run_peak_algorithm("cwt", seq_len(4), c(0, 1, 0, 0))
  expect_equal(nrow(below_ridge_length), 0L)
  expect_named(
    below_ridge_length,
    c("peak_id", "location", "height", "left_base", "right_base", "area")
  )
})
