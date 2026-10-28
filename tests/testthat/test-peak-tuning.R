peak_tuning_data <- function(noisy = TRUE) {
  location <- seq(0, 12, by = 0.1)
  value <- 0.6 *
    exp(-0.5 * ((location - 3) / 0.5)^2) +
    1.2 * exp(-0.5 * ((location - 8) / 0.6)^2)
  if (noisy) {
    value <- value + 0.1 * sin(7 * location)
  }

  tibble::tibble(
    id = "sample",
    outcome = 1,
    location = location,
    value = value
  )
}

peak_tuning_recipe <- function(
  min_height = 0.1,
  min_distance = 0,
  min_prominence = 0,
  snr_threshold = FALSE,
  max_iter = NULL,
  quality_threshold = NULL,
  noisy = TRUE
) {
  rec <- recipes::recipe(outcome ~ ., data = peak_tuning_data(noisy)) |>
    recipes::update_role(id, new_role = "id") |>
    step_measure_input_long(value, location = dplyr::vars(location)) |>
    step_measure_peaks_detect(
      algorithm = "prominence",
      min_height = min_height,
      min_distance = min_distance,
      min_prominence = min_prominence,
      snr_threshold = snr_threshold
    )

  if (!is.null(max_iter)) {
    rec <- step_measure_peaks_deconvolve(
      rec,
      max_iter = max_iter,
      quality_threshold = quality_threshold
    )
  }

  rec
}

test_that("peak tuning parameters expose useful, customizable ranges", {
  height <- peak_min_height(range = c(0.2, 2))
  distance <- peak_min_distance(range = c(0.5, 8))
  prominence <- peak_min_prominence(range = c(0.1, 1.5))
  snr <- peak_snr_threshold(range = c(0, 20))
  fit <- peak_fit_r_squared(range = c(0.8, 1))
  iterations <- peak_fit_iterations(range = c(10L, 500L))

  expect_s3_class(height, "quant_param")
  expect_equal(unname(unlist(height$range)), c(0.2, 2))
  expect_equal(unname(unlist(distance$range)), c(0.5, 8))
  expect_equal(unname(unlist(prominence$range)), c(0.1, 1.5))
  expect_equal(unname(unlist(snr$range)), c(0, 20))
  expect_equal(unname(unlist(fit$range)), c(0.8, 1))
  expect_equal(iterations$type, "integer")
  expect_equal(unname(unlist(iterations$range)), c(10, 500))
})

test_that("peak detection and fit parameters extract and finalize through tune", {
  rec <- peak_tuning_recipe(
    min_height = tune::tune(),
    min_distance = tune::tune(),
    min_prominence = tune::tune(),
    snr_threshold = tune::tune(),
    max_iter = tune::tune(),
    quality_threshold = tune::tune(),
    noisy = FALSE
  )

  detect_tunable <- recipes::tunable(rec$steps[[2]])
  expect_setequal(
    detect_tunable$name,
    c("min_height", "min_distance", "min_prominence", "snr_threshold")
  )

  parameters <- tune::extract_parameter_set_dials(rec)
  expect_setequal(
    parameters$id,
    c(
      "min_height",
      "min_distance",
      "min_prominence",
      "snr_threshold",
      "max_iter",
      "quality_threshold"
    )
  )
  expect_s3_class(
    parameters$object[[match("min_height", parameters$id)]],
    "quant_param"
  )
  expect_equal(
    parameters$object[[match("max_iter", parameters$id)]]$type,
    "integer"
  )
  expect_equal(
    parameters$object[[match("quality_threshold", parameters$id)]]$type,
    "double"
  )
  expect_equal(
    parameters$object[[match("snr_threshold", parameters$id)]]$range$upper,
    10
  )

  finalized <- tune::finalize_recipe(
    rec,
    list(
      min_height = 0.8,
      min_distance = 0,
      min_prominence = 0,
      snr_threshold = 0,
      max_iter = 100L,
      quality_threshold = 1
    )
  )

  expect_equal(finalized$steps[[2]]$min_height, 0.8)
  expect_identical(finalized$steps[[3]]$max_iter, 100L)
  expect_equal(finalized$steps[[3]]$quality_threshold, 1)

  prepped <- NULL
  expect_warning(
    prepped <- recipes::prep(finalized),
    "Deconvolution quality below threshold"
  )
  result <- recipes::bake(prepped, new_data = NULL)
  peaks <- result$.peaks[[1]]

  expect_equal(nrow(peaks), 1L)
  expect_true("fit_r_squared" %in% names(peaks))
  expect_true(all(is.finite(peaks$fit_r_squared)))
})

test_that("numeric SNR thresholds preserve the disabled FALSE behavior", {
  rec_disabled <- peak_tuning_recipe(
    min_height = 0.1,
    snr_threshold = FALSE
  ) |>
    recipes::prep()
  rec_zero <- peak_tuning_recipe(
    min_height = 0.1,
    snr_threshold = 0
  ) |>
    recipes::prep()
  rec_snr <- peak_tuning_recipe(
    min_height = 0.1,
    snr_threshold = 8
  ) |>
    recipes::prep()

  disabled_peaks <- recipes::bake(rec_disabled, new_data = NULL)$.peaks[[1]]
  zero_peaks <- recipes::bake(rec_zero, new_data = NULL)$.peaks[[1]]
  snr_peaks <- recipes::bake(rec_snr, new_data = NULL)$.peaks[[1]]

  expect_gt(nrow(disabled_peaks), 1L)
  expect_equal(nrow(zero_peaks), nrow(disabled_peaks))
  expect_equal(nrow(snr_peaks), 1L)

  tuned <- peak_tuning_recipe(
    min_height = tune::tune(),
    min_distance = tune::tune(),
    min_prominence = tune::tune(),
    snr_threshold = tune::tune()
  ) |>
    tune::finalize_recipe(
      list(
        min_height = 0.1,
        min_distance = 0,
        min_prominence = 0,
        snr_threshold = 8
      )
    ) |>
    recipes::prep()

  tuned_peaks <- recipes::bake(tuned, new_data = NULL)$.peaks[[1]]
  expect_equal(nrow(tuned_peaks), 1L)
})

test_that("tuned distance and prominence thresholds change peak counts", {
  rec <- peak_tuning_recipe(
    min_height = tune::tune(),
    min_distance = tune::tune(),
    min_prominence = tune::tune(),
    snr_threshold = tune::tune(),
    noisy = FALSE
  )

  finalize_detect <- function(min_distance, min_prominence) {
    finalized <- tune::finalize_recipe(
      rec,
      list(
        min_height = 0.1,
        min_distance = min_distance,
        min_prominence = min_prominence,
        snr_threshold = 0
      )
    )
    prepped <- recipes::prep(finalized)
    baked <- recipes::bake(prepped, new_data = NULL)

    nrow(baked$.peaks[[1]])
  }

  expect_equal(finalize_detect(0, 0), 2L)
  expect_equal(finalize_detect(6, 0), 1L)
  expect_equal(finalize_detect(0, 0.7), 1L)
})

test_that("peak tuning parameters reject invalid resolved numeric values", {
  rec <- peak_tuning_recipe(min_height = tune::tune())
  invalid <- tune::finalize_recipe(
    rec,
    list(min_height = Inf)
  )

  expect_error(recipes::prep(invalid), "finite")
  invalid_fit <- tune::finalize_recipe(
    peak_tuning_recipe(
      max_iter = tune::tune(),
      quality_threshold = tune::tune()
    ),
    list(max_iter = 1.5, quality_threshold = 0.8)
  )
  expect_error(recipes::prep(invalid_fit), "positive integer")
  invalid_quality <- tune::finalize_recipe(
    peak_tuning_recipe(
      max_iter = tune::tune(),
      quality_threshold = tune::tune()
    ),
    list(max_iter = 100L, quality_threshold = Inf)
  )
  expect_error(recipes::prep(invalid_quality), "between 0 and 1")
  expect_error(
    peak_tuning_recipe(min_height = c(0.1, 0.2)),
    "single finite"
  )
  expect_error(
    peak_tuning_recipe(snr_threshold = NA_real_),
    "single finite"
  )
  expect_error(
    step_measure_peaks_deconvolve(
      recipes::recipe(outcome ~ ., data = peak_tuning_data()),
      max_iter = 1.5
    ),
    "positive integer"
  )
  expect_error(
    step_measure_peaks_deconvolve(
      recipes::recipe(outcome ~ ., data = peak_tuning_data()),
      quality_threshold = NaN
    ),
    "between 0 and 1"
  )
})
