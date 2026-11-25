test_that("CWT tuning constructors describe scalar controls", {
  snr <- cwt_min_snr()
  ridge <- cwt_min_ridge_length()
  gap <- cwt_gap_thresh()
  window <- cwt_noise_window()
  quantile <- cwt_noise_quantile()

  expect_s3_class(snr, "quant_param")
  expect_equal(snr$type, "double")
  expect_equal(unname(unlist(snr$range)), c(0, 20))
  expect_equal(ridge$type, "integer")
  expect_equal(gap$type, "integer")
  expect_equal(window$type, "integer")
  expect_equal(quantile$type, "double")
  expect_equal(unname(quantile$inclusive), c(FALSE, FALSE))
})

test_that("nested CWT tuning keeps explicit IDs and fixed siblings", {
  rec <- recipes::recipe(~., data = data.frame(x = 1)) |>
    step_measure_peaks_detect(
      algorithm = "cwt",
      min_height = tune::tune(id = "height"),
      algorithm_params = list(
        scales = c(1, 2, 4),
        min_snr = tune::tune(),
        min_ridge_length = 3L,
        gap_thresh = tune::tune(id = "gap")
      )
    )

  step <- rec$steps[[1]]
  args <- generics::tune_args(step, full = TRUE)
  tunable_rows <- args[args$tunable, , drop = FALSE]

  expect_setequal(tunable_rows$name, c("min_height", "min_snr", "gap_thresh"))
  expect_setequal(tunable_rows$id, c("height", "cwt_min_snr", "gap"))

  parameters <- tune::extract_parameter_set_dials(rec)
  expect_setequal(parameters$id, c("height", "cwt_min_snr", "gap"))
  expect_equal(
    parameters$object[[match("cwt_min_snr", parameters$id)]]$type,
    "double"
  )
  expect_equal(
    parameters$object[[match("gap", parameters$id)]]$range$upper,
    2
  )
  expect_false("scales" %in% parameters$id)
  expect_false("min_ridge_length" %in% parameters$id)
})

test_that("direct and nested CWT values finalize together", {
  location <- seq(0, 100, by = 1)
  value <- exp(-0.5 * ((location - 20) / 1.5)^2) +
    0.7 * exp(-0.5 * ((location - 54) / 5)^2)
  data <- tibble::tibble(
    id = "sample-1",
    outcome = 1,
    location = location,
    intensity = value
  )

  rec <- recipes::recipe(outcome ~ ., data = data) |>
    recipes::update_role(id, new_role = "id") |>
    step_measure_input_long(intensity, location = dplyr::vars(location)) |>
    step_measure_peaks_detect(
      algorithm = "cwt",
      min_distance = tune::tune(id = "distance"),
      algorithm_params = list(
        scales = c(1, 2, 3, 4, 6, 8),
        min_snr = tune::tune(),
        min_ridge_length = 3L
      )
    )

  finalized <- tune::finalize_recipe(
    rec,
    list(distance = 40, cwt_min_snr = 3)
  )
  step <- finalized$steps[[2]]

  expect_equal(step$min_distance, 40)
  expect_equal(step$algorithm_params$min_snr, 3)
  expect_identical(step$algorithm_params$min_ridge_length, 3L)
  expect_equal(step$algorithm_params$scales, c(1, 2, 3, 4, 6, 8))

  trained <- recipes::prep(finalized)
  peaks <- recipes::bake(trained, new_data = NULL)$.peaks[[1]]
  expect_equal(nrow(peaks), 1L)
  expect_equal(peaks$location, 20)
})

test_that("tune_grid applies distinct nested CWT candidates", {
  skip_if_not_installed(c("parsnip", "rpart", "rsample", "workflows"))
  withr::local_seed(91)

  axis <- seq(0, 20, length.out = 201L)
  n <- 18L
  data <- tibble::tibble(
    index = seq_len(n),
    outcome = seq_len(n) / n
  )
  data$.measures <- new_measure_list(lapply(data$index, function(i) {
    signal <- exp(-0.5 * ((axis - 5) / 0.3)^2) +
      (i / n) * exp(-0.5 * ((axis - 14) / 0.8)^2) +
      stats::rnorm(length(axis), sd = 0.02)
    new_measure_tbl(axis, signal)
  }))

  rec <- recipes::recipe(outcome ~ ., data = data) |>
    step_measure_peaks_detect(
      algorithm = "cwt",
      min_distance = 2,
      algorithm_params = list(
        scales = c(1, 2, 4, 8, 12),
        min_snr = tune::tune(id = "wave")
      )
    ) |>
    recipes::step_mutate(peak_count = purrr::map_int(.peaks, nrow)) |>
    recipes::step_rm(.peaks, .measures)

  workflow <- workflows::workflow() |>
    workflows::add_recipe(rec) |>
    workflows::add_model(
      parsnip::decision_tree(min_n = 3) |>
        parsnip::set_mode("regression") |>
        parsnip::set_engine("rpart")
    )
  results <- tune::tune_grid(
    workflow,
    resamples = rsample::vfold_cv(data, v = 3),
    grid = tibble::tibble(wave = c(1, 100000)),
    metrics = yardstick::metric_set(yardstick::rmse),
    control = tune::control_grid(extract = function(fit) {
      processed <- workflows::extract_recipe(fit, estimated = TRUE)
      sum(recipes::bake(processed, new_data = data)$peak_count)
    })
  )
  counts <- tune::collect_extracts(results)
  counts$n_peaks <- unlist(counts$.extracts)

  expect_equal(nrow(counts), 6L)
  expect_true(all(counts$n_peaks[counts$wave == 1] > 0))
  expect_true(all(counts$n_peaks[counts$wave == 100000] == 0L))
  expect_equal(nrow(tune::collect_metrics(results)), 2L)
})

test_that("nested tuning rejects unsupported, ambiguous and shadowed controls", {
  rec <- recipes::recipe(~., data = tibble::tibble(x = 1))
  cwt <- function(params, ...) {
    step_measure_peaks_detect(
      rec,
      algorithm = "cwt",
      algorithm_params = params,
      ...
    )
  }
  marker <- tune::tune("candidate")
  expect_error(cwt(list(scales = marker)), "unsupported.*scales")
  expect_error(cwt(list(min_snr = list(marker))), "single.*marker")
  expect_error(cwt(list(marker)), "unique, non-empty")
  duplicate <- stats::setNames(list(marker, 3), rep("min_snr", 2))
  expect_error(cwt(duplicate), "unique, non-empty")
  expect_error(
    cwt(list(min_height = 2), min_height = marker),
    "overrides the direct"
  )
  expect_error(
    cwt(list(min_height = 2), snr_threshold = marker),
    "overrides the direct"
  )
  expect_error(
    recipes::prep(cwt(list(min_snr = marker))),
    "Finalize the unresolved"
  )
  expect_error(
    step_measure_peaks_detect(
      rec,
      algorithm = "local_maxima",
      algorithm_params = list(min_snr = marker)
    ),
    "unsupported.*min_snr"
  )
})
