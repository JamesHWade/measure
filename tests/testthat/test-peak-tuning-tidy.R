peak_tidy_base_recipe <- function(data = data.frame(x = 1)) {
  recipes::recipe(~., data = data)
}

peak_tidy_detect_parameters <- function() {
  list(
    min_height = 0,
    min_distance = 0,
    min_prominence = 0,
    snr_threshold = FALSE
  )
}

peak_tidy_deconvolve_parameters <- function() {
  list(max_iter = 500L, quality_threshold = 0.8)
}

peak_tidy_detect_with_parameter <- function(recipe, parameter, value) {
  switch(
    parameter,
    min_height = step_measure_peaks_detect(recipe, min_height = value),
    min_distance = step_measure_peaks_detect(recipe, min_distance = value),
    min_prominence = step_measure_peaks_detect(recipe, min_prominence = value),
    snr_threshold = step_measure_peaks_detect(recipe, snr_threshold = value)
  )
}

peak_tidy_deconvolve_with_parameter <- function(recipe, parameter, value) {
  switch(
    parameter,
    max_iter = step_measure_peaks_deconvolve(recipe, max_iter = value),
    quality_threshold = step_measure_peaks_deconvolve(
      recipe,
      quality_threshold = value
    )
  )
}

test_that("peak tidy methods retain individual unresolved tune parameters", {
  for (parameter in names(peak_tidy_detect_parameters())) {
    rec <- peak_tidy_detect_with_parameter(
      peak_tidy_base_recipe(),
      parameter,
      tune::tune()
    )

    tidy_result <- generics::tidy(rec, number = 1)
    expect_type(tidy_result[[parameter]], "list")
    expect_identical(
      tidy_result[[parameter]][[1]],
      tune::tune(),
      info = parameter
    )
  }

  for (parameter in names(peak_tidy_deconvolve_parameters())) {
    rec <- peak_tidy_deconvolve_with_parameter(
      peak_tidy_base_recipe(),
      parameter,
      tune::tune()
    )

    tidy_result <- generics::tidy(rec, number = 1)
    expect_type(tidy_result[[parameter]], "list")
    expect_identical(
      tidy_result[[parameter]][[1]],
      tune::tune(),
      info = parameter
    )
  }
})

test_that("peak tidy methods retain all unresolved tune parameters and IDs", {
  rec <- step_measure_peaks_detect(
    peak_tidy_base_recipe(),
    min_height = tune::tune(),
    min_distance = tune::tune(),
    min_prominence = tune::tune(),
    snr_threshold = tune::tune()
  )
  rec <- step_measure_peaks_deconvolve(
    rec,
    max_iter = tune::tune(),
    quality_threshold = tune::tune()
  )

  detect_tidy <- generics::tidy(rec, number = 1)
  deconvolve_tidy <- generics::tidy(rec, number = 2)

  for (parameter in names(peak_tidy_detect_parameters())) {
    expect_type(detect_tidy[[parameter]], "list")
    expect_identical(
      detect_tidy[[parameter]][[1]],
      tune::tune(),
      info = parameter
    )
  }
  for (parameter in names(peak_tidy_deconvolve_parameters())) {
    expect_type(deconvolve_tidy[[parameter]], "list")
    expect_identical(
      deconvolve_tidy[[parameter]][[1]],
      tune::tune(),
      info = parameter
    )
  }

  rec <- step_measure_peaks_detect(
    peak_tidy_base_recipe(),
    min_height = tune::tune(id = "min_height_custom"),
    min_distance = tune::tune(id = "min_distance_custom"),
    min_prominence = tune::tune(id = "min_prominence_custom"),
    snr_threshold = tune::tune(id = "snr_threshold_custom")
  )
  rec <- step_measure_peaks_deconvolve(
    rec,
    max_iter = tune::tune(id = "max_iter_custom"),
    quality_threshold = tune::tune(id = "quality_threshold_custom")
  )
  detect_tidy <- generics::tidy(rec, number = 1)
  deconvolve_tidy <- generics::tidy(rec, number = 2)

  for (parameter in names(peak_tidy_detect_parameters())) {
    expected <- tune::tune(id = paste0(parameter, "_custom"))
    expect_identical(detect_tidy[[parameter]][[1]], expected)
  }
  for (parameter in names(peak_tidy_deconvolve_parameters())) {
    expected <- tune::tune(id = paste0(parameter, "_custom"))
    expect_identical(
      deconvolve_tidy[[parameter]][[1]],
      expected,
      info = parameter
    )
  }
})

test_that("resolved peak tidy parameters keep their original scalar types", {
  for (snr_threshold in list(FALSE, TRUE, 2)) {
    rec <- peak_tidy_detect_with_parameter(
      peak_tidy_base_recipe(),
      "snr_threshold",
      snr_threshold
    )

    tidy_result <- generics::tidy(rec, number = 1)
    expect_identical(tidy_result$snr_threshold, snr_threshold)
    expect_type(tidy_result$min_height, "double")
    expect_type(tidy_result$min_distance, "double")
    expect_type(tidy_result$min_prominence, "double")
  }

  rec <- step_measure_peaks_deconvolve(peak_tidy_base_recipe())
  tidy_result <- generics::tidy(rec, number = 1)
  expect_type(tidy_result$max_iter, "integer")
  expect_type(tidy_result$quality_threshold, "double")
})

peak_tidy_long_data <- function() {
  location <- seq(0, 8, by = 0.2)
  value <- exp(-0.5 * ((location - 2) / 0.5)^2) +
    1.2 * exp(-0.5 * ((location - 6) / 0.7)^2)

  tibble::tibble(
    sample_id = "sample",
    location = location,
    value = value
  )
}

test_that("finalized peak tuning remains prep-able and bake-able", {
  rec <- recipes::recipe(~., data = peak_tidy_long_data()) |>
    recipes::update_role(sample_id, new_role = "id") |>
    step_measure_input_long(value, location = dplyr::vars(location)) |>
    step_measure_peaks_detect(
      min_height = tune::tune(),
      min_distance = tune::tune(),
      min_prominence = tune::tune(),
      snr_threshold = tune::tune()
    ) |>
    step_measure_peaks_deconvolve(
      max_iter = tune::tune(),
      quality_threshold = tune::tune()
    )

  finalized <- tune::finalize_recipe(
    rec,
    list(
      min_height = 0,
      min_distance = 0,
      min_prominence = 0,
      snr_threshold = 0,
      max_iter = 20L,
      quality_threshold = 0
    )
  )
  prepped <- recipes::prep(finalized)

  detect_tidy <- generics::tidy(prepped, number = 2)
  deconvolve_tidy <- generics::tidy(prepped, number = 3)
  expect_type(detect_tidy$min_height, "double")
  expect_type(detect_tidy$min_distance, "double")
  expect_type(detect_tidy$min_prominence, "double")
  expect_type(detect_tidy$snr_threshold, "double")
  expect_type(deconvolve_tidy$max_iter, "integer")
  expect_type(deconvolve_tidy$quality_threshold, "double")

  baked <- recipes::bake(prepped, new_data = NULL)
  expect_true(".peaks" %in% names(baked))
  expect_true(nrow(baked$.peaks[[1]]) > 0)
})

test_that("prepared peak tidy supports multiple measure columns after finalization", {
  data <- tibble::tibble(
    sample_id = c("a", "b"),
    a_1 = c(0, 0),
    a_2 = c(0.1, 0.2),
    a_3 = c(1, 0.8),
    a_4 = c(0.2, 0.1),
    b_1 = c(0, 0),
    b_2 = c(0.3, 0.1),
    b_3 = c(0.8, 1),
    b_4 = c(0.1, 0.2)
  )

  rec <- recipes::recipe(~., data = data) |>
    recipes::update_role(sample_id, new_role = "id") |>
    step_measure_input_wide(
      dplyr::starts_with("a_"),
      col_name = ".measure_a"
    ) |>
    step_measure_input_wide(
      dplyr::starts_with("b_"),
      col_name = ".measure_b"
    ) |>
    step_measure_peaks_detect(
      min_height = tune::tune(),
      min_distance = tune::tune(),
      min_prominence = tune::tune(),
      snr_threshold = tune::tune()
    )

  finalized <- tune::finalize_recipe(
    rec,
    list(
      min_height = 0,
      min_distance = 0,
      min_prominence = 0,
      snr_threshold = 0
    )
  )
  prepped <- recipes::prep(finalized)
  tidy_result <- generics::tidy(prepped, number = 3)

  expect_equal(tidy_result$terms, c(".measure_a", ".measure_b"))
  expect_equal(tidy_result$min_height, c(0, 0))
  expect_type(tidy_result$min_height, "double")
  expect_type(tidy_result$snr_threshold, "double")
})
