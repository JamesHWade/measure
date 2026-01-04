test_that("multiple step_measure_input_long calls work (Issue #67)", {
  # Regression test for Issue #67: step_measure_input_long fails when called
  # multiple times in same recipe

  set.seed(123)
  n_samples <- 5
  n_points <- 20

  test_data <- expand.grid(
    sample_id = paste0("S", seq_len(n_samples)),
    elution_time = seq(1, 10, length.out = n_points)
  ) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      ri_signal = rnorm(dplyr::n(), mean = elution_time / 10, sd = 0.1),
      uv_signal = rnorm(dplyr::n(), mean = elution_time / 5, sd = 0.2)
    )

  # Two calls should work when ID column is properly set
  rec2 <- recipe(
    ri_signal + uv_signal + elution_time ~ sample_id,
    data = test_data
  ) |>
    update_role(sample_id, new_role = "id") |>
    step_measure_input_long(
      ri_signal,
      location = vars(elution_time),
      col_name = "ri"
    ) |>
    step_measure_input_long(
      uv_signal,
      location = vars(elution_time),
      col_name = "uv"
    )

  # Should prep without error
  prep2 <- prep(rec2)

  # Check output structure
  result <- bake(prep2, new_data = NULL)
  expect_equal(nrow(result), n_samples)
  expect_true("ri" %in% names(result))
  expect_true("uv" %in% names(result))
  expect_true(inherits(result$ri, "measure_list"))
  expect_true(inherits(result$uv, "measure_list"))
  expect_equal(dim(result$ri[[1]]), c(n_points, 2L))
  expect_equal(dim(result$uv[[1]]), c(n_points, 2L))

  # Location values should match in both measures
  expect_equal(result$ri[[1]]$location, result$uv[[1]]$location)
})

test_that("multiple input steps work with output_wide (Issue #67)", {
  set.seed(123)
  n_samples <- 5
  n_points <- 20

  test_data <- expand.grid(
    sample_id = paste0("S", seq_len(n_samples)),
    elution_time = seq(1, 10, length.out = n_points)
  ) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      ri_signal = rnorm(dplyr::n(), mean = elution_time / 10, sd = 0.1),
      uv_signal = rnorm(dplyr::n(), mean = elution_time / 5, sd = 0.2)
    )

  rec <- recipe(
    ri_signal + uv_signal + elution_time ~ sample_id,
    data = test_data
  ) |>
    update_role(sample_id, new_role = "id") |>
    step_measure_input_long(
      ri_signal,
      location = vars(elution_time),
      col_name = "ri"
    ) |>
    step_measure_input_long(
      uv_signal,
      location = vars(elution_time),
      col_name = "uv"
    ) |>
    step_measure_output_wide(measures = "ri", prefix = "ri_")

  # Should prep and bake without error
  result <- suppressMessages(prep(rec) |> bake(new_data = NULL))

  expect_equal(nrow(result), n_samples)
  # Should have wide columns (ri_*) for the converted measure
  expect_true(any(grepl("^ri_", names(result))))
  # The ri measure should no longer exist as a list column
  expect_false("ri" %in% names(result))
  # Note: uv remains as a measure_list (only ri was converted to wide)
  expect_true("uv" %in% names(result))
})

test_that("three or more step_measure_input_long calls work (Issue #69)", {
  # Regression test for Issue #69: third step_measure_input_long fails
  # with 'Column must be numeric' error due to nested list column issue

  set.seed(123)
  n_samples <- 5
  n_points <- 20

  test_data <- expand.grid(
    sample_id = paste0("S", seq_len(n_samples)),
    elution_time = seq(1, 10, length.out = n_points)
  ) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      ri_signal = rnorm(dplyr::n(), mean = elution_time / 10, sd = 0.1),
      uv_signal = rnorm(dplyr::n(), mean = elution_time / 5, sd = 0.2),
      mals_signal = rnorm(dplyr::n(), mean = elution_time / 8, sd = 0.15)
    )

  # Three calls should work when ID column is properly set
  rec3 <- recipe(
    ri_signal + uv_signal + mals_signal + elution_time ~ sample_id,
    data = test_data
  ) |>
    update_role(sample_id, new_role = "id") |>
    step_measure_input_long(
      ri_signal,
      location = vars(elution_time),
      col_name = "ri"
    ) |>
    step_measure_input_long(
      uv_signal,
      location = vars(elution_time),
      col_name = "uv"
    ) |>
    step_measure_input_long(
      mals_signal,
      location = vars(elution_time),
      col_name = "mals"
    )

  # Should prep without error
  prep3 <- prep(rec3)

  # Check output structure
  result <- bake(prep3, new_data = NULL)
  expect_equal(nrow(result), n_samples)
  expect_true("ri" %in% names(result))
  expect_true("uv" %in% names(result))
  expect_true("mals" %in% names(result))
  expect_true(inherits(result$ri, "measure_list"))
  expect_true(inherits(result$uv, "measure_list"))
  expect_true(inherits(result$mals, "measure_list"))
  expect_equal(dim(result$ri[[1]]), c(n_points, 2L))
  expect_equal(dim(result$uv[[1]]), c(n_points, 2L))
  expect_equal(dim(result$mals[[1]]), c(n_points, 2L))

  # Location values should match in all measures
  expect_equal(result$ri[[1]]$location, result$uv[[1]]$location)
  expect_equal(result$ri[[1]]$location, result$mals[[1]]$location)
})

test_that("four step_measure_input_long calls work (Issue #69)", {
  # Extended regression test: ensure we handle 4+ detectors

  set.seed(456)
  n_samples <- 3
  n_points <- 15

  test_data <- expand.grid(
    sample_id = paste0("S", seq_len(n_samples)),
    elution_time = seq(1, 10, length.out = n_points)
  ) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      ri_signal = rnorm(dplyr::n()),
      uv_signal = rnorm(dplyr::n()),
      mals_signal = rnorm(dplyr::n()),
      visc_signal = rnorm(dplyr::n())
    )

  # Four calls should work
  rec4 <- recipe(
    ri_signal +
      uv_signal +
      mals_signal +
      visc_signal +
      elution_time ~ sample_id,
    data = test_data
  ) |>
    update_role(sample_id, new_role = "id") |>
    step_measure_input_long(
      ri_signal,
      location = vars(elution_time),
      col_name = "ri"
    ) |>
    step_measure_input_long(
      uv_signal,
      location = vars(elution_time),
      col_name = "uv"
    ) |>
    step_measure_input_long(
      mals_signal,
      location = vars(elution_time),
      col_name = "mals"
    ) |>
    step_measure_input_long(
      visc_signal,
      location = vars(elution_time),
      col_name = "visc"
    )

  # Should prep without error
  prep4 <- prep(rec4)

  # Check output structure
  result <- bake(prep4, new_data = NULL)
  expect_equal(nrow(result), n_samples)
  expect_true(all(c("ri", "uv", "mals", "visc") %in% names(result)))
  expect_true(all(vapply(
    result[c("ri", "uv", "mals", "visc")],
    inherits,
    logical(1),
    "measure_list"
  )))
})

test_that("multiple input steps work with output_long (Issue #67)", {
  set.seed(123)
  n_samples <- 5
  n_points <- 20

  test_data <- expand.grid(
    sample_id = paste0("S", seq_len(n_samples)),
    elution_time = seq(1, 10, length.out = n_points)
  ) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      ri_signal = rnorm(dplyr::n(), mean = elution_time / 10, sd = 0.1),
      uv_signal = rnorm(dplyr::n(), mean = elution_time / 5, sd = 0.2)
    )

  rec <- recipe(
    ri_signal + uv_signal + elution_time ~ sample_id,
    data = test_data
  ) |>
    update_role(sample_id, new_role = "id") |>
    step_measure_input_long(
      ri_signal,
      location = vars(elution_time),
      col_name = "ri"
    ) |>
    step_measure_input_long(
      uv_signal,
      location = vars(elution_time),
      col_name = "uv"
    ) |>
    step_measure_output_long(measures = "ri")

  # Should prep and bake without error
  result <- prep(rec) |> bake(new_data = NULL)

  # Should have n_samples * n_points rows (long format)
  expect_equal(nrow(result), n_samples * n_points)
  expect_true(".measure" %in% names(result))
  expect_true(".location" %in% names(result))
})

test_that("check_type_or_list_numeric accepts valid inputs", {
  # Numeric vectors should pass
  expect_silent(check_type_or_list_numeric(1:10, "test"))
  expect_silent(check_type_or_list_numeric(c(1.5, 2.5, 3.5), "test"))

  # List columns where all elements are numeric should pass
  expect_silent(check_type_or_list_numeric(list(1:3, 4:6), "test"))
  expect_silent(check_type_or_list_numeric(
    list(c(1.1, 2.2), c(3.3, 4.4)),
    "test"
  ))
})

test_that("check_type_or_list_numeric rejects invalid inputs", {
  # Character vectors should fail
  expect_error(
    check_type_or_list_numeric(c("a", "b"), "test"),
    "must be numeric"
  )

  # List with character elements should fail
  expect_error(
    check_type_or_list_numeric(list("a", "b"), "test"),
    "must be numeric"
  )

  # List with mixed types should fail
  expect_error(
    check_type_or_list_numeric(list(1:3, "x"), "test"),
    "must be numeric"
  )

  # Empty list should fail
  expect_error(
    check_type_or_list_numeric(list(), "test"),
    "empty list"
  )
})

test_that("ingest long format data", {
  meats_data <- data_meat_long()

  na_train <- meats_data$train
  na_train$absorp[2] <- NA_real_
  na_test <- meats_data$test
  na_test$absorp[2] <- NA_real_

  miss_train <- meats_data$train |> dplyr::slice(-5)
  miss_test <- meats_data$test |> dplyr::slice(-5)

  # ----------------------------------------------------------------------------

  rec_1 <-
    recipe(water + fat + protein ~ ., data = meats_data$train) |>
    step_measure_input_long(absorp, location = vars(ind), id = "potato")
  expect_snapshot(print(rec_1))
  expect_snapshot(print(summary(rec_1)))
  expect_snapshot(print(tidy(rec_1)))

  prep_1 <- prep(rec_1)
  expect_snapshot(print(prep_1))
  expect_snapshot(print(summary(prep_1)))
  expect_snapshot(print(tidy(prep_1)))

  bake_1 <- bake(prep_1, new_data = NULL)
  # Note: location column (ind) is now preserved as a list column to enable
  # multiple step_measure_input_long calls in sequence (Issue #67 fix)
  dat_ptype <-
    tibble::tibble(
      .sample_num = integer(0),
      water = numeric(0),
      fat = numeric(0),
      protein = numeric(0),
      .measures = new_measure_list(list()),
      ind = list()
    )
  expect_equal(bake_1[0, ], dat_ptype)
  measure_ptype <- new_measure_tbl(location = numeric(0), value = numeric(0))
  expect_equal(bake_1$.measures[[1]][0, ], measure_ptype)
  expect_equal(nrow(bake_1), 200L)
  expect_equal(dim(bake_1$.measures[[1]]), c(100L, 2L))

  bake_1_te <- bake(prep_1, new_data = meats_data$test)
  expect_equal(
    bake_1_te$.measures[[1]][0, ],
    new_measure_tbl(location = numeric(0), value = numeric(0))
  )
  expect_equal(nrow(bake_1_te), 15L)
  expect_equal(dim(bake_1_te$.measures[[1]]), c(100L, 2L))

  ## missing location
  expect_snapshot(
    recipe(water + fat + protein ~ absorp, data = na_train) |>
      step_measure_input_long(absorp) |>
      prep(),
    error = TRUE
  )

  ### missing rows

  expect_snapshot(
    recipe(water + fat + protein ~ ., data = miss_train) |>
      step_measure_input_long(absorp, location = vars(ind)) |>
      prep(),
    error = TRUE
  )

  ## missing values

  prep_3 <-
    recipe(water + fat + protein ~ ., data = na_train) |>
    step_measure_input_long(absorp, location = vars(ind)) |>
    prep()

  ## non-unique rows

  # TODO need to find a better way of figuring this out
  # expect_snapshot(
  #   recipe(water + fat + protein ~ .,
  #          data = meats_data$train |> select(-.sample_num)) |>
  #     step_measure_input_long(absorp, location = vars(ind)) |>
  #     prep(),
  #   error = TRUE
  # )
  # same_sample <- meats_data$test
  # same_sample$.sample_num <- 99
  #
  # bake(prep_1, new_data = same_sample)

  ## bad inputs
  expect_snapshot(
    recipe(water + fat + protein ~ ., data = na_train) |>
      step_measure_input_long(dplyr::everything(), location = vars(ind)) |>
      prep(),
    error = TRUE
  )
  expect_snapshot(
    recipe(water + fat + protein ~ ., data = na_train) |>
      step_measure_input_long(absorp, location = vars(dplyr::everything())) |>
      prep(),
    error = TRUE
  )
})

test_that("multiple input steps work with new_data (not training)", {
  # Regression test: verify fix works when baking genuinely new data,

  # not just training data (new_data = NULL)

  set.seed(123)
  n_train <- 5
  n_test <- 3
  n_points <- 20

  train_data <- expand.grid(
    sample_id = paste0("S", seq_len(n_train)),
    elution_time = seq(1, 10, length.out = n_points)
  ) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      ri_signal = rnorm(dplyr::n()),
      uv_signal = rnorm(dplyr::n()),
      mals_signal = rnorm(dplyr::n())
    )

  test_data <- expand.grid(
    sample_id = paste0("T", seq_len(n_test)),
    elution_time = seq(1, 10, length.out = n_points)
  ) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      ri_signal = rnorm(dplyr::n()),
      uv_signal = rnorm(dplyr::n()),
      mals_signal = rnorm(dplyr::n())
    )

  rec <- recipe(
    ri_signal + uv_signal + mals_signal + elution_time ~ sample_id,
    data = train_data
  ) |>
    update_role(sample_id, new_role = "id") |>
    step_measure_input_long(
      ri_signal,
      location = vars(elution_time),
      col_name = "ri"
    ) |>
    step_measure_input_long(
      uv_signal,
      location = vars(elution_time),
      col_name = "uv"
    ) |>
    step_measure_input_long(
      mals_signal,
      location = vars(elution_time),
      col_name = "mals"
    )

  prepped <- prep(rec)

  # Bake new data (not training data)
  result <- bake(prepped, new_data = test_data)

  expect_equal(nrow(result), n_test)
  expect_true(inherits(result$ri, "measure_list"))
  expect_true(inherits(result$uv, "measure_list"))
  expect_true(inherits(result$mals, "measure_list"))
  expect_equal(dim(result$ri[[1]]), c(n_points, 2L))
})

test_that("multiple input steps work with nD data", {
  # Test with 2D data (e.g., LC-DAD: retention time + wavelength)

  set.seed(123)
  n_samples <- 3

  test_data <- expand.grid(
    sample_id = paste0("S", seq_len(n_samples)),
    retention_time = c(1, 2, 3),
    wavelength = c(254, 280, 320)
  ) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      detector_1_signal = rnorm(dplyr::n()),
      detector_2_signal = rnorm(dplyr::n())
    )

  rec <- recipe(
    detector_1_signal +
      detector_2_signal +
      retention_time +
      wavelength ~ sample_id,
    data = test_data
  ) |>
    update_role(sample_id, new_role = "id") |>
    step_measure_input_long(
      detector_1_signal,
      location = vars(retention_time, wavelength),
      col_name = "detector_1"
    ) |>
    step_measure_input_long(
      detector_2_signal,
      location = vars(retention_time, wavelength),
      col_name = "detector_2"
    )

  # Should prep without error
  prepped <- prep(rec)
  result <- bake(prepped, new_data = NULL)

  expect_equal(nrow(result), n_samples)
  expect_true(inherits(result$detector_1, "measure_nd_list"))
  expect_true(inherits(result$detector_2, "measure_nd_list"))
})
