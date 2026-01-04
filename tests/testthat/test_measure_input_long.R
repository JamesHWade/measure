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
