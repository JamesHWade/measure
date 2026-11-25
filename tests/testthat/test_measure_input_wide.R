test_that("ingest wide format data", {
  meats_data <- data_meat_wide()

  na_train <- meats_data$train
  na_train$x_001[1] <- NA_real_
  na_test <- meats_data$test
  na_test$x_002[1] <- NA_real_

  # ----------------------------------------------------------------------------

  rec_1 <-
    recipe(water + fat + protein ~ ., data = meats_data$train) |>
    step_measure_input_wide(x_001:x_100, id = "potato")
  expect_snapshot(print(rec_1))
  expect_snapshot(print(summary(rec_1)))
  expect_snapshot(print(tidy(rec_1)))

  prep_1 <- prep(rec_1)
  expect_snapshot(print(prep_1))
  expect_snapshot(print(summary(prep_1)))
  expect_snapshot(print(tidy(prep_1)))

  bake_1 <- bake(prep_1, new_data = NULL)
  dat_ptype <-
    tibble::tibble(
      .sample_num = integer(0),
      water = numeric(0),
      fat = numeric(0),
      protein = numeric(0),
      .measures = new_measure_list(list())
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

  ## missing values

  prep_2 <-
    recipe(water + fat + protein ~ ., data = na_train) |>
    step_measure_input_wide(x_001:x_100) |>
    prep()

  ## bad inputs
  expect_snapshot(
    recipe(water + fat + protein ~ ., data = na_train) |>
      step_measure_input_wide(x_001:x_100, location_values = 1:2) |>
      prep(),
    error = TRUE
  )
})

test_that("wide input keeps one sample for every input row", {
  training <- tibble::tibble(
    sample_id = c("duplicate", "duplicate"),
    ..row = c("batch-A", "batch-A"),
    x_001 = c(1, 10),
    x_002 = c(2, 20),
    outcome = c(0, 0)
  )

  rec <-
    recipe(outcome ~ ., data = training) |>
    step_measure_input_wide(x_001:x_002, location_values = c(100, 200)) |>
    prep()

  result <- bake(rec, new_data = NULL)

  expect_equal(nrow(result), nrow(training))
  expect_equal(as.character(result$..row), training$..row)
  expect_equal(
    result$.measures[[1]],
    new_measure_tbl(location = c(100, 200), value = c(1, 2))
  )
  expect_equal(
    result$.measures[[2]],
    new_measure_tbl(location = c(100, 200), value = c(10, 20))
  )
})

test_that("wide input uses trained columns and axis order when baking", {
  training <- tibble::tibble(
    sample_id = 1:2,
    x_001 = c(1, 10),
    x_002 = c(2, 20),
    outcome = c(0, 1)
  )

  rec <-
    recipe(outcome ~ ., data = training) |>
    step_measure_input_wide(
      starts_with("x_"),
      location_values = c(100, 200)
    ) |>
    prep()

  new_data <- tibble::tibble(
    sample_id = 1:2,
    x_002 = c(20, 200),
    x_001 = c(10, 100),
    x_extra = c(99, 999),
    outcome = c(0, 1)
  )

  result <- bake.step_measure_input_wide(rec$steps[[1]], new_data)

  expect_equal(result$.measures[[1]]$location, c(100, 200))
  expect_equal(result$.measures[[1]]$value, c(10, 20))
  expect_equal(result$.measures[[2]]$value, c(100, 200))
  expect_in("x_extra", names(result))
})

test_that("wide input reports missing trained columns", {
  training <- tibble::tibble(
    sample_id = 1:2,
    x_001 = c(1, 10),
    x_002 = c(2, 20),
    outcome = c(0, 1)
  )

  rec <-
    recipe(outcome ~ ., data = training) |>
    step_measure_input_wide(starts_with("x_")) |>
    prep()

  expect_snapshot(
    bake.step_measure_input_wide(
      rec$steps[[1]],
      training[c("sample_id", "x_001", "outcome")]
    ),
    error = TRUE
  )
})

test_that("wide input rejects matrix-valued measurement columns", {
  training <- tibble::tibble(
    outcome = 1:2,
    measurements = I(matrix(1:4, nrow = 2))
  )

  expect_snapshot(
    recipe(outcome ~ ., data = training) |>
      step_measure_input_wide(measurements) |>
      prep(),
    error = TRUE
  )
})

test_that("wide input rejects output column collisions", {
  training <- tibble::tibble(
    sample_id = 1:2,
    x_001 = c(1, 10),
    x_002 = c(2, 20),
    outcome = c(0, 1)
  )

  expect_snapshot(
    recipe(outcome ~ ., data = training) |>
      step_measure_input_wide(x_001:x_002, col_name = "sample_id") |>
      prep(),
    error = TRUE
  )
})
