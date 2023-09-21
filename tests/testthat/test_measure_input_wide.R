test_that("ingest wide format data", {
  meats_data <- data_meat_wide()

  na_train <- meats_data$train
  na_train$x_001[1] <- NA_real_
  na_test <- meats_data$test
  na_test$x_002[1] <- NA_real_

  # ----------------------------------------------------------------------------

  rec_1 <-
    recipe(water + fat + protein ~ ., data = meats_data$train) %>%
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
      .measures = list()
    )
  expect_equal(bake_1[0,], dat_ptype)
  measure_ptype <-
    tibble::tibble(
      value = numeric(0),
      location = numeric(0)
    )
  expect_equal(bake_1$.measures[[1]][0,], measure_ptype)
  expect_equal(nrow(bake_1), 200L)
  expect_equal(dim(bake_1$.measures[[1]]), c(100L, 2L))

  bake_1_te <- bake(prep_1, new_data = meats_data$test)
  expect_equal(bake_1_te$.measures[[1]][0,], measure_ptype)
  expect_equal(nrow(bake_1_te), 15L)
  expect_equal(dim(bake_1_te$.measures[[1]]), c(100L, 2L))

  ## missing values

  prep_2 <-
    recipe(water + fat + protein ~ ., data = na_train) %>%
    step_measure_input_wide(x_001:x_100) %>%
    prep()

  ## bad inputs
  expect_snapshot(
    recipe(water + fat + protein ~ ., data = na_train) %>%
      step_measure_input_wide(x_001:x_100, location_values = 1:2) %>%
      prep(),
    error = TRUE
  )

})
