test_that("output wide format data", {
  meats_data <- data_meat_long()
  meats_train <- meats_data$train %>% filter(ind < 1.2)
  meats_test  <- meats_data$test  %>% filter(ind < 1.2)

  na_train <- meats_train
  na_train$absorp[1] <- NA_real_
  na_test <- meats_test
  na_test$absorp[1] <- NA_real_

  miss_train <- meats_train %>% dplyr::slice(-2)
  miss_test  <- meats_test  %>% dplyr::slice(-2)

  # ----------------------------------------------------------------------------

  rec_1 <-
    recipe(water + fat + protein ~ ., data = meats_train) %>%
    step_measure_input_long(absorp, location = vars(ind), id = "potato") %>%
    step_measure_output_wide(id = "turnip")
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
      measure_1.000000 = numeric(0),
      measure_1.161616 = numeric(0)
    )
  expect_equal(bake_1[0,], dat_ptype)
  expect_equal(nrow(bake_1), 200L)

  bake_1_te <- bake(prep_1, new_data = meats_test)
  expect_equal(bake_1_te[0,], dat_ptype)
  expect_equal(nrow(bake_1_te), 15L)

  ### missing rows

  expect_snapshot_error(
    recipe(water + fat + protein ~ ., data = miss_train_padded) %>%
      step_measure_input_long(absorp, location = vars(ind)) %>%
      step_measure_output_wide(id = "turnip") %>%
      prep() %>%
      bake(new_data = NULL)
  )

  ### missing rows with padding

  bake_2 <-
    recipe(water + fat + protein ~ ., data = miss_train) %>%
    step_measure_input_long(absorp, location = vars(ind), pad = TRUE) %>%
    step_measure_output_wide(id = "turnip") %>%
    prep() %>%
    bake(new_data = NULL)
  # one NA value from padding takes complete cases from 200L -> 199L
  expect_equal(sum(complete.cases(bake_2)), 199L)
  expect_true(is.na(bake_2$measure_1.161616[1]))

  ## missing values

  bake_3 <-
    recipe(water + fat + protein ~ ., data = na_train) %>%
    step_measure_input_long(absorp, location = vars(ind)) %>%
    step_measure_output_wide(id = "turnip") %>%
    prep() %>%
    bake(new_data = NULL)
  expect_equal(sum(complete.cases(bake_3)), 199L)
  expect_true(is.na(bake_3$measure_1.000000[1]))

  ## Bad format

  expect_snapshot(
    recipe(water + fat + protein ~ ., data = meats_train) %>%
      step_measure_output_wide() %>%
      prep(),
    error = TRUE
  )
})
