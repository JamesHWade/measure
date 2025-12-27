test_that("SNV transformation computes correctly", {
  # Create a simple test case with known values
  test_values <- c(1, 2, 3, 4, 5)
  expected_mean <- mean(test_values)
  expected_sd <- sd(test_values)
  expected_snv <- (test_values - expected_mean) / expected_sd

  # Verify our expected values
  expect_equal(mean(expected_snv), 0, tolerance = 1e-10)
  expect_equal(sd(expected_snv), 1, tolerance = 1e-10)

  # Test the internal function
  test_tibble <- tibble::tibble(location = 1:5, value = test_values)
  result <- .snv_single(test_tibble)

  expect_equal(result$value, expected_snv)
  expect_equal(result$location, 1:5)
})

test_that("SNV works with recipe workflow (long format)", {
  rec <-
    recipe(water + fat + protein ~ ., data = meats_long) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    step_measure_snv() %>%
    prep()

  result <- bake(rec, new_data = NULL)

  # Check that .measures column exists
  expect_true(".measures" %in% names(result))

  # Check that each spectrum has mean ~0 and sd ~1
  for (i in seq_len(nrow(result))) {
    values <- result$.measures[[i]]$value
    expect_equal(mean(values), 0, tolerance = 1e-10)
    expect_equal(sd(values), 1, tolerance = 1e-10)
  }
})

test_that("SNV works with recipe workflow (wide format)", {
  skip_if_not_installed("modeldata")
  data(meats, package = "modeldata")

  rec <-
    recipe(water + fat + protein ~ ., data = meats) %>%
    step_measure_input_wide(dplyr::starts_with("x_")) %>%
    step_measure_snv() %>%
    prep()

  result <- bake(rec, new_data = NULL)

  # Check that .measures column exists
  expect_true(".measures" %in% names(result))

  # Check that each spectrum has mean ~0 and sd ~1
  for (i in seq_len(nrow(result))) {
    values <- result$.measures[[i]]$value
    expect_equal(mean(values), 0, tolerance = 1e-10)
    expect_equal(sd(values), 1, tolerance = 1e-10)
  }
})

test_that("SNV preserves location values", {
  rec <-
    recipe(water + fat + protein ~ ., data = meats_long) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    prep()

  before <- bake(rec, new_data = NULL)

  rec_snv <-
    recipe(water + fat + protein ~ ., data = meats_long) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    step_measure_snv() %>%
    prep()

  after <- bake(rec_snv, new_data = NULL)

  # Check that locations are preserved
  for (i in seq_len(nrow(before))) {
    expect_equal(
      before$.measures[[i]]$location,
      after$.measures[[i]]$location
    )
  }
})

test_that("SNV fails without measure input step", {
  skip_if_not_installed("modeldata")
  data(meats, package = "modeldata")

  expect_error(
    recipe(water + fat + protein ~ ., data = meats) %>%
      step_measure_snv() %>%
      prep(),
    "should be in the data"
  )
})

test_that("SNV handles constant spectrum with warning", {
  # Create data with a constant spectrum
  constant_data <- meats_long
  # Set first sample to have constant values
  first_id <- unique(constant_data$id)[1]
  constant_data$transmittance[constant_data$id == first_id] <- 5.0

  expect_warning(
    recipe(water + fat + protein ~ ., data = constant_data) %>%
      update_role(id, new_role = "id") %>%
      step_measure_input_long(transmittance, location = vars(channel)) %>%
      step_measure_snv() %>%
      prep() %>%
      bake(new_data = NULL),
    "Standard deviation is zero"
  )
})

test_that("SNV print method works", {
  rec <-
    recipe(water + fat + protein ~ ., data = meats_long) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    step_measure_snv()

  expect_snapshot(rec)

  rec_prep <- prep(rec)
  expect_snapshot(rec_prep)
})

test_that("SNV tidy method works", {
  rec <-
    recipe(water + fat + protein ~ ., data = meats_long) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    step_measure_snv(id = "snv_test")

  # Before prep
  tidy_before <- tidy(rec, number = 2)
  expect_equal(tidy_before$terms, "<all measure columns>")
  expect_equal(tidy_before$id, "snv_test")

  # After prep
  rec_prep <- prep(rec)
  tidy_after <- tidy(rec_prep, number = 2)
  expect_equal(tidy_after$terms, ".measures")
  expect_equal(tidy_after$id, "snv_test")
})

test_that("SNV works on new data", {
  # Split the data
  train_ids <- unique(meats_long$id)[1:200]
  test_ids <- unique(meats_long$id)[201:215]

  train_data <- meats_long[meats_long$id %in% train_ids, ]
  test_data <- meats_long[meats_long$id %in% test_ids, ]

  rec <-
    recipe(water + fat + protein ~ ., data = train_data) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    step_measure_snv() %>%
    prep()

  # Bake on new data
  result <- bake(rec, new_data = test_data)

  # Check that each spectrum has mean ~0 and sd ~1
  for (i in seq_len(nrow(result))) {
    values <- result$.measures[[i]]$value
    expect_equal(mean(values), 0, tolerance = 1e-10)
    expect_equal(sd(values), 1, tolerance = 1e-10)
  }
})

test_that("SNV produces expected results compared to manual calculation", {
  skip_if_not_installed("modeldata")
  data(meats, package = "modeldata")

  # Get original spectrum data for first sample
  original_spectrum <- as.numeric(meats[1, grep("^x_", names(meats))])

  # Manual SNV calculation
  manual_snv <- (original_spectrum - mean(original_spectrum)) /
    sd(original_spectrum)

  # Recipe-based SNV
  rec <-
    recipe(water + fat + protein ~ ., data = meats[1, , drop = FALSE]) %>%
    step_measure_input_wide(dplyr::starts_with("x_")) %>%
    step_measure_snv() %>%
    prep()

  result <- bake(rec, new_data = NULL)
  recipe_snv <- result$.measures[[1]]$value

  expect_equal(recipe_snv, manual_snv, tolerance = 1e-10)
})
