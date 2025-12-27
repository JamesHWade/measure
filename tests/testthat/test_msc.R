test_that("MSC reference spectrum is computed correctly", {
  # Create simple test data
  test_spectra <- list(
    tibble::tibble(location = 1:5, value = c(1, 2, 3, 4, 5)),
    tibble::tibble(location = 1:5, value = c(2, 4, 6, 8, 10)),
    tibble::tibble(location = 1:5, value = c(3, 6, 9, 12, 15))
  )

  ref <- measure:::.compute_reference_spectrum(test_spectra)

  # Mean of each position: (1+2+3)/3, (2+4+6)/3, etc.
  expected_ref <- c(2, 4, 6, 8, 10)
  expect_equal(ref, expected_ref)
})

test_that("MSC single spectrum correction works", {
  # Create a reference spectrum
  ref_spectrum <- c(1, 2, 3, 4, 5)

  # Create a spectrum that is 2x the reference + 1 (slope=2, intercept=1)
  test_tibble <- tibble::tibble(
    location = 1:5,
    value = 2 * ref_spectrum + 1  # c(3, 5, 7, 9, 11)
  )

  result <- measure:::.msc_single(test_tibble, ref_spectrum)

  # MSC should recover: (values - intercept) / slope = (values - 1) / 2
  # Which should equal the reference spectrum
  expect_equal(result$value, ref_spectrum, tolerance = 1e-10)
  expect_equal(result$location, 1:5)
})

test_that("MSC works with recipe workflow (long format)", {
  rec <-
    recipe(water + fat + protein ~ ., data = meats_long) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    step_measure_msc() %>%
    prep()

  result <- bake(rec, new_data = NULL)

  # Check that .measures column exists
  expect_true(".measures" %in% names(result))

  # Check that the step stored a reference spectrum
  msc_step <- rec$steps[[2]]
  expect_true(!is.null(msc_step$ref_spectrum))
  expect_true(is.numeric(msc_step$ref_spectrum))
  expect_equal(length(msc_step$ref_spectrum), 100)  # meats has 100 channels
})

test_that("MSC works with recipe workflow (wide format)", {
  skip_if_not_installed("modeldata")
  data(meats, package = "modeldata")

  rec <-
    recipe(water + fat + protein ~ ., data = meats) %>%
    step_measure_input_wide(dplyr::starts_with("x_")) %>%
    step_measure_msc() %>%
    prep()

  result <- bake(rec, new_data = NULL)

  # Check that .measures column exists
  expect_true(".measures" %in% names(result))

  # Check reference spectrum is stored
  msc_step <- rec$steps[[2]]
  expect_true(!is.null(msc_step$ref_spectrum))
})

test_that("MSC preserves location values", {
  rec <-
    recipe(water + fat + protein ~ ., data = meats_long) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    prep()

  before <- bake(rec, new_data = NULL)

  rec_msc <-
    recipe(water + fat + protein ~ ., data = meats_long) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    step_measure_msc() %>%
    prep()

  after <- bake(rec_msc, new_data = NULL)

  # Check that locations are preserved
  for (i in seq_len(nrow(before))) {
    expect_equal(
      before$.measures[[i]]$location,
      after$.measures[[i]]$location
    )
  }
})

test_that("MSC fails without measure input step", {
  skip_if_not_installed("modeldata")
  data(meats, package = "modeldata")

  expect_error(
    recipe(water + fat + protein ~ ., data = meats) %>%
      step_measure_msc() %>%
      prep(),
    "should be in the data"
  )
})

test_that("MSC uses same reference for new data", {
  # Split the data
  train_ids <- unique(meats_long$id)[1:200]
  test_ids <- unique(meats_long$id)[201:215]

  train_data <- meats_long[meats_long$id %in% train_ids, ]
  test_data <- meats_long[meats_long$id %in% test_ids, ]

  rec <-
    recipe(water + fat + protein ~ ., data = train_data) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    step_measure_msc() %>%
    prep()

  # Get the reference spectrum from training
  ref_from_training <- rec$steps[[2]]$ref_spectrum

  # Bake on new data
  result <- bake(rec, new_data = test_data)

  # The reference spectrum should still be the same (from training)
  expect_equal(rec$steps[[2]]$ref_spectrum, ref_from_training)
  expect_true(".measures" %in% names(result))
})

test_that("MSC print method works", {
  rec <-
    recipe(water + fat + protein ~ ., data = meats_long) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    step_measure_msc()

  expect_snapshot(rec)

  rec_prep <- prep(rec)
  expect_snapshot(rec_prep)
})

test_that("MSC tidy method works", {
  rec <-
    recipe(water + fat + protein ~ ., data = meats_long) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    step_measure_msc(id = "msc_test")

  # Before prep
  tidy_before <- tidy(rec, number = 2)
  expect_equal(tidy_before$terms, ".measures")
  expect_equal(tidy_before$id, "msc_test")

  # After prep
  rec_prep <- prep(rec)
  tidy_after <- tidy(rec_prep, number = 2)
  expect_equal(tidy_after$terms, ".measures")
  expect_equal(tidy_after$id, "msc_test")
})

test_that("MSC produces expected results compared to prospectr", {
  skip_if_not_installed("prospectr")
  skip_if_not_installed("modeldata")
  data(meats, package = "modeldata")

  # Get spectra as matrix
  spectra_cols <- grep("^x_", names(meats))
  spectra_mat <- as.matrix(meats[, spectra_cols])

  # Apply MSC using prospectr
  prospectr_msc <- prospectr::msc(spectra_mat)

  # Apply MSC using measure
  rec <-
    recipe(water + fat + protein ~ ., data = meats) %>%
    step_measure_input_wide(dplyr::starts_with("x_")) %>%
    step_measure_msc() %>%
    prep()

  result <- bake(rec, new_data = NULL)

  # Convert measure result to matrix for comparison
  measure_msc <- measure:::measure_to_matrix(result$.measures)

  # Compare results (should be equal within tolerance)
  expect_equal(measure_msc, prospectr_msc, tolerance = 1e-10, ignore_attr = TRUE)
})

test_that("MSC reference spectrum matches mean of training spectra", {
  skip_if_not_installed("modeldata")
  data(meats, package = "modeldata")

  rec <-
    recipe(water + fat + protein ~ ., data = meats) %>%
    step_measure_input_wide(dplyr::starts_with("x_")) %>%
    step_measure_msc() %>%
    prep()

  # Get the stored reference spectrum
  stored_ref <- rec$steps[[2]]$ref_spectrum

  # Compute expected reference (column means of spectra)
  spectra_cols <- grep("^x_", names(meats))
  expected_ref <- colMeans(meats[, spectra_cols])

  expect_equal(stored_ref, unname(expected_ref), tolerance = 1e-10)
})
