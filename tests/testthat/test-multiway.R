# ==============================================================================
# Tests for Multi-Way Analysis Steps
#
# PARAFAC, Tucker, and MCR-ALS decomposition for multi-dimensional data.
# These tests require the 'multiway' package which is in Suggests.
# ==============================================================================

# ------------------------------------------------------------------------------
# Test data helpers
# ------------------------------------------------------------------------------

# Create test data: 3D array-like structure (samples x time x wavelength)
create_nd_test_data <- function(n_samples = 10, n_time = 5, n_wavelength = 4) {
  set.seed(42)

  # Simulate 3-component mixture data
  # Component profiles
  time_grid <- seq(0, 1, length.out = n_time)
  wavelength_grid <- seq(400, 500, length.out = n_wavelength)

  # Component time profiles (peaks at different times)
  C1_time <- dnorm(time_grid, mean = 0.3, sd = 0.15)
  C2_time <- dnorm(time_grid, mean = 0.5, sd = 0.15)
  C3_time <- dnorm(time_grid, mean = 0.7, sd = 0.15)

  # Component spectral profiles (peaks at different wavelengths)
  C1_wave <- dnorm(wavelength_grid, mean = 420, sd = 20)
  C2_wave <- dnorm(wavelength_grid, mean = 450, sd = 20)
  C3_wave <- dnorm(wavelength_grid, mean = 480, sd = 20)

  # Generate samples with random concentrations
  data_list <- list()
  concentrations <- matrix(NA, n_samples, 3)

  for (i in seq_len(n_samples)) {
    # Random concentrations
    conc <- c(runif(1, 0.5, 2), runif(1, 0.5, 2), runif(1, 0.5, 2))
    concentrations[i, ] <- conc

    # Build 2D measurement (time x wavelength)
    signal <- conc[1] *
      outer(C1_time, C1_wave) +
      conc[2] * outer(C2_time, C2_wave) +
      conc[3] * outer(C3_time, C3_wave) +
      matrix(rnorm(n_time * n_wavelength, 0, 0.01), n_time, n_wavelength)

    # Convert to long format with explicit location vectors
    grid <- expand.grid(
      loc1 = time_grid,
      loc2 = wavelength_grid
    )

    # Create measure_nd_tbl with proper named arguments
    data_list[[i]] <- new_measure_nd_tbl(
      location_1 = grid$loc1,
      location_2 = grid$loc2,
      value = as.vector(signal)
    )
  }

  # Create measure_nd_list
  nd_list <- new_measure_nd_list(data_list)

  tibble::tibble(
    id = seq_len(n_samples),
    spectrum = nd_list,
    conc_1 = concentrations[, 1],
    conc_2 = concentrations[, 2],
    conc_3 = concentrations[, 3],
    outcome = concentrations[, 1] + 0.5 * concentrations[, 2]
  )
}

# ==============================================================================
# PARAFAC Tests
# ==============================================================================

test_that("step_measure_parafac requires multiway package", {
  skip_if_not_installed("multiway")

  # If multiway is installed, just verify we can load it
  expect_true(requireNamespace("multiway", quietly = TRUE))
})

test_that("step_measure_parafac constructor works", {
  skip_if_not_installed("multiway")

  test_data <- create_nd_test_data(n_samples = 6)

  rec <- recipes::recipe(outcome ~ ., data = test_data) |>
    step_measure_parafac(spectrum, n_components = 2)

  expect_s3_class(rec, "recipe")
  expect_equal(length(rec$steps), 1)
  expect_s3_class(rec$steps[[1]], "step_measure_parafac")
  expect_equal(rec$steps[[1]]$n_components, 2L)
  expect_true(rec$steps[[1]]$center)
  expect_false(rec$steps[[1]]$scale)
})

test_that("step_measure_parafac prep extracts components", {
  skip_if_not_installed("multiway")

  test_data <- create_nd_test_data(n_samples = 8)

  suppressWarnings({
    rec <- recipes::recipe(outcome ~ ., data = test_data) |>
      step_measure_parafac(spectrum, n_components = 2) |>
      recipes::prep()
  })

  # Check step is trained
  expect_true(rec$steps[[1]]$trained)

  # Check loadings are extracted
  expect_true(!is.null(rec$steps[[1]]$loadings))
  expect_true("spectrum" %in% names(rec$steps[[1]]$loadings))
})

test_that("step_measure_parafac bake produces score columns", {
  skip_if_not_installed("multiway")

  test_data <- create_nd_test_data(n_samples = 8)

  suppressWarnings({
    rec <- recipes::recipe(outcome ~ ., data = test_data) |>
      step_measure_parafac(spectrum, n_components = 2) |>
      recipes::prep()
  })

  baked <- recipes::bake(rec, new_data = NULL)

  # Check score columns exist

  expect_true("parafac_spectrum_1" %in% names(baked))
  expect_true("parafac_spectrum_2" %in% names(baked))

  # Check original column removed
  expect_false("spectrum" %in% names(baked))

  # Check correct number of rows
  expect_equal(nrow(baked), 8)
})

test_that("step_measure_parafac works with centering/scaling options", {
  skip_if_not_installed("multiway")

  test_data <- create_nd_test_data(n_samples = 8)

  suppressWarnings({
    rec_centered <- recipes::recipe(outcome ~ ., data = test_data) |>
      step_measure_parafac(
        spectrum,
        n_components = 2,
        center = TRUE,
        scale = FALSE
      ) |>
      recipes::prep()

    rec_scaled <- recipes::recipe(outcome ~ ., data = test_data) |>
      step_measure_parafac(
        spectrum,
        n_components = 2,
        center = TRUE,
        scale = TRUE
      ) |>
      recipes::prep()
  })

  baked_centered <- recipes::bake(rec_centered, new_data = NULL)
  baked_scaled <- recipes::bake(rec_scaled, new_data = NULL)

  # Both should produce valid scores
  expect_equal(nrow(baked_centered), 8)
  expect_equal(nrow(baked_scaled), 8)

  # Scores should differ between centered and scaled versions
  # (not identical, but structure should be similar)
  expect_true(
    !identical(
      baked_centered$parafac_spectrum_1,
      baked_scaled$parafac_spectrum_1
    )
  )
})

test_that("step_measure_parafac tidy returns parameters", {
  skip_if_not_installed("multiway")

  test_data <- create_nd_test_data(n_samples = 8)

  rec_untrained <- recipes::recipe(outcome ~ ., data = test_data) |>
    step_measure_parafac(spectrum, n_components = 3)

  # Untrained tidy
  tidy_untrained <- recipes::tidy(rec_untrained, number = 1)
  expect_true("n_components" %in% names(tidy_untrained))
  expect_equal(tidy_untrained$n_components, 3)

  # Trained tidy
  suppressWarnings({
    rec_trained <- recipes::prep(rec_untrained)
  })

  tidy_trained <- recipes::tidy(rec_trained, number = 1)
  expect_true("terms" %in% names(tidy_trained))
  expect_equal(tidy_trained$terms, "spectrum")
})

test_that("step_measure_parafac print method works", {
  skip_if_not_installed("multiway")

  test_data <- create_nd_test_data(n_samples = 6)

  rec <- recipes::recipe(outcome ~ ., data = test_data) |>
    step_measure_parafac(spectrum, n_components = 3)

  expect_output(print(rec$steps[[1]]), "PARAFAC")
  expect_output(print(rec$steps[[1]]), "3 components")
})

# ==============================================================================
# Tucker Tests
# ==============================================================================

test_that("step_measure_tucker constructor works", {
  skip_if_not_installed("multiway")

  test_data <- create_nd_test_data(n_samples = 6)

  rec <- recipes::recipe(outcome ~ ., data = test_data) |>
    step_measure_tucker(spectrum, ranks = c(3, 2))

  expect_s3_class(rec, "recipe")
  expect_equal(length(rec$steps), 1)
  expect_s3_class(rec$steps[[1]], "step_measure_tucker")
  expect_equal(rec$steps[[1]]$ranks, c(3L, 2L))
})

test_that("step_measure_tucker with single rank expands to all modes", {
  skip_if_not_installed("multiway")

  test_data <- create_nd_test_data(n_samples = 8)

  suppressWarnings({
    rec <- recipes::recipe(outcome ~ ., data = test_data) |>
      step_measure_tucker(spectrum, ranks = 2) |>
      recipes::prep()
  })

  expect_true(rec$steps[[1]]$trained)
})

test_that("step_measure_tucker prep extracts loadings and core", {
  skip_if_not_installed("multiway")

  test_data <- create_nd_test_data(n_samples = 8)

  suppressWarnings({
    rec <- recipes::recipe(outcome ~ ., data = test_data) |>
      step_measure_tucker(spectrum, ranks = c(3, 2)) |>
      recipes::prep()
  })

  # Check loadings and core are extracted
  expect_true(!is.null(rec$steps[[1]]$loadings))
  expect_true(!is.null(rec$steps[[1]]$core))
  expect_true("spectrum" %in% names(rec$steps[[1]]$loadings))
})

test_that("step_measure_tucker bake produces score columns", {
  skip_if_not_installed("multiway")

  test_data <- create_nd_test_data(n_samples = 8)

  suppressWarnings({
    rec <- recipes::recipe(outcome ~ ., data = test_data) |>
      step_measure_tucker(spectrum, ranks = c(2, 2)) |>
      recipes::prep()
  })

  baked <- recipes::bake(rec, new_data = NULL)

  # Tucker with ranks (2, 2) should produce 2*2 = 4 scores
  expect_true("tucker_spectrum_1" %in% names(baked))
  expect_true("tucker_spectrum_4" %in% names(baked))

  # Check original column removed
  expect_false("spectrum" %in% names(baked))

  expect_equal(nrow(baked), 8)
})

test_that("step_measure_tucker print method works", {
  skip_if_not_installed("multiway")

  test_data <- create_nd_test_data(n_samples = 6)

  rec <- recipes::recipe(outcome ~ ., data = test_data) |>
    step_measure_tucker(spectrum, ranks = c(3, 2))

  expect_output(print(rec$steps[[1]]), "Tucker")
  expect_output(print(rec$steps[[1]]), "3")
})

test_that("step_measure_tucker tidy returns parameters", {
  skip_if_not_installed("multiway")

  test_data <- create_nd_test_data(n_samples = 8)

  rec_untrained <- recipes::recipe(outcome ~ ., data = test_data) |>
    step_measure_tucker(spectrum, ranks = c(3, 2))

  tidy_untrained <- recipes::tidy(rec_untrained, number = 1)
  expect_true("ranks" %in% names(tidy_untrained))

  suppressWarnings({
    rec_trained <- recipes::prep(rec_untrained)
  })

  tidy_trained <- recipes::tidy(rec_trained, number = 1)
  expect_true("terms" %in% names(tidy_trained))
})

# ==============================================================================
# MCR-ALS Tests
# ==============================================================================

test_that("step_measure_mcr_als constructor works", {
  test_data <- create_nd_test_data(n_samples = 6)

  # MCR-ALS emits a warning about being experimental
  expect_warning(
    rec <- recipes::recipe(outcome ~ ., data = test_data) |>
      step_measure_mcr_als(spectrum, n_components = 2),
    "experimental"
  )

  expect_s3_class(rec, "recipe")
  expect_s3_class(rec$steps[[1]], "step_measure_mcr_als")
  expect_equal(rec$steps[[1]]$n_components, 2L)
  expect_true(rec$steps[[1]]$non_negativity)
  expect_false(rec$steps[[1]]$unimodality)
})

test_that("step_measure_mcr_als prep extracts spectra", {
  test_data <- create_nd_test_data(n_samples = 8)

  suppressWarnings({
    rec <- recipes::recipe(outcome ~ ., data = test_data) |>
      step_measure_mcr_als(spectrum, n_components = 2) |>
      recipes::prep()
  })

  expect_true(rec$steps[[1]]$trained)
  expect_true(!is.null(rec$steps[[1]]$spectra))
  expect_true("spectrum" %in% names(rec$steps[[1]]$spectra))
})

test_that("step_measure_mcr_als bake produces score columns", {
  test_data <- create_nd_test_data(n_samples = 8)

  suppressWarnings({
    rec <- recipes::recipe(outcome ~ ., data = test_data) |>
      step_measure_mcr_als(spectrum, n_components = 2) |>
      recipes::prep()
  })

  baked <- recipes::bake(rec, new_data = NULL)

  expect_true("mcr_spectrum_1" %in% names(baked))
  expect_true("mcr_spectrum_2" %in% names(baked))
  expect_false("spectrum" %in% names(baked))
  expect_equal(nrow(baked), 8)
})

test_that("step_measure_mcr_als works with non-negativity constraint", {
  test_data <- create_nd_test_data(n_samples = 8)

  suppressWarnings({
    rec <- recipes::recipe(outcome ~ ., data = test_data) |>
      step_measure_mcr_als(
        spectrum,
        n_components = 2,
        non_negativity = TRUE
      ) |>
      recipes::prep()
  })

  # Check that spectra are non-negative
  spectra <- rec$steps[[1]]$spectra$spectrum
  expect_true(all(spectra >= 0))
})

test_that("step_measure_mcr_als print method works", {
  test_data <- create_nd_test_data(n_samples = 6)

  suppressWarnings({
    rec <- recipes::recipe(outcome ~ ., data = test_data) |>
      step_measure_mcr_als(spectrum, n_components = 3)
  })

  expect_output(print(rec$steps[[1]]), "MCR-ALS")
  expect_output(print(rec$steps[[1]]), "3 components")
  expect_output(print(rec$steps[[1]]), "non-neg")
})

test_that("step_measure_mcr_als tidy returns parameters and spectra", {
  test_data <- create_nd_test_data(n_samples = 8)

  suppressWarnings({
    rec_untrained <- recipes::recipe(outcome ~ ., data = test_data) |>
      step_measure_mcr_als(spectrum, n_components = 2)
  })

  tidy_untrained <- recipes::tidy(rec_untrained, number = 1)
  expect_true("n_components" %in% names(tidy_untrained))

  suppressWarnings({
    rec_trained <- recipes::prep(rec_untrained)
  })

  # Default tidy returns parameters
  tidy_params <- recipes::tidy(rec_trained, number = 1)
  expect_true("terms" %in% names(tidy_params))
  expect_true("non_negativity" %in% names(tidy_params))

  # tidy with type = "spectra" returns extracted spectra
  tidy_spectra <- recipes::tidy(rec_trained, number = 1, type = "spectra")
  expect_true("component_1" %in% names(tidy_spectra))
  expect_true("column" %in% names(tidy_spectra))
})

# ==============================================================================
# Integration Tests
# ==============================================================================

test_that("multi-way steps work in pipeline with other steps", {
  skip_if_not_installed("multiway")

  test_data <- create_nd_test_data(n_samples = 10)

  # PARAFAC in a pipeline
  suppressWarnings({
    rec <- recipes::recipe(outcome ~ ., data = test_data) |>
      recipes::step_rm(conc_1, conc_2, conc_3) |>
      step_measure_parafac(spectrum, n_components = 2) |>
      recipes::prep()
  })

  baked <- recipes::bake(rec, new_data = NULL)

  # Should have: id, parafac_spectrum_1, parafac_spectrum_2, outcome
  expect_true(all(
    c("id", "parafac_spectrum_1", "parafac_spectrum_2", "outcome") %in%
      names(baked)
  ))
  expect_false("spectrum" %in% names(baked))
})

test_that("multi-way steps fail gracefully without nD data", {
  skip_if_not_installed("multiway")

  # Data without nD measure columns
  simple_data <- tibble::tibble(
    x = 1:10,
    y = rnorm(10)
  )

  rec <- recipes::recipe(y ~ ., data = simple_data) |>
    step_measure_parafac(n_components = 2)

  expect_error(recipes::prep(rec), "No nD measure columns")
})

test_that("multi-way steps handle new data projection correctly", {
  skip_if_not_installed("multiway")

  # Create training and test data
  train_data <- create_nd_test_data(n_samples = 10)
  test_data <- create_nd_test_data(n_samples = 5)

  suppressWarnings({
    rec <- recipes::recipe(outcome ~ ., data = train_data) |>
      step_measure_parafac(spectrum, n_components = 2) |>
      recipes::prep()
  })

  # Bake on training data
  baked_train <- recipes::bake(rec, new_data = NULL)

  # Bake on test data (using trained loadings)
  baked_test <- recipes::bake(rec, new_data = test_data)

  expect_equal(nrow(baked_train), 10)
  expect_equal(nrow(baked_test), 5)
  expect_equal(names(baked_train), names(baked_test))
})
