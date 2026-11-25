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
create_nd_test_data <- function(
  n_samples = 10,
  n_time = 5,
  n_wavelength = 4,
  seed = 42
) {
  withr::local_seed(seed)

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

create_nd_3d_test_data <- function(n_samples = 6, seed = 98) {
  withr::local_seed(seed)
  grid <- expand.grid(
    location_1 = 1:3,
    location_2 = 1:3,
    location_3 = 1:2
  )

  tibble::tibble(
    id = seq_len(n_samples),
    cube = new_measure_nd_list(lapply(seq_len(n_samples), function(i) {
      new_measure_nd_tbl(
        location_1 = grid$location_1,
        location_2 = grid$location_2,
        location_3 = grid$location_3,
        value = rnorm(nrow(grid))
      )
    }))
  )
}

# ==============================================================================
# nD array assembly tests

test_that("nD array assembly follows coordinates and validates sparse grids", {
  # Deliberately shuffle the point order so that the array is assembled from
  # coordinates rather than relying on the row order of each measurement.
  shuffled <- new_measure_nd_tbl(
    location_1 = c(20, 10, 20, 10, 20, 10),
    location_2 = c(300, 200, 100, 100, 200, 300),
    value = c(6, 1, 5, 2, 4, 3)
  )
  complete <- new_measure_nd_tbl(
    location_1 = c(10, 10, 10, 20, 20, 20),
    location_2 = c(100, 200, 300, 100, 200, 300),
    value = 11:16
  )

  assembled <- .nd_list_to_array(new_measure_nd_list(list(shuffled, complete)))

  expect_equal(unname(dim(assembled$array)), c(2L, 2L, 3L))
  expect_equal(assembled$grid_info$location_1, c(10, 20))
  expect_equal(assembled$grid_info$location_2, c(100, 200, 300))
  actual <- assembled$array[1, , ]
  dim(actual) <- unname(dim(actual))
  expect_equal(
    actual,
    matrix(c(2, 1, 3, 5, 4, 6), nrow = 2, byrow = TRUE)
  )

  one_missing <- new_measure_nd_tbl(
    location_1 = c(10, 10, 10, 20, 20),
    location_2 = c(100, 200, 300, 100, 200),
    value = 21:25
  )
  expect_warning(
    sparse <- .nd_list_to_array(new_measure_nd_list(list(
      complete,
      one_missing
    ))),
    "missing"
  )
  expect_true(anyNA(sparse$array))

  two_missing <- new_measure_nd_tbl(
    location_1 = c(10, 10, 10, 20),
    location_2 = c(100, 200, 300, 100),
    value = 31:34
  )
  expect_error(
    .nd_list_to_array(new_measure_nd_list(list(complete, two_missing))),
    "missing values"
  )
})

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

  # Trained printing includes the selected measurement column, and tidy()
  # exposes every fitted mode when requested.
  expect_output(print(rec$steps[[1]]), "on spectrum")
  tidy_loadings <- recipes::tidy(rec, number = 1, type = "loadings")
  expect_true(all(
    c("column", "mode", "location", "id") %in% names(tidy_loadings)
  ))
  expect_setequal(unique(tidy_loadings$mode), 1:3)
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

test_that("PARAFAC projection solves overlapping component loadings jointly", {
  axis <- expand.grid(location_1 = 1:2, location_2 = 1:2)
  factors <- cbind(c(1, 0), c(1, 1))
  measure <- new_measure_nd_list(list(
    new_measure_nd_tbl(
      location_1 = axis$location_1,
      location_2 = axis$location_2,
      value = c(5, 3, 3, 3)
    )
  ))

  scores <- .project_parafac(
    measure,
    loadings = list(matrix(0, 1, 2), factors, factors),
    center_values = NULL,
    scale_values = NULL,
    grid_info = NULL
  )

  # The signal is exactly 2 * outer(factors[, 1], factors[, 1]) +
  # 3 * outer(factors[, 2], factors[, 2]). Separate normalized inner
  # products incorrectly return 5 and 3.5 for this nonorthogonal case.
  expect_equal(unname(scores), matrix(c(2, 3), nrow = 1), tolerance = 1e-12)
})

test_that("PARAFAC projection uses a minimum-norm solution for rank deficiency", {
  grid <- expand.grid(location_1 = 1:2, location_2 = 1:2)
  measure <- new_measure_nd_list(list(
    new_measure_nd_tbl(
      location_1 = grid$location_1,
      location_2 = grid$location_2,
      value = rep(5, nrow(grid))
    )
  ))
  duplicate <- matrix(1, nrow = 2, ncol = 2)

  scores <- .project_parafac(
    measure,
    loadings = list(matrix(0, 1, 2), duplicate, duplicate),
    center_values = NULL,
    scale_values = NULL,
    grid_info = NULL
  )

  # Identical content factors identify only the sum of the component scores;
  # the documented SVD projection chooses the minimum-norm split.
  expect_equal(unname(scores), matrix(c(2.5, 2.5), nrow = 1), tolerance = 1e-12)
})

test_that("PARAFAC can fit and project a training tensor with a missing cell", {
  skip_if_not_installed("multiway")
  set.seed(952)
  grid <- expand.grid(x = 1:5, y = 1:4)
  samples <- lapply(1:10, function(i) {
    values <- i * (grid$x + 1) * (grid$y + 2)
    if (i == 2L) {
      values[3L] <- NA_real_
    }
    new_measure_nd_tbl(location_1 = grid$x, location_2 = grid$y, value = values)
  })
  data <- tibble::tibble(signal = new_measure_nd_list(samples))
  rec <- recipes::recipe(~., data) |>
    step_measure_parafac(signal, n_components = 1L, center = FALSE)
  # Both fitting and retained training scores see the same missing cell.
  suppressWarnings(fit <- recipes::prep(rec))
  scores <- recipes::bake(fit, new_data = NULL)$parafac_signal_1
  expect_equal(scores / scores[1], 1:10, tolerance = 1e-5)
})

test_that("PARAFAC projection omits missing held-out cells", {
  grid <- expand.grid(location_1 = 1:5, location_2 = 1:5)
  factors <- list(
    cbind(c(1, 0, 0, 1, 0), c(1, 1, 0, 0, 1)),
    cbind(c(0, 1, 1, 0, 1), c(1, 0, 1, 1, 0))
  )
  signal <- outer(factors[[1]][, 1], factors[[2]][, 1]) *
    2 +
    outer(factors[[1]][, 2], factors[[2]][, 2]) * 3
  values <- as.vector(signal)
  values[13] <- NA_real_
  measure <- new_measure_nd_list(list(
    new_measure_nd_tbl(
      location_1 = grid$location_1,
      location_2 = grid$location_2,
      value = values
    )
  ))

  scores <- suppressWarnings({
    .project_parafac(
      measure,
      loadings = list(matrix(0, 1, 2), factors[[1]], factors[[2]]),
      center_values = NULL,
      scale_values = NULL,
      grid_info = list(location_1 = 1:5, location_2 = 1:5)
    )
  })

  expect_equal(unname(scores), matrix(c(2, 3), nrow = 1), tolerance = 1e-12)
})

test_that("PARAFAC projection returns zero for an all-zero loading design", {
  grid <- expand.grid(location_1 = 1:2, location_2 = 1:2)
  measure <- new_measure_nd_list(list(
    new_measure_nd_tbl(
      location_1 = grid$location_1,
      location_2 = grid$location_2,
      value = 1:4
    )
  ))
  loadings <- list(
    matrix(1, nrow = 1, ncol = 1),
    matrix(0, nrow = 2, ncol = 1),
    matrix(0, nrow = 2, ncol = 1)
  )

  scores <- .project_parafac(
    measure,
    loadings = loadings,
    center_values = NULL,
    scale_values = NULL,
    grid_info = NULL
  )

  expect_equal(scores, matrix(0, nrow = 1, ncol = 1))
})

test_that("PARAFAC projection includes the fourth tensor mode", {
  grid <- expand.grid(
    location_1 = 1:3,
    location_2 = 1:3,
    location_3 = 1:2
  )
  factors <- list(
    cbind(c(1, 0, 1), c(1, 1, 0)),
    cbind(c(0, 1, 1), c(1, 0, 1)),
    cbind(c(1, 1), c(1, -1))
  )
  rank_one <- function(component) {
    array(
      as.vector(
        outer(
          outer(factors[[1]][, component], factors[[2]][, component]),
          factors[[3]][, component]
        )
      ),
      dim = c(3, 3, 2)
    )
  }
  signal <- 2 * rank_one(1) + 3 * rank_one(2)
  measure <- new_measure_nd_list(list(
    new_measure_nd_tbl(
      location_1 = grid$location_1,
      location_2 = grid$location_2,
      location_3 = grid$location_3,
      value = as.vector(signal)
    )
  ))

  scores <- .project_parafac(
    measure,
    loadings = list(
      matrix(0, 1, 2),
      factors[[1]],
      factors[[2]],
      factors[[3]]
    ),
    center_values = NULL,
    scale_values = NULL,
    grid_info = list(
      location_1 = 1:3,
      location_2 = 1:3,
      location_3 = 1:2
    )
  )

  expect_equal(unname(scores), matrix(c(2, 3), nrow = 1), tolerance = 1e-12)
})

test_that("step_measure_parafac supports 3D prep and coordinate-ordered bake", {
  skip_if_not_installed("multiway")

  test_data <- create_nd_3d_test_data()
  suppressWarnings({
    rec <- recipes::recipe(~., data = test_data) |>
      step_measure_parafac(cube, n_components = 2, center = FALSE) |>
      recipes::prep()
  })

  expect_length(rec$steps[[1]]$loadings$cube, 4)
  expect_equal(
    vapply(rec$steps[[1]]$loadings$cube, nrow, integer(1)),
    c(6L, 3L, 3L, 2L)
  )

  held_out <- test_data[1:2, ]
  held_out$cube <- new_measure_nd_list(lapply(held_out$cube, function(m) {
    order <- rev(seq_len(nrow(m)))
    new_measure_nd_tbl(
      location_1 = m$location_1[order],
      location_2 = m$location_2[order],
      location_3 = m$location_3[order],
      value = m$value[order]
    )
  }))
  baked <- recipes::bake(rec, new_data = held_out)

  expect_equal(nrow(baked), 2L)
  expect_true(all(paste0("parafac_cube_", 1:2) %in% names(baked)))
  expect_false("cube" %in% names(baked))

  shifted <- held_out
  shifted$cube <- new_measure_nd_list(lapply(shifted$cube, function(m) {
    new_measure_nd_tbl(
      location_1 = m$location_1 + 100,
      location_2 = m$location_2,
      location_3 = m$location_3,
      value = m$value
    )
  }))
  expect_error(recipes::bake(rec, new_data = shifted), "grid|coordinate")
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
      step_measure_tucker(spectrum, ranks = c(3, 2), scale = TRUE) |>
      recipes::prep()
  })

  # Check loadings and core are extracted
  expect_true(!is.null(rec$steps[[1]]$loadings))
  expect_true(!is.null(rec$steps[[1]]$core))
  expect_true("spectrum" %in% names(rec$steps[[1]]$loadings))
  expect_true(!is.null(rec$steps[[1]]$scale_values$spectrum))
  expect_output(print(rec$steps[[1]]), "on spectrum")
  tidy_loadings <- recipes::tidy(rec, number = 1, type = "loadings")
  expect_true(all(
    c("column", "mode", "location", "id") %in% names(tidy_loadings)
  ))
  expect_setequal(unique(tidy_loadings$mode), 1:3)
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

test_that("Tucker validates the number of content ranks", {
  test_data <- create_nd_test_data(n_samples = 5)

  expect_error(
    .fit_tucker(
      test_data$spectrum,
      ranks = c(2, 2, 2),
      center = FALSE,
      scale = FALSE,
      max_iter = 1L,
      tol = 1e-6
    ),
    "must match number of dimensions"
  )
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
  withr::local_options("rlang:::message_always" = TRUE)
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
  expect_output(print(rec$steps[[1]]), "on spectrum")
})

test_that("MCR-ALS selects nD columns by default and reports iteration limits", {
  test_data <- create_nd_test_data(n_samples = 6)

  suppressWarnings({
    rec <- recipes::recipe(outcome ~ ., data = test_data) |>
      step_measure_mcr_als(n_components = 2) |>
      recipes::prep()
  })
  expect_equal(rec$steps[[1]]$measure_cols, "spectrum")

  expect_warning(
    fit <- .fit_mcr_als(
      test_data$spectrum,
      n_components = 2,
      max_iter = 1L,
      tol = 1e-12,
      non_negativity = TRUE,
      unimodality = FALSE
    ),
    "did not converge"
  )
  expect_equal(dim(fit$spectra), c(4L, 2L))
  expect_equal(unname(dim(fit$concentrations)), c(6L, 5L, 2L))
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

  # Without any measure columns, check_for_measure will fail first
  expect_error(recipes::prep(rec), "should be in the data")
})

test_that("multi-way steps reject scalar selections", {
  skip_if_not_installed("multiway")
  rec <- recipes::recipe(outcome ~ ., data = create_nd_test_data(n_samples = 4))
  for (step in list(
    step_measure_mcr_als,
    step_measure_parafac,
    step_measure_tucker
  )) {
    expect_error(recipes::prep(step(rec, id)), "not nD measure column")
  }
})

check_scaled_projection <- function(recipe, score_cols) {
  fit <- suppressWarnings(recipes::prep(recipe))
  held_out <- create_nd_test_data(n_samples = 3, seed = 717)
  scaled <- held_out
  scaled$spectrum <- new_measure_nd_list(lapply(held_out$spectrum, function(m) {
    m$value <- 1.75 * m$value
    m
  }))
  original <- recipes::bake(fit, new_data = held_out)
  transformed <- recipes::bake(fit, new_data = scaled)
  scores <- as.matrix(original[score_cols])
  expect_equal(original$id, held_out$id)
  expect_false("spectrum" %in% names(original))
  expect_gt(max(abs(scores)), 0)
  expect_equal(as.matrix(transformed[score_cols]), 1.75 * scores)
}

test_that("MCR-ALS projects held-out spectra linearly onto fixed components", {
  rec <- recipes::recipe(outcome ~ ., data = create_nd_test_data(n_samples = 8))
  check_scaled_projection(
    step_measure_mcr_als(rec, spectrum, n_components = 2),
    paste0("mcr_spectrum_", 1:2)
  )
})

test_that("uncentered Tucker and PARAFAC projections scale with held-out signal", {
  skip_if_not_installed("multiway")
  rec <- recipes::recipe(outcome ~ ., data = create_nd_test_data(n_samples = 8))
  check_scaled_projection(
    step_measure_tucker(rec, ranks = c(2, 2), center = FALSE, scale = TRUE),
    paste0("tucker_spectrum_", 1:4)
  )
  check_scaled_projection(
    step_measure_parafac(rec, spectrum, n_components = 2, center = FALSE),
    paste0("parafac_spectrum_", 1:2)
  )
})
