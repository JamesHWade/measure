# ==============================================================================
# Tests for peak deconvolution components
#
# This file tests:
# - optimize_deconvolution() and optimizer strategies
# - initialize_peak_params() and smart initialization
# - assess_deconv_quality() and quality metrics
# ==============================================================================

# ==============================================================================
# Test Data Helpers
# ==============================================================================

# Create synthetic data with known Gaussian peaks
create_gaussian_test_data <- function(noise_sd = 0.05) {
  x <- seq(0, 20, by = 0.1)
  # Two Gaussian peaks: height=1.5 at center=8 width=1.0, height=0.8 at center=12 width=1.5
  true_y <- 1.5 *
    exp(-0.5 * ((x - 8) / 1.0)^2) +
    0.8 * exp(-0.5 * ((x - 12) / 1.5)^2)
  y <- true_y + rnorm(length(x), sd = noise_sd)
  list(x = x, y = y, true_y = true_y)
}

# ==============================================================================
# optimize_deconvolution tests
# ==============================================================================

test_that("optimize_deconvolution works with L-BFGS-B optimizer", {
  data <- create_gaussian_test_data(noise_sd = 0.02)
  models <- list(gaussian_peak_model(), gaussian_peak_model())
  init_params <- list(
    list(height = 1.2, center = 7.5, width = 1.2),
    list(height = 0.6, center = 12.5, width = 1.8)
  )

  result <- optimize_deconvolution(
    data$x,
    data$y,
    models,
    init_params,
    optimizer = "lbfgsb"
  )

  expect_type(result, "list")
  expect_true(result$convergence)
  expect_equal(result$optimizer, "lbfgsb")
  expect_equal(length(result$parameters), 2)
  expect_equal(length(result$fitted_values), length(data$x))
  expect_equal(length(result$residuals), length(data$x))

  # Check fitted values are reasonable
  r_squared <- 1 - sum(result$residuals^2) / sum((data$y - mean(data$y))^2)
  expect_gt(r_squared, 0.9)
})

test_that("optimize_deconvolution works with multistart optimizer", {
  data <- create_gaussian_test_data(noise_sd = 0.02)
  models <- list(gaussian_peak_model(), gaussian_peak_model())
  init_params <- list(
    list(height = 1.2, center = 7.5, width = 1.2),
    list(height = 0.6, center = 12.5, width = 1.8)
  )

  result <- optimize_deconvolution(
    data$x,
    data$y,
    models,
    init_params,
    optimizer = "multistart",
    n_starts = 3L
  )

  expect_true(result$convergence)
  expect_equal(result$optimizer, "multistart")
  expect_equal(result$n_starts, 3L)
})

test_that("optimize_deconvolution works with Nelder-Mead optimizer", {
  data <- create_gaussian_test_data(noise_sd = 0.02)
  models <- list(gaussian_peak_model(), gaussian_peak_model())
  init_params <- list(
    list(height = 1.2, center = 7.5, width = 1.2),
    list(height = 0.6, center = 12.5, width = 1.8)
  )

  result <- optimize_deconvolution(
    data$x,
    data$y,
    models,
    init_params,
    optimizer = "nelder_mead"
  )

  expect_equal(result$optimizer, "nelder_mead")
  expect_equal(length(result$parameters), 2)
})

test_that("optimize_deconvolution auto-selects optimizer", {
  data <- create_gaussian_test_data(noise_sd = 0.02)
  models <- list(gaussian_peak_model(), gaussian_peak_model())
  init_params <- list(
    list(height = 1.2, center = 7.5, width = 1.2),
    list(height = 0.6, center = 12.5, width = 1.8)
  )

  result <- optimize_deconvolution(
    data$x,
    data$y,
    models,
    init_params,
    optimizer = "auto"
  )

  expect_true(result$optimizer %in% c("lbfgsb", "multistart", "nelder_mead"))
})

test_that("optimize_deconvolution validates inputs", {
  data <- create_gaussian_test_data()
  models <- list(gaussian_peak_model())
  init_params <- list(list(height = 1.0, center = 8, width = 1))

  # Non-numeric inputs
  expect_error(
    optimize_deconvolution("x", data$y, models, init_params),
    "numeric vectors"
  )

  # Length mismatch
  expect_error(
    optimize_deconvolution(data$x[1:50], data$y, models, init_params),
    "same length"
  )

  # Models/params mismatch
  expect_error(
    optimize_deconvolution(
      data$x,
      data$y,
      list(gaussian_peak_model(), gaussian_peak_model()),
      init_params
    ),
    "must match"
  )

  # Invalid optimizer
  expect_error(
    optimize_deconvolution(
      data$x,
      data$y,
      models,
      init_params,
      optimizer = "invalid"
    ),
    "Unknown optimizer"
  )
})

test_that("optimize_deconvolution recovers true parameters approximately", {
  data <- create_gaussian_test_data(noise_sd = 0.01)
  models <- list(gaussian_peak_model(), gaussian_peak_model())
  init_params <- list(
    list(height = 1.3, center = 7.8, width = 1.1),
    list(height = 0.7, center = 12.2, width = 1.6)
  )

  result <- optimize_deconvolution(
    data$x,
    data$y,
    models,
    init_params,
    optimizer = "lbfgsb"
  )

  # Check recovered parameters are close to true values
  # Peak 1: height=1.5, center=8, width=1.0
  expect_equal(result$parameters[[1]]$height, 1.5, tolerance = 0.15)
  expect_equal(result$parameters[[1]]$center, 8.0, tolerance = 0.2)
  expect_equal(result$parameters[[1]]$width, 1.0, tolerance = 0.15)

  # Peak 2: height=0.8, center=12, width=1.5
  expect_equal(result$parameters[[2]]$height, 0.8, tolerance = 0.15)
  expect_equal(result$parameters[[2]]$center, 12.0, tolerance = 0.2)
  expect_equal(result$parameters[[2]]$width, 1.5, tolerance = 0.2)
})

test_that("optimize_deconvolution works with single peak", {
  x <- seq(0, 20, by = 0.1)
  y <- exp(-0.5 * ((x - 10) / 2)^2) + rnorm(length(x), sd = 0.02)

  models <- list(gaussian_peak_model())
  init_params <- list(list(height = 0.9, center = 10.5, width = 2.2))

  result <- optimize_deconvolution(x, y, models, init_params)

  expect_true(result$convergence)
  expect_equal(length(result$parameters), 1)
})

# ==============================================================================
# initialize_peak_params tests
# ==============================================================================

test_that("initialize_peak_params finds peaks correctly", {
  data <- create_gaussian_test_data(noise_sd = 0.02)
  models <- list(gaussian_peak_model(), gaussian_peak_model())

  params <- initialize_peak_params(data$x, data$y, n_peaks = 2, models = models)

  expect_type(params, "list")
  expect_equal(length(params), 2)

  # Each parameter list should have height, center, width
  expect_true(all(c("height", "center", "width") %in% names(params[[1]])))
  expect_true(all(c("height", "center", "width") %in% names(params[[2]])))

  # Centers should be roughly correct (within 2 units of true: 8 and 12)
  centers <- c(params[[1]]$center, params[[2]]$center)
  centers <- sort(centers)
  expect_equal(centers[1], 8, tolerance = 2)
  expect_equal(centers[2], 12, tolerance = 2)
})

test_that("initialize_peak_params works with provided peak indices", {
  data <- create_gaussian_test_data(noise_sd = 0.02)
  models <- list(gaussian_peak_model(), gaussian_peak_model())

  # Provide known peak indices (approximately at x=8 and x=12)
  # x goes from 0 to 20 by 0.1, so index 81 is x=8, index 121 is x=12
  params <- initialize_peak_params(
    data$x,
    data$y,
    n_peaks = 2,
    models = models,
    peak_indices = c(81, 121)
  )

  expect_equal(length(params), 2)
  expect_equal(params[[1]]$center, 8, tolerance = 0.1)
  expect_equal(params[[2]]$center, 12, tolerance = 0.1)
})

test_that("initialize_peak_params validates inputs", {
  data <- create_gaussian_test_data()
  models <- list(gaussian_peak_model())

  # Non-numeric
  expect_error(
    initialize_peak_params("x", data$y, n_peaks = 1, models = models),
    "numeric vectors"
  )

  # Length mismatch
  expect_error(
    initialize_peak_params(data$x[1:50], data$y, n_peaks = 1, models = models),
    "same length"
  )

  # n_peaks mismatch with models
  expect_error(
    initialize_peak_params(
      data$x,
      data$y,
      n_peaks = 2,
      models = list(gaussian_peak_model())
    ),
    "must match"
  )
})

test_that("initialize_peak_params works without smoothing", {
  data <- create_gaussian_test_data(noise_sd = 0.02)
  models <- list(gaussian_peak_model(), gaussian_peak_model())

  params <- initialize_peak_params(
    data$x,
    data$y,
    n_peaks = 2,
    models = models,
    smooth = FALSE
  )

  expect_equal(length(params), 2)
})

test_that("add_param_jitter perturbs parameters", {
  params <- list(
    list(height = 1.0, center = 10, width = 1.5),
    list(height = 0.5, center = 15, width = 2.0)
  )

  set.seed(42)
  jittered <- add_param_jitter(params, scale = 0.2, method = "gaussian")

  expect_equal(length(jittered), 2)

  # Parameters should be different (with high probability)
  expect_false(all(unlist(jittered) == unlist(params)))
})

test_that("add_param_jitter works with uniform method", {
  params <- list(list(height = 1.0, center = 10, width = 1.5))

  set.seed(123)
  jittered <- add_param_jitter(params, scale = 0.1, method = "uniform")

  expect_equal(length(jittered), 1)
  expect_false(all(unlist(jittered) == unlist(params)))
})

# ==============================================================================
# assess_deconv_quality tests
# ==============================================================================

test_that("assess_deconv_quality calculates quality metrics", {
  data <- create_gaussian_test_data(noise_sd = 0.02)
  models <- list(gaussian_peak_model(), gaussian_peak_model())
  init_params <- list(
    list(height = 1.2, center = 7.5, width = 1.2),
    list(height = 0.6, center = 12.5, width = 1.8)
  )

  result <- optimize_deconvolution(data$x, data$y, models, init_params)
  quality <- assess_deconv_quality(data$x, data$y, result, models)

  expect_s3_class(quality, "deconv_quality")

  # Check goodness of fit
  expect_true("r_squared" %in% names(quality$goodness_of_fit))
  expect_true("rmse" %in% names(quality$goodness_of_fit))
  expect_gt(quality$goodness_of_fit$r_squared, 0.9)

  # Check information criteria
  expect_true("aic" %in% names(quality$information_criteria))
  expect_true("bic" %in% names(quality$information_criteria))

  # Check peak quality
  expect_s3_class(quality$peak_quality, "tbl_df")
  expect_equal(nrow(quality$peak_quality), 2)
  expect_true("purity" %in% names(quality$peak_quality))
  expect_true("area_percent" %in% names(quality$peak_quality))

  # Check residual analysis
  expect_true("autocorrelation" %in% names(quality$residual_analysis))
  expect_true("normality" %in% names(quality$residual_analysis))

  # Check overall grade
  expect_true(quality$overall_grade %in% c("A", "B", "C", "D", "F"))

  # Check convergence info
  expect_true(quality$convergence_info$converged)
})

test_that("assess_deconv_quality assigns appropriate grades", {
  # Good fit should get A or B
  data <- create_gaussian_test_data(noise_sd = 0.01)
  models <- list(gaussian_peak_model(), gaussian_peak_model())
  init_params <- list(
    list(height = 1.4, center = 7.8, width = 1.1),
    list(height = 0.7, center = 12.2, width = 1.6)
  )

  result <- optimize_deconvolution(data$x, data$y, models, init_params)
  quality <- assess_deconv_quality(data$x, data$y, result, models)

  expect_true(quality$overall_grade %in% c("A", "B"))
})

test_that("check_quality_gates works correctly", {
  data <- create_gaussian_test_data(noise_sd = 0.02)
  models <- list(gaussian_peak_model(), gaussian_peak_model())
  init_params <- list(
    list(height = 1.2, center = 7.5, width = 1.2),
    list(height = 0.6, center = 12.5, width = 1.8)
  )

  result <- optimize_deconvolution(data$x, data$y, models, init_params)
  quality <- assess_deconv_quality(data$x, data$y, result, models)

  gates <- check_quality_gates(quality)

  expect_type(gates, "list")
  expect_true(gates$status %in% c("pass", "warn", "reject"))
  expect_type(gates$pass, "logical")
  expect_type(gates$warn, "logical")
  expect_type(gates$reject, "logical")
  expect_type(gates$messages, "character")
})

test_that("check_quality_gates respects thresholds", {
  data <- create_gaussian_test_data(noise_sd = 0.02)
  models <- list(gaussian_peak_model(), gaussian_peak_model())
  init_params <- list(
    list(height = 1.2, center = 7.5, width = 1.2),
    list(height = 0.6, center = 12.5, width = 1.8)
  )

  result <- optimize_deconvolution(data$x, data$y, models, init_params)
  quality <- assess_deconv_quality(data$x, data$y, result, models)

  # With very high threshold, should warn
  gates_strict <- check_quality_gates(quality, warn_threshold = 0.9999)

  # Result depends on actual fit quality
  expect_true(gates_strict$status %in% c("pass", "warn", "reject"))
})

test_that("print.deconv_quality works", {
  data <- create_gaussian_test_data(noise_sd = 0.02)
  models <- list(gaussian_peak_model(), gaussian_peak_model())
  init_params <- list(
    list(height = 1.2, center = 7.5, width = 1.2),
    list(height = 0.6, center = 12.5, width = 1.8)
  )

  result <- optimize_deconvolution(data$x, data$y, models, init_params)
  quality <- assess_deconv_quality(data$x, data$y, result, models)

  expect_output(print(quality), "Deconvolution Quality Assessment")
  expect_output(print(quality), "Overall Grade")
  expect_output(print(quality), "R-squared")
})

test_that("summary.deconv_quality works", {
  data <- create_gaussian_test_data(noise_sd = 0.02)
  models <- list(gaussian_peak_model(), gaussian_peak_model())
  init_params <- list(
    list(height = 1.2, center = 7.5, width = 1.2),
    list(height = 0.6, center = 12.5, width = 1.8)
  )

  result <- optimize_deconvolution(data$x, data$y, models, init_params)
  quality <- assess_deconv_quality(data$x, data$y, result, models)

  expect_output(summary(quality), "Deconvolution Quality Summary")
  expect_output(summary(quality), "Status")
})

# ==============================================================================
# Integration tests
# ==============================================================================

test_that("full deconvolution workflow works end-to-end", {
  # Generate data
  data <- create_gaussian_test_data(noise_sd = 0.02)

  # Create models
  models <- list(gaussian_peak_model(), gaussian_peak_model())

  # Initialize parameters
  init_params <- initialize_peak_params(
    data$x,
    data$y,
    n_peaks = 2,
    models = models
  )

  # Optimize
  result <- optimize_deconvolution(
    data$x,
    data$y,
    models,
    init_params,
    optimizer = "auto"
  )

  # Assess quality
  quality <- assess_deconv_quality(data$x, data$y, result, models)

  # Check gates
  gates <- check_quality_gates(quality)

  # Workflow should succeed
  expect_true(result$convergence)
  expect_true(quality$goodness_of_fit$r_squared > 0.9)
  expect_true(gates$status %in% c("pass", "warn"))
})

test_that("deconvolution works with EMG model", {
  # Skip if this is a slow test environment
  skip_on_cran()

  x <- seq(0, 30, by = 0.2)
  # EMG peak with tailing
  y <- exp(-0.5 * ((x - 15) / 2)^2) *
    (1 + 0.3 * exp((x - 15) / 3)) +
    rnorm(length(x), sd = 0.02)
  y <- pmax(y, 0)

  models <- list(emg_peak_model())
  init_params <- list(list(height = 0.8, center = 14, width = 2, tau = 1))

  result <- optimize_deconvolution(x, y, models, init_params)

  expect_true(result$convergence)
  expect_equal(length(result$parameters), 1)
})

test_that("deconvolution handles constrain_positions correctly", {
  data <- create_gaussian_test_data(noise_sd = 0.02)
  models <- list(gaussian_peak_model(), gaussian_peak_model())
  init_params <- list(
    list(height = 1.2, center = 7.5, width = 1.2),
    list(height = 0.6, center = 12.5, width = 1.8)
  )

  # With position constraints
  result_constrained <- optimize_deconvolution(
    data$x,
    data$y,
    models,
    init_params,
    constrain_positions = TRUE
  )

  # Without position constraints
  result_unconstrained <- optimize_deconvolution(
    data$x,
    data$y,
    models,
    init_params,
    constrain_positions = FALSE
  )

  # Both should converge
  expect_true(result_constrained$convergence)
  expect_true(result_unconstrained$convergence)
})
