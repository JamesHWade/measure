# ==============================================================================
# Tests for Peak Model Class and Registry
# ==============================================================================

test_that("new_peak_model creates valid peak model objects", {
  model <- new_peak_model(
    name = "test",
    n_params = 3,
    param_names = c("a", "b", "c"),
    description = "Test model"
  )

  expect_s3_class(model, "peak_model")
  expect_s3_class(model, "test_peak_model")
  expect_equal(model$name, "test")
  expect_equal(model$n_params, 3L)
  expect_equal(model$param_names, c("a", "b", "c"))
  expect_equal(model$description, "Test model")
})

test_that("new_peak_model validates inputs", {
  expect_error(new_peak_model(), "name")
  expect_error(new_peak_model("test"), "n_params")
  expect_error(new_peak_model("test", 3), "param_names")

  # Mismatched n_params and param_names
  expect_error(
    new_peak_model("test", 3, c("a", "b")),
    "param_names"
  )
})

test_that("is_peak_model correctly identifies peak models", {
  model <- new_peak_model("test", 2, c("a", "b"))
  expect_true(is_peak_model(model))
  expect_false(is_peak_model(list()))
  expect_false(is_peak_model(NULL))
  expect_false(is_peak_model("string"))
})

# ==============================================================================
# Gaussian Peak Model Tests
# ==============================================================================

test_that("gaussian_peak_model creates correct object", {
  model <- gaussian_peak_model()

  expect_s3_class(model, "gaussian_peak_model")
  expect_s3_class(model, "peak_model")
  expect_equal(model$n_params, 3L)
  expect_equal(model$param_names, c("height", "center", "width"))
})

test_that("gaussian_peak_model evaluates correctly", {
  model <- gaussian_peak_model()
  x <- seq(-3, 3, by = 0.1)

  # Standard normal-like Gaussian

  params <- list(height = 1, center = 0, width = 1)
  y <- peak_model_value(model, x, params)

  # Peak should be at center
  expect_equal(y[which(x == 0)], 1, tolerance = 1e-10)

  # Value at center ± width should be exp(-0.5)
  expect_equal(y[which(x == 1)], exp(-0.5), tolerance = 1e-10)
  expect_equal(y[which(x == -1)], exp(-0.5), tolerance = 1e-10)

  # Symmetric
  expect_equal(y[which(x == -2)], y[which(x == 2)], tolerance = 1e-10)
})

test_that("gaussian_peak_model gradient is correct", {
  model <- gaussian_peak_model()
  x <- seq(-2, 2, by = 0.5)
  params <- list(height = 1.5, center = 0.3, width = 0.8)

  # Analytical gradient
  grad_analytical <- peak_model_gradient(model, x, params)

  # Numerical gradient
  grad_numerical <- peak_model_gradient_numerical(model, x, params)

  expect_equal(grad_analytical, grad_numerical, tolerance = 1e-4)
})

test_that("gaussian_peak_model area is correct", {
  model <- gaussian_peak_model()

  # Gaussian integral: h * σ * sqrt(2π)
  params <- list(height = 2, center = 0, width = 1)
  area <- peak_model_area(model, params)
  expected <- 2 * 1 * sqrt(2 * pi)

  expect_equal(area, expected, tolerance = 1e-10)
})

test_that("gaussian_peak_model initial guess is reasonable", {
  model <- gaussian_peak_model()

  # Create synthetic data
  x <- seq(0, 10, by = 0.1)
  true_params <- list(height = 5, center = 5, width = 0.8)
  y <- peak_model_value(model, x, true_params)

  # Find peak index
  peak_idx <- which.max(y)

  # Get initial guess
  guess <- peak_model_initial_guess(model, x, y, peak_idx)

  # Should be in the right ballpark

  expect_equal(guess$height, true_params$height, tolerance = 0.5)
  expect_equal(guess$center, true_params$center, tolerance = 0.1)
  expect_equal(guess$width, true_params$width, tolerance = 0.5)
})

test_that("gaussian_peak_model bounds are sensible", {
  model <- gaussian_peak_model()

  bounds <- peak_model_bounds(model, x_range = c(0, 10), y_range = c(0, 100))

  expect_named(bounds, c("lower", "upper"))
  expect_named(bounds$lower, c("height", "center", "width"))
  expect_named(bounds$upper, c("height", "center", "width"))

  # Height bounds

  expect_true(bounds$lower["height"] >= 0)
  expect_true(bounds$upper["height"] > bounds$lower["height"])

  # Center bounds
  expect_equal(unname(bounds$lower["center"]), 0)
  expect_equal(unname(bounds$upper["center"]), 10)

  # Width bounds
  expect_true(bounds$lower["width"] > 0)
  expect_true(bounds$upper["width"] <= 5) # half of x range
})

# ==============================================================================
# EMG Peak Model Tests
# ==============================================================================

test_that("emg_peak_model creates correct object", {
  model <- emg_peak_model()

  expect_s3_class(model, "emg_peak_model")
  expect_s3_class(model, "peak_model")
  expect_equal(model$n_params, 4L)
  expect_equal(model$param_names, c("height", "center", "width", "tau"))
})

test_that("emg_peak_model evaluates without errors", {
  model <- emg_peak_model()
  x <- seq(0, 15, by = 0.1)
  params <- list(height = 1, center = 5, width = 0.5, tau = 0.3)

  y <- peak_model_value(model, x, params)

  # Should produce numeric values

  expect_type(y, "double")
  expect_equal(length(y), length(x))
  expect_false(any(is.na(y)))
  expect_false(any(is.infinite(y)))

  # Peak should have tailing (asymmetric)
  peak_idx <- which.max(y)
  left_area <- sum(y[1:peak_idx])
  right_area <- sum(y[peak_idx:length(y)])
  expect_true(right_area > left_area) # Tailing to the right
})

test_that("emg_peak_model degenerates to Gaussian when tau=0", {
  emg_model <- emg_peak_model()
  gauss_model <- gaussian_peak_model()

  x <- seq(-3, 3, by = 0.1)

  # EMG with tau close to 0 should approximate Gaussian
  params_emg <- list(height = 1, center = 0, width = 1, tau = 0)
  params_gauss <- list(height = 1, center = 0, width = 1)

  y_emg <- peak_model_value(emg_model, x, params_emg)
  y_gauss <- peak_model_value(gauss_model, x, params_gauss)

  expect_equal(y_emg, y_gauss, tolerance = 1e-6)
})

# ==============================================================================
# Bi-Gaussian Peak Model Tests
# ==============================================================================

test_that("bigaussian_peak_model creates correct object", {
  model <- bigaussian_peak_model()

  expect_s3_class(model, "bigaussian_peak_model")
  expect_s3_class(model, "peak_model")
  expect_equal(model$n_params, 4L)
  expect_equal(
    model$param_names,
    c("height", "center", "width_left", "width_right")
  )
})

test_that("bigaussian_peak_model evaluates correctly", {
  model <- bigaussian_peak_model()
  x <- seq(-3, 7, by = 0.1)

  # Asymmetric widths
  params <- list(height = 1, center = 2, width_left = 0.5, width_right = 1.5)
  y <- peak_model_value(model, x, params)

  # Peak at center
  center_idx <- which(abs(x - 2) < 1e-6)
  expect_equal(y[center_idx], 1, tolerance = 1e-10)

  # Check asymmetry
  left_of_center <- x < 2
  right_of_center <- x > 2

  # At equal distance from center, wider side should have higher value
  left_val <- y[which(abs(x - 1) < 1e-6)] # 1 unit left
  right_val <- y[which(abs(x - 3) < 1e-6)] # 1 unit right

  # With width_right > width_left, the right side decays slower
  expect_true(right_val > left_val)
})

test_that("bigaussian degenerates to Gaussian when widths are equal", {
  bi_model <- bigaussian_peak_model()
  gauss_model <- gaussian_peak_model()

  x <- seq(-3, 7, by = 0.1)

  params_bi <- list(height = 1, center = 2, width_left = 1, width_right = 1)
  params_gauss <- list(height = 1, center = 2, width = 1)

  y_bi <- peak_model_value(bi_model, x, params_bi)
  y_gauss <- peak_model_value(gauss_model, x, params_gauss)

  expect_equal(y_bi, y_gauss, tolerance = 1e-10)
})

# ==============================================================================
# Lorentzian Peak Model Tests
# ==============================================================================

test_that("lorentzian_peak_model creates correct object", {
  model <- lorentzian_peak_model()

  expect_s3_class(model, "lorentzian_peak_model")
  expect_s3_class(model, "peak_model")
  expect_equal(model$n_params, 3L)
  expect_equal(model$param_names, c("height", "center", "gamma"))
})

test_that("lorentzian_peak_model evaluates correctly", {
  model <- lorentzian_peak_model()
  x <- seq(-5, 5, by = 0.1)

  params <- list(height = 1, center = 0, gamma = 1)
  y <- peak_model_value(model, x, params)

  # Peak at center
  expect_equal(y[which(x == 0)], 1, tolerance = 1e-10)

  # HWHM should be at gamma
  expect_equal(y[which(x == 1)], 0.5, tolerance = 1e-10)
  expect_equal(y[which(x == -1)], 0.5, tolerance = 1e-10)

  # Symmetric
  expect_equal(y[which(x == -2)], y[which(x == 2)], tolerance = 1e-10)
})

test_that("lorentzian_peak_model gradient is correct", {
  model <- lorentzian_peak_model()
  x <- seq(-2, 2, by = 0.5)
  params <- list(height = 1.5, center = 0.3, gamma = 0.8)

  grad_analytical <- peak_model_gradient(model, x, params)
  grad_numerical <- peak_model_gradient_numerical(model, x, params)

  expect_equal(grad_analytical, grad_numerical, tolerance = 1e-4)
})

test_that("lorentzian_peak_model area is correct", {
  model <- lorentzian_peak_model()

  # Lorentzian integral: h * γ * π
  params <- list(height = 2, center = 0, gamma = 1)
  area <- peak_model_area(model, params)
  expected <- 2 * 1 * pi

  expect_equal(area, expected, tolerance = 1e-10)
})

test_that("lorentzian has heavier tails than Gaussian", {
  lorentz <- lorentzian_peak_model()
  gauss <- gaussian_peak_model()

  x <- seq(-10, 10, by = 0.1)

  # Same height and similar width parameters
  # Gaussian HWHM = σ * sqrt(2 * ln(2)) ≈ 1.177 * σ
  # To match HWHM, set γ = σ * sqrt(2 * ln(2))
  sigma <- 1
  gamma <- sigma * sqrt(2 * log(2))

  params_gauss <- list(height = 1, center = 0, width = sigma)
  params_lorentz <- list(height = 1, center = 0, gamma = gamma)

  y_gauss <- peak_model_value(gauss, x, params_gauss)
  y_lorentz <- peak_model_value(lorentz, x, params_lorentz)

  # At the tails (x = ±5), Lorentzian should be much higher
  tail_idx <- which(abs(x) == 5)
  expect_true(all(y_lorentz[tail_idx] > y_gauss[tail_idx] * 10))
})

# ==============================================================================
# Utility Function Tests
# ==============================================================================

test_that("validate_peak_model_params works correctly", {
  model <- gaussian_peak_model()

  # Valid params
  valid_params <- list(height = 1, center = 0, width = 1)
  expect_true(validate_peak_model_params(model, valid_params))

  # Missing param
  expect_error(
    validate_peak_model_params(model, list(height = 1, center = 0)),
    "width"
  )

  # Extra params (generates warning but returns TRUE)
  extra_params <- list(height = 1, center = 0, width = 1, extra = 5)
  expect_warning(
    result <- validate_peak_model_params(model, extra_params),
    "Extra parameters ignored"
  )
  expect_true(result)
})

test_that("sum_peak_models combines multiple peaks", {
  model1 <- gaussian_peak_model()
  model2 <- gaussian_peak_model()
  x <- seq(-5, 10, by = 0.1)

  params_list <- list(
    list(height = 1, center = 0, width = 0.5),
    list(height = 2, center = 5, width = 1)
  )

  y_sum <- sum_peak_models(x, list(model1, model2), params_list)

  # Should be the sum of individual peaks
  y1 <- peak_model_value(model1, x, params_list[[1]])
  y2 <- peak_model_value(model2, x, params_list[[2]])

  expect_equal(y_sum, y1 + y2, tolerance = 1e-10)
})

test_that("sum_peak_models works with different models", {
  models <- list(gaussian_peak_model(), lorentzian_peak_model())
  x <- seq(-5, 10, by = 0.1)

  params_list <- list(
    list(height = 1, center = 0, width = 0.5),
    list(height = 2, center = 5, gamma = 1)
  )

  y_sum <- sum_peak_models(x, models, params_list)

  y1 <- peak_model_value(models[[1]], x, params_list[[1]])
  y2 <- peak_model_value(models[[2]], x, params_list[[2]])

  expect_equal(y_sum, y1 + y2, tolerance = 1e-10)
})

test_that("peak_model_gradient_numerical returns correct shape", {
  model <- bigaussian_peak_model()
  x <- seq(0, 10, by = 0.5)
  params <- list(height = 1, center = 5, width_left = 0.8, width_right = 1.2)

  grad <- peak_model_gradient_numerical(model, x, params)

  expect_equal(nrow(grad), length(x))
  expect_equal(ncol(grad), model$n_params)
  expect_equal(colnames(grad), model$param_names)
})

# ==============================================================================
# Registry Tests
# ==============================================================================

test_that("peak model registry starts empty and is populated on load", {
  # Reset and check empty
  .peak_model_registry_reset()
  expect_equal(nrow(peak_models()), 0)

  # Register core models
  .register_core_peak_models()

  # Should have the four built-in models
  models <- peak_models()
  expect_true("gaussian" %in% models$name)
  expect_true("emg" %in% models$name)
  expect_true("bigaussian" %in% models$name)
  expect_true("lorentzian" %in% models$name)
})

test_that("register_peak_model works", {
  .peak_model_registry_reset()

  register_peak_model(
    name = "test_model",
    constructor = function() new_peak_model("test_model", 2, c("a", "b")),
    pack_name = "test_pack",
    description = "A test model"
  )

  expect_true(has_peak_model("test_model"))

  models <- peak_models()
  test_row <- models[models$name == "test_model", ]
  expect_equal(as.character(test_row$pack_name), "test_pack")
  expect_equal(as.character(test_row$description), "A test model")

  # Clean up
  .peak_model_registry_reset()
  .register_core_peak_models()
})

test_that("unregister_peak_model removes models", {
  .peak_model_registry_reset()

  register_peak_model(
    name = "temp_model",
    constructor = function() new_peak_model("temp_model", 1, "x"),
    pack_name = "test",
    description = "Temporary"
  )

  expect_true(has_peak_model("temp_model"))

  unregister_peak_model("temp_model")

  expect_false(has_peak_model("temp_model"))

  # Clean up
  .peak_model_registry_reset()
  .register_core_peak_models()
})

test_that("create_peak_model creates model instances", {
  # Use core models
  gauss <- create_peak_model("gaussian")
  expect_s3_class(gauss, "gaussian_peak_model")

  emg <- create_peak_model("emg")
  expect_s3_class(emg, "emg_peak_model")

  lorentz <- create_peak_model("lorentzian")
  expect_s3_class(lorentz, "lorentzian_peak_model")
})

test_that("create_peak_model errors for unknown models", {
  expect_error(
    create_peak_model("nonexistent_model"),
    "not found"
  )
})

test_that("has_peak_model returns correct values", {
  expect_true(has_peak_model("gaussian"))
  expect_true(has_peak_model("emg"))
  expect_false(has_peak_model("nonexistent"))
})

test_that("peak_models can filter by packs", {
  # All core models from "measure"
  models <- peak_models(packs = "measure")
  expect_true(nrow(models) > 0)
  expect_true(all(models$pack_name == "measure"))

  # No models from nonexistent pack
  models <- peak_models(packs = "nonexistent_pack")
  expect_equal(nrow(models), 0)
})

# ==============================================================================
# Print Methods
# ==============================================================================

test_that("print.peak_model works", {
  model <- gaussian_peak_model()

  output <- capture.output(print(model))

  expect_true(any(grepl("gaussian", output, ignore.case = TRUE)))
  expect_true(any(grepl("height", output)))
  expect_true(any(grepl("center", output)))
  expect_true(any(grepl("width", output)))
})

# ==============================================================================
# Edge Cases
# ==============================================================================
test_that("models handle extreme parameter values", {
  model <- gaussian_peak_model()
  x <- seq(-10, 10, by = 0.1)

  # Very small width
  params_narrow <- list(height = 1, center = 0, width = 0.01)
  y_narrow <- peak_model_value(model, x, params_narrow)
  expect_false(any(is.na(y_narrow)))
  expect_false(any(is.infinite(y_narrow)))

  # Very large height
  params_tall <- list(height = 1e10, center = 0, width = 1)
  y_tall <- peak_model_value(model, x, params_tall)
  expect_false(any(is.na(y_tall)))
  expect_equal(max(y_tall), 1e10, tolerance = 1e-10)
})

test_that("models handle single-point evaluation", {
  model <- gaussian_peak_model()
  params <- list(height = 1, center = 0, width = 1)

  y <- peak_model_value(model, 0, params)
  expect_equal(y, 1)
  expect_equal(length(y), 1)
})
