# ==============================================================================
# Tests for spectral math transformations
#
# This file tests:
# - step_measure_absorbance
# - step_measure_transmittance
# - step_measure_log
# - step_measure_kubelka_munk
# - step_measure_derivative
# - step_measure_derivative_gap
# ==============================================================================

# ==============================================================================
# Test Helpers
# ==============================================================================

# Create test data in internal format
create_test_data <- function() {
  recipe(water + fat + protein ~ ., data = meats_long) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    prep() %>%
    bake(new_data = NULL)
}

# Get original values from test data
get_original_values <- function() {
  test_data <- create_test_data()
  test_data$.measures[[1]]$value
}

# Get locations from test data
get_locations <- function() {
  test_data <- create_test_data()
  test_data$.measures[[1]]$location
}

# Create synthetic reflectance data with values in (0, 1) for Kubelka-Munk tests
local({
  set.seed(123)
  reflectance_data <<- tibble::tibble(
    sample_id = rep(1:5, each = 100),
    wavelength = rep(seq(400, 700, length.out = 100), 5),
    reflectance = runif(500, min = 0.1, max = 0.9),
    outcome = rep(rnorm(5), each = 100)
  )
})

# ==============================================================================
# step_measure_absorbance tests
# ==============================================================================

test_that("step_measure_absorbance converts transmittance to absorbance", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    step_measure_absorbance() %>%
    prep()

  result <- bake(rec, new_data = NULL)
  original <- get_original_values()
  converted <- result$.measures[[1]]$value

  # A = -log10(T)
  expected <- -log10(original)
  expect_equal(converted, expected, tolerance = 1e-10)
})

test_that("step_measure_absorbance preserves locations", {
  test_data <- create_test_data()

  rec <- recipe(water + fat + protein ~ ., data = meats_long) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    step_measure_absorbance() %>%
    prep()

  result <- bake(rec, new_data = NULL)

  original_locs <- test_data$.measures[[1]]$location
  result_locs <- result$.measures[[1]]$location
  expect_equal(result_locs, original_locs)
})

test_that("step_measure_absorbance print method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    step_measure_absorbance()

  expect_output(print(rec$steps[[2]]), "Transmittance to absorbance")

  rec_prepped <- prep(rec)
  expect_output(print(rec_prepped$steps[[2]]), "Transmittance to absorbance")
})

test_that("step_measure_absorbance tidy method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    step_measure_absorbance() %>%
    prep()

  tidy_result <- tidy(rec, number = 2)

  expect_s3_class(tidy_result, "tbl_df")
  expect_true("terms" %in% names(tidy_result))
})

# ==============================================================================
# step_measure_transmittance tests
# ==============================================================================

test_that("step_measure_transmittance converts absorbance to transmittance", {
  # First convert to absorbance, then back to transmittance
  rec <- recipe(water + fat + protein ~ ., data = meats_long) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    step_measure_absorbance() %>%
    step_measure_transmittance() %>%
    prep()

  result <- bake(rec, new_data = NULL)
  original <- get_original_values()
  converted <- result$.measures[[1]]$value

  # Round-trip should preserve values

  expect_equal(converted, original, tolerance = 1e-10)
})

test_that("step_measure_transmittance preserves locations", {
  test_data <- create_test_data()

  rec <- recipe(water + fat + protein ~ ., data = meats_long) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    step_measure_transmittance() %>%
    prep()

  result <- bake(rec, new_data = NULL)

  original_locs <- test_data$.measures[[1]]$location
  result_locs <- result$.measures[[1]]$location
  expect_equal(result_locs, original_locs)
})

test_that("step_measure_transmittance print method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    step_measure_transmittance()

  expect_output(print(rec$steps[[2]]), "Absorbance to transmittance")

  rec_prepped <- prep(rec)
  expect_output(print(rec_prepped$steps[[2]]), "Absorbance to transmittance")
})

test_that("step_measure_transmittance tidy method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    step_measure_transmittance() %>%
    prep()

  tidy_result <- tidy(rec, number = 2)

  expect_s3_class(tidy_result, "tbl_df")
  expect_true("terms" %in% names(tidy_result))
})

# ==============================================================================
# step_measure_log tests
# ==============================================================================

test_that("step_measure_log applies natural log by default", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    step_measure_log() %>%
    prep()

  result <- bake(rec, new_data = NULL)
  original <- get_original_values()
  converted <- result$.measures[[1]]$value

  expected <- log(original)
  expect_equal(converted, expected, tolerance = 1e-10)
})

test_that("step_measure_log works with base 10", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    step_measure_log(base = 10) %>%
    prep()

  result <- bake(rec, new_data = NULL)
  original <- get_original_values()
  converted <- result$.measures[[1]]$value

  expected <- log10(original)
  expect_equal(converted, expected, tolerance = 1e-10)
})

test_that("step_measure_log works with offset", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    step_measure_log(offset = 1) %>%
    prep()

  result <- bake(rec, new_data = NULL)
  original <- get_original_values()
  converted <- result$.measures[[1]]$value

  expected <- log(original + 1)
  expect_equal(converted, expected, tolerance = 1e-10)
})

test_that("step_measure_log preserves locations", {
  test_data <- create_test_data()

  rec <- recipe(water + fat + protein ~ ., data = meats_long) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    step_measure_log() %>%
    prep()

  result <- bake(rec, new_data = NULL)

  original_locs <- test_data$.measures[[1]]$location
  result_locs <- result$.measures[[1]]$location
  expect_equal(result_locs, original_locs)
})

test_that("step_measure_log print method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    step_measure_log(base = 10)

  expect_output(print(rec$steps[[2]]), "Log transformation")

  rec_prepped <- prep(rec)
  expect_output(print(rec_prepped$steps[[2]]), "base 10")
})

test_that("step_measure_log tidy method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    step_measure_log(base = 10, offset = 0.5) %>%
    prep()

  tidy_result <- tidy(rec, number = 2)

  expect_s3_class(tidy_result, "tbl_df")
  expect_equal(tidy_result$base, 10)
  expect_equal(tidy_result$offset, 0.5)
})

# ==============================================================================
# step_measure_kubelka_munk tests
# ==============================================================================

test_that("step_measure_kubelka_munk computes K-M transformation", {
  # Use reflectance data with values in (0, 1)
  input_rec <- recipe(outcome ~ ., data = reflectance_data) %>%
    step_measure_input_long(reflectance, location = vars(wavelength)) %>%
    prep()
  input_data <- bake(input_rec, new_data = NULL)
  original <- input_data$.measures[[1]]$value

  rec <- recipe(outcome ~ ., data = reflectance_data) %>%
    step_measure_input_long(reflectance, location = vars(wavelength)) %>%
    step_measure_kubelka_munk() %>%
    prep()

  result <- bake(rec, new_data = NULL)
  converted <- result$.measures[[1]]$value

  # K-M: f(R) = (1-R)^2 / (2R)
  expected <- (1 - original)^2 / (2 * original)
  expect_equal(converted, expected, tolerance = 1e-10)
})

test_that("step_measure_kubelka_munk preserves locations", {
  input_rec <- recipe(outcome ~ ., data = reflectance_data) %>%
    step_measure_input_long(reflectance, location = vars(wavelength)) %>%
    prep()
  input_data <- bake(input_rec, new_data = NULL)

  rec <- recipe(outcome ~ ., data = reflectance_data) %>%
    step_measure_input_long(reflectance, location = vars(wavelength)) %>%
    step_measure_kubelka_munk() %>%
    prep()

  result <- bake(rec, new_data = NULL)

  original_locs <- input_data$.measures[[1]]$location
  result_locs <- result$.measures[[1]]$location
  expect_equal(result_locs, original_locs)
})

test_that("step_measure_kubelka_munk print method works", {
  rec <- recipe(outcome ~ ., data = reflectance_data) %>%
    step_measure_input_long(reflectance, location = vars(wavelength)) %>%
    step_measure_kubelka_munk()

  expect_output(print(rec$steps[[2]]), "Kubelka-Munk")

  rec_prepped <- prep(rec)
  expect_output(print(rec_prepped$steps[[2]]), "Kubelka-Munk")
})

test_that("step_measure_kubelka_munk tidy method works", {
  rec <- recipe(outcome ~ ., data = reflectance_data) %>%
    step_measure_input_long(reflectance, location = vars(wavelength)) %>%
    step_measure_kubelka_munk() %>%
    prep()

  tidy_result <- tidy(rec, number = 2)

  expect_s3_class(tidy_result, "tbl_df")
  expect_true("terms" %in% names(tidy_result))
})

# ==============================================================================
# step_measure_derivative tests
# ==============================================================================

test_that("step_measure_derivative computes first derivative", {
  test_data <- create_test_data()

  rec <- recipe(water + fat + protein ~ ., data = meats_long) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    step_measure_derivative(order = 1L) %>%
    prep()

  result <- bake(rec, new_data = NULL)

  original_vals <- test_data$.measures[[1]]$value
  original_locs <- test_data$.measures[[1]]$location

  result_vals <- result$.measures[[1]]$value
  result_locs <- result$.measures[[1]]$location

  # First derivative reduces length by 1
  expect_equal(length(result_vals), length(original_vals) - 1)
  expect_equal(length(result_locs), length(original_locs) - 1)

  # Verify derivative calculation for first few points
  dx <- diff(original_locs)
  dy <- diff(original_vals)
  expected_deriv <- dy / dx

  expect_equal(result_vals, expected_deriv, tolerance = 1e-10)
})

test_that("step_measure_derivative computes second derivative", {
  test_data <- create_test_data()

  rec <- recipe(water + fat + protein ~ ., data = meats_long) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    step_measure_derivative(order = 2L) %>%
    prep()

  result <- bake(rec, new_data = NULL)

  original_vals <- test_data$.measures[[1]]$value

  result_vals <- result$.measures[[1]]$value

  # Second derivative reduces length by 2
  expect_equal(length(result_vals), length(original_vals) - 2)
})

test_that("step_measure_derivative uses left locations", {
  test_data <- create_test_data()

  rec <- recipe(water + fat + protein ~ ., data = meats_long) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    step_measure_derivative(order = 1L) %>%
    prep()

  result <- bake(rec, new_data = NULL)

  original_locs <- test_data$.measures[[1]]$location
  result_locs <- result$.measures[[1]]$location

  # Left locations (drop the last point)
  expected_locs <- original_locs[-length(original_locs)]
  expect_equal(result_locs, expected_locs)
})

test_that("step_measure_derivative validates order parameter", {
  expect_error(
    recipe(water + fat + protein ~ ., data = meats_long) %>%
      update_role(id, new_role = "id") %>%
      step_measure_input_long(transmittance, location = vars(channel)) %>%
      step_measure_derivative(order = 0L) %>%
      prep(),
    "must be 1 or 2"
  )
})

test_that("step_measure_derivative print method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    step_measure_derivative(order = 2L)

  expect_output(print(rec$steps[[2]]), "2nd derivative")

  rec_prepped <- prep(rec)
  expect_output(print(rec_prepped$steps[[2]]), "2nd derivative")
})

test_that("step_measure_derivative tidy method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    step_measure_derivative(order = 2L) %>%
    prep()

  tidy_result <- tidy(rec, number = 2)

  expect_s3_class(tidy_result, "tbl_df")
  expect_equal(tidy_result$order, 2L)
})

test_that("step_measure_derivative is tunable", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    step_measure_derivative(order = 1L)

  tun <- tunable(rec$steps[[2]])
  expect_true("order" %in% tun$name)
})

# ==============================================================================
# step_measure_derivative_gap tests
# ==============================================================================

test_that("step_measure_derivative_gap computes gap derivative", {
  test_data <- create_test_data()

  rec <- recipe(water + fat + protein ~ ., data = meats_long) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    step_measure_derivative_gap(gap = 2L, segment = 1L) %>%
    prep()

  result <- bake(rec, new_data = NULL)

  original_vals <- test_data$.measures[[1]]$value
  original_locs <- test_data$.measures[[1]]$location
  n <- length(original_vals)

  result_vals <- result$.measures[[1]]$value
  result_locs <- result$.measures[[1]]$location

  # Gap derivative with gap=2 reduces length by 2*gap = 4
  expect_equal(length(result_vals), n - 4)
  expect_equal(length(result_locs), n - 4)

  # Verify calculation for center indices
  gap <- 2L
  idx <- (gap + 1):(n - gap)
  values_plus <- original_vals[idx + gap]
  values_minus <- original_vals[idx - gap]
  dx <- original_locs[idx + gap] - original_locs[idx - gap]
  expected <- (values_plus - values_minus) / dx

  expect_equal(result_vals, expected, tolerance = 1e-10)
})

test_that("step_measure_derivative_gap uses center locations", {
  test_data <- create_test_data()

  rec <- recipe(water + fat + protein ~ ., data = meats_long) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    step_measure_derivative_gap(gap = 2L, segment = 1L) %>%
    prep()

  result <- bake(rec, new_data = NULL)

  original_locs <- test_data$.measures[[1]]$location
  n <- length(original_locs)
  result_locs <- result$.measures[[1]]$location

  # Center locations
  gap <- 2L
  idx <- (gap + 1):(n - gap)
  expected_locs <- original_locs[idx]

  expect_equal(result_locs, expected_locs)
})

test_that("step_measure_derivative_gap with segment > 1 uses averaging", {
  test_data <- create_test_data()

  rec <- recipe(water + fat + protein ~ ., data = meats_long) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    step_measure_derivative_gap(gap = 3L, segment = 2L) %>%
    prep()

  result <- bake(rec, new_data = NULL)

  original_vals <- test_data$.measures[[1]]$value
  original_locs <- test_data$.measures[[1]]$location
  n <- length(original_vals)

  result_vals <- result$.measures[[1]]$value

  # Verify averaging calculation
  gap <- 3L
  segment <- 2L
  effective_gap <- gap + segment - 1  # 4
  idx <- (effective_gap + 1):(n - effective_gap)

  # Calculate expected values with segment averaging
  expected <- numeric(length(idx))
  for (j in seq_along(idx)) {
    i <- idx[j]
    plus_range <- i + gap + seq(0, segment - 1)
    minus_range <- i - gap - seq(0, segment - 1)
    mean_plus <- mean(original_vals[plus_range])
    mean_minus <- mean(original_vals[minus_range])
    dx <- original_locs[i + gap] - original_locs[i - gap]
    expected[j] <- (mean_plus - mean_minus) / dx
  }

  expect_equal(result_vals, expected, tolerance = 1e-10)
})

test_that("step_measure_derivative_gap validates gap parameter", {
  expect_error(
    recipe(water + fat + protein ~ ., data = meats_long) %>%
      update_role(id, new_role = "id") %>%
      step_measure_input_long(transmittance, location = vars(channel)) %>%
      step_measure_derivative_gap(gap = 0L) %>%
      prep(),
    "at least 1"
  )
})

test_that("step_measure_derivative_gap validates segment parameter", {
  expect_error(
    recipe(water + fat + protein ~ ., data = meats_long) %>%
      update_role(id, new_role = "id") %>%
      step_measure_input_long(transmittance, location = vars(channel)) %>%
      step_measure_derivative_gap(gap = 2L, segment = 0L) %>%
      prep(),
    "at least 1"
  )
})

test_that("step_measure_derivative_gap print method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    step_measure_derivative_gap(gap = 3L, segment = 2L)

  expect_output(print(rec$steps[[2]]), "Gap derivative")

  rec_prepped <- prep(rec)
  expect_output(print(rec_prepped$steps[[2]]), "gap=3")
  expect_output(print(rec_prepped$steps[[2]]), "segment=2")
})

test_that("step_measure_derivative_gap tidy method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    step_measure_derivative_gap(gap = 3L, segment = 2L) %>%
    prep()

  tidy_result <- tidy(rec, number = 2)

  expect_s3_class(tidy_result, "tbl_df")
  expect_equal(tidy_result$gap, 3L)
  expect_equal(tidy_result$segment, 2L)
})

test_that("step_measure_derivative_gap is tunable", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    step_measure_derivative_gap(gap = 2L)

  tun <- tunable(rec$steps[[2]])
  expect_true("gap" %in% tun$name)
  expect_true("segment" %in% tun$name)
})

# ==============================================================================
# Integration tests
# ==============================================================================

test_that("spectral math steps work in a pipeline", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    step_measure_absorbance() %>%
    step_measure_derivative(order = 1L) %>%
    prep()

  result <- bake(rec, new_data = NULL)

  expect_s3_class(result, "tbl_df")
  expect_true(".measures" %in% names(result))
})

test_that("absorbance and transmittance are inverses", {
  original <- get_original_values()

  rec <- recipe(water + fat + protein ~ ., data = meats_long) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    step_measure_absorbance() %>%
    step_measure_transmittance() %>%
    prep()

  result <- bake(rec, new_data = NULL)
  round_trip <- result$.measures[[1]]$value

  expect_equal(round_trip, original, tolerance = 1e-10)
})

test_that("spectral math steps work with wide input format", {
  test_data_wide <- meats_long %>%
    tidyr::pivot_wider(
      names_from = channel,
      names_prefix = "x_",
      values_from = transmittance
    )

  rec <- recipe(water + fat + protein ~ ., data = test_data_wide) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_wide(starts_with("x_")) %>%
    step_measure_absorbance() %>%
    prep()

  result <- bake(rec, new_data = NULL)

  expect_s3_class(result, "tbl_df")
  expect_true(".measures" %in% names(result))
})

test_that("required_pkgs methods work for all steps", {
  rec1 <- recipe(water + fat + protein ~ ., data = meats_long) %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    step_measure_absorbance()

  rec2 <- recipe(water + fat + protein ~ ., data = meats_long) %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    step_measure_transmittance()

  rec3 <- recipe(water + fat + protein ~ ., data = meats_long) %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    step_measure_log()

  rec4 <- recipe(water + fat + protein ~ ., data = meats_long) %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    step_measure_kubelka_munk()

  rec5 <- recipe(water + fat + protein ~ ., data = meats_long) %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    step_measure_derivative()

  rec6 <- recipe(water + fat + protein ~ ., data = meats_long) %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    step_measure_derivative_gap(gap = 2L)

  expect_true("measure" %in% required_pkgs(rec1$steps[[2]]))
  expect_true("measure" %in% required_pkgs(rec2$steps[[2]]))
  expect_true("measure" %in% required_pkgs(rec3$steps[[2]]))
  expect_true("measure" %in% required_pkgs(rec4$steps[[2]]))
  expect_true("measure" %in% required_pkgs(rec5$steps[[2]]))
  expect_true("measure" %in% required_pkgs(rec6$steps[[2]]))
})

# ==============================================================================
# New data / bake tests
# ==============================================================================

test_that("spectral math steps work on new data", {
  # Split data
  train_ids <- unique(meats_long$id)[1:100]
  test_ids <- unique(meats_long$id)[101:150]

  train_data <- meats_long %>% dplyr::filter(id %in% train_ids)
  test_data <- meats_long %>% dplyr::filter(id %in% test_ids)

  rec <- recipe(water + fat + protein ~ ., data = train_data) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    step_measure_absorbance() %>%
    step_measure_derivative(order = 1L) %>%
    prep()

  # Bake on new data
  result <- bake(rec, new_data = test_data)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), length(test_ids))
  expect_true(".measures" %in% names(result))
})
