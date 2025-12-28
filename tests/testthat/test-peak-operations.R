# ==============================================================================
# Tests for peak operations
#
# This file tests:
# - step_measure_peaks_detect
# - step_measure_peaks_integrate
# - step_measure_peaks_filter
# - step_measure_peaks_to_table
# ==============================================================================

# ==============================================================================
# Test Helpers
# ==============================================================================

# Create test data with clear peaks
create_peaked_data <- function() {
  # Create synthetic data with known peaks
  x <- seq(0, 100, by = 1)
  # Three Gaussian peaks at x = 20, 50, 80
  y <- 0.5 * exp(-((x - 20)^2) / 20) +
       1.0 * exp(-((x - 50)^2) / 30) +
       0.3 * exp(-((x - 80)^2) / 15) +
       rnorm(length(x), sd = 0.01)

  tibble::tibble(
    id = "sample1",
    location = x,
    value = y
  )
}

# Create recipe with peaked data
prep_peaked_recipe <- function(steps_fn) {
  data <- create_peaked_data()

  rec <- recipe(~., data = data) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(value, location = vars(location))

  rec <- steps_fn(rec)
  prep(rec)
}

# ==============================================================================
# step_measure_peaks_detect tests
# ==============================================================================

test_that("step_measure_peaks_detect finds peaks with derivative method", {
  rec <- prep_peaked_recipe(function(r) {
    step_measure_peaks_detect(r, method = "derivative", min_height = 0.1)
  })

  result <- bake(rec, new_data = NULL)

  expect_true(".peaks" %in% names(result))
  expect_true(is_peaks_list(result$.peaks))

  peaks <- result$.peaks[[1]]
  expect_gt(nrow(peaks), 0)
  expect_true(all(c("peak_id", "location", "height") %in% names(peaks)))
})

test_that("step_measure_peaks_detect finds peaks with prominence method", {
  rec <- prep_peaked_recipe(function(r) {
    step_measure_peaks_detect(r, method = "prominence", min_prominence = 0.1)
  })

  result <- bake(rec, new_data = NULL)

  expect_true(".peaks" %in% names(result))
  peaks <- result$.peaks[[1]]
  expect_gt(nrow(peaks), 0)
})

test_that("step_measure_peaks_detect respects min_height", {
  rec_low <- prep_peaked_recipe(function(r) {
    step_measure_peaks_detect(r, method = "derivative", min_height = 0.1)
  })

  rec_high <- prep_peaked_recipe(function(r) {
    step_measure_peaks_detect(r, method = "derivative", min_height = 0.8)
  })

  peaks_low <- bake(rec_low, new_data = NULL)$.peaks[[1]]
  peaks_high <- bake(rec_high, new_data = NULL)$.peaks[[1]]

  expect_gte(nrow(peaks_low), nrow(peaks_high))
})

test_that("step_measure_peaks_detect respects min_distance", {
  rec <- prep_peaked_recipe(function(r) {
    step_measure_peaks_detect(r, method = "derivative", min_height = 0.05,
                              min_distance = 40)
  })

  result <- bake(rec, new_data = NULL)
  peaks <- result$.peaks[[1]]


  # Ensure at least one expectation always runs
  expect_true(nrow(peaks) >= 0)

  # If multiple peaks, verify min_distance constraint
  if (nrow(peaks) > 1) {
    distances <- diff(peaks$location)
    expect_true(all(distances >= 40))
  }
})

test_that("step_measure_peaks_detect preserves .measures column", {
  rec <- prep_peaked_recipe(function(r) {
    step_measure_peaks_detect(r, method = "prominence")
  })

  result <- bake(rec, new_data = NULL)

  expect_true(".measures" %in% names(result))
  expect_true(is_measure_list(result$.measures))
})

test_that("step_measure_peaks_detect print method works", {
  rec <- recipe(~., data = create_peaked_data()) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(value, location = vars(location)) |>
    step_measure_peaks_detect(method = "prominence")

  expect_output(print(rec$steps[[2]]), "Peak detection")
})

test_that("step_measure_peaks_detect tidy method works", {
  rec <- prep_peaked_recipe(function(r) {
    step_measure_peaks_detect(r, method = "prominence", min_height = 0.2)
  })

  tidy_result <- tidy(rec, number = 2)
  expect_s3_class(tidy_result, "tbl_df")
  expect_true("method" %in% names(tidy_result))
})

# ==============================================================================
# step_measure_peaks_integrate tests
# ==============================================================================

test_that("step_measure_peaks_integrate calculates areas", {
  rec <- prep_peaked_recipe(function(r) {
    r |>
      step_measure_peaks_detect(method = "prominence", min_height = 0.1) |>
      step_measure_peaks_integrate()
  })

  result <- bake(rec, new_data = NULL)
  peaks <- result$.peaks[[1]]

  expect_false(all(is.na(peaks$area)))
  expect_true(all(peaks$area[!is.na(peaks$area)] > 0))
})

test_that("step_measure_peaks_integrate with trapezoid method works", {
  rec <- prep_peaked_recipe(function(r) {
    r |>
      step_measure_peaks_detect(method = "prominence", min_height = 0.1) |>
      step_measure_peaks_integrate(method = "trapezoid")
  })

  result <- bake(rec, new_data = NULL)
  peaks <- result$.peaks[[1]]
  expect_false(all(is.na(peaks$area)))
})

test_that("step_measure_peaks_integrate with simpson method works", {
  rec <- prep_peaked_recipe(function(r) {
    r |>
      step_measure_peaks_detect(method = "prominence", min_height = 0.1) |>
      step_measure_peaks_integrate(method = "simpson")
  })

  result <- bake(rec, new_data = NULL)
  peaks <- result$.peaks[[1]]
  expect_false(all(is.na(peaks$area)))
})

test_that("step_measure_peaks_integrate with local baseline works", {
  rec <- prep_peaked_recipe(function(r) {
    r |>
      step_measure_peaks_detect(method = "prominence", min_height = 0.1) |>
      step_measure_peaks_integrate(baseline = "local")
  })

  result <- bake(rec, new_data = NULL)
  peaks <- result$.peaks[[1]]
  expect_false(all(is.na(peaks$area)))
})

test_that("step_measure_peaks_integrate errors without peaks", {
  expect_error(
    recipe(~., data = create_peaked_data()) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(value, location = vars(location)) |>
      step_measure_peaks_integrate() |>
      prep(),
    "No peaks column found"
  )
})

test_that("step_measure_peaks_integrate print method works", {
  rec <- recipe(~., data = create_peaked_data()) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(value, location = vars(location)) |>
    step_measure_peaks_detect() |>
    step_measure_peaks_integrate()

  expect_output(print(rec$steps[[3]]), "Peak integration")
})

# ==============================================================================
# step_measure_peaks_filter tests
# ==============================================================================

test_that("step_measure_peaks_filter by min_height works", {
  rec <- prep_peaked_recipe(function(r) {
    r |>
      step_measure_peaks_detect(method = "prominence", min_height = 0.05) |>
      step_measure_peaks_filter(min_height = 0.4)
  })

  result <- bake(rec, new_data = NULL)
  peaks <- result$.peaks[[1]]

  expect_true(all(peaks$height >= 0.4))
})

test_that("step_measure_peaks_filter by min_area works", {
  rec <- prep_peaked_recipe(function(r) {
    r |>
      step_measure_peaks_detect(method = "prominence", min_height = 0.05) |>
      step_measure_peaks_integrate() |>
      step_measure_peaks_filter(min_area = 1)
  })

  result <- bake(rec, new_data = NULL)
  peaks <- result$.peaks[[1]]

  expect_true(all(peaks$area >= 1 | is.na(peaks$area)))
})

test_that("step_measure_peaks_filter by min_area_pct works", {
  rec <- prep_peaked_recipe(function(r) {
    r |>
      step_measure_peaks_detect(method = "prominence", min_height = 0.05) |>
      step_measure_peaks_integrate() |>
      step_measure_peaks_filter(min_area_pct = 10)
  })

  result <- bake(rec, new_data = NULL)
  peaks <- result$.peaks[[1]]

  # Ensure at least one expectation always runs
  expect_s3_class(peaks, "tbl_df")

  # Each remaining peak should be >= 10% of total
  if (nrow(peaks) > 0) {
    total <- sum(peaks$area, na.rm = TRUE)
    pcts <- peaks$area / total * 100
    expect_true(all(pcts >= 10, na.rm = TRUE))
  }
})

test_that("step_measure_peaks_filter by max_peaks works", {
  rec <- prep_peaked_recipe(function(r) {
    r |>
      step_measure_peaks_detect(method = "prominence", min_height = 0.05) |>
      step_measure_peaks_integrate() |>
      step_measure_peaks_filter(max_peaks = 2)
  })

  result <- bake(rec, new_data = NULL)
  peaks <- result$.peaks[[1]]

  expect_lte(nrow(peaks), 2)
})

test_that("step_measure_peaks_filter print method works", {
  rec <- recipe(~., data = create_peaked_data()) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(value, location = vars(location)) |>
    step_measure_peaks_detect() |>
    step_measure_peaks_filter(min_height = 0.5, max_peaks = 3)

  expect_output(print(rec$steps[[3]]), "Peak filtering")
})

# ==============================================================================
# step_measure_peaks_to_table tests
# ==============================================================================

test_that("step_measure_peaks_to_table creates wide format", {
  rec <- prep_peaked_recipe(function(r) {
    r |>
      step_measure_peaks_detect(method = "prominence", min_height = 0.1) |>
      step_measure_peaks_integrate() |>
      step_measure_peaks_to_table(max_peaks = 3)
  })

  result <- bake(rec, new_data = NULL)

  # Should have peak columns
  expect_true("peak_1_location" %in% names(result))
  expect_true("peak_1_height" %in% names(result))
  expect_true("peak_1_area" %in% names(result))

  # Should NOT have .measures or .peaks
  expect_false(".measures" %in% names(result))
  expect_false(".peaks" %in% names(result))
})

test_that("step_measure_peaks_to_table respects max_peaks", {
  rec <- prep_peaked_recipe(function(r) {
    r |>
      step_measure_peaks_detect(method = "prominence", min_height = 0.05) |>
      step_measure_peaks_integrate() |>
      step_measure_peaks_to_table(max_peaks = 2)
  })

  result <- bake(rec, new_data = NULL)

  # Should have columns for peak 1 and 2, but not 3
  expect_true("peak_2_location" %in% names(result))
  expect_false("peak_3_location" %in% names(result))
})

test_that("step_measure_peaks_to_table respects prefix", {
  rec <- prep_peaked_recipe(function(r) {
    r |>
      step_measure_peaks_detect(method = "prominence", min_height = 0.1) |>
      step_measure_peaks_integrate() |>
      step_measure_peaks_to_table(prefix = "pk_", max_peaks = 2)
  })

  result <- bake(rec, new_data = NULL)

  expect_true("pk_1_location" %in% names(result))
  expect_false("peak_1_location" %in% names(result))
})

test_that("step_measure_peaks_to_table print method works", {
  rec <- recipe(~., data = create_peaked_data()) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(value, location = vars(location)) |>
    step_measure_peaks_detect() |>
    step_measure_peaks_to_table(max_peaks = 5)

  expect_output(print(rec$steps[[3]]), "Convert peaks to table")
})

# ==============================================================================
# Integration tests
# ==============================================================================

test_that("full peak workflow works", {
  rec <- prep_peaked_recipe(function(r) {
    r |>
      step_measure_peaks_detect(method = "prominence", min_height = 0.1) |>
      step_measure_peaks_integrate(method = "trapezoid", baseline = "local") |>
      step_measure_peaks_filter(min_area_pct = 5, max_peaks = 5) |>
      step_measure_peaks_to_table(max_peaks = 5)
  })

  result <- bake(rec, new_data = NULL)

  # Should have peak columns, no list columns
  expect_true(any(grepl("^peak_", names(result))))
  expect_false(".measures" %in% names(result))
  expect_false(".peaks" %in% names(result))
})

test_that("peak operations work with meats_long data", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_peaks_detect(method = "prominence", min_height = 0.4) |>
    prep()

  result <- bake(rec, new_data = NULL)

  expect_true(".peaks" %in% names(result))
  expect_true(is_peaks_list(result$.peaks))
})

test_that("is_peaks_list and find_peaks_cols work correctly", {
  rec <- prep_peaked_recipe(function(r) {
    step_measure_peaks_detect(r, method = "prominence")
  })

  result <- bake(rec, new_data = NULL)

  expect_true(is_peaks_list(result$.peaks))
  expect_equal(find_peaks_cols(result), ".peaks")
})
