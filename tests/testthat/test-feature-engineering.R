# ==============================================================================
# Tests for feature engineering steps
#
# This file tests:
# - step_measure_integrals
# - step_measure_ratios
# - step_measure_moments
# - step_measure_bin
# ==============================================================================

# ==============================================================================
# Test Helpers
# ==============================================================================

# Create test data with known characteristics
create_test_spectrum <- function() {
  tibble::tibble(
    id = "sample1",
    location = 1:100,
    value = sin(seq(0, 4 * pi, length.out = 100)) + 1  # Values 0-2
  )
}

create_multi_sample_data <- function(n = 5) {
  purrr::map_dfr(seq_len(n), function(i) {
    tibble::tibble(
      id = paste0("sample", i),
      location = 1:100,
      value = sin(seq(0, 4 * pi, length.out = 100) + i * 0.5) + 1 + rnorm(100, sd = 0.01)
    )
  })
}

prep_test_recipe <- function(steps_fn) {
  data <- create_multi_sample_data()

  rec <- recipe(~., data = data) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(value, location = vars(location))

  rec <- steps_fn(rec)
  prep(rec)
}

# ==============================================================================
# step_measure_integrals tests
# ==============================================================================

test_that("step_measure_integrals calculates areas with trapezoid method", {
  rec <- prep_test_recipe(function(r) {
    step_measure_integrals(r, regions = list(low = c(1, 30), high = c(70, 100)))
  })

  result <- bake(rec, new_data = NULL)

  expect_true("integral_low" %in% names(result))
  expect_true("integral_high" %in% names(result))
  expect_false(anyNA(result$integral_low))
  expect_false(anyNA(result$integral_high))
})

test_that("step_measure_integrals works with simpson method", {
  rec <- prep_test_recipe(function(r) {
    step_measure_integrals(r,
      regions = list(region1 = c(10, 50)),
      method = "simpson"
    )
  })

  result <- bake(rec, new_data = NULL)

  expect_true("integral_region1" %in% names(result))
  expect_false(anyNA(result$integral_region1))
})

test_that("step_measure_integrals uses custom prefix", {
  rec <- prep_test_recipe(function(r) {
    step_measure_integrals(r,
      regions = list(peak = c(40, 60)),
      prefix = "area_"
    )
  })

  result <- bake(rec, new_data = NULL)

  expect_true("area_peak" %in% names(result))
  expect_false("integral_peak" %in% names(result))
})

test_that("step_measure_integrals handles unnamed regions", {
  rec <- prep_test_recipe(function(r) {
    step_measure_integrals(r, regions = list(c(1, 30), c(70, 100)))
  })

  result <- bake(rec, new_data = NULL)

  expect_true("integral_1" %in% names(result))
  expect_true("integral_2" %in% names(result))
})

test_that("step_measure_integrals validates regions", {
  expect_error(
    recipe(~., data = create_test_spectrum()) |>
      step_measure_integrals(regions = c(1, 50)),
    "must be a list"
  )

  expect_error(
    recipe(~., data = create_test_spectrum()) |>
      step_measure_integrals(regions = list(c(50, 10))),
    "min < max"
  )
})

test_that("step_measure_integrals print method works", {
  rec <- recipe(~., data = create_test_spectrum()) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(value, location = vars(location)) |>
    step_measure_integrals(regions = list(r1 = c(1, 50), r2 = c(50, 100)))

  expect_output(print(rec$steps[[2]]), "integrals.*2 region")
})

test_that("step_measure_integrals tidy method works", {
  rec <- prep_test_recipe(function(r) {
    step_measure_integrals(r, regions = list(r1 = c(1, 50)))
  })

  tidy_result <- tidy(rec, number = 2)
  expect_s3_class(tidy_result, "tbl_df")
  expect_true("method" %in% names(tidy_result))
})

# ==============================================================================
# step_measure_ratios tests
# ==============================================================================

test_that("step_measure_ratios calculates ratios", {
  rec <- prep_test_recipe(function(r) {
    step_measure_ratios(r,
      numerator = c(1, 30),
      denominator = c(70, 100),
      name = "low_high_ratio"
    )
  })

  result <- bake(rec, new_data = NULL)

  expect_true("low_high_ratio" %in% names(result))
  expect_false(anyNA(result$low_high_ratio))
  expect_true(all(result$low_high_ratio > 0))
})

test_that("step_measure_ratios works with simpson method", {
  rec <- prep_test_recipe(function(r) {
    step_measure_ratios(r,
      numerator = c(1, 50),
      denominator = c(50, 100),
      method = "simpson"
    )
  })

  result <- bake(rec, new_data = NULL)

  expect_true("ratio_1" %in% names(result))
})

test_that("step_measure_ratios validates inputs", {
  expect_error(
    recipe(~., data = create_test_spectrum()) |>
      step_measure_ratios(numerator = c(100, 1), denominator = c(1, 50)),
    "min < max"
  )

  expect_error(
    recipe(~., data = create_test_spectrum()) |>
      step_measure_ratios(numerator = c(1, 50), denominator = c(100, 1)),
    "min < max"
  )
})

test_that("step_measure_ratios print method works", {
  rec <- recipe(~., data = create_test_spectrum()) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(value, location = vars(location)) |>
    step_measure_ratios(numerator = c(1, 30), denominator = c(70, 100))

  expect_output(print(rec$steps[[2]]), "ratio")
})

test_that("step_measure_ratios tidy method works", {
  rec <- prep_test_recipe(function(r) {
    step_measure_ratios(r, numerator = c(1, 30), denominator = c(70, 100))
  })

  tidy_result <- tidy(rec, number = 2)
  expect_s3_class(tidy_result, "tbl_df")
  expect_true("numerator_min" %in% names(tidy_result))
  expect_true("denominator_min" %in% names(tidy_result))
})

# ==============================================================================
# step_measure_moments tests
# ==============================================================================

test_that("step_measure_moments calculates default moments", {
  rec <- prep_test_recipe(function(r) {
    step_measure_moments(r)
  })

  result <- bake(rec, new_data = NULL)

  expect_true("moment_mean" %in% names(result))
  expect_true("moment_sd" %in% names(result))
  expect_true("moment_skewness" %in% names(result))
  expect_true("moment_kurtosis" %in% names(result))
})

test_that("step_measure_moments calculates selected moments", {
  rec <- prep_test_recipe(function(r) {
    step_measure_moments(r, moments = c("mean", "entropy"))
  })

  result <- bake(rec, new_data = NULL)

  expect_true("moment_mean" %in% names(result))
  expect_true("moment_entropy" %in% names(result))
  expect_false("moment_sd" %in% names(result))
})

test_that("step_measure_moments works with weighted option", {
  rec_unweighted <- prep_test_recipe(function(r) {
    step_measure_moments(r, moments = c("mean"), weighted = FALSE, prefix = "uw_")
  })

  rec_weighted <- prep_test_recipe(function(r) {
    step_measure_moments(r, moments = c("mean"), weighted = TRUE, prefix = "w_")
  })

  result_uw <- bake(rec_unweighted, new_data = NULL)
  result_w <- bake(rec_weighted, new_data = NULL)

  # Weighted and unweighted should produce different results
  expect_false(all(result_uw$uw_mean == result_w$w_mean))
})

test_that("step_measure_moments validates moment names", {
  expect_error(
    recipe(~., data = create_test_spectrum()) |>
      step_measure_moments(moments = c("mean", "invalid")),
    "Invalid moments"
  )
})

test_that("step_measure_moments print method works", {
  rec <- recipe(~., data = create_test_spectrum()) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(value, location = vars(location)) |>
    step_measure_moments(moments = c("mean", "sd"))

  expect_output(print(rec$steps[[2]]), "moments.*mean.*sd")
})

test_that("step_measure_moments tidy method works", {
  rec <- prep_test_recipe(function(r) {
    step_measure_moments(r, moments = c("mean", "sd"))
  })

  tidy_result <- tidy(rec, number = 2)
  expect_s3_class(tidy_result, "tbl_df")
  expect_true("moments" %in% names(tidy_result))
})

# ==============================================================================
# step_measure_bin tests
# ==============================================================================

test_that("step_measure_bin reduces spectrum with n_bins", {
  rec <- prep_test_recipe(function(r) {
    step_measure_bin(r, n_bins = 10)
  })

  result <- bake(rec, new_data = NULL)

  # Each spectrum should have 10 points
  expect_equal(nrow(result$.measures[[1]]), 10)
})

test_that("step_measure_bin reduces spectrum with bin_width", {
  rec <- prep_test_recipe(function(r) {
    step_measure_bin(r, bin_width = 20)
  })

  result <- bake(rec, new_data = NULL)

  # Should have approximately 100/20 = 5 bins (may have 6 if not evenly divisible)
  expect_lte(nrow(result$.measures[[1]]), 6)
  expect_gte(nrow(result$.measures[[1]]), 5)
})

test_that("step_measure_bin works with different methods", {
  rec_mean <- prep_test_recipe(function(r) {
    step_measure_bin(r, n_bins = 10, method = "mean")
  })

  rec_sum <- prep_test_recipe(function(r) {
    step_measure_bin(r, n_bins = 10, method = "sum")
  })

  rec_median <- prep_test_recipe(function(r) {
    step_measure_bin(r, n_bins = 10, method = "median")
  })

  rec_max <- prep_test_recipe(function(r) {
    step_measure_bin(r, n_bins = 10, method = "max")
  })

  result_mean <- bake(rec_mean, new_data = NULL)
  result_sum <- bake(rec_sum, new_data = NULL)
  result_median <- bake(rec_median, new_data = NULL)
  result_max <- bake(rec_max, new_data = NULL)

  # Sum should be larger than mean (since each bin has ~10 points)
  expect_gt(
    sum(result_sum$.measures[[1]]$value),
    sum(result_mean$.measures[[1]]$value)
  )

  # Max should be >= mean
  expect_gte(
    sum(result_max$.measures[[1]]$value),
    sum(result_mean$.measures[[1]]$value)
  )
})

test_that("step_measure_bin validates inputs", {
  expect_error(
    recipe(~., data = create_test_spectrum()) |>
      step_measure_bin(),
    "Either.*n_bins.*or.*bin_width"
  )

  expect_error(
    recipe(~., data = create_test_spectrum()) |>
      step_measure_bin(n_bins = 10, bin_width = 5),
    "mutually exclusive"
  )

  expect_error(
    recipe(~., data = create_test_spectrum()) |>
      step_measure_bin(n_bins = 1),
    "must be a positive integer >= 2"
  )
})

test_that("step_measure_bin print method works", {
  rec <- recipe(~., data = create_test_spectrum()) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(value, location = vars(location)) |>
    step_measure_bin(n_bins = 20)

  expect_output(print(rec$steps[[2]]), "Bin.*20.*bins")
})

test_that("step_measure_bin tidy method works", {
  rec <- prep_test_recipe(function(r) {
    step_measure_bin(r, n_bins = 10)
  })

  tidy_result <- tidy(rec, number = 2)
  expect_s3_class(tidy_result, "tbl_df")
  expect_true("n_bins" %in% names(tidy_result))
  expect_true("method" %in% names(tidy_result))
})

test_that("step_measure_bin is tunable", {
  step <- step_measure_bin(recipe(~., data = create_test_spectrum()), n_bins = 10)
  tunable_result <- tunable(step$steps[[1]])
  expect_s3_class(tunable_result, "tbl_df")
  expect_true("n_bins" %in% tunable_result$name)
})

# ==============================================================================
# Integration tests
# ==============================================================================

test_that("feature engineering steps work with meats_long data", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_integrals(regions = list(r1 = c(1, 50), r2 = c(51, 100))) |>
    step_measure_moments(moments = c("mean", "sd")) |>
    prep()

  result <- bake(rec, new_data = NULL)

  expect_true("integral_r1" %in% names(result))
  expect_true("integral_r2" %in% names(result))
  expect_true("moment_mean" %in% names(result))
  expect_true("moment_sd" %in% names(result))
  expect_true(".measures" %in% names(result))
})

test_that("binning followed by feature extraction works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_bin(n_bins = 20) |>
    step_measure_moments(moments = c("mean", "sd")) |>
    prep()

  result <- bake(rec, new_data = NULL)

  # Should have binned measures
  expect_equal(nrow(result$.measures[[1]]), 20)
  # And moment columns
  expect_true("moment_mean" %in% names(result))
})
