# ==============================================================================
# Tests for Alignment Steps
# ==============================================================================

# ------------------------------------------------------------------------------
# step_measure_align_shift
# ------------------------------------------------------------------------------

test_that("step_measure_align_shift runs in recipe workflow", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_align_shift(max_shift = 5) |>
    prep()

  result <- bake(rec, new_data = NULL)

  expect_s3_class(result, "tbl_df")
  expect_true(".measures" %in% names(result))
  expect_true(is_measure_list(result$.measures))
})

test_that("step_measure_align_shift corrects shifts", {
  # Create data with a shifted spectrum
  n_points <- 100
  base_signal <- sin(seq(0, 4 * pi, length.out = n_points))

  # Sample 1: reference
  # Sample 2: shifted by 3 points
  synthetic_data <- tibble::tibble(
    id = c(rep(1, n_points), rep(2, n_points)),
    outcome = rep(1, 2 * n_points),
    value = c(base_signal, c(rep(0, 3), base_signal[1:(n_points - 3)])),
    location = rep(seq_len(n_points), 2)
  )

  rec <- recipe(outcome ~ ., data = synthetic_data) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(value, location = vars(location)) |>
    step_measure_align_shift(max_shift = 5, reference = "first") |>
    prep()

  result <- bake(rec, new_data = NULL)

  # After alignment, spectra should be more similar
  orig_s1 <- base_signal
  orig_s2 <- c(rep(0, 3), base_signal[1:(n_points - 3)])
  aligned_s1 <- result$.measures[[1]]$value
  aligned_s2 <- result$.measures[[2]]$value

  # Original correlation is lower
  orig_corr <- cor(orig_s1, orig_s2)
  # Aligned correlation should be higher
  aligned_corr <- cor(aligned_s1, aligned_s2)

  expect_gt(aligned_corr, orig_corr)
})

test_that("step_measure_align_shift reference options work", {
  for (ref in c("mean", "median", "first")) {
    rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_measure_align_shift(reference = ref) |>
      prep()

    result <- bake(rec, new_data = NULL)
    expect_true(is_measure_list(result$.measures))
  }
})

test_that("step_measure_align_shift validates max_shift", {
  expect_error(
    recipe(water + fat + protein ~ ., data = meats_long) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_measure_align_shift(max_shift = 0) |>
      prep(),
    "max_shift"
  )
})

test_that("step_measure_align_shift tidy method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_align_shift(max_shift = 10, reference = "median")

  tidy_result <- tidy(rec, number = 2)
  expect_equal(tidy_result$max_shift, 10)
  expect_equal(tidy_result$reference, "median")
})


# ------------------------------------------------------------------------------
# step_measure_align_reference
# ------------------------------------------------------------------------------

test_that("step_measure_align_reference runs with provided reference", {
  # Get a reference spectrum from the data
  rec_input <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    prep()

  input_data <- bake(rec_input, new_data = NULL)
  ref <- input_data$.measures[[1]]$value

  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_align_reference(ref_spectrum = ref, max_shift = 5) |>
    prep()

  result <- bake(rec, new_data = NULL)

  expect_s3_class(result, "tbl_df")
  expect_true(is_measure_list(result$.measures))
})


# ------------------------------------------------------------------------------
# step_measure_align_dtw
# ------------------------------------------------------------------------------

test_that("step_measure_align_dtw runs in recipe workflow", {
  skip_if_not_installed("dtw")

  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_align_dtw() |>
    prep()

  result <- bake(rec, new_data = NULL)

  expect_s3_class(result, "tbl_df")
  expect_true(is_measure_list(result$.measures))
})

test_that("step_measure_align_dtw window options work", {
  skip_if_not_installed("dtw")

  for (wtype in c("none", "sakoechiba", "slantedband")) {
    rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_measure_align_dtw(window_type = wtype, window_size = 5) |>
      prep()

    result <- bake(rec, new_data = NULL)
    expect_true(is_measure_list(result$.measures))
  }
})

test_that("step_measure_align_dtw requires dtw package", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_align_dtw()

  pkgs <- required_pkgs(rec$steps[[2]])
  expect_true("dtw" %in% pkgs)
})

test_that("step_measure_align_dtw tidy method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_align_dtw(reference = "median", window_type = "sakoechiba")

  tidy_result <- tidy(rec, number = 2)
  expect_equal(tidy_result$reference, "median")
  expect_equal(tidy_result$window_type, "sakoechiba")
})


# ------------------------------------------------------------------------------
# step_measure_align_ptw
# ------------------------------------------------------------------------------

test_that("step_measure_align_ptw runs in recipe workflow", {
  skip_if_not_installed("ptw")

  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_align_ptw() |>
    prep()

  result <- bake(rec, new_data = NULL)

  expect_s3_class(result, "tbl_df")
  expect_true(is_measure_list(result$.measures))
})

test_that("step_measure_align_ptw requires ptw package", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_align_ptw()

  pkgs <- required_pkgs(rec$steps[[2]])
  expect_true("ptw" %in% pkgs)
})

test_that("step_measure_align_ptw tidy method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_align_ptw(reference = "median")

  tidy_result <- tidy(rec, number = 2)
  expect_equal(tidy_result$reference, "median")
})


# ------------------------------------------------------------------------------
# step_measure_align_cow
# ------------------------------------------------------------------------------

test_that("step_measure_align_cow runs in recipe workflow", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_align_cow(segment_length = 20, slack = 1) |>
    prep()

  result <- bake(rec, new_data = NULL)

  expect_s3_class(result, "tbl_df")
  expect_true(is_measure_list(result$.measures))
})

test_that("step_measure_align_cow corrects shifts", {
  # Create synthetic data with a warped spectrum
  n_points <- 100
  base_signal <- sin(seq(0, 4 * pi, length.out = n_points))

  # Sample 1: reference
  # Sample 2: slightly warped (stretched in middle)
  x_warped <- c(
    seq(1, 40, length.out = 35),
    seq(40, 60, length.out = 30),  # stretched
    seq(60, 100, length.out = 35)
  )
  warped_signal <- stats::approx(1:n_points, base_signal, xout = x_warped)$y

  synthetic_data <- tibble::tibble(
    id = c(rep(1, n_points), rep(2, n_points)),
    outcome = rep(1, 2 * n_points),
    value = c(base_signal, warped_signal),
    location = rep(seq_len(n_points), 2)
  )

  rec <- recipe(outcome ~ ., data = synthetic_data) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(value, location = vars(location)) |>
    step_measure_align_cow(segment_length = 20, slack = 2, reference = "first") |>
    prep()

  result <- bake(rec, new_data = NULL)

  # After alignment, spectra should be more similar
  orig_corr <- cor(base_signal, warped_signal)
  aligned_s1 <- result$.measures[[1]]$value
  aligned_s2 <- result$.measures[[2]]$value
  aligned_corr <- cor(aligned_s1, aligned_s2)

  # Alignment should improve or maintain correlation
  expect_gte(aligned_corr, orig_corr - 0.1)  # Allow small tolerance
})

test_that("step_measure_align_cow reference options work", {
  for (ref in c("mean", "median", "first")) {
    rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_measure_align_cow(segment_length = 20, reference = ref) |>
      prep()

    result <- bake(rec, new_data = NULL)
    expect_true(is_measure_list(result$.measures))
  }
})

test_that("step_measure_align_cow validates parameters", {
  expect_error(
    recipe(water + fat + protein ~ ., data = meats_long) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_measure_align_cow(segment_length = 1) |>
      prep(),
    "segment_length"
  )

  expect_error(
    recipe(water + fat + protein ~ ., data = meats_long) |>
      update_role(id, new_role = "id") |>
      step_measure_input_long(transmittance, location = vars(channel)) |>
      step_measure_align_cow(slack = -1) |>
      prep(),
    "slack"
  )
})

test_that("step_measure_align_cow tidy method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_align_cow(segment_length = 25, slack = 2, reference = "median")

  tidy_result <- tidy(rec, number = 2)
  expect_equal(tidy_result$segment_length, 25)
  expect_equal(tidy_result$slack, 2)
  expect_equal(tidy_result$reference, "median")
})

test_that("step_measure_align_cow is tunable", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_align_cow()

  tunable_params <- tunable(rec$steps[[2]])
  expect_true("segment_length" %in% tunable_params$name)
  expect_true("slack" %in% tunable_params$name)
})
