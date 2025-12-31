# ==============================================================================
# Tests for Multi-Channel Operations
# ==============================================================================

# ------------------------------------------------------------------------------
# Test data setup - uses WIDE format (one row per sample)
# ------------------------------------------------------------------------------

test_df_wide_aligned <- function() {
  # Wide format: one row per sample, measurements as separate columns
  # Both UV and RI share the same time grid (0-9)
  n_samples <- 3

  tibble::tibble(
    id = 1:n_samples,
    # UV measurements at each time point
    uv_0 = rnorm(n_samples, 100, 10),
    uv_1 = rnorm(n_samples, 100, 10),
    uv_2 = rnorm(n_samples, 100, 10),
    uv_3 = rnorm(n_samples, 100, 10),
    uv_4 = rnorm(n_samples, 100, 10),
    uv_5 = rnorm(n_samples, 100, 10),
    uv_6 = rnorm(n_samples, 100, 10),
    uv_7 = rnorm(n_samples, 100, 10),
    uv_8 = rnorm(n_samples, 100, 10),
    uv_9 = rnorm(n_samples, 100, 10),
    # RI measurements at each time point (same grid)
    ri_0 = rnorm(n_samples, 50, 5),
    ri_1 = rnorm(n_samples, 50, 5),
    ri_2 = rnorm(n_samples, 50, 5),
    ri_3 = rnorm(n_samples, 50, 5),
    ri_4 = rnorm(n_samples, 50, 5),
    ri_5 = rnorm(n_samples, 50, 5),
    ri_6 = rnorm(n_samples, 50, 5),
    ri_7 = rnorm(n_samples, 50, 5),
    ri_8 = rnorm(n_samples, 50, 5),
    ri_9 = rnorm(n_samples, 50, 5),
    concentration = c(10, 25, 50)
  )
}

test_df_wide_misaligned <- function() {
  # Wide format: different grids for UV (0-9) and RI (0.5-9.5)
  n_samples <- 3

  tibble::tibble(
    id = 1:n_samples,
    # UV at times 0, 1, 2, ..., 9
    uv_0 = rnorm(n_samples, 100, 10),
    uv_1 = rnorm(n_samples, 100, 10),
    uv_2 = rnorm(n_samples, 100, 10),
    uv_3 = rnorm(n_samples, 100, 10),
    uv_4 = rnorm(n_samples, 100, 10),
    uv_5 = rnorm(n_samples, 100, 10),
    uv_6 = rnorm(n_samples, 100, 10),
    uv_7 = rnorm(n_samples, 100, 10),
    uv_8 = rnorm(n_samples, 100, 10),
    uv_9 = rnorm(n_samples, 100, 10),
    # RI at times 0.5, 1.5, 2.5, ..., 9.5 (offset by 0.5)
    `ri_0.5` = rnorm(n_samples, 50, 5),
    `ri_1.5` = rnorm(n_samples, 50, 5),
    `ri_2.5` = rnorm(n_samples, 50, 5),
    `ri_3.5` = rnorm(n_samples, 50, 5),
    `ri_4.5` = rnorm(n_samples, 50, 5),
    `ri_5.5` = rnorm(n_samples, 50, 5),
    `ri_6.5` = rnorm(n_samples, 50, 5),
    `ri_7.5` = rnorm(n_samples, 50, 5),
    `ri_8.5` = rnorm(n_samples, 50, 5),
    `ri_9.5` = rnorm(n_samples, 50, 5),
    concentration = c(10, 25, 50)
  )
}

# ------------------------------------------------------------------------------
# step_measure_channel_align tests
# ------------------------------------------------------------------------------

test_that("step_measure_channel_align aligns two channels with union method", {
  set.seed(42)
  df <- test_df_wide_misaligned()

  # Use explicit location values for misaligned grids
  uv_locs <- 0:9
  ri_locs <- seq(0.5, 9.5, by = 1)

  rec <- recipes::recipe(concentration ~ ., data = df) |>
    recipes::update_role(id, new_role = "id") |>
    step_measure_input_wide(
      dplyr::starts_with("uv_"),
      col_name = "uv",
      location_values = uv_locs
    ) |>
    step_measure_input_wide(
      dplyr::starts_with("ri_"),
      col_name = "ri",
      location_values = ri_locs
    ) |>
    step_measure_channel_align(method = "union") |>
    recipes::prep()

  result <- recipes::bake(rec, new_data = NULL)

  # Both channels should now have the same grid
  grid_uv <- result$uv[[1]]$location
  grid_ri <- result$ri[[1]]$location
  expect_equal(grid_uv, grid_ri)

  # Union should have 20 points (10 from each grid)
  expect_equal(length(grid_uv), 20)
})


test_that("step_measure_channel_align works with intersection method", {
  set.seed(42)
  df <- test_df_wide_misaligned()

  uv_locs <- 0:9
  ri_locs <- seq(0.5, 9.5, by = 1)

  rec <- recipes::recipe(concentration ~ ., data = df) |>
    recipes::update_role(id, new_role = "id") |>
    step_measure_input_wide(
      dplyr::starts_with("uv_"),
      col_name = "uv",
      location_values = uv_locs
    ) |>
    step_measure_input_wide(
      dplyr::starts_with("ri_"),
      col_name = "ri",
      location_values = ri_locs
    ) |>
    step_measure_channel_align(method = "intersection") |>
    recipes::prep()

  result <- recipes::bake(rec, new_data = NULL)

  # Both channels should have the same grid
  grid_uv <- result$uv[[1]]$location
  grid_ri <- result$ri[[1]]$location
  expect_equal(grid_uv, grid_ri)

  # Intersection has no common points (0.5 offset) - should be empty
  expect_equal(length(grid_uv), 0)
})


test_that("step_measure_channel_align works with reference method", {
  set.seed(42)
  df <- test_df_wide_misaligned()

  uv_locs <- 0:9
  ri_locs <- seq(0.5, 9.5, by = 1)

  rec <- recipes::recipe(concentration ~ ., data = df) |>
    recipes::update_role(id, new_role = "id") |>
    step_measure_input_wide(
      dplyr::starts_with("uv_"),
      col_name = "uv",
      location_values = uv_locs
    ) |>
    step_measure_input_wide(
      dplyr::starts_with("ri_"),
      col_name = "ri",
      location_values = ri_locs
    ) |>
    step_measure_channel_align(method = "reference", reference = 1L) |>
    recipes::prep()

  result <- recipes::bake(rec, new_data = NULL)

  # RI should now match UV's original grid
  grid_ri <- result$ri[[1]]$location
  expect_equal(grid_ri, 0:9)
})


test_that("step_measure_channel_align supports different interpolation methods", {
  set.seed(42)
  df <- test_df_wide_misaligned()

  uv_locs <- 0:9
  ri_locs <- seq(0.5, 9.5, by = 1)

  for (interp in c("linear", "spline", "constant")) {
    rec <- recipes::recipe(concentration ~ ., data = df) |>
      recipes::update_role(id, new_role = "id") |>
      step_measure_input_wide(
        dplyr::starts_with("uv_"),
        col_name = "uv",
        location_values = uv_locs
      ) |>
      step_measure_input_wide(
        dplyr::starts_with("ri_"),
        col_name = "ri",
        location_values = ri_locs
      ) |>
      step_measure_channel_align(
        method = "reference",
        interpolation = interp
      ) |>
      recipes::prep()

    result <- recipes::bake(rec, new_data = NULL)
    expect_s3_class(result$ri, "measure_list")
  }
})


test_that("step_measure_channel_align errors with < 2 columns", {
  set.seed(42)
  df <- test_df_wide_aligned()

  rec <- recipes::recipe(concentration ~ ., data = df) |>
    recipes::update_role(id, new_role = "id") |>
    step_measure_input_wide(dplyr::starts_with("uv_"), col_name = "uv") |>
    step_measure_channel_align()

  expect_error(recipes::prep(rec), "at least 2 measure columns")
})


test_that("step_measure_channel_align tidy method works", {
  set.seed(42)
  df <- test_df_wide_misaligned()

  uv_locs <- 0:9
  ri_locs <- seq(0.5, 9.5, by = 1)

  rec <- recipes::recipe(concentration ~ ., data = df) |>
    recipes::update_role(id, new_role = "id") |>
    step_measure_input_wide(
      dplyr::starts_with("uv_"),
      col_name = "uv",
      location_values = uv_locs
    ) |>
    step_measure_input_wide(
      dplyr::starts_with("ri_"),
      col_name = "ri",
      location_values = ri_locs
    ) |>
    step_measure_channel_align() |>
    recipes::prep()

  tidy_result <- recipes::tidy(rec, number = 3)
  expect_s3_class(tidy_result, "tbl_df")
  expect_true("method" %in% names(tidy_result))
  expect_true("n_grid_points" %in% names(tidy_result))
})


test_that("step_measure_channel_align print method works", {
  set.seed(42)
  df <- test_df_wide_misaligned()

  uv_locs <- 0:9
  ri_locs <- seq(0.5, 9.5, by = 1)

  rec <- recipes::recipe(concentration ~ ., data = df) |>
    recipes::update_role(id, new_role = "id") |>
    step_measure_input_wide(
      dplyr::starts_with("uv_"),
      col_name = "uv",
      location_values = uv_locs
    ) |>
    step_measure_input_wide(
      dplyr::starts_with("ri_"),
      col_name = "ri",
      location_values = ri_locs
    ) |>
    step_measure_channel_align() |>
    recipes::prep()

  step <- rec$steps[[3]]
  expect_no_error(print(step))
})


# ------------------------------------------------------------------------------
# step_measure_channel_combine tests
# ------------------------------------------------------------------------------

test_that("step_measure_channel_combine stacks channels into nD", {
  set.seed(42)
  df <- test_df_wide_aligned()

  rec <- recipes::recipe(concentration ~ ., data = df) |>
    recipes::update_role(id, new_role = "id") |>
    step_measure_input_wide(dplyr::starts_with("uv_"), col_name = "uv") |>
    step_measure_input_wide(dplyr::starts_with("ri_"), col_name = "ri") |>
    step_measure_channel_combine(strategy = "stack") |>
    recipes::prep()

  result <- recipes::bake(rec, new_data = NULL)

  # Should have a single .measures column
  expect_true(".measures" %in% names(result))
  expect_false("uv" %in% names(result)) # Original removed
  expect_false("ri" %in% names(result))

  # Should be nD
  expect_s3_class(result$.measures, "measure_nd_list")
  expect_equal(measure_ndim(result$.measures[[1]]), 2L)
})


test_that("step_measure_channel_combine concatenates channels", {
  set.seed(42)
  df <- test_df_wide_aligned()

  rec <- recipes::recipe(concentration ~ ., data = df) |>
    recipes::update_role(id, new_role = "id") |>
    step_measure_input_wide(dplyr::starts_with("uv_"), col_name = "uv") |>
    step_measure_input_wide(dplyr::starts_with("ri_"), col_name = "ri") |>
    step_measure_channel_combine(strategy = "concat") |>
    recipes::prep()

  result <- recipes::bake(rec, new_data = NULL)

  # Should have 1D measure with doubled length
  expect_s3_class(result$.measures, "measure_list")
  expect_equal(nrow(result$.measures[[1]]), 20) # 10 + 10
})


test_that("step_measure_channel_combine computes weighted sum", {
  set.seed(42)
  df <- test_df_wide_aligned()

  rec <- recipes::recipe(concentration ~ ., data = df) |>
    recipes::update_role(id, new_role = "id") |>
    step_measure_input_wide(dplyr::starts_with("uv_"), col_name = "uv") |>
    step_measure_input_wide(dplyr::starts_with("ri_"), col_name = "ri") |>
    step_measure_channel_combine(
      strategy = "weighted_sum",
      weights = c(0.7, 0.3)
    ) |>
    recipes::prep()

  result <- recipes::bake(rec, new_data = NULL)

  # Should be 1D with original length
  expect_s3_class(result$.measures, "measure_list")
  expect_equal(nrow(result$.measures[[1]]), 10)
})


test_that("step_measure_channel_combine computes mean", {
  set.seed(42)
  df <- test_df_wide_aligned()

  rec <- recipes::recipe(concentration ~ ., data = df) |>
    recipes::update_role(id, new_role = "id") |>
    step_measure_input_wide(dplyr::starts_with("uv_"), col_name = "uv") |>
    step_measure_input_wide(dplyr::starts_with("ri_"), col_name = "ri") |>
    step_measure_channel_combine(strategy = "mean") |>
    recipes::prep()

  result <- recipes::bake(rec, new_data = NULL)

  # Should be 1D with original length
  expect_s3_class(result$.measures, "measure_list")
  expect_equal(nrow(result$.measures[[1]]), 10)
})


test_that("step_measure_channel_combine keeps original with remove_original=FALSE", {
  set.seed(42)
  df <- test_df_wide_aligned()

  rec <- recipes::recipe(concentration ~ ., data = df) |>
    recipes::update_role(id, new_role = "id") |>
    step_measure_input_wide(dplyr::starts_with("uv_"), col_name = "uv") |>
    step_measure_input_wide(dplyr::starts_with("ri_"), col_name = "ri") |>
    step_measure_channel_combine(strategy = "mean", remove_original = FALSE) |>
    recipes::prep()

  result <- recipes::bake(rec, new_data = NULL)

  # Should have both original and combined
  expect_true(".measures" %in% names(result))
  expect_true("uv" %in% names(result))
  expect_true("ri" %in% names(result))
})


test_that("step_measure_channel_combine uses custom output_col", {
  set.seed(42)
  df <- test_df_wide_aligned()

  rec <- recipes::recipe(concentration ~ ., data = df) |>
    recipes::update_role(id, new_role = "id") |>
    step_measure_input_wide(dplyr::starts_with("uv_"), col_name = "uv") |>
    step_measure_input_wide(dplyr::starts_with("ri_"), col_name = "ri") |>
    step_measure_channel_combine(strategy = "mean", output_col = "combined") |>
    recipes::prep()

  result <- recipes::bake(rec, new_data = NULL)
  expect_true("combined" %in% names(result))
})


test_that("step_measure_channel_combine errors with mismatched weights", {
  set.seed(42)
  df <- test_df_wide_aligned()

  rec <- recipes::recipe(concentration ~ ., data = df) |>
    recipes::update_role(id, new_role = "id") |>
    step_measure_input_wide(dplyr::starts_with("uv_"), col_name = "uv") |>
    step_measure_input_wide(dplyr::starts_with("ri_"), col_name = "ri") |>
    step_measure_channel_combine(
      strategy = "weighted_sum",
      weights = c(0.5, 0.3, 0.2)
    )

  expect_error(recipes::prep(rec), "Weights length")
})


test_that("step_measure_channel_combine tidy method works", {
  set.seed(42)
  df <- test_df_wide_aligned()

  rec <- recipes::recipe(concentration ~ ., data = df) |>
    recipes::update_role(id, new_role = "id") |>
    step_measure_input_wide(dplyr::starts_with("uv_"), col_name = "uv") |>
    step_measure_input_wide(dplyr::starts_with("ri_"), col_name = "ri") |>
    step_measure_channel_combine(strategy = "stack") |>
    recipes::prep()

  tidy_result <- recipes::tidy(rec, number = 3)
  expect_s3_class(tidy_result, "tbl_df")
  expect_true("strategy" %in% names(tidy_result))
})


test_that("step_measure_channel_combine print method works", {
  set.seed(42)
  df <- test_df_wide_aligned()

  rec <- recipes::recipe(concentration ~ ., data = df) |>
    recipes::update_role(id, new_role = "id") |>
    step_measure_input_wide(dplyr::starts_with("uv_"), col_name = "uv") |>
    step_measure_input_wide(dplyr::starts_with("ri_"), col_name = "ri") |>
    step_measure_channel_combine(strategy = "stack") |>
    recipes::prep()

  step <- rec$steps[[3]]
  expect_no_error(print(step))
})


# ------------------------------------------------------------------------------
# step_measure_channel_ratio tests
# ------------------------------------------------------------------------------

test_that("step_measure_channel_ratio computes basic ratio", {
  set.seed(42)
  df <- test_df_wide_aligned()

  rec <- recipes::recipe(concentration ~ ., data = df) |>
    recipes::update_role(id, new_role = "id") |>
    step_measure_input_wide(dplyr::starts_with("uv_"), col_name = "uv") |>
    step_measure_input_wide(dplyr::starts_with("ri_"), col_name = "ri") |>
    step_measure_channel_ratio(numerator = "uv", denominator = "ri") |>
    recipes::prep()

  result <- recipes::bake(rec, new_data = NULL)

  # Should have ratio column
  expect_true("ratio_uv_ri" %in% names(result))
  expect_s3_class(result$ratio_uv_ri, "measure_list")

  # Original columns still present by default
  expect_true("uv" %in% names(result))
  expect_true("ri" %in% names(result))
})


test_that("step_measure_channel_ratio applies log transform", {
  set.seed(42)
  df <- test_df_wide_aligned()

  rec <- recipes::recipe(concentration ~ ., data = df) |>
    recipes::update_role(id, new_role = "id") |>
    step_measure_input_wide(dplyr::starts_with("uv_"), col_name = "uv") |>
    step_measure_input_wide(dplyr::starts_with("ri_"), col_name = "ri") |>
    step_measure_channel_ratio(
      numerator = "uv",
      denominator = "ri",
      log_transform = TRUE
    ) |>
    recipes::prep()

  result <- recipes::bake(rec, new_data = NULL)

  # Log of ratio should include positive and possibly negative values
  ratio_vals <- result$ratio_uv_ri[[1]]$value
  expect_true(all(is.finite(ratio_vals)))
})


test_that("step_measure_channel_ratio removes original columns when requested", {
  set.seed(42)
  df <- test_df_wide_aligned()

  rec <- recipes::recipe(concentration ~ ., data = df) |>
    recipes::update_role(id, new_role = "id") |>
    step_measure_input_wide(dplyr::starts_with("uv_"), col_name = "uv") |>
    step_measure_input_wide(dplyr::starts_with("ri_"), col_name = "ri") |>
    step_measure_channel_ratio(
      numerator = "uv",
      denominator = "ri",
      remove_original = TRUE
    ) |>
    recipes::prep()

  result <- recipes::bake(rec, new_data = NULL)

  expect_true("ratio_uv_ri" %in% names(result))
  expect_false("uv" %in% names(result))
  expect_false("ri" %in% names(result))
})


test_that("step_measure_channel_ratio uses custom prefix", {
  set.seed(42)
  df <- test_df_wide_aligned()

  rec <- recipes::recipe(concentration ~ ., data = df) |>
    recipes::update_role(id, new_role = "id") |>
    step_measure_input_wide(dplyr::starts_with("uv_"), col_name = "uv") |>
    step_measure_input_wide(dplyr::starts_with("ri_"), col_name = "ri") |>
    step_measure_channel_ratio(
      numerator = "uv",
      denominator = "ri",
      output_prefix = "my_ratio_"
    ) |>
    recipes::prep()

  result <- recipes::bake(rec, new_data = NULL)
  expect_true("my_ratio_uv_ri" %in% names(result))
})


test_that("step_measure_channel_ratio handles multiple pairs", {
  set.seed(42)
  # Three channels
  n_samples <- 3
  df <- tibble::tibble(
    id = 1:n_samples,
    ch1_0 = rnorm(n_samples, 100, 10),
    ch1_1 = rnorm(n_samples, 100, 10),
    ch1_2 = rnorm(n_samples, 100, 10),
    ch1_3 = rnorm(n_samples, 100, 10),
    ch1_4 = rnorm(n_samples, 100, 10),
    ch2_0 = rnorm(n_samples, 50, 5),
    ch2_1 = rnorm(n_samples, 50, 5),
    ch2_2 = rnorm(n_samples, 50, 5),
    ch2_3 = rnorm(n_samples, 50, 5),
    ch2_4 = rnorm(n_samples, 50, 5),
    ch3_0 = rnorm(n_samples, 75, 7),
    ch3_1 = rnorm(n_samples, 75, 7),
    ch3_2 = rnorm(n_samples, 75, 7),
    ch3_3 = rnorm(n_samples, 75, 7),
    ch3_4 = rnorm(n_samples, 75, 7),
    outcome = c(1, 2, 3)
  )

  rec <- recipes::recipe(outcome ~ ., data = df) |>
    recipes::update_role(id, new_role = "id") |>
    step_measure_input_wide(dplyr::starts_with("ch1_"), col_name = "ch1") |>
    step_measure_input_wide(dplyr::starts_with("ch2_"), col_name = "ch2") |>
    step_measure_input_wide(dplyr::starts_with("ch3_"), col_name = "ch3") |>
    step_measure_channel_ratio(
      numerator = c("ch1", "ch2"),
      denominator = c("ch2", "ch3")
    ) |>
    recipes::prep()

  result <- recipes::bake(rec, new_data = NULL)

  expect_true("ratio_ch1_ch2" %in% names(result))
  expect_true("ratio_ch2_ch3" %in% names(result))
})


test_that("step_measure_channel_ratio errors on length mismatch", {
  expect_error(
    step_measure_channel_ratio(
      recipes::recipe(~., data = data.frame(x = 1)),
      numerator = c("a", "b"),
      denominator = "c"
    ),
    "same length"
  )
})


test_that("step_measure_channel_ratio handles division by near-zero", {
  set.seed(42)
  n_samples <- 1
  df <- tibble::tibble(
    id = 1,
    uv_0 = 100,
    uv_1 = 100,
    uv_2 = 100,
    uv_3 = 100,
    uv_4 = 100,
    # Near-zero RI values
    ri_0 = 1e-15,
    ri_1 = 1e-15,
    ri_2 = 1e-15,
    ri_3 = 50,
    ri_4 = 50,
    outcome = 1
  )

  rec <- recipes::recipe(outcome ~ ., data = df) |>
    recipes::update_role(id, new_role = "id") |>
    step_measure_input_wide(dplyr::starts_with("uv_"), col_name = "uv") |>
    step_measure_input_wide(dplyr::starts_with("ri_"), col_name = "ri") |>
    step_measure_channel_ratio(
      numerator = "uv",
      denominator = "ri",
      epsilon = 1e-10
    ) |>
    recipes::prep()

  result <- recipes::bake(rec, new_data = NULL)

  # Should produce finite values (not Inf)
  ratio_vals <- result$ratio_uv_ri[[1]]$value
  expect_true(all(is.finite(ratio_vals)))
})


test_that("step_measure_channel_ratio tidy method works", {
  set.seed(42)
  df <- test_df_wide_aligned()

  rec <- recipes::recipe(concentration ~ ., data = df) |>
    recipes::update_role(id, new_role = "id") |>
    step_measure_input_wide(dplyr::starts_with("uv_"), col_name = "uv") |>
    step_measure_input_wide(dplyr::starts_with("ri_"), col_name = "ri") |>
    step_measure_channel_ratio(numerator = "uv", denominator = "ri") |>
    recipes::prep()

  tidy_result <- recipes::tidy(rec, number = 3)
  expect_s3_class(tidy_result, "tbl_df")
  expect_true("numerator" %in% names(tidy_result))
  expect_true("denominator" %in% names(tidy_result))
  expect_true("output_col" %in% names(tidy_result))
})


test_that("step_measure_channel_ratio print method works", {
  set.seed(42)
  df <- test_df_wide_aligned()

  rec <- recipes::recipe(concentration ~ ., data = df) |>
    recipes::update_role(id, new_role = "id") |>
    step_measure_input_wide(dplyr::starts_with("uv_"), col_name = "uv") |>
    step_measure_input_wide(dplyr::starts_with("ri_"), col_name = "ri") |>
    step_measure_channel_ratio(
      numerator = "uv",
      denominator = "ri",
      log_transform = TRUE
    ) |>
    recipes::prep()

  step <- rec$steps[[3]]
  expect_no_error(print(step))
})


# ------------------------------------------------------------------------------
# Integration tests
# ------------------------------------------------------------------------------

test_that("full channel workflow: align -> combine", {
  set.seed(42)
  df <- test_df_wide_misaligned()

  uv_locs <- 0:9
  ri_locs <- seq(0.5, 9.5, by = 1)

  rec <- recipes::recipe(concentration ~ ., data = df) |>
    recipes::update_role(id, new_role = "id") |>
    step_measure_input_wide(
      dplyr::starts_with("uv_"),
      col_name = "uv",
      location_values = uv_locs
    ) |>
    step_measure_input_wide(
      dplyr::starts_with("ri_"),
      col_name = "ri",
      location_values = ri_locs
    ) |>
    step_measure_channel_align(method = "union") |>
    step_measure_channel_combine(strategy = "stack") |>
    recipes::prep()

  result <- recipes::bake(rec, new_data = NULL)

  expect_true(".measures" %in% names(result))
  expect_s3_class(result$.measures, "measure_nd_list")
})


test_that("full channel workflow: align -> ratio", {
  set.seed(42)
  df <- test_df_wide_misaligned()

  uv_locs <- 0:9
  ri_locs <- seq(0.5, 9.5, by = 1)

  rec <- recipes::recipe(concentration ~ ., data = df) |>
    recipes::update_role(id, new_role = "id") |>
    step_measure_input_wide(
      dplyr::starts_with("uv_"),
      col_name = "uv",
      location_values = uv_locs
    ) |>
    step_measure_input_wide(
      dplyr::starts_with("ri_"),
      col_name = "ri",
      location_values = ri_locs
    ) |>
    step_measure_channel_align(method = "reference") |>
    step_measure_channel_ratio(numerator = "uv", denominator = "ri") |>
    recipes::prep()

  result <- recipes::bake(rec, new_data = NULL)

  expect_true("ratio_uv_ri" %in% names(result))
  # Grids should match after alignment
  expect_equal(
    result$uv[[1]]$location,
    result$ri[[1]]$location
  )
})
