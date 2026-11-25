# Tests for coordinate correspondence in one-dimensional preprocessing steps.

reference_grid_batch <- function(
  locations = 1:4,
  values = rbind(1:4, 2:5),
  outcome = seq_len(nrow(values))
) {
  tibble::tibble(
    outcome = outcome,
    signal = new_measure_list(
      lapply(
        seq_len(nrow(values)),
        function(i) new_measure_tbl(locations, values[i, ])
      )
    )
  )
}

test_that("variable-wise steps reject shifted and reversed prediction grids", {
  training <- reference_grid_batch()
  shifted <- reference_grid_batch(
    locations = 11:14,
    values = matrix(12:15, nrow = 1)
  )
  reversed <- reference_grid_batch(
    locations = 4:1,
    values = matrix(4:1, nrow = 1)
  )

  steps <- list(
    step_measure_center,
    step_measure_scale_auto,
    step_measure_scale_pareto,
    step_measure_scale_range,
    step_measure_scale_vast
  )

  for (step_fn in steps) {
    fit <- recipes::prep(
      recipes::recipe(~., data = training) |>
        step_fn()
    )

    expect_error(
      recipes::bake(fit, new_data = shifted),
      "same locations"
    )
    expect_error(
      recipes::bake(fit, new_data = reversed),
      "same locations"
    )
  }
})

test_that("variable-wise prep rejects inconsistent training grids", {
  training <- tibble::tibble(
    signal = new_measure_list(list(
      new_measure_tbl(1:4, 1:4),
      new_measure_tbl(c(1, 2, 4, 5), 2:5)
    ))
  )

  expect_error(
    recipes::prep(
      recipes::recipe(~., data = training) |>
        step_measure_center()
    ),
    "same locations"
  )
})

test_that("selected one-dimensional measure columns keep their scope", {
  training <- tibble::tibble(
    first = new_measure_list(list(new_measure_tbl(1:4, 1:4))),
    second = new_measure_list(list(new_measure_tbl(1:4, 5:8)))
  )
  prediction <- tibble::tibble(
    first = new_measure_list(list(new_measure_tbl(11:14, 10:13))),
    second = new_measure_list(list(new_measure_tbl(1:4, 10:13)))
  )

  center_fit <- recipes::prep(
    recipes::recipe(~., data = training) |>
      step_measure_center(measures = "second")
  )
  center_result <- recipes::bake(center_fit, new_data = prediction)
  expect_equal(center_result$first[[1]]$location, 11:14)
  expect_equal(center_result$second[[1]]$location, 1:4)

  msc_fit <- recipes::prep(
    recipes::recipe(~., data = training) |>
      step_measure_msc(measures = "second")
  )
  msc_result <- recipes::bake(msc_fit, new_data = prediction)
  expect_equal(msc_result$first[[1]]$location, 11:14)
  expect_equal(msc_result$second[[1]]$location, 1:4)
})

test_that("reference-based steps reject shifted and reversed prediction grids", {
  base <- sin(seq(0, 2 * pi, length.out = 6))
  training <- reference_grid_batch(
    locations = seq_len(6),
    values = rbind(base, base + 0.1, base - 0.1, base + 0.2)
  )
  shifted <- reference_grid_batch(
    locations = 11:16,
    values = matrix(12:17, nrow = 1)
  )
  reversed <- reference_grid_batch(
    locations = 6:1,
    values = matrix(6:1, nrow = 1)
  )

  steps <- list(
    step_measure_msc,
    step_measure_emsc
  )

  for (step_fn in steps) {
    fit <- recipes::prep(
      recipes::recipe(~., data = training) |>
        step_fn()
    )

    expect_error(
      recipes::bake(fit, new_data = shifted),
      "same locations"
    )
    expect_error(
      recipes::bake(fit, new_data = reversed),
      "same locations"
    )
  }
})

test_that("OSC and outlier QC reject shifted prediction grids", {
  training <- reference_grid_batch(
    locations = seq_len(6),
    values = rbind(
      seq_len(6),
      seq_len(6) + 1,
      seq_len(6) + c(0, 1, 0, 1, 0, 1),
      seq_len(6) + c(1, 0, 1, 0, 1, 0)
    )
  )
  shifted <- reference_grid_batch(
    locations = 11:16,
    values = matrix(12:17, nrow = 1)
  )

  osc_fit <- recipes::prep(
    recipes::recipe(outcome ~ ., data = training) |>
      step_measure_osc()
  )
  expect_error(
    recipes::bake(osc_fit, new_data = shifted),
    "same locations"
  )

  outlier_fit <- recipes::prep(
    recipes::recipe(outcome ~ ., data = training) |>
      step_measure_qc_outlier()
  )
  expect_error(
    recipes::bake(outlier_fit, new_data = shifted),
    "same locations"
  )
})

test_that("sample-local QC steps allow independent grids", {
  training <- tibble::tibble(
    signal = new_measure_list(list(
      new_measure_tbl(1:4, c(1, 2, NA, 4)),
      new_measure_tbl(11:14, c(2, 3, 4, 5))
    ))
  )
  prediction <- tibble::tibble(
    signal = new_measure_list(list(
      new_measure_tbl(21:24, c(2, NA, 4, 5))
    ))
  )

  for (step_fn in list(
    step_measure_qc_snr,
    step_measure_qc_saturated,
    step_measure_impute
  )) {
    fit <- recipes::prep(
      recipes::recipe(~., data = training) |>
        step_fn()
    )
    result <- recipes::bake(fit, new_data = prediction)
    expect_true(is_measure_list(result$signal))
  }
})

test_that("dependent bakes fail clearly for legacy steps without a grid", {
  training <- reference_grid_batch()

  scale_fit <- recipes::prep(
    recipes::recipe(~., data = training) |>
      step_measure_center()
  )
  expect_s3_class(recipes::bake(scale_fit, new_data = training), "tbl_df")
  scale_fit$steps[[1]]$learned_params$signal$locations <- NULL
  expect_error(
    recipes::bake(scale_fit, new_data = training),
    "Prep the recipe again"
  )

  emsc_training <- reference_grid_batch(
    locations = seq_len(6),
    values = rbind(
      sin(seq(0, 2 * pi, length.out = 6)),
      sin(seq(0, 2 * pi, length.out = 6)) + 0.1
    )
  )
  emsc_fit <- recipes::prep(
    recipes::recipe(~., data = emsc_training) |>
      step_measure_emsc()
  )
  expect_s3_class(recipes::bake(emsc_fit, new_data = emsc_training), "tbl_df")
  emsc_fit$steps[[1]]$locations <- NULL
  expect_error(
    recipes::bake(emsc_fit, new_data = emsc_training),
    "Prep the recipe again"
  )

  msc_fit <- recipes::prep(
    recipes::recipe(~., data = training) |>
      step_measure_msc()
  )
  msc_fit$steps[[1]]$locations <- NULL
  expect_error(
    recipes::bake(msc_fit, new_data = training),
    "Prep the recipe again"
  )

  projection_training <- reference_grid_batch(
    locations = seq_len(6),
    values = rbind(
      seq_len(6),
      seq_len(6) + 1,
      seq_len(6) + c(0, 1, 0, 1, 0, 1),
      seq_len(6) + c(1, 0, 1, 0, 1, 0)
    )
  )
  osc_fit <- recipes::prep(
    recipes::recipe(outcome ~ ., data = projection_training) |>
      step_measure_osc()
  )
  osc_fit$steps[[1]]$locations <- NULL
  expect_error(
    recipes::bake(osc_fit, new_data = projection_training),
    "Prep the recipe again"
  )

  outlier_fit <- recipes::prep(
    recipes::recipe(outcome ~ ., data = projection_training) |>
      step_measure_qc_outlier()
  )
  outlier_fit$steps[[1]]$locations <- NULL
  expect_error(
    recipes::bake(outlier_fit, new_data = projection_training),
    "Prep the recipe again"
  )
})
