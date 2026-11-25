# Regression tests for pointwise alignment and reference corrections.

.alignment_reference_grid_data <- function(grids, values, sample_type = NULL) {
  measures <- purrr::map2(grids, values, new_measure_tbl)
  out <- tibble::tibble(signal = new_measure_list(measures))
  if (!is.null(sample_type)) {
    out$sample_type <- sample_type
  }
  out
}

test_that("equal-length shifted and reversed training grids are rejected", {
  shifted <- .alignment_reference_grid_data(
    list(1:4, 11:14),
    list(c(1, 2, 3, 4), c(2, 3, 4, 5))
  )
  reversed <- .alignment_reference_grid_data(
    list(1:4, 4:1),
    list(c(1, 2, 3, 4), c(2, 3, 4, 5))
  )

  for (training in list(shifted, reversed)) {
    expect_error(
      recipe(~., data = training) |>
        step_measure_align_shift(max_shift = 1) |>
        prep(),
      "same locations"
    )
    expect_error(
      recipe(~., data = training) |>
        step_measure_subtract_reference(reference = rep(0, 4)) |>
        prep(),
      "same locations"
    )
  }
})

test_that("prediction grids must match a trained pointwise reference", {
  training <- .alignment_reference_grid_data(
    list(1:4, 1:4),
    list(c(1, 2, 3, 4), c(3, 4, 5, 6))
  )
  fit <- recipe(~., data = training) |>
    step_measure_subtract_reference(reference = rep(1, 4)) |>
    prep()

  for (grid in list(11:14, 4:1)) {
    prediction <- .alignment_reference_grid_data(
      list(grid),
      list(c(10, 11, 12, 13))
    )
    expect_error(
      bake(fit, new_data = prediction),
      "same locations"
    )
  }
})

test_that("valid prediction grids retain pointwise reference behavior", {
  training <- .alignment_reference_grid_data(
    list(1:4, 1:4),
    list(c(1, 2, 3, 4), c(3, 4, 5, 6))
  )
  prediction <- .alignment_reference_grid_data(
    list(1:4),
    list(c(10, 11, 12, 13))
  )

  fit <- recipe(~., data = training) |>
    step_measure_subtract_reference(reference = rep(1, 4)) |>
    prep()
  result <- bake(fit, new_data = prediction)

  expect_equal(result$signal[[1]]$location, 1:4)
  expect_equal(result$signal[[1]]$value, c(9, 10, 11, 12))
})

test_that("learned alignment accepts a valid prediction grid", {
  training <- .alignment_reference_grid_data(
    list(1:4, 1:4),
    list(c(1, 2, 3, 4), c(3, 4, 5, 6))
  )
  prediction <- .alignment_reference_grid_data(
    list(1:4),
    list(c(2, 3, 4, 5))
  )
  fit <- recipe(~., data = training) |>
    step_measure_align_shift(max_shift = 1, reference = "first") |>
    prep()

  result <- bake(fit, new_data = prediction)
  expect_equal(result$signal[[1]]$location, 1:4)
})

test_that("learned blanks also validate prediction grids", {
  training <- .alignment_reference_grid_data(
    list(1:4, 1:4),
    list(c(1, 2, 3, 4), c(3, 4, 5, 6)),
    sample_type = c("blank", "sample")
  )
  fit <- recipe(~., data = training) |>
    step_measure_subtract_blank(
      blank_col = sample_type,
      blank_value = "blank"
    ) |>
    prep()
  prediction <- .alignment_reference_grid_data(
    list(11:14),
    list(c(10, 11, 12, 13)),
    sample_type = "sample"
  )

  expect_error(
    bake(fit, new_data = prediction),
    "same locations"
  )
})

test_that("input recipes and reference corrections share the grid contract", {
  training <- tibble::tibble(
    id = rep(c(1, 2), each = 4),
    channel = rep(1:4, 2),
    signal = c(1, 2, 3, 4, 3, 4, 5, 6)
  )
  prediction <- tibble::tibble(
    id = 3,
    channel = 1:4,
    signal = c(10, 11, 12, 13)
  )
  fit <- recipe(~., data = training) |>
    update_role(id, new_role = "id") |>
    step_measure_input_long(signal, location = vars(channel)) |>
    step_measure_subtract_reference(reference = rep(1, 4)) |>
    prep()

  result <- bake(fit, new_data = prediction)
  expect_equal(result$.measures[[1]]$value, c(9, 10, 11, 12))

  shifted_prediction <- prediction
  shifted_prediction$channel <- shifted_prediction$channel + 10
  expect_error(
    bake(fit, new_data = shifted_prediction),
    "same locations"
  )
})

test_that("coordinate-bearing references can still be interpolated", {
  training <- .alignment_reference_grid_data(
    list(1:4),
    list(c(10, 20, 30, 40))
  )
  prediction <- .alignment_reference_grid_data(
    list(1:4),
    list(c(10, 20, 30, 40))
  )
  reference <- new_measure_tbl(c(0, 2, 4, 6), c(0, 2, 4, 6))

  expect_warning(
    fit <- recipe(~., data = training) |>
      step_measure_subtract_reference(reference = reference) |>
      prep(),
    "Interpolating"
  )
  result <- bake(fit, new_data = prediction)

  expect_equal(result$signal[[1]]$value, c(9, 18, 27, 36))
})

test_that("selected measure columns retain their existing behavior", {
  training <- tibble::tibble(
    first = new_measure_list(list(new_measure_tbl(1:4, 1:4))),
    second = new_measure_list(list(new_measure_tbl(1:4, 5:8)))
  )
  prediction <- tibble::tibble(
    first = new_measure_list(list(new_measure_tbl(11:14, 10:13))),
    second = new_measure_list(list(new_measure_tbl(1:4, 10:13)))
  )
  fit <- recipe(~., data = training) |>
    step_measure_subtract_reference(
      reference = rep(1, 4),
      measures = "second"
    ) |>
    prep()

  result <- bake(fit, new_data = prediction)
  expect_equal(result$second[[1]]$value, 9:12)
  expect_equal(result$first[[1]]$location, 11:14)
})

test_that("legacy trained pointwise steps ask to be re-prepped", {
  training <- .alignment_reference_grid_data(
    list(1:4),
    list(c(1, 2, 3, 4))
  )
  prediction <- .alignment_reference_grid_data(
    list(1:4),
    list(c(10, 11, 12, 13))
  )
  fit <- recipe(~., data = training) |>
    step_measure_align_shift(max_shift = 1) |>
    prep()

  fit$steps[[1]]$locations <- NULL

  expect_error(
    bake(fit, new_data = prediction),
    "Prep the recipe again"
  )
})
