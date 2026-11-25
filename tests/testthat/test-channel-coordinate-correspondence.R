test_that("stack preserves each channel's coordinates", {
  data <- tibble::tibble(
    left = new_measure_list(list(
      new_measure_tbl(c(1, 2, 3), c(10, 20, 30))
    )),
    right = new_measure_list(list(
      new_measure_tbl(c(3, 2, 1), c(300, 200, 100))
    ))
  )

  rec <- recipes::recipe(~., data = data) |>
    step_measure_channel_combine(
      left,
      right,
      strategy = "stack"
    )
  fit <- recipes::prep(rec)
  result <- recipes::bake(fit, new_data = NULL)
  stacked <- result$.measures[[1]]

  expect_equal(stacked$location_1, c(1, 2, 3, 3, 2, 1))
  expect_equal(stacked$location_2, c(rep(1, 3), rep(2, 3)))
  expect_equal(stacked$value, c(10, 20, 30, 300, 200, 100))
  expect_true(measure_is_regular(stacked))
})

test_that("stack retains sparse coordinates and independent sample grids", {
  data <- tibble::tibble(
    left = new_measure_list(list(
      new_measure_tbl(c(1, 2, 3), c(10, 20, 30)),
      new_measure_tbl(c(10, 20), c(40, 50))
    )),
    right = new_measure_list(list(
      new_measure_tbl(c(11, 12), c(60, 70)),
      new_measure_tbl(c(10, 20), c(80, 90))
    ))
  )

  rec <- recipes::recipe(~., data = data) |>
    step_measure_channel_combine(
      left,
      right,
      strategy = "stack"
    )
  fit <- recipes::prep(rec)
  result <- recipes::bake(fit, new_data = NULL)

  first <- result$.measures[[1]]
  second <- result$.measures[[2]]
  expect_equal(first$location_1, c(1, 2, 3, 11, 12))
  expect_equal(first$value, c(10, 20, 30, 60, 70))
  expect_false(measure_is_regular(first))
  expect_equal(second$location_1, c(10, 20, 10, 20))
  expect_equal(second$value, c(40, 50, 80, 90))
  expect_true(measure_is_regular(second))
})

test_that("concat accepts independent grids for each sample", {
  data <- tibble::tibble(
    left = new_measure_list(list(
      new_measure_tbl(c(1, 2), c(10, 20)),
      new_measure_tbl(c(10, 11), c(30, 40))
    )),
    right = new_measure_list(list(
      new_measure_tbl(c(100, 101), c(50, 60)),
      new_measure_tbl(c(200, 201), c(70, 80))
    ))
  )

  rec <- recipes::recipe(~., data = data) |>
    step_measure_channel_combine(
      left,
      right,
      strategy = "concat"
    ) |>
    recipes::prep()
  result <- recipes::bake(rec, new_data = NULL)

  expect_equal(result$.measures[[1]]$location, c(1, 2, 103, 104))
  expect_equal(result$.measures[[1]]$value, c(10, 20, 50, 60))
  expect_equal(result$.measures[[2]]$location, c(10, 11, 212, 213))
  expect_equal(result$.measures[[2]]$value, c(30, 40, 70, 80))
})

test_that("weighted combinations reject mismatched within-sample grids", {
  data <- tibble::tibble(
    left = new_measure_list(list(new_measure_tbl(1:3, c(10, 20, 30)))),
    right = new_measure_list(list(new_measure_tbl(4:6, c(40, 50, 60))))
  )

  rec <- recipes::recipe(~., data = data) |>
    step_measure_channel_combine(
      left,
      right,
      strategy = "weighted_sum"
    )

  expect_error(
    recipes::prep(rec),
    "identical ordered locations"
  )
})

test_that("weighted combinations allow independent grids across samples", {
  data <- tibble::tibble(
    left = new_measure_list(list(
      new_measure_tbl(1:2, c(10, 20)),
      new_measure_tbl(10:11, c(30, 40))
    )),
    right = new_measure_list(list(
      new_measure_tbl(1:2, c(2, 4)),
      new_measure_tbl(10:11, c(6, 8))
    ))
  )

  rec <- recipes::recipe(~., data = data) |>
    step_measure_channel_combine(
      left,
      right,
      strategy = "weighted_sum",
      weights = c(0.5, 0.5)
    ) |>
    recipes::prep()
  result <- recipes::bake(rec, new_data = NULL)

  expect_equal(result$.measures[[1]]$location, 1:2)
  expect_equal(result$.measures[[1]]$value, c(6, 12))
  expect_equal(result$.measures[[2]]$location, 10:11)
  expect_equal(result$.measures[[2]]$value, c(18, 24))
})

test_that("ratios interpolate mismatched grids and report every sample", {
  data <- tibble::tibble(
    numerator = new_measure_list(list(
      new_measure_tbl(c(0, 1, 2), c(20, 30, 40)),
      new_measure_tbl(c(0, 1, 2), c(20, 30, 40))
    )),
    denominator = new_measure_list(list(
      new_measure_tbl(c(0, 1, 2), c(10, 15, 20)),
      new_measure_tbl(c(0.5, 1.5, 2.5), c(10, 20, 30))
    ))
  )

  rec <- recipes::recipe(~., data = data) |>
    step_measure_channel_ratio(
      numerator = "numerator",
      denominator = "denominator",
      epsilon = 0
    )
  expect_warning(recipes::prep(rec), "sample 2")
  fit <- suppressWarnings(recipes::prep(rec))
  result <- recipes::bake(fit, new_data = NULL)

  expect_equal(result$ratio_numerator_denominator[[1]]$location, c(0, 1, 2))
  expect_equal(result$ratio_numerator_denominator[[1]]$value, c(2, 2, 2))
  expect_equal(result$ratio_numerator_denominator[[2]]$location, c(0, 1, 2))
  expect_equal(
    result$ratio_numerator_denominator[[2]]$value,
    c(2, 2, 40 / 25)
  )
})
