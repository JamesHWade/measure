metadata_test_data <- function(tag = FALSE, n = 16L) {
  location <- seq(100, by = 2, length.out = n)
  signal <- new_measure_list(list(
    new_measure_tbl(location, sin(location / 17)),
    new_measure_tbl(location, cos(location / 23))
  ))

  if (tag) {
    signal <- set_measure_value_metadata(
      signal,
      quantity = "absorbance",
      unit = "AU"
    )
  }

  tibble::tibble(signal = signal)
}

metadata_mixed_data <- function(tag = FALSE, short_n = 1L, n = 16L) {
  location <- seq(100, by = 2, length.out = n)
  short_location <- seq(100, by = 2, length.out = short_n)
  signal <- new_measure_list(list(
    new_measure_tbl(location, sin(location / 17)),
    new_measure_tbl(short_location, sin(short_location / 17))
  ))

  if (tag) {
    signal <- set_measure_value_metadata(
      signal,
      quantity = "mass_concentration",
      unit = "mg/mL"
    )
  }

  tibble::tibble(signal = signal)
}

metadata_bake <- function(
  data,
  operation = c("derivative", "gap", "savitzky_golay"),
  order = 1L,
  gap = 2L,
  segment = 1L,
  differentiation_order = 0L
) {
  operation <- match.arg(operation)
  rec <- recipe(~., data = data)

  if (operation == "derivative") {
    rec <- step_measure_derivative(rec, order = order)
  } else if (operation == "gap") {
    rec <- step_measure_derivative_gap(
      rec,
      gap = gap,
      segment = segment
    )
  } else {
    rec <- step_measure_savitzky_golay(
      rec,
      degree = 3L,
      window_side = 2L,
      differentiation_order = differentiation_order
    )
  }

  bake(prep(rec), new_data = data)
}

expect_same_measure_values <- function(x, y) {
  expect_equal(length(x), length(y))
  for (i in seq_along(x)) {
    expect_equal(x[[i]]$location, y[[i]]$location)
    expect_equal(x[[i]]$value, y[[i]]$value)
  }
}

test_that("finite-difference derivatives declare metadata at each order", {
  for (order in 1:2) {
    untagged <- metadata_test_data()
    tagged <- metadata_test_data(tag = TRUE)

    expected <- metadata_bake(untagged, order = order)
    result <- metadata_bake(tagged, order = order)

    expect_same_measure_values(result$signal, expected$signal)
    expect_equal(
      measure_value_metadata(result$signal),
      list(quantity = "signal_derivative", unit = NULL)
    )
  }
})

test_that("gap derivatives declare metadata without changing values", {
  untagged <- metadata_test_data()
  tagged <- metadata_test_data(tag = TRUE)

  expected <- metadata_bake(
    untagged,
    operation = "gap",
    gap = 2L,
    segment = 2L
  )
  result <- metadata_bake(
    tagged,
    operation = "gap",
    gap = 2L,
    segment = 2L
  )

  expect_same_measure_values(result$signal, expected$signal)
  expect_equal(
    measure_value_metadata(result$signal),
    list(quantity = "signal_derivative", unit = NULL)
  )
})

test_that("Savitzky-Golay preserves values metadata when not differentiating", {
  skip_if_not_installed("prospectr")

  result <- metadata_bake(
    metadata_test_data(tag = TRUE),
    operation = "savitzky_golay",
    differentiation_order = 0L
  )

  expect_equal(
    measure_value_metadata(result$signal),
    list(quantity = "absorbance", unit = "AU")
  )
})

test_that("Savitzky-Golay derivatives classify each positive order", {
  skip_if_not_installed("prospectr")

  for (order in 1:2) {
    untagged <- metadata_test_data()
    tagged <- metadata_test_data(tag = TRUE)

    expected <- metadata_bake(
      untagged,
      operation = "savitzky_golay",
      differentiation_order = order
    )
    result <- metadata_bake(
      tagged,
      operation = "savitzky_golay",
      differentiation_order = order
    )

    expect_same_measure_values(result$signal, expected$signal)
    expect_equal(
      measure_value_metadata(result$signal),
      list(quantity = "signal_derivative", unit = NULL)
    )
  }
})

test_that("fresh derivative bakes retain input and output metadata", {
  tagged <- metadata_test_data(tag = TRUE)
  rec <- recipe(~., data = tagged) |>
    step_measure_derivative(order = 2L) |>
    prep()

  first <- bake(rec, new_data = tagged)
  second <- bake(rec, new_data = tagged)

  expect_equal(
    measure_value_metadata(tagged$signal),
    list(quantity = "absorbance", unit = "AU")
  )
  expect_equal(
    measure_value_metadata(first$signal),
    list(quantity = "signal_derivative", unit = NULL)
  )
  expect_equal(
    measure_value_metadata(second$signal),
    list(quantity = "signal_derivative", unit = NULL)
  )
  expect_same_measure_values(first$signal, second$signal)
})

test_that("tagged finite derivatives reject mixed short traces", {
  training <- metadata_test_data(tag = TRUE, n = 8L)
  rec <- recipe(~., data = training) |>
    step_measure_derivative(order = 1L) |>
    prep()
  mixed <- metadata_mixed_data(tag = TRUE, short_n = 1L, n = 8L)

  expect_error(
    bake(rec, new_data = mixed),
    "must each contain at least 2 points"
  )
})

test_that("tagged gap derivatives reject mixed short traces", {
  training <- metadata_test_data(tag = TRUE, n = 8L)
  rec <- recipe(~., data = training) |>
    step_measure_derivative_gap(gap = 2L) |>
    prep()
  mixed <- metadata_mixed_data(tag = TRUE, short_n = 3L, n = 8L)

  expect_error(
    bake(rec, new_data = mixed),
    "must each contain at least 5 points"
  )
})

test_that("untagged mixed traces retain legacy derivative fallbacks", {
  finite_training <- metadata_test_data(n = 8L)
  finite_rec <- recipe(~., data = finite_training) |>
    step_measure_derivative(order = 1L) |>
    prep()
  finite_mixed <- metadata_mixed_data(short_n = 1L, n = 8L)
  finite_result <- NULL
  expect_warning(
    finite_result <- bake(finite_rec, new_data = finite_mixed),
    "fewer than 2 points"
  )

  gap_training <- metadata_test_data(n = 8L)
  gap_rec <- recipe(~., data = gap_training) |>
    step_measure_derivative_gap(gap = 2L) |>
    prep()
  gap_mixed <- metadata_mixed_data(short_n = 3L, n = 8L)
  gap_result <- NULL
  expect_warning(
    gap_result <- bake(gap_rec, new_data = gap_mixed),
    "need >= 5"
  )

  expect_null(measure_value_metadata(finite_result$signal))
  expect_null(measure_value_metadata(gap_result$signal))
  expect_equal(nrow(finite_result$signal[[1]]), 7L)
  expect_equal(nrow(finite_result$signal[[2]]), 1L)
  expect_equal(nrow(gap_result$signal[[1]]), 4L)
  expect_equal(nrow(gap_result$signal[[2]]), 3L)
})

test_that("tagged higher-order derivatives preflight every trace", {
  training <- metadata_test_data(tag = TRUE, n = 8L)
  rec <- recipe(~., data = training) |>
    step_measure_derivative(order = 2L) |>
    prep()
  mixed <- metadata_mixed_data(tag = TRUE, short_n = 2L, n = 8L)

  expect_error(
    bake(rec, new_data = mixed),
    "must each contain at least 3 points"
  )
})

test_that("empty tagged batches return empty derivative-typed results", {
  training <- metadata_test_data(tag = TRUE, n = 8L)
  finite_rec <- recipe(~., data = training) |>
    step_measure_derivative(order = 2L) |>
    prep()
  gap_rec <- recipe(~., data = training) |>
    step_measure_derivative_gap(gap = 2L) |>
    prep()
  empty <- tibble::tibble(
    signal = set_measure_value_metadata(
      new_measure_list(list()),
      quantity = "mass_concentration",
      unit = "mg/mL"
    )
  )

  finite_result <- bake(finite_rec, new_data = empty)
  gap_result <- bake(gap_rec, new_data = empty)

  expect_length(finite_result$signal, 0L)
  expect_length(gap_result$signal, 0L)
  expect_equal(
    measure_value_metadata(finite_result$signal),
    list(quantity = "signal_derivative", unit = NULL)
  )
  expect_equal(
    measure_value_metadata(gap_result$signal),
    list(quantity = "signal_derivative", unit = NULL)
  )
})
