make_value_metadata_data <- function(
  quantity = NULL,
  unit = NULL,
  scale = 1
) {
  location <- seq_len(7)
  measures <- new_measure_list(list(
    new_measure_tbl(location, scale * c(1, 2, 4, 3, 2, 1, 2)),
    new_measure_tbl(location, scale * c(2, 1, 3, 5, 3, 2, 1))
  ))
  data <- tibble::tibble(
    id = 1:2,
    outcome = 1:2,
    .measures = measures
  )

  if (!is.null(quantity)) {
    data$.measures <- set_measure_value_metadata(
      data$.measures,
      quantity = quantity,
      unit = unit
    )
  }

  data
}

make_operation_recipe <- function(data, operation) {
  recipe(outcome ~ ., data = data) |>
    update_role(id, new_role = "id") |>
    operation() |>
    prep()
}

expect_same_measure_values <- function(tagged, untagged, info) {
  expect_equal(length(tagged$.measures), length(untagged$.measures))
  for (i in seq_along(tagged$.measures)) {
    expect_equal(
      tagged$.measures[[i]],
      untagged$.measures[[i]],
      info = paste(info, "sample", i)
    )
  }
}

test_that("operation helper preserves only declared metadata", {
  input <- make_value_metadata_data(quantity = "signal", unit = "mAU")$.measures
  output <- list(new_measure_tbl(1:2, c(3, 4)))

  conserved <- .measure_operation_result(input, output)
  normalized <- .measure_operation_result(
    input,
    output,
    quantity = "normalized_signal",
    unit = "1"
  )
  untagged <- .measure_operation_result(
    make_value_metadata_data()$.measures,
    output
  )

  expect_equal(
    measure_value_metadata(conserved),
    list(quantity = "signal", unit = "mAU")
  )
  expect_equal(
    measure_value_metadata(normalized),
    list(quantity = "normalized_signal", unit = "1")
  )
  expect_null(measure_value_metadata(untagged))
})

test_that("conserving recipes use metadata from fresh bake data", {
  operations <- list(
    smooth_ma = function(recipe) step_measure_smooth_ma(recipe, window = 3),
    smooth_median = function(recipe) {
      step_measure_smooth_median(recipe, window = 3)
    },
    smooth_gaussian = function(recipe) {
      step_measure_smooth_gaussian(recipe, sigma = 1, window = 3)
    },
    filter_fourier = function(recipe) {
      step_measure_filter_fourier(recipe, cutoff = 0.2)
    },
    baseline_als = function(recipe) {
      step_measure_baseline_als(recipe, lambda = 10, max_iter = 2)
    },
    baseline_poly = function(recipe) {
      step_measure_baseline_poly(recipe, degree = 1)
    }
  )
  training <- make_value_metadata_data(
    quantity = "training_signal",
    unit = "mAU"
  )
  fresh <- make_value_metadata_data(
    quantity = "fresh_signal",
    unit = "AU",
    scale = 2
  )
  fresh_untagged <- make_value_metadata_data(scale = 2)

  for (name in names(operations)) {
    operation <- operations[[name]]
    rec <- make_operation_recipe(training, operation)
    result <- bake(rec, new_data = fresh)
    untagged_result <- bake(rec, new_data = fresh_untagged)

    expect_equal(
      measure_value_metadata(result$.measures),
      list(quantity = "fresh_signal", unit = "AU")
    )
    expect_same_measure_values(result, untagged_result, name)
  }
})

test_that("normalizing recipes classify tagged output by operation", {
  operations <- list(
    normalize_sum = function(recipe) step_measure_normalize_sum(recipe),
    normalize_max = function(recipe) step_measure_normalize_max(recipe),
    normalize_range = function(recipe) step_measure_normalize_range(recipe),
    normalize_vector = function(recipe) step_measure_normalize_vector(recipe),
    normalize_auc = function(recipe) step_measure_normalize_auc(recipe),
    normalize_peak_mean = function(recipe) {
      step_measure_normalize_peak(
        recipe,
        location_min = 2,
        location_max = 5,
        method = "mean"
      )
    },
    normalize_peak_integral = function(recipe) {
      step_measure_normalize_peak(
        recipe,
        location_min = 2,
        location_max = 5,
        method = "integral"
      )
    }
  )
  expected_units <- c(
    normalize_sum = "1",
    normalize_max = "1",
    normalize_range = "1",
    normalize_vector = "1",
    normalize_auc = NA_character_,
    normalize_peak_mean = "1",
    normalize_peak_integral = NA_character_
  )
  training <- make_value_metadata_data(
    quantity = "training_signal",
    unit = "mAU"
  )
  fresh <- make_value_metadata_data(
    quantity = "fresh_signal",
    unit = "AU",
    scale = 2
  )
  fresh_untagged <- make_value_metadata_data(scale = 2)

  for (name in names(operations)) {
    rec <- make_operation_recipe(training, operations[[name]])
    result <- bake(rec, new_data = fresh)
    untagged_result <- bake(rec, new_data = fresh_untagged)
    metadata <- measure_value_metadata(result$.measures)

    expect_equal(metadata$quantity, "normalized_signal", info = name)
    if (is.na(expected_units[[name]])) {
      expect_null(metadata$unit, info = name)
    } else {
      expect_equal(metadata$unit, expected_units[[name]], info = name)
    }
    expect_same_measure_values(result, untagged_result, name)
  }
})

test_that("untagged recipe inputs remain untagged", {
  operations <- list(
    function(recipe) step_measure_smooth_ma(recipe, window = 3),
    function(recipe) step_measure_baseline_poly(recipe, degree = 1),
    function(recipe) step_measure_normalize_sum(recipe),
    function(recipe) step_measure_normalize_auc(recipe)
  )
  data <- make_value_metadata_data()

  for (operation in operations) {
    rec <- make_operation_recipe(data, operation)
    result <- bake(rec, new_data = data)
    expect_null(measure_value_metadata(result$.measures))
  }
})

test_that("operation metadata helper handles empty measure columns", {
  input <- set_measure_value_metadata(
    new_measure_list(),
    quantity = "signal",
    unit = "AU"
  )

  conserved <- .measure_operation_result(input, list())
  normalized <- .measure_operation_result(
    input,
    list(),
    quantity = "normalized_signal",
    unit = "1"
  )

  expect_length(conserved, 0)
  expect_length(normalized, 0)
  expect_equal(
    measure_value_metadata(conserved),
    list(quantity = "signal", unit = "AU")
  )
  expect_equal(
    measure_value_metadata(normalized),
    list(quantity = "normalized_signal", unit = "1")
  )
})

test_that("typed normalization rejects a singular sample in a fresh bake", {
  training <- make_value_metadata_data(
    quantity = "training_signal",
    unit = "AU"
  )
  fresh <- make_value_metadata_data(
    quantity = "fresh_signal",
    unit = "AU"
  )
  fresh$.measures <- set_measure_value_metadata(
    new_measure_list(list(
      fresh$.measures[[1]],
      new_measure_tbl(seq_len(7), c(-1, 1, 0, 0, 0, 0, 0))
    )),
    quantity = "fresh_signal",
    unit = "AU"
  )
  fresh_untagged <- make_value_metadata_data()
  fresh_untagged$.measures <- new_measure_list(list(
    fresh_untagged$.measures[[1]],
    new_measure_tbl(seq_len(7), c(-1, 1, 0, 0, 0, 0, 0))
  ))

  rec <- make_operation_recipe(
    training,
    function(recipe) step_measure_normalize_sum(recipe)
  )

  expect_error(
    bake(rec, new_data = fresh),
    "Cannot normalize tagged measurement values"
  )

  result <- expect_warning(
    bake(rec, new_data = fresh_untagged),
    "zero or NA"
  )
  expect_equal(
    result$.measures[[2]]$value,
    fresh_untagged$.measures[[2]]$value
  )
  expect_null(measure_value_metadata(result$.measures))
})
