window_step_data <- function() {
  axis <- seq_len(15)
  tibble::tibble(
    outcome = 1,
    .measures = new_measure_list(list(
      new_measure_tbl(axis, c(1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))
    ))
  )
}

test_that("window-step tuning markers preserve IDs and tidy columns", {
  rec_ma <- recipes::recipe(outcome ~ ., data = window_step_data()) |>
    step_measure_smooth_ma(window = tune::tune(id = "ma_window"))
  rec_median <- recipes::recipe(outcome ~ ., data = window_step_data()) |>
    step_measure_smooth_median(window = tune::tune(id = "median_window"))
  rec_despike <- recipes::recipe(outcome ~ ., data = window_step_data()) |>
    step_measure_despike(
      window = tune::tune(id = "despike_window"),
      threshold = tune::tune(id = "despike_threshold")
    )
  rec_shift <- recipes::recipe(outcome ~ ., data = window_step_data()) |>
    step_measure_align_shift(max_shift = tune::tune(id = "shift_max"))

  expect_setequal(
    tune::extract_parameter_set_dials(rec_ma)$id,
    "ma_window"
  )
  expect_setequal(
    tune::extract_parameter_set_dials(rec_median)$id,
    "median_window"
  )
  expect_setequal(
    tune::extract_parameter_set_dials(rec_despike)$id,
    c("despike_window", "despike_threshold")
  )
  expect_setequal(
    tune::extract_parameter_set_dials(rec_shift)$id,
    "shift_max"
  )

  tidy_ma <- recipes::tidy(rec_ma, number = 1)
  expect_true(rlang::is_call(tidy_ma$window[[1]], "tune"))
  expect_identical(tidy_ma$window[[1]], tune::tune("ma_window"))
})

test_that("unresolved window-step tuning markers are rejected by prep", {
  recipes_to_check <- list(
    recipes::recipe(outcome ~ ., data = window_step_data()) |>
      step_measure_smooth_ma(window = tune::tune()),
    recipes::recipe(outcome ~ ., data = window_step_data()) |>
      step_measure_smooth_median(window = tune::tune()),
    recipes::recipe(outcome ~ ., data = window_step_data()) |>
      step_measure_despike(window = tune::tune()),
    recipes::recipe(outcome ~ ., data = window_step_data()) |>
      step_measure_despike(threshold = tune::tune()),
    recipes::recipe(outcome ~ ., data = window_step_data()) |>
      step_measure_align_shift(max_shift = tune::tune())
  )

  for (rec in recipes_to_check) {
    expect_error(recipes::prep(rec), "tunable|finalized")
  }
})

test_that("finalized window-step values retain concrete validation", {
  cases <- list(
    list(
      recipe = recipes::recipe(outcome ~ ., data = window_step_data()) |>
        step_measure_smooth_ma(window = tune::tune(id = "window")),
      values = list(window = 4),
      message = "odd"
    ),
    list(
      recipe = recipes::recipe(outcome ~ ., data = window_step_data()) |>
        step_measure_smooth_median(window = tune::tune(id = "window")),
      values = list(window = 2),
      message = "odd"
    ),
    list(
      recipe = recipes::recipe(outcome ~ ., data = window_step_data()) |>
        step_measure_despike(window = tune::tune(id = "window")),
      values = list(window = 4),
      message = "odd"
    ),
    list(
      recipe = recipes::recipe(outcome ~ ., data = window_step_data()) |>
        step_measure_despike(threshold = tune::tune(id = "threshold")),
      values = list(threshold = 0),
      message = "positive"
    ),
    list(
      recipe = recipes::recipe(outcome ~ ., data = window_step_data()) |>
        step_measure_align_shift(max_shift = tune::tune(id = "max_shift")),
      values = list(max_shift = 0),
      message = "positive"
    )
  )

  for (case in cases) {
    finalized <- tune::finalize_recipe(case$recipe, case$values)
    expect_error(recipes::prep(finalized), case$message)
  }
})

test_that("finalized window-step recipes bake successfully", {
  recipes_to_check <- list(
    tune::finalize_recipe(
      recipes::recipe(outcome ~ ., data = window_step_data()) |>
        step_measure_smooth_ma(window = tune::tune(id = "window")),
      list(window = 3L)
    ),
    tune::finalize_recipe(
      recipes::recipe(outcome ~ ., data = window_step_data()) |>
        step_measure_smooth_median(window = tune::tune(id = "window")),
      list(window = 3L)
    ),
    tune::finalize_recipe(
      recipes::recipe(outcome ~ ., data = window_step_data()) |>
        step_measure_despike(
          window = tune::tune(id = "window"),
          threshold = tune::tune(id = "threshold")
        ),
      list(window = 3L, threshold = 2)
    ),
    tune::finalize_recipe(
      recipes::recipe(outcome ~ ., data = window_step_data()) |>
        step_measure_align_shift(max_shift = tune::tune(id = "max_shift")),
      list(max_shift = 1L)
    )
  )

  for (rec in recipes_to_check) {
    prepped <- recipes::prep(rec)
    baked <- recipes::bake(prepped, new_data = NULL)
    expect_true(is_measure_list(baked$.measures))
    expect_false(any(vapply(
      prepped$steps,
      function(x) {
        any(vapply(x, rlang::is_call, logical(1), name = "tune"))
      },
      logical(1)
    )))
  }
})
