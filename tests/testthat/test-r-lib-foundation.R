# ==============================================================================
# Tests for r-lib error attribution, arg_match(), and type checks (#212)
# ==============================================================================

test_that("arg_match() produces informative caller-attributed errors", {
  rec <- recipes::recipe(water + fat + protein ~ ., data = meats_long)

  expect_snapshot(
    error = TRUE,
    step_measure_smooth_ma(rec, edge_method = "refelct")
  )

  expect_snapshot(
    error = TRUE,
    validate_measure(
      new_measure_tbl(1:5, 1:5),
      checks = c("monotonic", "bogus")
    )
  )
})

test_that("shared validators attribute errors to user-facing callers", {
  rec <- recipes::recipe(water + fat + protein ~ ., data = meats_long)

  expect_snapshot(
    error = TRUE,
    step_measure_smooth_ma(rec, window = 4L)
  )

  expect_snapshot(
    error = TRUE,
    validate_measure(tibble::tibble(a = 1:3))
  )

  c1 <- new_measure_tbl(c(1, 2, 3), c(0.1, 0.5, 0.2))
  mini_rec <- measure_record(
    samples = tibble::tibble(sample_id = "S1"),
    injections = tibble::tibble(injection_id = "INJ-1", sample_id = "S1"),
    channels = tibble::tibble(channel_id = "uv", channel_name = "UV"),
    curves = tibble::tibble(
      curve_id = "C1",
      injection_id = "INJ-1",
      channel_id = "uv",
      .measures = list(c1)
    )
  )

  expect_snapshot(
    error = TRUE,
    measure_quantify_batch(mini_rec, manifest = "not-a-df", window = c(1, 3))
  )

  expect_snapshot(
    error = TRUE,
    measure_quantify_batch(
      mini_rec,
      manifest = tibble::tibble(
        injection_id = "INJ-1",
        injection_type = "standard",
        nominal_conc = 10
      ),
      window = c(3, 1)
    )
  )
})
