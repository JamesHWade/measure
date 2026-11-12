test_that("empty records have the complete typed schema", {
  record <- measure_record()

  expect_s3_class(record, "measure_record")
  expect_true(is_measure_record(record))
  expect_identical(
    names(record),
    c(
      "samples",
      "injections",
      "channels",
      "curves",
      "sources"
    )
  )
  expect_named(record$samples, "sample_id")
  expect_type(record$samples$sample_id, "character")
  expect_named(record$injections, c("injection_id", "sample_id"))
  expect_named(record$channels, c("channel_id", "channel_name"))
  expect_named(
    record$curves,
    c(
      "curve_id",
      "injection_id",
      "channel_id",
      "source_id",
      "axis_name",
      "axis_unit",
      "value_name",
      "value_unit",
      ".measures",
      ".source_rows"
    )
  )
  expect_true(is_measure_list(record$curves$.measures))
  expect_identical(record$curves$.source_rows, list())
  expect_named(record$sources, c("source_id", "source_name", "source_sha256"))
  expect_invisible(validate_measure_record(record))
  expect_snapshot(print(record))
})

test_that("records preserve explicit identities, independent grids, and source rows", {
  curve_a <- tibble::tibble(
    location = c(200L, 205L, 215L),
    value = c(0L, NA_integer_, -3L)
  )
  curve_b <- tibble::tibble(
    location = c(310, 300),
    value = c(5.5, 0)
  )
  curve_c <- tibble::tibble(
    location = 7,
    value = 1.25
  )
  payloads <- list(curve_a, curve_b, curve_c)
  input_curves <- tibble::tibble(
    curve_id = c("curve-a", "curve-b", "curve-c"),
    injection_id = c("inj-0001", "inj-0002", "inj-0003"),
    channel_id = c("uv", "uv", "ms"),
    source_id = c("source-1", "source-1", NA_character_),
    axis_unit = c("nm", "not-yet-known", NA_character_),
    .measures = payloads,
    .source_rows = list(c(1, 2, NA_real_), c(1L, 2L), NA_integer_)
  )
  before <- input_curves
  input_samples <- data.frame(
    sample_id = c("0007", "NA", "Sample α", "Unused")
  )
  before_samples <- input_samples

  record <- measure_record(
    samples = input_samples,
    injections = tibble::tibble(
      injection_id = c("inj-0001", "inj-0002", "inj-0003"),
      sample_id = c("0007", "0007", "Sample α")
    ),
    channels = tibble::tibble(
      channel_id = c("uv", "ms", "unused-channel"),
      channel_name = c("UV detector", "mass channel", NA_character_)
    ),
    curves = input_curves,
    sources = tibble::tibble(
      source_id = "source-1",
      source_name = "run α.csv",
      source_sha256 = paste(rep("a", 64), collapse = "")
    )
  )

  expect_identical(
    record$samples$sample_id,
    c("0007", "NA", "Sample α", "Unused")
  )
  expect_identical(
    record$injections$sample_id,
    c("0007", "0007", "Sample α")
  )
  expect_identical(record$channels$channel_id, c("uv", "ms", "unused-channel"))
  expect_identical(record$curves$curve_id, c("curve-a", "curve-b", "curve-c"))
  expect_identical(
    record$curves$axis_unit,
    c("nm", "not-yet-known", NA_character_)
  )
  expect_identical(record$curves$.measures[[1]]$location, c(200, 205, 215))
  expect_identical(record$curves$.measures[[1]]$value, c(0, NA_real_, -3))
  expect_identical(record$curves$.measures[[2]]$location, c(310, 300))
  expect_identical(record$curves$.measures[[3]]$location, 7)
  expect_identical(
    record$curves$.source_rows,
    list(
      c(1L, 2L, NA_integer_),
      c(1L, 2L),
      NA_integer_
    )
  )
  expect_identical(
    record$sources$source_sha256,
    paste(rep("a", 64), collapse = "")
  )
  expect_type(record$curves$.measures[[1]]$location, "double")
  expect_identical(input_curves, before)
  expect_identical(input_samples, before_samples)
})

test_that("pairwise location checks tolerate extreme finite double ranges", {
  record <- measure_record(
    samples = data.frame(sample_id = "sample"),
    injections = data.frame(injection_id = "inj", sample_id = "sample"),
    channels = data.frame(channel_id = "channel"),
    curves = data.frame(
      curve_id = "curve",
      injection_id = "inj",
      channel_id = "channel",
      .measures = I(list(tibble::tibble(
        location = c(-.Machine$double.xmax, 0, .Machine$double.xmax),
        value = c(1, 2, 3)
      )))
    )
  )

  expect_identical(
    record$curves$.measures[[1]]$location,
    c(-.Machine$double.xmax, 0, .Machine$double.xmax)
  )
})

test_that("missing source rows are typed missing ordinals", {
  record <- measure_record(
    samples = tibble::tibble(sample_id = "sample"),
    injections = tibble::tibble(injection_id = "inj", sample_id = "sample"),
    channels = tibble::tibble(channel_id = "uv"),
    curves = tibble::tibble(
      curve_id = "curve",
      injection_id = "inj",
      channel_id = "uv",
      .measures = list(new_measure_tbl(c(2, 1), c(4, 3)))
    )
  )

  expect_identical(
    record$curves$.source_rows,
    list(c(NA_integer_, NA_integer_))
  )
  expect_type(record$curves$.source_rows[[1]], "integer")
})

test_that("the record print method shows entity counts", {
  record <- measure_record(
    samples = tibble::tibble(sample_id = "sample"),
    injections = tibble::tibble(injection_id = "inj", sample_id = "sample"),
    channels = tibble::tibble(channel_id = "uv"),
    curves = tibble::tibble(
      curve_id = "curve",
      injection_id = "inj",
      channel_id = "uv",
      .measures = list(new_measure_tbl(1:2, c(4, 3)))
    )
  )

  expect_snapshot(print(record))
})

test_that("constructor rejects unknown and duplicate columns", {
  expect_snapshot(
    error = TRUE,
    measure_record(
      samples = tibble::tibble(sample_id = "sample", note = "extra")
    )
  )

  duplicate <- data.frame(
    first = "sample",
    second = "also-sample",
    check.names = FALSE
  )
  names(duplicate) <- c("sample_id", "sample_id")
  expect_snapshot(error = TRUE, measure_record(samples = duplicate))
})

test_that("constructor and validator enforce relation keys and references", {
  expect_snapshot(
    error = TRUE,
    measure_record(
      samples = tibble::tibble(sample_id = "sample"),
      injections = tibble::tibble(injection_id = "inj", sample_id = "absent")
    )
  )

  record <- measure_record(
    samples = tibble::tibble(sample_id = "sample"),
    injections = tibble::tibble(injection_id = "inj", sample_id = "sample"),
    channels = tibble::tibble(channel_id = "uv"),
    curves = tibble::tibble(
      curve_id = "curve",
      injection_id = "inj",
      channel_id = "uv",
      .measures = list(new_measure_tbl(1:2, c(4, 3)))
    )
  )
  record$channels <- tibble::add_row(record$channels, channel_id = "uv")

  expect_snapshot(error = TRUE, validate_measure_record(record))
})

test_that("one injection cannot have two curves for the same channel", {
  expect_snapshot(
    error = TRUE,
    measure_record(
      samples = tibble::tibble(sample_id = "sample"),
      injections = tibble::tibble(injection_id = "inj", sample_id = "sample"),
      channels = tibble::tibble(channel_id = "uv"),
      curves = tibble::tibble(
        curve_id = c("curve-a", "curve-b"),
        injection_id = c("inj", "inj"),
        channel_id = c("uv", "uv"),
        .measures = list(new_measure_tbl(1, 2), new_measure_tbl(2, 3))
      )
    )
  )
})

test_that("known curve sources must resolve to declared source rows", {
  expect_snapshot(
    error = TRUE,
    measure_record(
      samples = tibble::tibble(sample_id = "sample"),
      injections = tibble::tibble(injection_id = "inj", sample_id = "sample"),
      channels = tibble::tibble(channel_id = "uv"),
      curves = tibble::tibble(
        curve_id = "curve",
        injection_id = "inj",
        channel_id = "uv",
        source_id = "source-1",
        .measures = list(new_measure_tbl(1, 2))
      )
    )
  )
})

test_that("IDs, metadata, and source hashes have explicit types", {
  byte_id <- intToUtf8(0x00E9)
  Encoding(byte_id) <- "bytes"
  matrix_id <- matrix("sample", nrow = 1)
  class(matrix_id) <- NULL

  expect_snapshot(
    error = TRUE,
    measure_record(
      samples = tibble::tibble(sample_id = factor("007"))
    )
  )

  expect_snapshot(
    error = TRUE,
    measure_record(samples = tibble::tibble(sample_id = byte_id))
  )

  expect_snapshot(
    error = TRUE,
    measure_record(samples = tibble::tibble(sample_id = matrix_id))
  )

  record <- measure_record(samples = tibble::tibble(sample_id = "sample"))
  record$samples$sample_id <- matrix_id
  expect_snapshot(error = TRUE, validate_measure_record(record))

  expect_snapshot(
    error = TRUE,
    measure_record(
      samples = tibble::tibble(sample_id = intToUtf8(0x00A0))
    )
  )

  expect_snapshot(
    error = TRUE,
    measure_record(
      samples = tibble::tibble(sample_id = "sample"),
      injections = tibble::tibble(injection_id = "inj", sample_id = "sample"),
      channels = tibble::tibble(channel_id = "uv"),
      curves = tibble::tibble(
        curve_id = "curve",
        injection_id = "inj",
        channel_id = "uv",
        axis_unit = "   ",
        .measures = list(new_measure_tbl(1, 2))
      )
    )
  )

  expect_snapshot(
    error = TRUE,
    measure_record(
      sources = tibble::tibble(source_id = "source", source_sha256 = "abc")
    )
  )
})

test_that("curve payloads reject invalid numeric values and unsupported dimensions", {
  expect_snapshot(
    error = TRUE,
    measure_record(
      samples = tibble::tibble(sample_id = "sample"),
      injections = tibble::tibble(injection_id = "inj", sample_id = "sample"),
      channels = tibble::tibble(channel_id = "uv"),
      curves = tibble::tibble(
        curve_id = "curve",
        injection_id = "inj",
        channel_id = "uv",
        .measures = list(new_measure_tbl(c(1, 1), c(2, 3)))
      )
    )
  )

  expect_snapshot(
    error = TRUE,
    measure_record(
      samples = tibble::tibble(sample_id = "sample"),
      injections = tibble::tibble(injection_id = "inj", sample_id = "sample"),
      channels = tibble::tibble(channel_id = "uv"),
      curves = tibble::tibble(
        curve_id = "curve",
        injection_id = "inj",
        channel_id = "uv",
        .measures = list(new_measure_tbl(1:2, c(2, Inf)))
      )
    )
  )

  expect_snapshot(
    error = TRUE,
    measure_record(
      samples = tibble::tibble(sample_id = "sample"),
      injections = tibble::tibble(injection_id = "inj", sample_id = "sample"),
      channels = tibble::tibble(channel_id = "uv"),
      curves = tibble::tibble(
        curve_id = "curve",
        injection_id = "inj",
        channel_id = "uv",
        .measures = list(new_measure_nd_tbl(
          location_1 = 1:2,
          location_2 = c(2, 3),
          value = c(4, 5)
        ))
      )
    )
  )
})

test_that("source row ordinals are positive, aligned, and unique per curve", {
  expect_snapshot(
    error = TRUE,
    measure_record(
      samples = tibble::tibble(sample_id = "sample"),
      injections = tibble::tibble(injection_id = "inj", sample_id = "sample"),
      channels = tibble::tibble(channel_id = "uv"),
      curves = tibble::tibble(
        curve_id = "curve",
        injection_id = "inj",
        channel_id = "uv",
        .measures = list(new_measure_tbl(1:2, c(4, 3))),
        .source_rows = list(c(1L, 1L))
      )
    )
  )

  expect_snapshot(
    error = TRUE,
    measure_record(
      samples = tibble::tibble(sample_id = "sample"),
      injections = tibble::tibble(injection_id = "inj", sample_id = "sample"),
      channels = tibble::tibble(channel_id = "uv"),
      curves = tibble::tibble(
        curve_id = "curve",
        injection_id = "inj",
        channel_id = "uv",
        .measures = list(new_measure_tbl(1:2, c(4, 3))),
        .source_rows = list(c(1, 2.5))
      )
    )
  )
})

test_that("source row columns reject matrix list columns at both boundaries", {
  source_rows <- matrix(list(c(1L, 2L)), nrow = 1)

  expect_snapshot(
    error = TRUE,
    measure_record(
      samples = tibble::tibble(sample_id = "sample"),
      injections = tibble::tibble(injection_id = "inj", sample_id = "sample"),
      channels = tibble::tibble(channel_id = "uv"),
      curves = tibble::tibble(
        curve_id = "curve",
        injection_id = "inj",
        channel_id = "uv",
        .measures = list(new_measure_tbl(1:2, c(4, 3))),
        .source_rows = source_rows
      )
    )
  )

  record <- measure_record(
    samples = tibble::tibble(sample_id = "sample"),
    injections = tibble::tibble(injection_id = "inj", sample_id = "sample"),
    channels = tibble::tibble(channel_id = "uv"),
    curves = tibble::tibble(
      curve_id = "curve",
      injection_id = "inj",
      channel_id = "uv",
      .measures = list(new_measure_tbl(1:2, c(4, 3)))
    )
  )
  record$curves$.source_rows <- source_rows

  expect_snapshot(error = TRUE, validate_measure_record(record))
})

test_that("empty nD measurement lists are rejected at both boundaries", {
  empty_curves <- tibble::tibble(
    curve_id = character(),
    injection_id = character(),
    channel_id = character()
  )
  empty_curves$.measures <- new_measure_nd_list()

  expect_snapshot(
    error = TRUE,
    measure_record(curves = empty_curves)
  )

  record <- measure_record()
  record$curves$.measures <- new_measure_nd_list()
  expect_snapshot(error = TRUE, validate_measure_record(record))
})

test_that("dimensional measure lists are rejected at both boundaries", {
  measures <- new_measure_list(list(new_measure_tbl(1, 2)))
  attr(measures, "dim") <- c(1L, 1L)

  expect_snapshot(
    error = TRUE,
    measure_record(
      samples = tibble::tibble(sample_id = "sample"),
      injections = tibble::tibble(injection_id = "inj", sample_id = "sample"),
      channels = tibble::tibble(channel_id = "uv"),
      curves = tibble::tibble(
        curve_id = "curve",
        injection_id = "inj",
        channel_id = "uv",
        .measures = measures
      )
    )
  )

  record <- measure_record()
  attr(record$curves$.measures, "dim") <- c(0L, 1L)
  expect_snapshot(error = TRUE, validate_measure_record(record))
})

test_that("constructor canonicalizes negative zero and validator rechecks it", {
  negative_zero <- -1 / Inf
  record <- measure_record(
    samples = tibble::tibble(sample_id = "sample"),
    injections = tibble::tibble(injection_id = "inj", sample_id = "sample"),
    channels = tibble::tibble(channel_id = "uv"),
    curves = tibble::tibble(
      curve_id = "curve",
      injection_id = "inj",
      channel_id = "uv",
      .measures = list(new_measure_tbl(
        location = c(negative_zero, 1),
        value = c(negative_zero, 2)
      ))
    )
  )

  expect_identical(1 / record$curves$.measures[[1]]$location[[1]], Inf)
  expect_identical(1 / record$curves$.measures[[1]]$value[[1]], Inf)

  payload <- record$curves$.measures[[1]]
  payload$value[[1]] <- negative_zero
  record$curves$.measures[[1]] <- payload

  expect_snapshot(error = TRUE, validate_measure_record(record))
})

test_that("validator rejects changed schemas and orphaned curve relationships", {
  record <- measure_record()
  record$extra <- tibble::tibble()
  expect_snapshot(error = TRUE, validate_measure_record(record))

  record <- measure_record(
    samples = tibble::tibble(sample_id = "sample"),
    injections = tibble::tibble(injection_id = "inj", sample_id = "sample"),
    channels = tibble::tibble(channel_id = "uv"),
    curves = tibble::tibble(
      curve_id = "curve",
      injection_id = "inj",
      channel_id = "uv",
      .measures = list(new_measure_tbl(1, 2))
    )
  )
  record$channels$channel_name <- NULL
  expect_snapshot(error = TRUE, validate_measure_record(record))

  record <- measure_record(
    samples = tibble::tibble(sample_id = "sample"),
    injections = tibble::tibble(injection_id = "inj", sample_id = "sample"),
    channels = tibble::tibble(channel_id = "uv"),
    curves = tibble::tibble(
      curve_id = "curve",
      injection_id = "inj",
      channel_id = "uv",
      .measures = list(new_measure_tbl(1:2, c(4, 3)))
    )
  )
  record$injections$sample_id <- "removed"
  expect_snapshot(error = TRUE, validate_measure_record(record))
})
