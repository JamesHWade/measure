.trace_test_scan <- function(
  scan_id,
  scan_index,
  scan_status = "acquired",
  retention_time = 1,
  polarity = "positive",
  ms_level = 1L,
  spectrum_type = "centroid",
  source_scan_id = paste0("raw-", scan_id),
  mz = double(),
  intensity = double(),
  source_rows = rep(NA_integer_, length(mz)),
  saturated = rep(NA, length(mz))
) {
  list(
    scan_id = scan_id,
    scan_index = scan_index,
    scan_status = scan_status,
    retention_time = retention_time,
    polarity = polarity,
    ms_level = ms_level,
    spectrum_type = spectrum_type,
    source_scan_id = source_scan_id,
    mz = mz,
    intensity = intensity,
    source_rows = source_rows,
    saturated = saturated
  )
}

.trace_test_acquisition <- function(
  scans,
  stream_id = "ms-1",
  stream_overrides = list()
) {
  record <- measure_record(
    samples = tibble::tibble(sample_id = "sample-1"),
    injections = tibble::tibble(
      injection_id = "injection-1",
      sample_id = "sample-1"
    ),
    channels = tibble::tibble(channel_id = "channel-1"),
    sources = tibble::tibble(
      source_id = "source-1",
      source_sha256 = paste(rep("a", 64L), collapse = "")
    )
  )

  stream <- utils::modifyList(
    list(
      stream_id = stream_id,
      injection_id = "injection-1",
      channel_id = "channel-1",
      kind = "ms",
      source_id = "source-1",
      time_unit = "s",
      mz_unit = "Da",
      value_name = "abundance",
      value_unit = "count"
    ),
    stream_overrides,
    keep.null = TRUE
  )

  scan_table <- tibble::tibble(
    scan_id = vapply(scans, `[[`, character(1), "scan_id"),
    stream_id = rep(stream_id, length(scans)),
    scan_index = vapply(scans, `[[`, integer(1), "scan_index"),
    scan_status = vapply(scans, `[[`, character(1), "scan_status"),
    retention_time = vapply(scans, `[[`, numeric(1), "retention_time"),
    polarity = vapply(scans, `[[`, character(1), "polarity"),
    ms_level = vapply(scans, `[[`, integer(1), "ms_level"),
    spectrum_type = vapply(scans, `[[`, character(1), "spectrum_type"),
    source_scan_id = vapply(
      scans,
      `[[`,
      character(1),
      "source_scan_id"
    ),
    .measures = lapply(scans, function(scan) {
      new_measure_tbl(location = scan$mz, value = scan$intensity)
    }),
    .source_rows = lapply(scans, `[[`, "source_rows"),
    .saturated = lapply(scans, `[[`, "saturated")
  )

  measure_acquisition(
    record = record,
    streams = tibble::as_tibble(stream),
    scans = scan_table
  )
}

test_that("TIC sums raw centroid points in scan-index order", {
  acquisition <- .trace_test_acquisition(list(
    .trace_test_scan(
      "scan-b",
      2L,
      retention_time = 4.2,
      mz = c(150, 100),
      intensity = c(3, 1),
      source_rows = c(8L, 7L),
      saturated = c(FALSE, FALSE)
    ),
    .trace_test_scan(
      "scan-a",
      1L,
      retention_time = 4.2,
      mz = c(100, 100, 200),
      intensity = c(1, 2, 5),
      source_rows = c(2L, 3L, 4L),
      saturated = c(FALSE, FALSE, FALSE)
    ),
    .trace_test_scan("scan-c", 4L, retention_time = 1.1)
  ))

  trace <- measure_ms_trace(
    acquisition,
    stream_id = "ms-1",
    polarity = "positive",
    ms_level = 1L
  )

  expect_identical(class(trace), c("tbl_df", "tbl", "data.frame"))
  expect_identical(
    names(trace),
    c(
      "scan_id",
      "scan_index",
      "source_scan_id",
      "retention_time",
      "value",
      "status",
      "selected_points",
      "saturation_unknown",
      ".point_indices"
    )
  )
  expect_identical(trace$scan_id, c("scan-a", "scan-b", "scan-c"))
  expect_identical(trace$scan_index, c(1L, 2L, 4L))
  expect_identical(trace$retention_time, c(4.2, 4.2, 1.1))
  expect_identical(trace$value, c(8, 4, 0))
  expect_identical(trace$status, c("calculated", "calculated", "empty_scan"))
  expect_identical(trace$selected_points, c(3L, 2L, 0L))
  expect_identical(
    trace$.point_indices,
    list(1:3, 1:2, integer())
  )

  processing <- attr(trace, "processing")
  expect_identical(
    processing,
    list(
      version = 1L,
      operation = "tic",
      package = "measure",
      package_version = as.character(utils::packageVersion("measure")),
      stream_id = "ms-1",
      injection_id = "injection-1",
      channel_id = "channel-1",
      source_id = "source-1",
      source_sha256 = paste(rep("a", 64L), collapse = ""),
      polarity = "positive",
      ms_level = 1L,
      mz_range = NULL,
      mz_unit = "Da",
      time_unit = "s",
      value_name = "abundance",
      value_unit = "count",
      aggregation = "centroid_sum",
      missing_intensity = "propagate",
      saturation = "propagate",
      synchronization = "none"
    )
  )
})

test_that("XIC windows are inclusive and retain selected raw point positions", {
  acquisition <- .trace_test_acquisition(list(
    .trace_test_scan(
      "xic",
      1L,
      mz = c(99, 100, 101, 102),
      intensity = c(NaN, 2, 3, Inf),
      source_rows = c(1L, 2L, 2L, 3L),
      saturated = c(NA, FALSE, NA, TRUE)
    )
  ))

  trace <- measure_ms_trace(
    acquisition,
    stream_id = "ms-1",
    polarity = "positive",
    ms_level = 1,
    mz_range = c(100, 101)
  )

  expect_identical(trace$value, 5)
  expect_identical(trace$status, "calculated")
  expect_identical(trace$selected_points, 2L)
  expect_identical(trace$saturation_unknown, TRUE)
  expect_identical(trace$.point_indices, list(2:3))
  expect_identical(attr(trace, "processing")$operation, "xic")
  expect_identical(attr(trace, "processing")$mz_range, c(100, 101))
})

test_that("missing, acquired-empty, and empty-window scans remain distinct", {
  acquisition <- .trace_test_acquisition(list(
    .trace_test_scan(
      "missing",
      1L,
      scan_status = "missing",
      spectrum_type = NA_character_
    ),
    .trace_test_scan("empty", 3L),
    .trace_test_scan("outside", 8L, mz = c(10, 20), intensity = c(3, 4))
  ))

  trace <- measure_ms_trace(
    acquisition,
    stream_id = "ms-1",
    polarity = "positive",
    ms_level = 1L,
    mz_range = c(100, 200)
  )

  expect_identical(trace$scan_id, c("missing", "empty", "outside"))
  expect_identical(trace$value, c(NA_real_, 0, 0))
  expect_identical(
    trace$status,
    c("missing_scan", "empty_scan", "empty_window")
  )
  expect_identical(trace$selected_points, c(0L, 0L, 0L))
  expect_identical(trace$saturation_unknown, c(FALSE, FALSE, FALSE))
  expect_identical(
    trace$.point_indices,
    list(integer(), integer(), integer())
  )
})

test_that("saturation and missing intensity propagate with declared precedence", {
  acquisition <- .trace_test_acquisition(list(
    .trace_test_scan(
      "saturated",
      1L,
      mz = c(100, 101),
      intensity = c(2, NA_real_),
      saturated = c(TRUE, FALSE)
    ),
    .trace_test_scan(
      "missing-intensity",
      2L,
      mz = 100,
      intensity = NA_real_,
      saturated = NA
    ),
    .trace_test_scan(
      "unknown-mask",
      3L,
      mz = c(100, 101),
      intensity = c(2, 3),
      saturated = c(NA, FALSE)
    )
  ))

  trace <- measure_ms_trace(
    acquisition,
    stream_id = "ms-1",
    polarity = "positive",
    ms_level = 1L
  )

  expect_identical(trace$value, c(NA_real_, NA_real_, 5))
  expect_identical(
    trace$status,
    c("saturated", "missing_intensity", "calculated")
  )
  expect_identical(trace$selected_points, c(2L, 1L, 2L))
  expect_identical(trace$saturation_unknown, c(FALSE, TRUE, TRUE))
  expect_identical(trace$.point_indices, list(1:2, 1L, 1:2))
})

test_that("unknown scan selectors error only when the row could match", {
  ambiguous <- .trace_test_acquisition(list(
    .trace_test_scan("match", 1L, mz = 100, intensity = 1),
    .trace_test_scan("ambiguous", 2L, polarity = NA_character_)
  ))
  expect_snapshot(
    error = TRUE,
    measure_ms_trace(
      ambiguous,
      stream_id = "ms-1",
      polarity = "positive",
      ms_level = 1L
    )
  )

  excluded <- .trace_test_acquisition(list(
    .trace_test_scan("match", 1L, mz = 100, intensity = 1),
    .trace_test_scan(
      "known-other-polarity",
      2L,
      polarity = "negative",
      ms_level = NA_integer_
    ),
    .trace_test_scan(
      "known-other-level",
      3L,
      polarity = NA_character_,
      ms_level = 2L
    )
  ))
  expect_identical(
    measure_ms_trace(
      excluded,
      stream_id = "ms-1",
      polarity = "positive",
      ms_level = 1L
    )$scan_id,
    "match"
  )
})

test_that("unknown time or m/z units prevent the extraction that needs them", {
  unknown_time <- .trace_test_acquisition(
    list(.trace_test_scan("scan", 1L, mz = 100, intensity = 1)),
    stream_overrides = list(time_unit = NA_character_)
  )
  expect_snapshot(
    error = TRUE,
    measure_ms_trace(
      unknown_time,
      stream_id = "ms-1",
      polarity = "positive",
      ms_level = 1L
    )
  )

  unknown_mz <- .trace_test_acquisition(
    list(.trace_test_scan("scan", 1L, mz = 100, intensity = 1)),
    stream_overrides = list(mz_unit = NA_character_)
  )
  expect_identical(
    measure_ms_trace(
      unknown_mz,
      stream_id = "ms-1",
      polarity = "positive",
      ms_level = 1L
    )$value,
    1
  )
  expect_snapshot(
    error = TRUE,
    measure_ms_trace(
      unknown_mz,
      stream_id = "ms-1",
      polarity = "positive",
      ms_level = 1L,
      mz_range = c(90, 110)
    )
  )
})

test_that("trace extraction rejects unsupported scan and coordinate states", {
  profile <- .trace_test_acquisition(list(
    .trace_test_scan(
      "profile",
      1L,
      spectrum_type = "profile",
      mz = 100,
      intensity = 1
    )
  ))
  expect_snapshot(
    error = TRUE,
    measure_ms_trace(
      profile,
      stream_id = "ms-1",
      polarity = "positive",
      ms_level = 1L
    )
  )

  unknown_type <- .trace_test_acquisition(list(
    .trace_test_scan(
      "unknown-type",
      1L,
      spectrum_type = NA_character_,
      mz = 100,
      intensity = 1
    )
  ))
  expect_snapshot(
    error = TRUE,
    measure_ms_trace(
      unknown_type,
      stream_id = "ms-1",
      polarity = "positive",
      ms_level = 1L
    )
  )

  bad_mz <- .trace_test_acquisition(list(
    .trace_test_scan("bad-mz", 1L, mz = c(100, Inf), intensity = c(1, 2))
  ))
  expect_snapshot(
    error = TRUE,
    measure_ms_trace(
      bad_mz,
      stream_id = "ms-1",
      polarity = "positive",
      ms_level = 1L,
      mz_range = c(90, 110)
    )
  )

  bad_time <- .trace_test_acquisition(list(
    .trace_test_scan(
      "bad-time",
      1L,
      retention_time = NA_real_,
      mz = 100,
      intensity = 1
    )
  ))
  expect_snapshot(
    error = TRUE,
    measure_ms_trace(
      bad_time,
      stream_id = "ms-1",
      polarity = "positive",
      ms_level = 1L
    )
  )
})

test_that("scan IDs are treated as text when an error is formatted", {
  withr::local_options(trace_extract_probe = NULL)
  scan_id <- "{options(trace_extract_probe = TRUE)}"
  acquisition <- .trace_test_acquisition(list(
    .trace_test_scan(
      scan_id,
      1L,
      spectrum_type = "profile",
      mz = 100,
      intensity = 1
    )
  ))

  error <- tryCatch(
    measure_ms_trace(
      acquisition,
      stream_id = "ms-1",
      polarity = "positive",
      ms_level = 1L
    ),
    error = identity
  )

  expect_s3_class(error, "error")
  expect_identical(getOption("trace_extract_probe"), NULL)
  expect_identical(
    grepl(scan_id, conditionMessage(error), fixed = TRUE),
    TRUE
  )
})

test_that("selected invalid intensities and unrepresentable sums error", {
  for (invalid in list(NaN, Inf, -1)) {
    acquisition <- .trace_test_acquisition(list(
      .trace_test_scan(
        "invalid",
        1L,
        mz = 100,
        intensity = invalid,
        saturated = TRUE
      )
    ))
    expect_snapshot(
      error = TRUE,
      measure_ms_trace(
        acquisition,
        stream_id = "ms-1",
        polarity = "positive",
        ms_level = 1L
      )
    )
  }

  overflow <- .trace_test_acquisition(list(
    .trace_test_scan(
      "overflow",
      1L,
      mz = c(100, 101),
      intensity = c(.Machine$double.xmax, .Machine$double.xmax),
      saturated = c(TRUE, FALSE)
    )
  ))
  expect_snapshot(
    error = TRUE,
    measure_ms_trace(
      overflow,
      stream_id = "ms-1",
      polarity = "positive",
      ms_level = 1L
    )
  )
})

test_that("selectors and XIC intervals are explicit plain values", {
  acquisition <- .trace_test_acquisition(list(
    .trace_test_scan("scan", 1L, mz = 100, intensity = 1)
  ))

  expect_snapshot(
    error = TRUE,
    measure_ms_trace(
      acquisition,
      stream_id = "ms-1",
      polarity = "positive"
    )
  )
  expect_snapshot(
    error = TRUE,
    measure_ms_trace(
      acquisition,
      stream_id = "ms-1",
      polarity = "unknown",
      ms_level = 1L
    )
  )
  expect_snapshot(
    error = TRUE,
    measure_ms_trace(
      acquisition,
      stream_id = "ms-1",
      polarity = "positive",
      ms_level = 0L
    )
  )
  expect_snapshot(
    error = TRUE,
    measure_ms_trace(
      acquisition,
      stream_id = "ms-1",
      polarity = "positive",
      ms_level = 1L,
      mz_range = c(100, Inf)
    )
  )
  expect_snapshot(
    error = TRUE,
    measure_ms_trace(
      acquisition,
      stream_id = "ms-1",
      polarity = "positive",
      ms_level = 1L,
      mz_range = c(101, 100)
    )
  )
  expect_snapshot(
    error = TRUE,
    measure_ms_trace(
      acquisition,
      stream_id = "missing-stream",
      polarity = "positive",
      ms_level = 1L
    )
  )
  expect_snapshot(
    error = TRUE,
    measure_ms_trace(
      acquisition,
      stream_id = "ms-1",
      polarity = "positive",
      ms_level = 2L
    )
  )
})
