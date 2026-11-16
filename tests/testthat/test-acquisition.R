test_that("measurement acquisitions canonicalize empty owned tables", {
  acquisition <- measure_acquisition(record = measure_record())

  expect_true(is_measure_acquisition(acquisition))
  expect_s3_class(acquisition, "measure_acquisition")
  expect_identical(
    names(acquisition),
    c("record", "streams", "scans", "arrays")
  )
  expect_identical(
    names(acquisition$streams),
    c(
      "stream_id",
      "injection_id",
      "channel_id",
      "kind",
      "source_id",
      "time_unit",
      "mz_unit",
      "value_name",
      "value_unit"
    )
  )
  expect_type(acquisition$scans$scan_index, "integer")
  expect_type(acquisition$scans$retention_time, "double")
  expect_type(acquisition$scans$.measures, "list")
  expect_length(acquisition$arrays$.source_rows, 0)
  expect_invisible(validate_measure_acquisition(acquisition))
  expect_output(print(acquisition), "<measure_acquisition>")
  expect_output(print(summary(acquisition)), "<summary_measure_acquisition>")
})

test_that("acquisitions preserve raw scan points and canonicalize declarations", {
  record <- measure_record(
    samples = tibble::tibble(sample_id = "sample"),
    injections = tibble::tibble(
      injection_id = "injection",
      sample_id = "sample"
    ),
    channels = tibble::tibble(channel_id = c("ms", "dad")),
    sources = tibble::tibble(source_id = c("raw-ms", "raw-dad"))
  )
  raw <- new_measure_tbl(
    location = c(500L, 100L, 100L),
    value = c(2L, NA_integer_, 4L)
  )
  nd <- new_measure_nd_tbl(
    location_1 = c(2, 1, 1),
    location_2 = c(280L, 280L, 290L),
    value = c(NA_real_, NaN, Inf),
    dim_names = c("time", "wavelength"),
    dim_units = c("min", "nm")
  )
  acquisition <- measure_acquisition(
    record,
    streams = data.frame(
      stream_id = c("ms-stream", "dad-stream"),
      injection_id = c("injection", "injection"),
      channel_id = c("ms", "dad"),
      kind = c("ms", "nd"),
      source_id = c("raw-ms", "raw-dad"),
      time_unit = c("min", NA_character_),
      mz_unit = c("m/z", NA_character_),
      stringsAsFactors = FALSE
    ),
    scans = tibble::tibble(
      scan_id = c("scan-a", "scan-empty", "scan-missing"),
      stream_id = rep("ms-stream", 3),
      scan_index = c(7, 12, 20),
      scan_status = c("acquired", "acquired", "missing"),
      retention_time = c(-0, 1L, NA_real_),
      polarity = c("positive", "positive", NA_character_),
      ms_level = c(1, 1L, NA_integer_),
      spectrum_type = c("centroid", "centroid", NA_character_),
      .measures = list(raw, new_measure_tbl(), new_measure_tbl())
    ),
    arrays = tibble::tibble(
      array_id = "array-1",
      stream_id = "dad-stream",
      .measures = list(nd)
    )
  )

  expect_identical(acquisition$streams$stream_id, c("ms-stream", "dad-stream"))
  expect_identical(acquisition$scans$scan_index, c(7L, 12L, 20L))
  expect_identical(typeof(acquisition$scans$retention_time), "double")
  expect_identical(1 / acquisition$scans$retention_time[[1]], -Inf)
  expect_identical(
    acquisition$scans$.measures[[1]]$location,
    c(500L, 100L, 100L)
  )
  expect_identical(
    acquisition$scans$.measures[[1]]$value,
    c(2L, NA_integer_, 4L)
  )
  expect_identical(acquisition$scans$.source_rows[[1]], rep(NA_integer_, 3))
  expect_identical(acquisition$scans$.saturated[[1]], rep(NA, 3))
  expect_identical(nrow(acquisition$scans$.measures[[2]]), 0L)
  expect_identical(nrow(acquisition$scans$.measures[[3]]), 0L)
  expect_identical(acquisition$arrays$.measures[[1]], nd)
  expect_identical(
    attr(acquisition$arrays$.measures[[1]], "dim_names"),
    c("time", "wavelength")
  )
})

test_that("per-point references allow duplicates and preserve unknown masks", {
  record <- measure_record(
    samples = tibble::tibble(sample_id = "s"),
    injections = tibble::tibble(injection_id = "i", sample_id = "s"),
    channels = tibble::tibble(channel_id = "ms")
  )
  measure_list <- new_measure_list(list(new_measure_tbl(1:3, 4:6)))
  acquisition <- measure_acquisition(
    record,
    streams = tibble::tibble(
      stream_id = "stream",
      injection_id = "i",
      channel_id = "ms",
      kind = "ms"
    ),
    scans = tibble::tibble(
      scan_id = "scan",
      stream_id = "stream",
      scan_index = 1L,
      scan_status = "acquired",
      .measures = measure_list,
      .source_rows = list(c(2L, 2L, NA_integer_)),
      .saturated = list(c(TRUE, FALSE, NA))
    )
  )

  expect_identical(acquisition$scans$.source_rows[[1]], c(2L, 2L, NA_integer_))
  expect_identical(acquisition$scans$.saturated[[1]], c(TRUE, FALSE, NA))
  expect_identical(attributes(acquisition$scans$.measures), NULL)
  expect_identical(acquisition$scans$retention_time, NA_real_)
  expect_identical(acquisition$scans$polarity, NA_character_)
  expect_identical(acquisition$scans$ms_level, NA_integer_)
  expect_identical(acquisition$scans$spectrum_type, NA_character_)
  expect_invisible(validate_measure_acquisition(acquisition))
})

test_that("acquisition construction rejects invalid declarations", {
  record <- measure_record(
    samples = tibble::tibble(sample_id = "s"),
    injections = tibble::tibble(injection_id = "i", sample_id = "s"),
    channels = tibble::tibble(channel_id = c("ms", "nd"))
  )
  streams <- tibble::tibble(
    stream_id = c("ms-stream", "nd-stream"),
    injection_id = c("i", "i"),
    channel_id = c("ms", "nd"),
    kind = c("ms", "nd")
  )
  scans <- tibble::tibble(
    scan_id = "scan",
    stream_id = "ms-stream",
    scan_index = 1L,
    scan_status = "acquired",
    .measures = list(new_measure_tbl())
  )

  expect_snapshot(
    error = TRUE,
    measure_acquisition(
      record,
      streams = streams,
      scans = transform(scans, scan_index = 0L)
    )
  )
  expect_snapshot(
    error = TRUE,
    measure_acquisition(
      record,
      streams = streams,
      scans = transform(scans, stream_id = "nd-stream")
    )
  )
  expect_snapshot(
    error = TRUE,
    measure_acquisition(
      record,
      streams = streams,
      arrays = tibble::tibble(
        array_id = "a",
        stream_id = "ms-stream",
        .measures = list(new_measure_nd_tbl(
          location_1 = 1,
          location_2 = 2,
          value = 3
        ))
      )
    )
  )
  expect_snapshot(
    error = TRUE,
    measure_acquisition(
      record,
      streams = tibble::tibble(
        stream_id = "nd-stream",
        injection_id = "i",
        channel_id = "nd",
        kind = "nd",
        time_unit = "min"
      )
    )
  )
  named_measures <- new_measure_list(
    stats::setNames(list(new_measure_tbl()), "named")
  )
  expect_snapshot(
    error = TRUE,
    measure_acquisition(
      record,
      streams = streams,
      scans = tibble::tibble(
        scan_id = "scan",
        stream_id = "ms-stream",
        scan_index = 1L,
        scan_status = "acquired",
        .measures = named_measures
      )
    )
  )
})

test_that("acquisitions reject invalid mutations during revalidation", {
  record <- measure_record(
    samples = tibble::tibble(sample_id = "s"),
    injections = tibble::tibble(injection_id = "i", sample_id = "s"),
    channels = tibble::tibble(channel_id = "ms")
  )
  acquisition <- measure_acquisition(
    record,
    streams = tibble::tibble(
      stream_id = "stream",
      injection_id = "i",
      channel_id = "ms",
      kind = "ms"
    ),
    scans = tibble::tibble(
      scan_id = "scan",
      stream_id = "stream",
      scan_index = 1L,
      scan_status = "acquired",
      .measures = list(new_measure_tbl(1, 2))
    )
  )
  bad_index <- acquisition
  bad_index$scans$scan_index <- 1
  bad_mask <- acquisition
  bad_mask$scans$.saturated[[1]] <- logical()
  bad_payload <- acquisition
  attr(bad_payload$scans$.measures[[1]]$value, "unit") <- "counts"

  expect_snapshot(error = TRUE, validate_measure_acquisition(bad_index))
  expect_snapshot(error = TRUE, validate_measure_acquisition(bad_mask))
  expect_snapshot(error = TRUE, validate_measure_acquisition(bad_payload))

  bad_record <- acquisition
  attr(bad_record$record$curves, "extra") <- "not supported"
  expect_snapshot(error = TRUE, validate_measure_acquisition(bad_record))
})

test_that("acquisition validation rejects malformed table and payload shapes", {
  malformed_measure <- structure(
    list(location = 1, value = 1:3),
    names = c("location", "value"),
    row.names = c(NA_integer_, -2L),
    class = c("measure_tbl", "tbl_df", "tbl", "data.frame")
  )
  record <- measure_record(
    samples = tibble::tibble(sample_id = "s"),
    injections = tibble::tibble(injection_id = "i", sample_id = "s"),
    channels = tibble::tibble(channel_id = "ms")
  )
  streams <- tibble::tibble(
    stream_id = "stream",
    injection_id = "i",
    channel_id = "ms",
    kind = "ms"
  )
  scans <- tibble::tibble(
    scan_id = "scan",
    stream_id = "stream",
    scan_index = 1L,
    scan_status = "acquired",
    .measures = list(malformed_measure)
  )
  acquisition <- measure_acquisition(
    record,
    streams = streams,
    scans = tibble::tibble(
      scan_id = "scan",
      stream_id = "stream",
      scan_index = 1L,
      scan_status = "acquired",
      .measures = list(new_measure_tbl(1, 2))
    )
  )

  expect_snapshot(error = TRUE, measure_acquisition(record, streams, scans))

  malformed_input_streams <- structure(
    list(
      stream_id = c("stream-a", "stream-b"),
      injection_id = "i",
      channel_id = c("ms", "ms"),
      kind = c("ms", "ms")
    ),
    names = c("stream_id", "injection_id", "channel_id", "kind"),
    row.names = c(NA_integer_, -2L),
    class = c("tbl_df", "tbl", "data.frame")
  )
  expect_snapshot(
    error = TRUE,
    measure_acquisition(record, streams = malformed_input_streams)
  )

  bad_payload <- acquisition
  bad_payload$scans$.measures[[1]] <- malformed_measure
  expect_snapshot(error = TRUE, validate_measure_acquisition(bad_payload))

  bad_stream_table <- acquisition
  bad_stream_table$streams <- structure(
    list(
      stream_id = c("stream-a", "stream-b"),
      injection_id = c("i", "i"),
      channel_id = c("ms", "ms"),
      kind = c("ms", "ms"),
      source_id = NA_character_,
      time_unit = c(NA_character_, NA_character_),
      mz_unit = c(NA_character_, NA_character_),
      value_name = c(NA_character_, NA_character_),
      value_unit = c(NA_character_, NA_character_)
    ),
    names = .acq_schemas$streams,
    row.names = c(NA_integer_, -2L),
    class = c("tbl_df", "tbl", "data.frame")
  )
  expect_snapshot(error = TRUE, validate_measure_acquisition(bad_stream_table))

  bad_record_table <- acquisition
  bad_record_table$record$channels <- structure(
    list(channel_id = c("ms", "other"), channel_name = NA_character_),
    names = c("channel_id", "channel_name"),
    row.names = c(NA_integer_, -2L),
    class = c("tbl_df", "tbl", "data.frame")
  )
  expect_snapshot(error = TRUE, validate_measure_acquisition(bad_record_table))
})
