local_jcamp_write_record <- function(
  axis_unit = "1/CM",
  value_unit = "ABSORBANCE",
  selected_id = "curve-α"
) {
  measure_record(
    samples = tibble::tibble(sample_id = c("sample-1", "sample-2")),
    injections = tibble::tibble(
      injection_id = c("injection-1", "injection-2"),
      sample_id = c("sample-1", "sample-2")
    ),
    channels = tibble::tibble(
      channel_id = c("ir", "other"),
      channel_name = c("IR", "other detector")
    ),
    curves = tibble::tibble(
      curve_id = c(selected_id, "other-curve"),
      injection_id = c("injection-1", "injection-2"),
      channel_id = c("ir", "other"),
      source_id = c("source-1", "source-2"),
      axis_name = c("wavenumber", "time"),
      axis_unit = c(axis_unit, "s"),
      value_name = c("absorbance", "signal"),
      value_unit = c(value_unit, "ABSORBANCE"),
      .measures = list(
        new_measure_tbl(
          location = c(4000, 2500, 1000),
          value = c(pi, NA_real_, .Machine$double.xmax)
        ),
        new_measure_tbl(
          location = c(1, 2),
          value = c(10, 11)
        )
      )
    ),
    sources = tibble::tibble(
      source_id = c("source-1", "source-2"),
      source_name = c("sample-1.raw", "other.raw"),
      source_sha256 = c(NA_character_, NA_character_)
    )
  )
}

test_that("writer emits one exact, descending irregular AFFN spectrum", {
  record <- local_jcamp_write_record()
  path <- file.path(withr::local_tempdir(), "selected.jdx")

  output <- measure_write_jcamp(
    record = record,
    path = path,
    curve_id = "curve-α",
    title = "Selected IR",
    origin = "Laboratory A\r\nNIST-compatible export",
    owner = "Copyright (C) 2026\nPublic Domain"
  )

  expect_identical(output, normalizePath(path, mustWork = TRUE))
  bytes <- readBin(path, what = "raw", n = file.info(path)$size)
  expect_identical(any(as.integer(bytes) > 0x7f), FALSE)
  expect_identical(any(bytes == as.raw(0x0d)), FALSE)

  text <- rawToChar(bytes)
  expect_identical(substr(text, nchar(text), nchar(text)), "\n")
  lines <- strsplit(text, "\n", fixed = TRUE)[[1L]]
  expect_identical(
    lines[seq_len(17L)],
    c(
      "##TITLE= Selected IR",
      "##JCAMP-DX= 4.24",
      "##DATA TYPE= INFRARED SPECTRUM",
      "##ORIGIN= Laboratory A",
      "NIST-compatible export",
      "##OWNER= Copyright (C) 2026",
      "Public Domain",
      "##NPOINTS= 3",
      "##XUNITS= 1/CM",
      "##YUNITS= ABSORBANCE",
      "##XLABEL= wavenumber",
      "##YLABEL= absorbance",
      "##XFACTOR= 1",
      "##YFACTOR= 1",
      "##FIRSTX= 4000",
      "##LASTX= 1000",
      "##FIRSTY= 3.1415926535897931"
    )
  )
  expect_identical(lines[[18L]], "##XYPOINTS= (XY..XY)")
  expect_identical(
    lines[19:21],
    c(
      "4000,3.1415926535897931",
      "2500,?",
      "1000,1.7976931348623157e+308"
    )
  )
  expect_identical(lines[[22L]], "##END=")
  expect_identical(
    grepl("sample-1|injection-1|source-1|other-curve", text),
    FALSE
  )
})

test_that("writer preserves exact doubles independently of display settings", {
  record <- local_jcamp_write_record()
  path <- file.path(withr::local_tempdir(), "precision.jdx")
  withr::local_options(digits = 3, scipen = 1000)

  measure_write_jcamp(
    record,
    path,
    "curve-α",
    "Precision",
    "Origin",
    "Owner"
  )

  lines <- readLines(path, warn = FALSE)
  points <- lines[grepl("^[+-]?(?:[0-9]|\\.)", lines, perl = TRUE)]
  point_tokens <- strsplit(points, ",", fixed = TRUE)
  locations <- .record_parse_double(
    vapply(point_tokens, function(x) x[[1L]], character(1)),
    "location"
  )
  signal_tokens <- vapply(point_tokens, function(x) x[[2L]], character(1))
  signals <- rep(NA_real_, length(signal_tokens))
  finite <- signal_tokens != "?"
  signals[finite] <- .record_parse_double(signal_tokens[finite], "value")

  expect_identical(locations, c(4000, 2500, 1000))
  expect_identical(
    signals,
    c(pi, NA_real_, .Machine$double.xmax)
  )
})

test_that("writer requires an unambiguous curve and complete source units", {
  record <- local_jcamp_write_record()
  output <- withr::local_tempdir()

  expect_error(
    measure_write_jcamp(
      record,
      file.path(output, "unknown.jdx"),
      "missing",
      "Title",
      "Origin",
      "Owner"
    ),
    "No curve"
  )
  expect_error(
    measure_write_jcamp(
      record,
      file.path(output, "multiple.jdx"),
      c("curve-α", "other-curve"),
      "Title",
      "Origin",
      "Owner"
    ),
    "curve_id"
  )

  missing_axis <- local_jcamp_write_record(axis_unit = NA_character_)
  missing_path <- file.path(output, "missing-unit.jdx")
  expect_error(
    measure_write_jcamp(
      missing_axis,
      missing_path,
      "curve-α",
      "Title",
      "Origin",
      "Owner"
    ),
    "axis_unit"
  )
  expect_identical(file.exists(missing_path), FALSE)
})

test_that("writer rejects unsafe or overlong literal header values before output", {
  record <- local_jcamp_write_record()
  output <- withr::local_tempdir()
  cases <- list(
    title_newline = list(value = "Title\n##END=", field = "title"),
    title_comment = list(value = "Title$$comment", field = "title"),
    origin_label = list(value = "Lab\n##END=", field = "origin"),
    owner_comment = list(value = "Owner\n$$comment", field = "owner"),
    title_non_ascii = list(value = "IR é", field = "title"),
    title_long = list(value = strrep("a", 72L), field = "title"),
    owner_long = list(
      value = paste0("Owner\n", strrep("a", 81L)),
      field = "owner"
    )
  )

  for (case_name in names(cases)) {
    case <- cases[[case_name]]
    path <- file.path(output, paste0(case$field, "-", case_name, ".jdx"))
    args <- list(
      record = record,
      path = path,
      curve_id = "curve-α",
      title = "Title",
      origin = "Origin",
      owner = "Owner"
    )
    args[[case$field]] <- case$value
    expect_error(do.call(measure_write_jcamp, args), regexp = case$field)
    expect_identical(file.exists(path), FALSE)
  }
})

test_that("writer preserves an existing destination", {
  record <- local_jcamp_write_record()
  path <- file.path(withr::local_tempdir(), "existing.jdx")
  writeBin(charToRaw("do not replace\n"), path)

  expect_error(
    measure_write_jcamp(
      record,
      path,
      "curve-α",
      "Title",
      "Origin",
      "Owner"
    ),
    "already exists"
  )
  expect_identical(readLines(path, warn = FALSE), "do not replace")
})

test_that("writer enforces the one-million-point limit before staging", {
  record <- local_jcamp_write_record()
  large <- new_measure_tbl(
    location = as.double(seq_len(1000001L)),
    value = rep(1, 1000001L)
  )
  record$curves$.measures[[1L]] <- large
  record$curves$.source_rows[[1L]] <- seq_len(1000001L)
  path <- file.path(withr::local_tempdir(), "too-large.jdx")

  expect_error(
    measure_write_jcamp(
      record,
      path,
      "curve-α",
      "Title",
      "Origin",
      "Owner"
    ),
    "1,000,000"
  )
  expect_identical(file.exists(path), FALSE)
})
