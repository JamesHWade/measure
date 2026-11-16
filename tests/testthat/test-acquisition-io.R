acquisition_io_fixture <- function() {
  record <- measure_record(
    samples = tibble::tibble(sample_id = c("sample-1", "NA", "unused-sample")),
    injections = tibble::tibble(
      injection_id = c("injection-b", "injection-a"),
      sample_id = c("sample-1", "NA")
    ),
    channels = tibble::tibble(
      channel_id = c("uv", "ms", "nd", "unused-channel"),
      channel_name = c("UV", "MS", NA_character_, "Unused")
    ),
    sources = tibble::tibble(
      source_id = c("source-1", "source-unused"),
      source_name = c("instrument.csv", NA_character_),
      source_sha256 = c(NA_character_, paste(rep("a", 64), collapse = ""))
    ),
    curves = tibble::tibble(
      curve_id = c("curve-b", "curve-a"),
      injection_id = c("injection-b", "injection-a"),
      channel_id = c("uv", "uv"),
      source_id = c("source-1", NA_character_),
      axis_name = c("time", NA_character_),
      axis_unit = c("min", NA_character_),
      value_name = c("signal", "NA"),
      value_unit = c("AU", "AU"),
      .measures = list(
        new_measure_tbl(location = c(0, 1, 2), value = c(2, NA_real_, 4)),
        new_measure_tbl(location = c(5, 4), value = c(0, -3))
      ),
      .source_rows = list(c(1L, 2L, NA_integer_), c(8L, 10L))
    )
  )
  streams <- tibble::tibble(
    stream_id = c("ms-main", "nd-main", "ms-unused"),
    injection_id = c("injection-b", "injection-b", "injection-a"),
    channel_id = c("ms", "nd", "ms"),
    kind = c("ms", "nd", "ms"),
    source_id = c("source-1", "source-1", "source-1"),
    time_unit = c("min", NA_character_, "s"),
    mz_unit = c("m/z", NA_character_, "m/z"),
    value_name = c("intensity", "response", NA_character_),
    value_unit = c("counts", "AU", NA_character_)
  )
  scans <- tibble::tibble(
    scan_id = c("scan-7", "scan-11", "scan-19"),
    stream_id = c("ms-main", "ms-main", "ms-main"),
    scan_index = c(7L, 11L, 19L),
    scan_status = c("acquired", "acquired", "missing"),
    retention_time = c(-0, NA_real_, 12.5),
    polarity = c("positive", "negative", NA_character_),
    ms_level = c(1L, 2L, NA_integer_),
    spectrum_type = c("centroid", "profile", NA_character_),
    source_scan_id = c("NA", NA_character_, "source-scan-19"),
    .measures = list(
      new_measure_tbl(
        location = c(100L, 101L, 101L),
        value = c(-0, NaN, Inf)
      ),
      new_measure_tbl(location = double(), value = integer()),
      new_measure_tbl(location = double(), value = double())
    ),
    .source_rows = list(c(4L, 4L, NA_integer_), integer(), integer()),
    .saturated = list(c(NA, TRUE, FALSE), logical(), logical())
  )
  arrays <- tibble::tibble(
    array_id = c("array-main", "array-empty"),
    stream_id = c("nd-main", "nd-main"),
    .measures = list(
      new_measure_nd_tbl(
        location_1 = c(1L, 1L),
        location_2 = c(0, -0),
        value = c(NA_real_, Inf),
        dim_names = c("time", NA_character_),
        dim_units = NULL
      ),
      new_measure_nd_tbl(
        location_1 = integer(),
        location_2 = double(),
        value = integer(),
        dim_names = c(NA_character_, NA_character_),
        dim_units = c("nm", "nm")
      )
    ),
    .source_rows = list(c(NA_integer_, 5L), integer()),
    .saturated = list(c(FALSE, NA), logical())
  )
  measure_acquisition(
    record = record,
    streams = streams,
    scans = scans,
    arrays = arrays
  )
}

acquisition_io_read_raw <- function(path) {
  connection <- file(path, open = "rb")
  on.exit(close(connection), add = TRUE)
  readBin(connection, what = "raw", n = as.integer(file.info(path)$size))
}

acquisition_io_rehash <- function(bundle, table, delim = ",") {
  payload <- file.path(bundle, .acquisition_io_filename(table, delim))
  manifest_path <- file.path(bundle, "manifest.dcf")
  manifest <- .acquisition_io_parse_manifest(acquisition_io_read_raw(
    manifest_path
  ))
  manifest[[paste0(table, "-SHA256")]] <- digest::digest(
    acquisition_io_read_raw(payload),
    algo = "sha256",
    serialize = FALSE
  )
  .acquisition_io_write_manifest(manifest, manifest_path)
}

acquisition_io_rewrite_payload <- function(bundle, table, data, delim = ",") {
  path <- file.path(bundle, .acquisition_io_filename(table, delim))
  .record_io_write_table(data, path, delim)
  acquisition_io_rehash(bundle, table, delim)
}

test_that("acquisition bundles roundtrip raw data and metadata in CSV and TSV", {
  x <- acquisition_io_fixture()
  output <- withr::local_tempdir()

  for (delim in c(",", "\t")) {
    extension <- if (identical(delim, ",")) "csv" else "tsv"
    bundle <- file.path(output, paste0("acquisition-", extension))
    write_measure_acquisition(x, bundle, delim = delim)
    expect_identical(read_measure_acquisition(bundle), x)

    expected_files <- c(
      "manifest.dcf",
      paste0(
        c(
          "samples",
          "injections",
          "channels",
          "sources",
          "curves",
          "points",
          "streams",
          "scans",
          "scan_points",
          "arrays",
          "array_axes",
          "array_points"
        ),
        ".",
        extension
      )
    )
    expect_setequal(list.files(bundle), expected_files)
    manifest <- read.dcf(file.path(bundle, "manifest.dcf"))
    expected_manifest <- c(
      "Format",
      "Version",
      "Delimiter",
      unlist(
        lapply(names(.acquisition_io_table_specs), function(table) {
          paste0(table, c("-Rows", "-Columns", "-SHA256"))
        }),
        use.names = FALSE
      )
    )
    expect_identical(colnames(manifest), expected_manifest)
  }
})

test_that("metadata row indexes and owner point indexes restore physical row order", {
  x <- acquisition_io_fixture()
  bundle <- file.path(withr::local_tempdir(), "reordered")
  write_measure_acquisition(x, bundle)

  for (table in names(.acquisition_io_table_specs)) {
    payload <- file.path(bundle, .acquisition_io_filename(table, ","))
    data <- .record_parse_delim(acquisition_io_read_raw(payload), ",")
    if (nrow(data) > 1L) {
      data <- data[rev(seq_len(nrow(data))), , drop = FALSE]
      acquisition_io_rewrite_payload(bundle, table, data)
    }
  }
  expect_identical(read_measure_acquisition(bundle), x)
})

test_that("an acquisition with all-empty owned tables keeps typed empty schemas", {
  x <- measure_acquisition(record = measure_record())
  bundle <- file.path(withr::local_tempdir(), "empty-acquisition")
  write_measure_acquisition(x, bundle, delim = "\t")

  restored <- read_measure_acquisition(bundle)
  expect_identical(restored, x)
  expect_identical(typeof(restored$scans$scan_index), "integer")
  expect_identical(typeof(restored$scans$retention_time), "double")
  expect_identical(typeof(restored$scans$ms_level), "integer")
  expect_identical(names(restored$streams), .acq_schemas$streams)
  expect_identical(names(restored$scans), .acq_schemas$scans)
  expect_identical(names(restored$arrays), .acq_schemas$arrays)
})

test_that("unknown stream sources roundtrip with empty source metadata", {
  record <- measure_record(
    samples = tibble::tibble(sample_id = "sample-1"),
    injections = tibble::tibble(
      injection_id = "injection-1",
      sample_id = "sample-1"
    ),
    channels = tibble::tibble(channel_id = "ms")
  )
  x <- measure_acquisition(
    record = record,
    streams = tibble::tibble(
      stream_id = "stream-1",
      injection_id = "injection-1",
      channel_id = "ms",
      kind = "ms",
      source_id = NA_character_
    )
  )
  expect_identical(nrow(x$record$sources), 0L)
  expect_identical(x$streams$source_id, NA_character_)

  root <- withr::local_tempdir()
  for (delim in c(",", "\t")) {
    extension <- if (identical(delim, ",")) "csv" else "tsv"
    bundle <- file.path(root, paste0("unknown-source-", extension))
    write_measure_acquisition(x, bundle, delim = delim)
    expect_identical(read_measure_acquisition(bundle), x)
  }

  invalid <- file.path(root, "invalid-source")
  write_measure_acquisition(x, invalid)
  streams <- .record_parse_delim(
    acquisition_io_read_raw(file.path(invalid, "streams.csv")),
    ","
  )
  streams$source_id[[1L]] <- "missing-source"
  acquisition_io_rewrite_payload(invalid, "Streams", streams)
  expect_error(read_measure_acquisition(invalid), "known stream source")
})

test_that("all thirteen selected files share one actual-byte budget", {
  bundle <- file.path(withr::local_tempdir(), "bounded")
  write_measure_acquisition(acquisition_io_fixture(), bundle)
  selected <- file.path(
    bundle,
    c(
      "manifest.dcf",
      vapply(
        names(.acquisition_io_table_specs),
        .acquisition_io_filename,
        character(1),
        delim = ","
      )
    )
  )
  total <- sum(file.info(selected)$size)
  expect_identical(
    read_measure_acquisition(bundle, max_bytes = total),
    acquisition_io_fixture()
  )
  expect_error(
    read_measure_acquisition(bundle, max_bytes = total - 1),
    "max_bytes"
  )
})

test_that("bundle hashes, schemas, keys, and descriptor counts are checked", {
  bundle <- file.path(withr::local_tempdir(), "corrupt")
  write_measure_acquisition(acquisition_io_fixture(), bundle)
  connection <- file(file.path(bundle, "scans.csv"), open = "ab")
  writeBin(charToRaw("\n"), connection)
  close(connection)
  expect_error(read_measure_acquisition(bundle), "hash")

  keyed <- file.path(withr::local_tempdir(), "bad-key")
  write_measure_acquisition(acquisition_io_fixture(), keyed)
  scan_points <- .record_parse_delim(
    acquisition_io_read_raw(file.path(keyed, "scan_points.csv")),
    ","
  )
  scan_points$point_index[2L] <- scan_points$point_index[1L]
  acquisition_io_rewrite_payload(keyed, "ScanPoints", scan_points)
  expect_error(read_measure_acquisition(keyed), "point_index")

  counted <- file.path(withr::local_tempdir(), "bad-count")
  write_measure_acquisition(acquisition_io_fixture(), counted)
  manifest_path <- file.path(counted, "manifest.dcf")
  manifest <- .acquisition_io_parse_manifest(acquisition_io_read_raw(
    manifest_path
  ))
  manifest[["Scans-Rows"]] <- manifest[["Scans-Rows"]] + 1L
  .acquisition_io_write_manifest(manifest, manifest_path)
  expect_error(read_measure_acquisition(counted), "row count")

  versioned <- file.path(withr::local_tempdir(), "unsupported-version")
  write_measure_acquisition(acquisition_io_fixture(), versioned)
  version_manifest <- .acquisition_io_parse_manifest(
    acquisition_io_read_raw(file.path(versioned, "manifest.dcf"))
  )
  version_manifest[["Version"]] <- "2"
  .acquisition_io_write_manifest(
    version_manifest,
    file.path(versioned, "manifest.dcf")
  )
  expect_error(
    read_measure_acquisition(versioned),
    "Unsupported measure-acquisition"
  )
})

test_that("attributes and existing destinations reject before publication", {
  x <- acquisition_io_fixture()
  attr(x$scans$.measures[[1L]]$location, "unsupported") <- "extra"
  destination <- file.path(withr::local_tempdir(), "must-not-exist")
  expect_error(
    write_measure_acquisition(x, destination),
    "attribute|unsupported"
  )
  expect_false(file.exists(destination))

  existing <- file.path(withr::local_tempdir(), "existing")
  dir.create(existing)
  expect_error(
    write_measure_acquisition(acquisition_io_fixture(), existing),
    "already exists"
  )
})

test_that("selected symlinks reject and unselected entries are ignored", {
  bundle <- file.path(withr::local_tempdir(), "filesystem")
  write_measure_acquisition(acquisition_io_fixture(), bundle)
  writeLines("not selected", file.path(bundle, "extra.txt"))
  expect_identical(read_measure_acquisition(bundle), acquisition_io_fixture())

  original <- file.path(bundle, "samples.csv")
  backup <- file.path(bundle, "samples-held.csv")
  expect_true(file.rename(original, backup))
  if (!isTRUE(file.symlink(backup, original))) {
    skip("Could not create a symlink on this filesystem.")
  }
  expect_error(read_measure_acquisition(bundle), "symbolic links")
})

test_that("identifier text containing braces stays literal in IO errors", {
  x <- acquisition_io_fixture()
  marker <- file.path(withr::local_tempdir(), "expression-ran")
  literal_id <- paste0('{system("touch ', marker, '")}')
  x$scans$scan_id[[1L]] <- literal_id
  bundle <- file.path(withr::local_tempdir(), "literal-id")
  write_measure_acquisition(x, bundle)

  scan_points <- .record_parse_delim(
    acquisition_io_read_raw(file.path(bundle, "scan_points.csv")),
    ","
  )
  selected <- which(scan_points$scan_id == literal_id)[[1L]]
  scan_points$location[[selected]] <- "not-numeric"
  acquisition_io_rewrite_payload(bundle, "ScanPoints", scan_points)

  expect_error(
    read_measure_acquisition(bundle),
    "Invalid (numeric|integer) token"
  )
  expect_false(file.exists(marker))
})

test_that("acquisition, scalar record, and nD readers reject other profiles", {
  bundle <- file.path(withr::local_tempdir(), "acquisition")
  write_measure_acquisition(acquisition_io_fixture(), bundle)
  expect_error(read_measure_record(bundle), "missing, duplicate, or unknown")
  expect_error(read_measure_nd(bundle), "missing, duplicate, or unknown")

  scalar <- file.path(withr::local_tempdir(), "record")
  write_measure_record(acquisition_io_fixture()$record, scalar)
  expect_error(
    read_measure_acquisition(scalar),
    "missing, duplicate, or unknown"
  )

  nd <- file.path(withr::local_tempdir(), "nd")
  write_measure_nd(
    new_measure_nd_tbl(location_1 = 1, location_2 = 2, value = 3),
    nd
  )
  expect_error(read_measure_acquisition(nd), "missing, duplicate, or unknown")
})
