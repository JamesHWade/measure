record_io_fixture <- function() {
  min_subnormal <- .Machine$double.xmin * .Machine$double.eps
  measure_record(
    samples = tibble::tibble(
      sample_id = c("0007", "NA", "unused-sample")
    ),
    injections = tibble::tibble(
      injection_id = c("injection-0007", "injection-2", "unused-injection"),
      sample_id = c("0007", "0007", "unused-sample")
    ),
    channels = tibble::tibble(
      channel_id = c("uv", "ms", "unused-channel"),
      channel_name = c("Detector, \"A\"\r\nsecond line", "-MS", NA_character_)
    ),
    curves = tibble::tibble(
      curve_id = c("0007", "NA", "curve-ms"),
      injection_id = c("injection-0007", "injection-2", "injection-2"),
      channel_id = c("uv", "uv", "ms"),
      source_id = c("0007", "NA", NA_character_),
      axis_name = c("time,\nby minute", "NA", NA_character_),
      axis_unit = c("unknown-unit", "min", NA_character_),
      value_name = c("signal", "NA", NA_character_),
      value_unit = c("AU", "AU", NA_character_),
      .measures = list(
        new_measure_tbl(
          location = c(1, 1 + .Machine$double.eps, 3),
          value = c(pi, NA_real_, -.Machine$double.xmax)
        ),
        new_measure_tbl(
          location = 5,
          value = min_subnormal
        ),
        new_measure_tbl(
          location = c(9, 7, 4, 2, 1),
          value = c(
            0,
            -pi,
            1 + .Machine$double.eps,
            .Machine$double.xmax,
            .Machine$double.xmin
          )
        )
      ),
      .source_rows = list(
        c(1L, 4L, 8L),
        2L,
        c(3L, 5L, 7L, 9L, 11L)
      )
    ),
    sources = tibble::tibble(
      source_id = c("0007", "NA", "unused-source"),
      source_name = c("raw,é λ 😀\"file\"\nsecond line", "NA", NA_character_),
      source_sha256 = c(
        paste(rep("a", 64), collapse = ""),
        NA_character_,
        NA_character_
      )
    )
  )
}

record_io_read_raw <- function(path) {
  connection <- file(path, open = "rb")
  on.exit(close(connection), add = TRUE)
  readBin(connection, what = "raw", n = as.integer(file.info(path)$size))
}

record_io_write_table <- function(x, path, delim) {
  columns <- lapply(x, .record_io_quote_fields)
  header <- paste(.record_io_quote_fields(names(x)), collapse = delim)
  rows <- if (nrow(x) == 0L) {
    character()
  } else {
    vapply(
      seq_len(nrow(x)),
      function(row) {
        paste(
          vapply(columns, function(column) column[[row]], character(1)),
          collapse = delim
        )
      },
      character(1),
      USE.NAMES = FALSE
    )
  }
  connection <- file(path, open = "wb")
  on.exit(close(connection), add = TRUE)
  writeLines(c(header, rows), connection, sep = "\n", useBytes = TRUE)
}

record_io_rehash <- function(bundle, table, delim = ",") {
  extension <- if (identical(delim, ",")) "csv" else "tsv"
  filename <- paste0(tolower(table), ".", extension)
  bytes <- record_io_read_raw(file.path(bundle, filename))
  manifest_path <- file.path(bundle, "manifest.dcf")
  field <- paste0(table, "-SHA256")
  lines <- readLines(manifest_path, warn = FALSE)
  selected <- grep(paste0("^", field, ": "), lines)
  stopifnot(length(selected) == 1L)
  lines[[selected]] <- paste0(
    field,
    ": ",
    digest::digest(bytes, algo = "sha256", serialize = FALSE)
  )
  writeLines(lines, manifest_path, useBytes = TRUE)
}

record_io_rewrite_payload <- function(bundle, table, data, delim = ",") {
  extension <- if (identical(delim, ",")) "csv" else "tsv"
  path <- file.path(bundle, paste0(tolower(table), ".", extension))
  record_io_write_table(data, path, delim)
  record_io_rehash(bundle, table, delim)
}

test_that("bundle roundtrips preserve tables, points, and text in CSV and TSV", {
  record <- record_io_fixture()
  output <- withr::local_tempdir()

  for (delim in c(",", "\t")) {
    suffix <- if (identical(delim, ",")) "csv" else "tsv"
    bundle <- file.path(output, paste0("bundle-", suffix))
    write_measure_record(record, bundle, delim = delim)

    expect_identical(read_measure_record(bundle), record)
    expect_setequal(
      list.files(bundle),
      c(
        "manifest.dcf",
        paste0(
          c("sources", "samples", "injections", "channels", "curves", "points"),
          ".",
          suffix
        )
      )
    )
    manifest <- read.dcf(file.path(bundle, "manifest.dcf"))
    expect_identical(
      colnames(manifest),
      c(
        "Format",
        "Version",
        "Delimiter",
        "Sources-Rows",
        "Sources-Columns",
        "Sources-SHA256",
        "Samples-Rows",
        "Samples-Columns",
        "Samples-SHA256",
        "Injections-Rows",
        "Injections-Columns",
        "Injections-SHA256",
        "Channels-Rows",
        "Channels-Columns",
        "Channels-SHA256",
        "Curves-Rows",
        "Curves-Columns",
        "Curves-SHA256",
        "Points-Rows",
        "Points-Columns",
        "Points-SHA256"
      )
    )

    # Manifest fields form one fixed set; field order does not select files.
    manifest_lines <- readLines(file.path(bundle, "manifest.dcf"), warn = FALSE)
    writeLines(
      rev(manifest_lines),
      file.path(bundle, "manifest.dcf"),
      useBytes = TRUE
    )
    expect_identical(read_measure_record(bundle), record)
  }
})

test_that("point row order is restored from each curve's point_index", {
  record <- record_io_fixture()
  bundle <- file.path(withr::local_tempdir(), "bundle")
  write_measure_record(record, bundle)
  points <- .record_parse_delim(
    record_io_read_raw(file.path(bundle, "points.csv")),
    ","
  )
  points <- points[rev(seq_len(nrow(points))), ]
  points$value[
    points$curve_id == "0007" & points$point_index == "2"
  ] <- "NA"
  record_io_rewrite_payload(bundle, "Points", points)

  expect_identical(read_measure_record(bundle), record)
})

test_that("writer preserves UTF-8 text under a non-UTF-8 character locale", {
  record <- record_io_fixture()
  original_locale <- Sys.getlocale("LC_CTYPE")
  on.exit(
    suppressWarnings(Sys.setlocale("LC_CTYPE", original_locale)),
    add = TRUE
  )
  selected_locale <- suppressWarnings(Sys.setlocale("LC_CTYPE", "C"))
  if (is.na(selected_locale) || !nzchar(selected_locale)) {
    skip("A C character locale is unavailable.")
  }

  bundle <- file.path(withr::local_tempdir(), "bundle")
  write_measure_record(record, bundle)
  restored <- read_measure_record(bundle)
  suppressWarnings(Sys.setlocale("LC_CTYPE", original_locale))
  expect_identical(restored, record)
})

test_that("empty records produce readable fixed tables in both formats", {
  record <- measure_record()
  output <- withr::local_tempdir()

  for (delim in c(",", "\t")) {
    bundle <- file.path(output, if (delim == ",") "empty-csv" else "empty-tsv")
    write_measure_record(record, bundle, delim = delim)
    expect_identical(read_measure_record(bundle), record)
  }
})

test_that("bundle reads enforce total byte bounds and payload hashes", {
  bundle <- file.path(withr::local_tempdir(), "bundle")
  write_measure_record(record_io_fixture(), bundle)
  files <- file.path(
    bundle,
    c(
      "manifest.dcf",
      "sources.csv",
      "samples.csv",
      "injections.csv",
      "channels.csv",
      "curves.csv",
      "points.csv"
    )
  )
  total <- sum(file.info(files)$size)
  expect_identical(
    read_measure_record(bundle, max_bytes = total),
    record_io_fixture()
  )
  expect_error(
    read_measure_record(bundle, max_bytes = total - 1),
    "max_bytes"
  )

  connection <- file(file.path(bundle, "samples.csv"), open = "ab")
  writeBin(charToRaw("\n"), connection)
  close(connection)
  expect_error(read_measure_record(bundle), "hash")
})

test_that("unsupported manifest versions and noncanonical counts are rejected", {
  record <- record_io_fixture()
  template <- withr::local_tempdir()

  unsupported <- file.path(template, "unsupported")
  write_measure_record(record, unsupported)
  manifest <- file.path(unsupported, "manifest.dcf")
  lines <- readLines(manifest, warn = FALSE)
  lines[grepl("^Version: ", lines)] <- "Version: 2"
  writeLines(lines, manifest, useBytes = TRUE)
  expect_error(read_measure_record(unsupported), "Unsupported")

  bad_count <- file.path(template, "bad-count")
  write_measure_record(record, bad_count)
  manifest <- file.path(bad_count, "manifest.dcf")
  lines <- readLines(manifest, warn = FALSE)
  lines[grepl("^Samples-Rows: ", lines)] <- "Samples-Rows: 00"
  writeLines(lines, manifest, useBytes = TRUE)
  expect_error(read_measure_record(bad_count), "integer")
})

test_that("manifest and point structure are checked after valid rehashing", {
  template <- withr::local_tempdir()
  source <- file.path(template, "source")
  write_measure_record(record_io_fixture(), source)

  cases <- c("header", "index", "orphan")
  for (case in cases) {
    bundle <- file.path(template, case)
    expect_true(dir.create(bundle))
    expect_true(all(file.copy(
      file.path(source, list.files(source)),
      bundle
    )))
    if (case == "header") {
      curves <- .record_parse_delim(
        record_io_read_raw(file.path(bundle, "curves.csv")),
        ","
      )
      names(curves) <- rev(names(curves))
      record_io_rewrite_payload(bundle, "Curves", curves)
      expect_error(read_measure_record(bundle), "columns")
    } else {
      points <- .record_parse_delim(
        record_io_read_raw(file.path(bundle, "points.csv")),
        ","
      )
      if (case == "index") {
        points$point_index[[1]] <- "2"
        message <- "indices"
      } else {
        points$curve_id[[1]] <- "orphan"
        message <- "orphan"
      }
      record_io_rewrite_payload(bundle, "Points", points)
      expect_error(read_measure_record(bundle), message)
    }
  }
})

test_that("bundle members cannot be symbolic links", {
  skip_if(Sys.info()[["sysname"]] == "Windows")
  bundle <- file.path(withr::local_tempdir(), "bundle")
  write_measure_record(record_io_fixture(), bundle)
  samples <- file.path(bundle, "samples.csv")
  target <- file.path(bundle, "channels.csv")
  expect_true(file.remove(samples))
  if (!file.symlink(target, samples)) {
    skip("This filesystem does not allow symbolic links.")
  }
  expect_error(read_measure_record(bundle), "symbolic links")
})

test_that("writer preserves existing destination files and directories", {
  output <- withr::local_tempdir()
  record <- record_io_fixture()
  existing <- file.path(output, "existing")
  expect_true(dir.create(existing))
  sentinel <- file.path(existing, "keep.txt")
  writeLines("keep", sentinel)
  expect_error(
    write_measure_record(record, existing),
    "already exists"
  )
  expect_identical(readLines(sentinel), "keep")

  existing_file <- file.path(output, "existing-file")
  writeLines("leave me", existing_file)
  expect_error(
    write_measure_record(record, existing_file),
    "already exists"
  )
  expect_identical(readLines(existing_file), "leave me")
})

test_that("writer preserves dangling symbolic link destinations", {
  skip_if(Sys.info()[["sysname"]] == "Windows")
  output <- withr::local_tempdir()
  record <- record_io_fixture()
  dangling <- file.path(output, "dangling")
  if (!file.symlink(file.path(output, "missing-target"), dangling)) {
    skip("This filesystem does not allow symbolic links.")
  }
  expect_error(
    write_measure_record(record, dangling),
    "already exists"
  )
  expect_match(Sys.readlink(dangling), "missing-target")
})

test_that("writer leaves raced destinations untouched and commits manifest last", {
  output <- withr::local_tempdir()
  record <- record_io_fixture()
  raced_destination <- file.path(output, "raced")
  local_mocked_bindings(.record_io_reserve_destination = function(path) {
    base::dir.create(path)
    writeLines("other writer", file.path(path, "owner.txt"))
    FALSE
  })
  expect_error(
    write_measure_record(record, raced_destination),
    "appeared"
  )
  expect_identical(
    readLines(file.path(raced_destination, "owner.txt")),
    "other writer"
  )
  expect_false(file.exists(file.path(raced_destination, "manifest.dcf")))
})

test_that("writer rejects a member collision during fixed-file copying", {
  output <- withr::local_tempdir()
  record <- record_io_fixture()
  destination <- file.path(output, "collision")
  local_mocked_bindings(.record_io_reserve_destination = function(path) {
    base::dir.create(path)
    base::dir.create(file.path(path, "sources.csv"))
    TRUE
  })
  expect_error(
    write_measure_record(record, destination),
    "appeared during writing"
  )
  expect_true(dir.exists(file.path(destination, "sources.csv")))
  expect_false(file.exists(file.path(destination, "manifest.dcf")))
})

test_that("failed staged copies leave an incomplete bundle without a manifest", {
  output <- withr::local_tempdir()
  record <- record_io_fixture()
  destination <- file.path(output, "partial")
  copy_file <- .record_io_copy_file
  local_mocked_bindings(.record_io_copy_file = function(from, to) {
    if (identical(basename(to), "samples.csv")) {
      stop("forced copy failure", call. = FALSE)
    }
    copy_file(from, to)
  })
  expect_error(
    write_measure_record(record, destination),
    "forced copy failure"
  )
  expect_true(file.exists(file.path(destination, "sources.csv")))
  expect_false(file.exists(file.path(destination, "manifest.dcf")))
  expect_false(any(grepl("-stage-", list.files(output, all.files = TRUE))))
})


test_that("bundle member validation rejects named pipes without opening them", {
  skip_if(.Platform$OS.type == "windows")
  fifo_command <- Sys.which("mkfifo")
  skip_if(!nzchar(fifo_command), "mkfifo is not available")
  fifo_path <- file.path(withr::local_tempdir(), "manifest.dcf")

  expect_identical(system2(unname(fifo_command), shQuote(fifo_path)), 0L)
  expect_error(.record_io_check_member(fifo_path), "existing regular file")
})
