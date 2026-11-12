record_import_columns <- c(
  sample_id = "sample_id",
  injection_id = "injection_id",
  channel_id = "channel_id",
  curve_id = "curve_id",
  location = "location",
  value = "value",
  channel_name = "channel_name",
  axis_name = "axis_name",
  axis_unit = "axis_unit",
  value_name = "value_name",
  value_unit = "value_unit"
)

record_import_long_data <- function() {
  tibble::tibble(
    sample_id = c("0007", "sample-2", "0007", "0007"),
    injection_id = c("inj-1", "inj-2", "inj-1", "inj-1"),
    channel_id = c("detector-uv", "detector-uv", "detector-ms", "detector-uv"),
    curve_id = c("curve-a", "curve-b", "curve-c", "curve-a"),
    location = c(3, 0, 0.5, 1),
    value = c(1, -2, 0, 3),
    channel_name = c("UV α", "UV α", "-MS", "UV α"),
    axis_name = c(
      "retention time",
      "retention time",
      "scan axis",
      "retention time"
    ),
    axis_unit = c("min", "min", NA_character_, "min"),
    value_name = c("signal", "signal", "counts", "signal"),
    value_unit = c("AU", "AU", NA_character_, "AU")
  )
}

test_that("long-table import uses explicit identities and preserves point order", {
  data <- record_import_long_data()
  data$value[[1L]] <- NA_real_

  record <- measure_record_from_long(
    data,
    record_import_columns,
    source_id = "source-1"
  )

  expect_s3_class(record, "measure_record")
  expect_identical(record$samples$sample_id, c("0007", "sample-2"))
  expect_identical(record$injections$injection_id, c("inj-1", "inj-2"))
  expect_identical(record$injections$sample_id, c("0007", "sample-2"))
  expect_identical(record$channels$channel_id, c("detector-uv", "detector-ms"))
  expect_identical(record$channels$channel_name, c("UV α", "-MS"))
  expect_identical(record$curves$curve_id, c("curve-a", "curve-b", "curve-c"))
  expect_identical(record$curves$.source_rows, list(c(1L, 4L), 2L, 3L))
  expect_identical(record$curves$.measures[[1L]]$location, c(3, 1))
  expect_identical(record$curves$.measures[[1L]]$value, c(NA_real_, 3))
  expect_identical(record$curves$.measures[[2L]]$location, 0)
  expect_identical(record$curves$.measures[[3L]]$location, 0.5)
  expect_identical(record$sources$source_id, "source-1")
  expect_identical(record$sources$source_name, NA_character_)
  expect_identical(record$sources$source_sha256, NA_character_)
})

test_that("long-table import requires named, unique, explicit mappings", {
  data <- record_import_long_data()
  reused <- record_import_columns
  reused[["channel_id"]] <- "sample_id"
  missing <- record_import_columns[names(record_import_columns) != "value"]
  unknown <- c(record_import_columns, unexpected = "axis_unit")
  duplicate <- record_import_columns
  names(duplicate)[[2L]] <- names(duplicate)[[1L]]
  matrix_columns <- record_import_columns
  dim(matrix_columns) <- c(length(matrix_columns), 1L)
  names(matrix_columns) <- names(record_import_columns)
  class_columns <- structure(record_import_columns, class = "mapped_fields")

  expect_error(
    measure_record_from_long(data, reused, "src"),
    "cannot be mapped to more than one"
  )
  expect_error(
    measure_record_from_long(data, missing, "src"),
    "Missing required mapping.*value"
  )
  expect_error(
    measure_record_from_long(data, unknown, "src"),
    "Unknown canonical mapping"
  )
  expect_error(
    measure_record_from_long(data, duplicate, "src"),
    "unique, non-empty canonical"
  )
  expect_error(
    measure_record_from_long(data, unname(record_import_columns), "src"),
    "named character"
  )
  expect_error(
    measure_record_from_long(data, matrix_columns, "src"),
    "named character"
  )
  expect_error(
    measure_record_from_long(data, class_columns, "src"),
    "named character"
  )
  expect_error(
    measure_record_from_long(data, record_import_columns, " "),
    "nonblank character"
  )
  expect_error(
    measure_record_from_long(data, record_import_columns, NA_character_),
    "nonblank character"
  )

  data$extra <- "not mapped"
  expect_error(
    measure_record_from_long(data, record_import_columns, "src"),
    "Unmapped input columns"
  )
  expect_s3_class(
    measure_record_from_long(
      data,
      record_import_columns,
      "src",
      allow_extra_columns = TRUE
    ),
    "measure_record"
  )
})

test_that("public import errors explain ignored columns and malformed numeric fields", {
  data <- record_import_long_data()
  data$note <- "unmapped"
  expect_snapshot(
    error = TRUE,
    measure_record_from_long(data, record_import_columns, "source-1")
  )

  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  writeBin(
    charToRaw(paste0(
      "sample_id,injection_id,channel_id,curve_id,location,value\n",
      "sample-1,inj-1,uv,curve-1,1,2x\n"
    )),
    path
  )
  columns <- record_import_columns[
    names(record_import_columns) %in%
      c(
        "sample_id",
        "injection_id",
        "channel_id",
        "curve_id",
        "location",
        "value"
      )
  ]
  expect_snapshot(
    error = TRUE,
    read_measure_record_delim(path, columns, "source-1")
  )
})

test_that("quoted numeric cells do not hide a trailing newline", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  writeBin(
    charToRaw(paste0(
      "sample_id,injection_id,channel_id,curve_id,location,value\n",
      "sample-1,inj-1,uv,curve-1,1,\"2\n\"\n"
    )),
    path
  )
  columns <- record_import_columns[
    names(record_import_columns) %in%
      c(
        "sample_id",
        "injection_id",
        "channel_id",
        "curve_id",
        "location",
        "value"
      )
  ]

  expect_snapshot(
    error = TRUE,
    read_measure_record_delim(path, columns, "source-1")
  )
})

test_that("ignored extra columns still require strict UTF-8", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  columns <- record_import_columns[
    names(record_import_columns) %in%
      c(
        "sample_id",
        "injection_id",
        "channel_id",
        "curve_id",
        "location",
        "value"
      )
  ]
  writeBin(
    c(
      charToRaw(
        "sample_id,injection_id,channel_id,curve_id,location,value,note\n"
      ),
      charToRaw("sample-1,inj-1,uv,curve-1,1,2,"),
      as.raw(c(0xf4, 0x90, 0x80, 0x80)),
      charToRaw("\n")
    ),
    path
  )

  expect_snapshot(
    error = TRUE,
    read_measure_record_delim(
      path,
      columns,
      "source-1",
      allow_extra_columns = TRUE
    )
  )
})

test_that("long-table import rejects factor identifiers and labels", {
  data <- record_import_long_data()
  data$sample_id <- factor(data$sample_id)
  expect_error(
    measure_record_from_long(data, record_import_columns, "src"),
    "character vector"
  )

  data <- record_import_long_data()
  data$channel_name <- factor(data$channel_name)
  expect_error(
    measure_record_from_long(data, record_import_columns, "src"),
    "character vector"
  )
})

test_that("long-table import rejects conflicting entity declarations", {
  data <- record_import_long_data()
  data$sample_id[[4L]] <- "other-sample"
  expect_error(
    measure_record_from_long(data, record_import_columns, "src"),
    "Conflicting `sample_id`"
  )

  data <- record_import_long_data()
  data$axis_unit[[4L]] <- NA_character_
  expect_error(
    measure_record_from_long(data, record_import_columns, "src"),
    "missing-versus-known"
  )

  data <- record_import_long_data()
  data$channel_name[[4L]] <- "different label"
  expect_error(
    measure_record_from_long(data, record_import_columns, "src"),
    "Conflicting `channel_name`"
  )
})

test_that("long-table import delegates monotonicity and finite-value rules to the record constructor", {
  data <- record_import_long_data()
  data$location[[4L]] <- 3
  expect_error(
    measure_record_from_long(data, record_import_columns, "src"),
    "location|monotone|unique"
  )

  data <- record_import_long_data()
  data$location[[1L]] <- Inf
  expect_error(
    measure_record_from_long(data, record_import_columns, "src"),
    "location.*finite"
  )

  data <- record_import_long_data()
  data$value[[1L]] <- NaN
  expect_error(
    measure_record_from_long(data, record_import_columns, "src"),
    "value.*finite"
  )

  data <- record_import_long_data()
  data$axis_name[[1L]] <- ""
  expect_error(
    measure_record_from_long(data, record_import_columns, "src"),
    "axis_name|blank"
  )
})

test_that("CSV import retains identities, hashes exact bytes, and turns only blank metadata into missing", {
  path <- test_path("fixtures", "record", "interleaved.csv")
  raw <- readBin(path, what = "raw", n = file.info(path)$size)

  expect_error(
    read_measure_record_delim(
      path,
      record_import_columns,
      source_id = "source-1"
    ),
    "Unmapped input columns"
  )
  record <- read_measure_record_delim(
    path,
    record_import_columns,
    source_id = "source-1",
    allow_extra_columns = TRUE
  )

  expect_identical(record$samples$sample_id, c("0007", "NA"))
  expect_identical(record$channels$channel_name, c("UV α", "NA", "-MS"))
  expect_identical(record$curves$curve_id, c("curve-a", "curve-b", "curve-c"))
  expect_identical(record$curves$.source_rows, list(c(1L, 3L, 5L), 2L, 4L))
  expect_identical(record$curves$.measures[[1L]]$location, c(3, 2, 1))
  expect_identical(record$curves$.measures[[1L]]$value, c(1, NA_real_, 3))
  expect_identical(
    record$curves$axis_name,
    c("retention time", NA_character_, "scan axis")
  )
  expect_identical(
    record$curves$axis_unit,
    c("min", NA_character_, NA_character_)
  )
  expect_identical(record$sources$source_id, "source-1")
  expect_identical(record$sources$source_name, basename(path))
  expect_identical(
    record$sources$source_sha256,
    digest::digest(raw, algo = "sha256", serialize = FALSE)
  )
})

test_that("delimited import uses a declared TSV delimiter and strict numeric grammar", {
  path <- tempfile(fileext = ".tsv")
  on.exit(unlink(path), add = TRUE)
  writeBin(
    charToRaw(paste0(
      "sample_id\tinjection_id\tchannel_id\tcurve_id\tlocation\tvalue\taxis_unit\n",
      "0007\tinj-1\tuv\tcurve-1\t.5\t+0001.\tmin\n",
      "NA\tinj-2\tuv\tcurve-2\t1e3\tNA\t\n"
    )),
    path
  )
  columns <- c(
    sample_id = "sample_id",
    injection_id = "injection_id",
    channel_id = "channel_id",
    curve_id = "curve_id",
    location = "location",
    value = "value",
    axis_unit = "axis_unit"
  )

  record <- read_measure_record_delim(
    path,
    columns,
    "declared-tsv",
    delim = "\t"
  )
  expect_identical(record$samples$sample_id, c("0007", "NA"))
  expect_identical(record$curves$.measures[[1L]]$location, 0.5)
  expect_identical(record$curves$.measures[[1L]]$value, 1)
  expect_identical(record$curves$.measures[[2L]]$location, 1000)
  expect_identical(record$curves$.measures[[2L]]$value, NA_real_)
  expect_identical(record$curves$axis_unit, c("min", NA_character_))

  writeBin(
    charToRaw(paste0(
      "sample_id\tinjection_id\tchannel_id\tcurve_id\tlocation\tvalue\taxis_unit\n",
      "0007\tinj-1\tuv\tcurve-1\t1\t1x\tmin\n"
    )),
    path
  )
  expect_error(
    read_measure_record_delim(path, columns, "src", delim = "\t"),
    "field `value`.*row 1"
  )
})

test_that("header-only imports keep their source declaration", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  writeBin(
    charToRaw(paste0(names(record_import_columns), collapse = ",")),
    path
  )

  record <- read_measure_record_delim(
    path,
    record_import_columns,
    "empty-source"
  )
  expect_equal(nrow(record$samples), 0L)
  expect_equal(nrow(record$curves), 0L)
  expect_identical(record$sources$source_id, "empty-source")
  expect_identical(record$sources$source_name, basename(path))
})
