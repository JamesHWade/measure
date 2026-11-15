nd_io_test_tbl <- function() {
  subnormal <- .Machine$double.xmin * .Machine$double.eps
  location_1 <- c(
    NA_real_,
    NaN,
    Inf,
    -Inf,
    -0,
    0,
    .Machine$double.xmax,
    -.Machine$double.xmax,
    .Machine$double.xmin,
    subnormal,
    2 * subnormal,
    1 + .Machine$double.eps,
    1 - .Machine$double.eps / 2,
    pi
  )
  location_2 <- c(
    NA_integer_,
    1L,
    2L,
    3L,
    4L,
    4L,
    .Machine$integer.max,
    -2147483647L,
    8L,
    9L,
    10L,
    11L,
    12L,
    13L
  )
  value <- c(
    NA_real_,
    NaN,
    Inf,
    -Inf,
    -0,
    0,
    .Machine$double.xmax,
    -.Machine$double.xmax,
    .Machine$double.xmin,
    subnormal,
    2 * subnormal,
    1 + .Machine$double.eps,
    1 - .Machine$double.eps / 2,
    -pi
  )
  new_measure_nd_tbl(
    location_1 = location_1,
    location_2 = location_2,
    value = value,
    dim_names = c("time, \"raw\"\nsecond", "NA"),
    dim_units = c(" min ", "μm")
  )
}

nd_io_test_signature <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  if (typeof(x) == "integer") {
    return(list(type = "integer", values = x))
  }
  state <- rep("finite", length(x))
  state[is.na(x) & !is.nan(x)] <- "NA"
  state[is.nan(x)] <- "NaN"
  state[is.infinite(x) & x > 0] <- "Inf"
  state[is.infinite(x) & x < 0] <- "-Inf"
  finite <- is.finite(x)
  connection <- rawConnection(raw(), open = "wb")
  on.exit(close(connection), add = TRUE)
  writeBin(x[finite], connection, size = 8L, endian = "little")
  list(
    type = "double",
    state = state,
    finite_bits = rawConnectionValue(connection)
  )
}

nd_io_test_character <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  missing <- is.na(x)
  list(missing = missing, values = enc2utf8(x[!missing]))
}

nd_io_expect_tbl <- function(actual, expected) {
  expect_identical(class(actual), class(expected))
  expect_identical(names(actual), names(expected))
  expect_identical(nrow(actual), nrow(expected))
  expect_identical(
    attr(actual, "ndim", exact = TRUE),
    attr(expected, "ndim", exact = TRUE)
  )
  expect_identical(
    nd_io_test_character(attr(actual, "dim_names", exact = TRUE)),
    nd_io_test_character(attr(expected, "dim_names", exact = TRUE))
  )
  expect_identical(
    nd_io_test_character(attr(actual, "dim_units", exact = TRUE)),
    nd_io_test_character(attr(expected, "dim_units", exact = TRUE))
  )
  for (column in names(expected)) {
    expect_identical(
      nd_io_test_signature(actual[[column]]),
      nd_io_test_signature(expected[[column]])
    )
  }
}

nd_io_expect_object <- function(actual, expected) {
  if (is_measure_nd_tbl(expected)) {
    nd_io_expect_tbl(actual, expected)
    return(invisible(NULL))
  }
  expect_identical(class(actual), class(expected))
  expect_identical(length(actual), length(expected))
  expect_identical(
    nd_io_test_character(attr(actual, "names", exact = TRUE)),
    nd_io_test_character(attr(expected, "names", exact = TRUE))
  )
  nd_io_expect_tbl(
    attr(actual, "ptype", exact = TRUE),
    attr(expected, "ptype", exact = TRUE)
  )
  for (i in seq_len(length(expected))) {
    nd_io_expect_tbl(actual[[i]], expected[[i]])
  }
  invisible(NULL)
}

nd_io_read_raw <- function(path) {
  connection <- file(path, open = "rb")
  on.exit(close(connection), add = TRUE)
  readBin(connection, what = "raw", n = as.integer(file.info(path)$size))
}

nd_io_rehash <- function(bundle, table, delim = ",") {
  extension <- if (identical(delim, ",")) "csv" else "tsv"
  filename <- paste0(tolower(table), ".", extension)
  bytes <- nd_io_read_raw(file.path(bundle, filename))
  field <- paste0(table, "-SHA256")
  lines <- readLines(file.path(bundle, "manifest.dcf"), warn = FALSE)
  selected <- which(startsWith(lines, paste0(field, ": ")))
  stopifnot(length(selected) == 1L)
  lines[[selected]] <- paste0(
    field,
    ": ",
    digest::digest(bytes, algo = "sha256", serialize = FALSE)
  )
  writeLines(lines, file.path(bundle, "manifest.dcf"), useBytes = TRUE)
}

nd_io_write_table <- function(x, bundle, table, delim = ",") {
  extension <- if (identical(delim, ",")) "csv" else "tsv"
  .record_io_write_table(
    x,
    file.path(bundle, paste0(tolower(table), ".", extension)),
    delim
  )
  nd_io_rehash(bundle, table, delim)
}

nd_io_fixture_list <- function() {
  first <- new_measure_nd_tbl(
    location_1 = c(5L, 1L, 5L),
    location_2 = c(254, 280, 254),
    value = c(10, NA_real_, Inf),
    dim_names = c("retention time", "wavelength"),
    dim_units = c("min", "nm")
  )
  second <- new_measure_nd_tbl(
    location_1 = c(9.5, 2.25),
    location_2 = c(280L, 254L),
    value = c(3L, -2L),
    dim_names = c(NA_character_, "detector"),
    dim_units = NULL
  )
  new_measure_nd_list(list(first = first, second = second))
}

test_that("nD bundles roundtrip typed observations in CSV and TSV", {
  measurement <- nd_io_test_tbl()
  output <- withr::local_tempdir()

  for (delim in c(",", "\t")) {
    suffix <- if (identical(delim, ",")) "csv" else "tsv"
    bundle <- file.path(output, paste0("measurement-", suffix))
    write_measure_nd(measurement, bundle, delim = delim)
    restored <- read_measure_nd(bundle)

    nd_io_expect_object(restored, measurement)
    expect_setequal(
      list.files(bundle),
      c("manifest.dcf", paste0(c("members", "axes", "points"), ".", suffix))
    )
    manifest <- read.dcf(file.path(bundle, "manifest.dcf"))
    expect_identical(
      colnames(manifest),
      c(
        "Format",
        "Version",
        "Delimiter",
        "Object-Type",
        "Dimensions",
        "List-Names",
        "Members-Rows",
        "Members-Columns",
        "Members-SHA256",
        "Axes-Rows",
        "Axes-Columns",
        "Axes-SHA256",
        "Points-Rows",
        "Points-Columns",
        "Points-SHA256"
      )
    )
    expect_identical(unname(manifest[1L, "Format"]), "measure-nd")
    expect_identical(unname(manifest[1L, "Object-Type"]), "tbl")
    expect_identical(unname(manifest[1L, "Dimensions"]), "2")
  }
})

test_that("nD lists retain member order, names, grids, types, and metadata", {
  measurements <- nd_io_fixture_list()
  bundle <- file.path(withr::local_tempdir(), "bundle")
  write_measure_nd(measurements, bundle)
  restored <- read_measure_nd(bundle)

  nd_io_expect_object(restored, measurements)
  expect_type(restored[[1]]$location_1, "integer")
  expect_type(restored[[1]]$location_2, "double")
  expect_type(restored[[2]]$value, "integer")
  expect_identical(
    attr(restored[[2]], "dim_names"),
    c(NA_character_, "detector")
  )
  expect_null(attr(restored[[2]], "dim_units", exact = TRUE))
  expect_identical(
    readLines(file.path(bundle, "members.csv"), warn = FALSE)[[1L]],
    '"member_index","member_name","member_name_missing","dim_names_present","dim_units_present","value_type","point_count"'
  )
})

test_that("member name states and list names presence are distinct", {
  measurement <- new_measure_nd_tbl(
    location_1 = 1L,
    location_2 = 2L,
    value = 3L
  )
  measurements <- new_measure_nd_list(rep(list(measurement), 5L))
  attr(measurements, "names") <- c("", NA_character_, "same", "same", "NA")
  bundle <- file.path(withr::local_tempdir(), "named")
  write_measure_nd(measurements, bundle)
  restored <- read_measure_nd(bundle)
  expect_identical(
    attr(restored, "names", exact = TRUE),
    c("", NA_character_, "same", "same", "NA")
  )

  unnamed <- new_measure_nd_list(list(measurement))
  bundle_unnamed <- file.path(withr::local_tempdir(), "unnamed")
  write_measure_nd(unnamed, bundle_unnamed)
  restored_unnamed <- read_measure_nd(bundle_unnamed)
  expect_null(attr(restored_unnamed, "names", exact = TRUE))

  present_empty_names <- new_measure_nd_list(list(named = measurement))[0]
  attr(present_empty_names, "names") <- character()
  expect_identical(
    attr(present_empty_names, "names", exact = TRUE),
    character()
  )
  bundle_empty_names <- file.path(withr::local_tempdir(), "empty-names")
  write_measure_nd(present_empty_names, bundle_empty_names)
  restored_empty_names <- read_measure_nd(bundle_empty_names)
  expect_identical(
    attr(restored_empty_names, "names", exact = TRUE),
    character()
  )
})

test_that("nD bundles preserve NULL and all-unknown metadata and empty prototypes", {
  empty_tbl <- new_measure_nd_tbl(
    location_1 = integer(),
    location_2 = double(),
    location_3 = integer(),
    value = double(),
    dim_names = c("time", NA_character_, "NA"),
    dim_units = c(NA_character_, "nm", "nm")
  )
  empty_list <- new_measure_nd_list(list(empty_tbl))[0]
  rich_ptype <- new_measure_nd_tbl(
    location_1 = integer(),
    location_2 = double(),
    location_3 = integer(),
    value = integer(),
    dim_names = c(NA_character_, NA_character_, NA_character_),
    dim_units = NULL
  )
  attr(empty_list, "ptype") <- rich_ptype

  for (object in list(empty_tbl, empty_list, new_measure_nd_list())) {
    bundle <- file.path(withr::local_tempdir(), "empty")
    write_measure_nd(object, bundle)
    restored <- read_measure_nd(bundle)
    nd_io_expect_object(restored, object)
  }

  nonempty_list <- new_measure_nd_list(list(
    empty = empty_tbl,
    observed = new_measure_nd_tbl(
      location_1 = 1L,
      location_2 = 2,
      location_3 = 3L,
      value = 4,
      dim_names = c("time", NA_character_, "NA"),
      dim_units = c(NA_character_, "nm", "nm")
    )
  ))
  nonempty_bundle <- file.path(withr::local_tempdir(), "empty-member")
  write_measure_nd(nonempty_list, nonempty_bundle)
  nd_io_expect_object(read_measure_nd(nonempty_bundle), nonempty_list)

  null_metadata <- new_measure_nd_tbl(
    location_1 = double(),
    location_2 = integer(),
    value = double()
  )
  unknown_metadata <- new_measure_nd_tbl(
    location_1 = double(),
    location_2 = integer(),
    value = double(),
    dim_names = c(NA_character_, NA_character_),
    dim_units = c(NA_character_, NA_character_)
  )
  for (object in list(null_metadata, unknown_metadata)) {
    bundle <- file.path(withr::local_tempdir(), "metadata")
    write_measure_nd(object, bundle)
    nd_io_expect_object(read_measure_nd(bundle), object)
  }
})

test_that("valid member, axis, and point row reordering uses explicit keys", {
  original <- nd_io_fixture_list()
  bundle <- file.path(withr::local_tempdir(), "bundle")
  write_measure_nd(original, bundle)

  for (table in c("Members", "Axes", "Points")) {
    extension <- ".csv"
    path <- file.path(bundle, paste0(tolower(table), extension))
    parsed <- .record_parse_delim(nd_io_read_raw(path), ",")
    if (nrow(parsed) > 1L) {
      nd_io_write_table(
        parsed[rev(seq_len(nrow(parsed))), , drop = FALSE],
        bundle,
        table
      )
    }
  }

  nd_io_expect_object(read_measure_nd(bundle), original)
})

test_that("unsupported attributes fail before creating staging or output", {
  output <- withr::local_tempdir()
  destination <- file.path(output, "must-not-exist")
  objects <- list(
    extra_table_attribute = local({
      x <- nd_io_test_tbl()
      attr(x, "instrument") <- "unserialized"
      x
    }),
    subclass = local({
      x <- nd_io_test_tbl()
      class(x) <- c("custom_nd", class(x))
      x
    }),
    column_names = local({
      x <- nd_io_test_tbl()
      attr(x$location_1, "names") <- paste0("p", seq_along(x$location_1))
      x
    }),
    metadata_names = local({
      x <- nd_io_test_tbl()
      attr(x, "dim_names") <- structure(
        attr(x, "dim_names"),
        names = c("first", "second")
      )
      x
    }),
    custom_row_names = local({
      x <- nd_io_test_tbl()
      attr(x, "row.names") <- paste0("row", seq_len(nrow(x)))
      x
    }),
    list_attribute = local({
      x <- nd_io_fixture_list()
      attr(x, "instrument") <- "unserialized"
      x
    }),
    ptype_attribute = local({
      x <- new_measure_nd_list(list(new_measure_nd_tbl(
        location_1 = integer(),
        location_2 = double(),
        value = double()
      )))
      ptype <- attr(x, "ptype", exact = TRUE)
      attr(ptype$location_1, "names") <- character()
      attr(x, "ptype") <- ptype
      x
    }),
    names_vector_attributes = local({
      x <- new_measure_nd_list(list(
        member = new_measure_nd_tbl(
          location_1 = 1L,
          location_2 = 2L,
          value = 3L
        )
      ))
      attr(x, "names") <- structure("member", names = "name-of-name")
      x
    }),
    missing_ptype = local({
      x <- new_measure_nd_list()
      attr(x, "ptype") <- NULL
      x
    })
  )

  for (object in objects) {
    expect_error(
      write_measure_nd(object, destination),
      "attribute|class|row names|standard|ptype"
    )
    expect_false(file.exists(destination))
    expect_length(list.files(output, all.files = TRUE, no.. = TRUE), 0L)
  }
})

test_that("reader validates counts, descriptors, keys, and numeric types", {
  output <- withr::local_tempdir()
  make_bundle <- function() {
    path <- tempfile("bundle-", tmpdir = output)
    write_measure_nd(nd_io_fixture_list(), path)
    path
  }

  bundle <- make_bundle()
  lines <- readLines(file.path(bundle, "manifest.dcf"), warn = FALSE)
  lines[startsWith(lines, "Points-Rows: ")] <- "Points-Rows: 999"
  writeLines(lines, file.path(bundle, "manifest.dcf"), useBytes = TRUE)
  expect_error(read_measure_nd(bundle), "row count")

  bundle <- make_bundle()
  lines <- readLines(file.path(bundle, "manifest.dcf"), warn = FALSE)
  lines[startsWith(lines, "Points-Rows: ")] <- "Points-Rows: 2147483647"
  writeLines(lines, file.path(bundle, "manifest.dcf"), useBytes = TRUE)
  expect_error(read_measure_nd(bundle), "row count")

  bundle <- make_bundle()
  members <- .record_parse_delim(
    nd_io_read_raw(file.path(bundle, "members.csv")),
    ","
  )
  members$member_index[[3L]] <- "1"
  nd_io_write_table(members, bundle, "Members")
  expect_error(read_measure_nd(bundle), "contiguous")

  bundle <- make_bundle()
  members <- .record_parse_delim(
    nd_io_read_raw(file.path(bundle, "members.csv")),
    ","
  )
  members$point_count[[1L]] <- "1"
  nd_io_write_table(members, bundle, "Members")
  expect_error(read_measure_nd(bundle), "prototype descriptor")

  bundle <- make_bundle()
  axes <- .record_parse_delim(
    nd_io_read_raw(file.path(bundle, "axes.csv")),
    ","
  )
  axes$axis_index[[1L]] <- "3"
  nd_io_write_table(axes, bundle, "Axes")
  expect_error(read_measure_nd(bundle), "axis references")

  bundle <- make_bundle()
  members <- .record_parse_delim(
    nd_io_read_raw(file.path(bundle, "members.csv")),
    ","
  )
  members$dim_names_present[[2L]] <- "0"
  nd_io_write_table(members, bundle, "Members")
  expect_error(read_measure_nd(bundle), "Absent dim_names")

  bundle <- make_bundle()
  axes <- .record_parse_delim(
    nd_io_read_raw(file.path(bundle, "axes.csv")),
    ","
  )
  axes$axis_name[axes$member_index == "1"] <- "same"
  nd_io_write_table(axes, bundle, "Axes")
  expect_error(read_measure_nd(bundle), "unique")

  bundle <- make_bundle()
  points <- .record_parse_delim(
    nd_io_read_raw(file.path(bundle, "points.csv")),
    ","
  )
  points$point_index[[2L]] <- points$point_index[[1L]]
  nd_io_write_table(points, bundle, "Points")
  expect_error(read_measure_nd(bundle), "contiguous")

  bundle <- make_bundle()
  points <- .record_parse_delim(
    nd_io_read_raw(file.path(bundle, "points.csv")),
    ","
  )
  points$member_index[[1L]] <- "99"
  nd_io_write_table(points, bundle, "Points")
  expect_error(read_measure_nd(bundle), "orphan")

  bundle <- make_bundle()
  axes <- .record_parse_delim(
    nd_io_read_raw(file.path(bundle, "axes.csv")),
    ","
  )
  axes$storage_type[[1L]] <- "logical"
  nd_io_write_table(axes, bundle, "Axes")
  expect_error(read_measure_nd(bundle), "storage_type")

  bundle <- make_bundle()
  members <- .record_parse_delim(
    nd_io_read_raw(file.path(bundle, "members.csv")),
    ","
  )
  members$value_type[[2L]] <- "integer"
  nd_io_write_table(members, bundle, "Members")
  expect_error(read_measure_nd(bundle), "Invalid integer token")
})

test_that("reader rejects malformed manifests, headers, and payload hashes", {
  bundle <- file.path(withr::local_tempdir(), "bundle")
  write_measure_nd(nd_io_test_tbl(), bundle)
  manifest_path <- file.path(bundle, "manifest.dcf")
  original_manifest <- readLines(manifest_path, warn = FALSE)

  writeLines(c(original_manifest, "Unknown: field"), manifest_path)
  expect_error(read_measure_nd(bundle), "missing, duplicate, or unknown")

  writeLines(c(original_manifest, "Version: 1"), manifest_path)
  expect_error(read_measure_nd(bundle), "duplicate fields")

  writeLines(
    original_manifest[!startsWith(original_manifest, "Dimensions: ")],
    manifest_path
  )
  expect_error(read_measure_nd(bundle), "missing, duplicate, or unknown")

  writeLines(original_manifest, manifest_path)
  points_path <- file.path(bundle, "points.csv")
  points <- .record_parse_delim(nd_io_read_raw(points_path), ",")
  names(points)[[2L]] <- "bad_index"
  .record_io_write_table(points, points_path, ",")
  nd_io_rehash(bundle, "Points")
  expect_error(read_measure_nd(bundle), "unexpected columns")

  for (field in c("Format", "Version")) {
    bundle <- file.path(
      withr::local_tempdir(),
      paste0("bad-", tolower(field))
    )
    write_measure_nd(nd_io_test_tbl(), bundle)
    lines <- readLines(file.path(bundle, "manifest.dcf"), warn = FALSE)
    lines[startsWith(lines, paste0(field, ": "))] <- paste0(field, ": wrong")
    writeLines(lines, file.path(bundle, "manifest.dcf"))
    expect_error(read_measure_nd(bundle), "Unsupported")
  }

  for (replacement in c(255L, 0L)) {
    bundle <- file.path(
      withr::local_tempdir(),
      paste0("bad-byte-", replacement)
    )
    write_measure_nd(nd_io_test_tbl(), bundle)
    members_path <- file.path(bundle, "members.csv")
    bytes <- nd_io_read_raw(members_path)
    bytes[[3L]] <- as.raw(replacement)
    writeBin(bytes, members_path)
    nd_io_rehash(bundle, "Members")
    expected <- if (replacement == 255L) "valid UTF-8" else "NUL"
    expect_error(read_measure_nd(bundle), expected)
  }

  bundle <- file.path(withr::local_tempdir(), "hash")
  write_measure_nd(nd_io_test_tbl(), bundle)
  cat("change", file = file.path(bundle, "members.csv"), append = TRUE)
  expect_error(read_measure_nd(bundle), "hash")

  bundle <- file.path(withr::local_tempdir(), "wrong-dimensions")
  write_measure_nd(nd_io_test_tbl(), bundle)
  lines <- readLines(file.path(bundle, "manifest.dcf"), warn = FALSE)
  lines[startsWith(lines, "Dimensions: ")] <- "Dimensions: 1025"
  writeLines(lines, file.path(bundle, "manifest.dcf"))
  expect_error(read_measure_nd(bundle), "between 2 and 1024")
})

test_that("reader enforces cumulative byte limits and fixed member files", {
  bundle <- file.path(withr::local_tempdir(), "bundle")
  write_measure_nd(nd_io_test_tbl(), bundle)
  selected <- file.path(
    bundle,
    c("manifest.dcf", "members.csv", "axes.csv", "points.csv")
  )
  total <- sum(file.info(selected)$size)
  nd_io_expect_object(
    read_measure_nd(bundle, max_bytes = total),
    nd_io_test_tbl()
  )
  expect_error(read_measure_nd(bundle, max_bytes = total - 1L), "max_bytes")

  missing <- file.path(withr::local_tempdir(), "missing")
  write_measure_nd(nd_io_test_tbl(), missing)
  unlink(file.path(missing, "axes.csv"))
  expect_error(read_measure_nd(missing), "regular file")

  symlink_bundle <- file.path(withr::local_tempdir(), "symlink")
  write_measure_nd(nd_io_test_tbl(), symlink_bundle)
  axes <- file.path(symlink_bundle, "axes.csv")
  target <- tempfile("axes-target-")
  file.rename(axes, target)
  if (file.symlink(target, axes)) {
    expect_error(read_measure_nd(symlink_bundle), "symbolic links")
  } else {
    skip("This filesystem does not allow symbolic links.")
  }

  directory_member <- file.path(withr::local_tempdir(), "directory-member")
  write_measure_nd(nd_io_test_tbl(), directory_member)
  unlink(file.path(directory_member, "members.csv"))
  dir.create(file.path(directory_member, "members.csv"))
  expect_error(read_measure_nd(directory_member), "regular file")

  skip_if(.Platform$OS.type == "windows")
  fifo_command <- Sys.which("mkfifo")
  skip_if(!nzchar(fifo_command), "mkfifo is not available")
  fifo_bundle <- file.path(withr::local_tempdir(), "fifo-member")
  write_measure_nd(nd_io_test_tbl(), fifo_bundle)
  fifo_path <- file.path(fifo_bundle, "members.csv")
  unlink(fifo_path)
  expect_identical(system2(unname(fifo_command), shQuote(fifo_path)), 0L)
  expect_error(read_measure_nd(fifo_bundle), "regular file")
})

test_that("writer does not overwrite paths and cleans staging after failures", {
  output <- withr::local_tempdir()
  measurement <- nd_io_test_tbl()
  existing <- file.path(output, "existing")
  dir.create(existing)
  writeLines("keep", file.path(existing, "sentinel.txt"))
  expect_error(write_measure_nd(measurement, existing), "already exists")
  expect_identical(readLines(file.path(existing, "sentinel.txt")), "keep")

  existing_file <- file.path(output, "existing-file")
  writeLines("leave me", existing_file)
  expect_error(write_measure_nd(measurement, existing_file), "already exists")
  expect_identical(readLines(existing_file), "leave me")

  dangling <- file.path(output, "dangling")
  if (file.symlink(file.path(output, "missing-target"), dangling)) {
    expect_error(write_measure_nd(measurement, dangling), "already exists")
    expect_match(Sys.readlink(dangling), "missing-target")
  }

  destination <- file.path(output, "copy-failure")
  copy_file <- .record_io_copy_file
  local_mocked_bindings(.record_io_copy_file = function(from, to) {
    if (identical(basename(to), "axes.csv")) {
      stop("forced copy failure", call. = FALSE)
    }
    copy_file(from, to)
  })
  expect_error(
    write_measure_nd(measurement, destination),
    "forced copy failure"
  )
  expect_true(file.exists(file.path(destination, "members.csv")))
  expect_false(file.exists(file.path(destination, "manifest.dcf")))
  expect_false(any(grepl("-stage-", list.files(output, all.files = TRUE))))
})

test_that("nD and scalar record bundles reject cross-format reads", {
  nd_bundle <- file.path(withr::local_tempdir(), "nd")
  write_measure_nd(nd_io_test_tbl(), nd_bundle)
  expect_error(read_measure_record(nd_bundle), "missing, duplicate, or unknown")

  record_bundle <- file.path(withr::local_tempdir(), "record")
  write_measure_record(measure_record(), record_bundle)
  expect_error(read_measure_nd(record_bundle), "missing, duplicate, or unknown")
})
