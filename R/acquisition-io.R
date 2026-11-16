# Native flat-file interchange for linked acquisition data.

.acquisition_io_table_specs <- list(
  Samples = c("row_index", "sample_id"),
  Injections = c("row_index", "injection_id", "sample_id"),
  Channels = c("row_index", "channel_id", "channel_name"),
  Sources = c("row_index", "source_id", "source_name", "source_sha256"),
  Curves = c(
    "row_index",
    "curve_id",
    "injection_id",
    "channel_id",
    "source_id",
    "axis_name",
    "axis_unit",
    "value_name",
    "value_unit"
  ),
  Points = c("curve_id", "point_index", "location", "value", "source_row"),
  Streams = c(
    "row_index",
    "stream_id",
    "injection_id",
    "channel_id",
    "kind",
    "source_id",
    "time_unit",
    "mz_unit",
    "value_name",
    "value_unit"
  ),
  Scans = c(
    "row_index",
    "scan_id",
    "stream_id",
    "scan_index",
    "scan_status",
    "retention_time",
    "polarity",
    "ms_level",
    "spectrum_type",
    "source_scan_id",
    "location_type",
    "value_type",
    "point_count"
  ),
  ScanPoints = c(
    "scan_id",
    "point_index",
    "location",
    "value",
    "source_row",
    "saturated"
  ),
  Arrays = c(
    "row_index",
    "array_id",
    "stream_id",
    "dim_names_present",
    "dim_units_present",
    "value_type",
    "point_count"
  ),
  ArrayAxes = c(
    "array_id",
    "axis_index",
    "storage_type",
    "axis_name",
    "axis_unit"
  ),
  ArrayPoints = c(
    "array_id",
    "point_index",
    "location_1",
    "location_2",
    "value",
    "source_row",
    "saturated"
  )
)

.acquisition_io_manifest_fields <- c(
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

#' Read and write native measurement acquisition bundles
#'
#' Acquisition bundles use one fixed DCF manifest and twelve flat CSV or TSV
#' payloads. The profile retains linked scalar record metadata, raw MS scans,
#' and two-dimensional measurement arrays. It does not infer missing scans,
#' units, interpretation, or source identity.
#'
#' @param x A validated `measure_acquisition` object.
#' @param path For writing, a new local directory path whose parent exists. For
#'   reading, a directory containing a complete acquisition bundle.
#' @param delim A comma or tab field delimiter used for all payloads.
#' @param max_bytes A positive whole-number limit for actual cumulative bytes
#'   read from the manifest and twelve selected payloads.
#'
#' @return `write_measure_acquisition()` invisibly returns the normalized
#'   destination path. `read_measure_acquisition()` returns a validated
#'   `measure_acquisition` object.
#'
#' @section Bundle version 1:
#' The selected files and exact ordered headers are specified in
#' `inst/specs/measure-acquisition-v1.md`. All selected members have fixed
#' names; manifest paths are never interpreted. Payload hashes cover exact
#' bytes and detect changes, but do not authenticate a source.
#'
#' The writer validates supported object attributes before staging, reads back
#' the staged bundle and checks fidelity, then reserves a new destination and
#' publishes the manifest last. Existing paths are never replaced.
#'
#' @name measure-acquisition-bundle
NULL

#' @rdname measure-acquisition-bundle
#' @export
write_measure_acquisition <- function(x, path, delim = c(",", "\t")) {
  delim <- match.arg(delim)
  validate_measure_acquisition(x)

  destination <- .record_io_destination(path)
  if (.record_io_path_exists(destination)) {
    stop("path already exists.", call. = FALSE)
  }

  stage <- .record_io_make_stage(dirname(destination), basename(destination))
  on.exit(unlink(stage, recursive = TRUE, force = TRUE), add = TRUE)

  prepared <- .acquisition_io_prepare_tables(x)
  hashes <- character(length(.acquisition_io_table_specs))
  names(hashes) <- names(.acquisition_io_table_specs)
  for (table in names(.acquisition_io_table_specs)) {
    staged_file <- file.path(stage, .acquisition_io_filename(table, delim))
    .record_io_write_table(prepared[[table]], staged_file, delim)
    bytes <- .record_io_staged_bytes(staged_file)
    hashes[[table]] <- digest::digest(bytes, algo = "sha256", serialize = FALSE)
  }

  manifest <- .acquisition_io_make_manifest(prepared, hashes, delim)
  manifest_stage <- file.path(stage, "manifest.dcf")
  .acquisition_io_write_manifest(manifest, manifest_stage)

  staged_paths <- file.path(
    stage,
    c(
      "manifest.dcf",
      vapply(
        names(.acquisition_io_table_specs),
        .acquisition_io_filename,
        character(1),
        delim = delim
      )
    )
  )
  staged_bytes <- sum(file.info(staged_paths)$size)
  if (!is.finite(staged_bytes) || staged_bytes > .Machine$integer.max - 1) {
    stop(
      "Staged acquisition bundle is too large to verify safely.",
      call. = FALSE
    )
  }
  staged <- read_measure_acquisition(stage, max_bytes = max(1, staged_bytes))
  if (!.acquisition_io_fidelity_equal(staged, x)) {
    stop(
      "Staged acquisition bundle did not preserve the supplied object.",
      call. = FALSE
    )
  }

  if (!.record_io_reserve_destination(destination)) {
    stop(
      "Could not reserve the destination directory; it may have appeared during writing.",
      call. = FALSE
    )
  }
  for (table in names(.acquisition_io_table_specs)) {
    .record_io_copy_file(
      file.path(stage, .acquisition_io_filename(table, delim)),
      file.path(destination, .acquisition_io_filename(table, delim))
    )
  }
  .record_io_copy_file(manifest_stage, file.path(destination, "manifest.dcf"))
  invisible(destination)
}

#' @rdname measure-acquisition-bundle
#' @export
read_measure_acquisition <- function(path, max_bytes = 100 * 1024^2) {
  max_bytes <- .record_io_max_bytes(max_bytes)
  directory <- .record_io_read_directory(path)
  manifest_path <- file.path(directory, "manifest.dcf")
  .record_io_check_member(manifest_path)
  manifest_raw <- .record_read_raw(manifest_path, max_bytes)
  remaining <- max_bytes - length(manifest_raw)
  manifest <- .acquisition_io_parse_manifest(manifest_raw)
  delim <- if (identical(manifest[["Delimiter"]], "csv")) "," else "\t"

  raw_tables <- vector("list", length(.acquisition_io_table_specs))
  names(raw_tables) <- names(.acquisition_io_table_specs)
  for (table in names(raw_tables)) {
    member <- file.path(directory, .acquisition_io_filename(table, delim))
    .record_io_check_member(member)
    bytes <- .acquisition_io_read_cumulative_member(member, remaining)
    remaining <- remaining - length(bytes)
    actual_hash <- digest::digest(bytes, algo = "sha256", serialize = FALSE)
    if (!identical(actual_hash, manifest[[paste0(table, "-SHA256")]])) {
      stop(
        "Bundle payload hash does not match the manifest for ",
        .acquisition_io_filename(table, delim),
        ".",
        call. = FALSE
      )
    }
    raw_tables[[table]] <- bytes
  }

  tables <- lapply(names(raw_tables), function(table) {
    .acquisition_io_parse_payload(raw_tables[[table]], table, delim, manifest)
  })
  names(tables) <- names(raw_tables)
  decoded <- .acquisition_io_decode_tables(tables)
  .acquisition_io_build_object(decoded)
}

.acquisition_io_filename <- function(table, delim) {
  extension <- if (identical(delim, ",")) "csv" else "tsv"
  stem <- switch(
    table,
    ScanPoints = "scan_points",
    ArrayAxes = "array_axes",
    ArrayPoints = "array_points",
    tolower(table)
  )
  paste0(stem, ".", extension)
}

.acquisition_io_add_row_index <- function(x) {
  columns <- c(
    list(row_index = as.character(seq_len(nrow(x)))),
    lapply(names(x), function(column) x[[column]])
  )
  names(columns) <- c("row_index", names(x))
  tibble::as_tibble(columns, .name_repair = "minimal")
}

.acquisition_io_nullable_text <- function(x) {
  out <- enc2utf8(x)
  out[is.na(out)] <- ""
  out
}

.acquisition_io_metadata_table <- function(columns) {
  columns <- lapply(columns, function(x) {
    x %||% character()
  })
  if (!all(vapply(columns, is.character, logical(1)))) {
    stop(
      "Internal acquisition payload columns must be character.",
      call. = FALSE
    )
  }
  tibble::as_tibble(columns, .name_repair = "minimal")
}

.acquisition_io_format_optional_positive <- function(x) {
  out <- rep("", length(x))
  present <- which(!is.na(x))
  if (length(present) > 0L) {
    out[present] <- as.character(x[present])
  }
  out
}

.acquisition_io_format_mask <- function(x) {
  out <- rep("", length(x))
  out[!is.na(x) & x] <- "1"
  out[!is.na(x) & !x] <- "0"
  out
}

.acquisition_io_format_point_indices <- function(counts) {
  unlist(
    lapply(counts, function(n) as.character(seq_len(n))),
    use.names = FALSE
  )
}

.acquisition_io_prepare_tables <- function(x) {
  scalar <- .record_io_prepare_tables(x$record)$tables
  scans <- x$scans
  arrays <- x$arrays
  scan_measures <- scans$.measures
  array_measures <- arrays$.measures
  scan_counts <- vapply(scan_measures, nrow, integer(1))
  array_counts <- vapply(array_measures, nrow, integer(1))
  scan_ids <- scans$scan_id
  array_ids <- arrays$array_id

  scan_location_types <- if (length(scan_measures) == 0L) {
    character()
  } else {
    vapply(
      scan_measures,
      function(measure) typeof(measure$location),
      character(1)
    )
  }
  scan_value_types <- if (length(scan_measures) == 0L) {
    character()
  } else {
    vapply(scan_measures, function(measure) typeof(measure$value), character(1))
  }
  scan_points <- .acquisition_io_metadata_table(list(
    scan_id = rep(scan_ids, scan_counts),
    point_index = .acquisition_io_format_point_indices(scan_counts),
    location = unlist(
      lapply(scan_measures, function(measure) {
        .nd_io_format_numeric(measure$location)
      }),
      use.names = FALSE
    ),
    value = unlist(
      lapply(scan_measures, function(measure) {
        .nd_io_format_numeric(measure$value)
      }),
      use.names = FALSE
    ),
    source_row = unlist(
      lapply(scans$.source_rows, .acquisition_io_format_optional_positive),
      use.names = FALSE
    ),
    saturated = unlist(
      lapply(scans$.saturated, .acquisition_io_format_mask),
      use.names = FALSE
    )
  ))

  scan_table <- .acquisition_io_metadata_table(list(
    row_index = as.character(seq_len(nrow(scans))),
    scan_id = scans$scan_id,
    stream_id = scans$stream_id,
    scan_index = as.character(scans$scan_index),
    scan_status = scans$scan_status,
    retention_time = .nd_io_format_numeric(scans$retention_time),
    polarity = .acquisition_io_nullable_text(scans$polarity),
    ms_level = .acquisition_io_format_optional_positive(scans$ms_level),
    spectrum_type = .acquisition_io_nullable_text(scans$spectrum_type),
    source_scan_id = .acquisition_io_nullable_text(scans$source_scan_id),
    location_type = scan_location_types,
    value_type = scan_value_types,
    point_count = as.character(scan_counts)
  ))

  array_axes <- .acquisition_io_metadata_table(list(
    array_id = rep(array_ids, each = 2L),
    axis_index = rep(c("1", "2"), times = length(array_ids)),
    storage_type = unlist(
      lapply(array_measures, function(measure) {
        c(typeof(measure$location_1), typeof(measure$location_2))
      }),
      use.names = FALSE
    ),
    axis_name = unlist(
      lapply(array_measures, function(measure) {
        metadata <- attr(measure, "dim_names", exact = TRUE)
        if (is.null(metadata)) {
          rep("", 2L)
        } else {
          .acquisition_io_nullable_text(metadata)
        }
      }),
      use.names = FALSE
    ),
    axis_unit = unlist(
      lapply(array_measures, function(measure) {
        metadata <- attr(measure, "dim_units", exact = TRUE)
        if (is.null(metadata)) {
          rep("", 2L)
        } else {
          .acquisition_io_nullable_text(metadata)
        }
      }),
      use.names = FALSE
    )
  ))

  array_points <- .acquisition_io_metadata_table(list(
    array_id = rep(array_ids, array_counts),
    point_index = .acquisition_io_format_point_indices(array_counts),
    location_1 = unlist(
      lapply(array_measures, function(measure) {
        .nd_io_format_numeric(measure$location_1)
      }),
      use.names = FALSE
    ),
    location_2 = unlist(
      lapply(array_measures, function(measure) {
        .nd_io_format_numeric(measure$location_2)
      }),
      use.names = FALSE
    ),
    value = unlist(
      lapply(array_measures, function(measure) {
        .nd_io_format_numeric(measure$value)
      }),
      use.names = FALSE
    ),
    source_row = unlist(
      lapply(arrays$.source_rows, .acquisition_io_format_optional_positive),
      use.names = FALSE
    ),
    saturated = unlist(
      lapply(arrays$.saturated, .acquisition_io_format_mask),
      use.names = FALSE
    )
  ))

  array_table <- .acquisition_io_metadata_table(list(
    row_index = as.character(seq_len(nrow(arrays))),
    array_id = arrays$array_id,
    stream_id = arrays$stream_id,
    dim_names_present = vapply(
      array_measures,
      function(measure) {
        if (is.null(attr(measure, "dim_names", exact = TRUE))) "0" else "1"
      },
      character(1)
    ),
    dim_units_present = vapply(
      array_measures,
      function(measure) {
        if (is.null(attr(measure, "dim_units", exact = TRUE))) "0" else "1"
      },
      character(1)
    ),
    value_type = if (length(array_measures) == 0L) {
      character()
    } else {
      vapply(
        array_measures,
        function(measure) typeof(measure$value),
        character(1)
      )
    },
    point_count = as.character(array_counts)
  ))

  stream_table <- x$streams
  streams <- .acquisition_io_metadata_table(list(
    row_index = as.character(seq_len(nrow(stream_table))),
    stream_id = stream_table$stream_id,
    injection_id = stream_table$injection_id,
    channel_id = stream_table$channel_id,
    kind = stream_table$kind,
    source_id = .acquisition_io_nullable_text(stream_table$source_id),
    time_unit = .acquisition_io_nullable_text(stream_table$time_unit),
    mz_unit = .acquisition_io_nullable_text(stream_table$mz_unit),
    value_name = .acquisition_io_nullable_text(stream_table$value_name),
    value_unit = .acquisition_io_nullable_text(stream_table$value_unit)
  ))

  tables <- list(
    Samples = .acquisition_io_add_row_index(scalar$Samples),
    Injections = .acquisition_io_add_row_index(scalar$Injections),
    Channels = .acquisition_io_add_row_index(scalar$Channels),
    Sources = .acquisition_io_add_row_index(scalar$Sources),
    Curves = .acquisition_io_add_row_index(scalar$Curves),
    Points = scalar$Points,
    Streams = streams,
    Scans = scan_table,
    ScanPoints = scan_points,
    Arrays = array_table,
    ArrayAxes = array_axes,
    ArrayPoints = array_points
  )
  for (table in names(tables)) {
    if (
      !identical(names(tables[[table]]), .acquisition_io_table_specs[[table]])
    ) {
      stop("Internal acquisition table schema is inconsistent.", call. = FALSE)
    }
  }
  tables
}

.acquisition_io_make_manifest <- function(tables, hashes, delim) {
  fields <- c(
    Format = "measure-acquisition",
    Version = "1",
    Delimiter = if (identical(delim, ",")) "csv" else "tsv"
  )
  for (table in names(.acquisition_io_table_specs)) {
    fields[paste0(table, "-Rows")] <- as.character(nrow(tables[[table]]))
    fields[paste0(table, "-Columns")] <- as.character(ncol(tables[[table]]))
    fields[paste0(table, "-SHA256")] <- hashes[[table]]
  }
  if (!identical(names(fields), .acquisition_io_manifest_fields)) {
    stop("Internal acquisition manifest schema is inconsistent.", call. = FALSE)
  }
  fields
}

.acquisition_io_write_manifest <- function(fields, path) {
  if (!identical(names(fields), .acquisition_io_manifest_fields)) {
    stop("Internal acquisition manifest schema is inconsistent.", call. = FALSE)
  }
  lines <- paste0(names(fields), ": ", unname(fields))
  connection <- file(path, open = "wb")
  on.exit(close(connection), add = TRUE)
  writeBin(charToRaw(paste0(paste(lines, collapse = "\n"), "\n")), connection)
  invisible(path)
}

.acquisition_io_parse_manifest <- function(raw) {
  if (!is.raw(raw) || length(raw) == 0L || any(raw == as.raw(0))) {
    stop("Bundle manifest is empty or malformed.", call. = FALSE)
  }
  if (any(as.integer(raw) > 127L)) {
    stop("Bundle manifest must contain only ASCII fields.", call. = FALSE)
  }
  text <- sub("\n$", "", rawToChar(raw))
  if (!nzchar(text)) {
    stop("Bundle manifest is empty or malformed.", call. = FALSE)
  }
  lines <- sub("\r$", "", strsplit(text, "\n", fixed = TRUE)[[1L]])
  field_lines <- lines[nzchar(lines) & !grepl("^[[:space:]]", lines)]
  field_keys <- sub(":.*$", "", field_lines)
  if (anyDuplicated(field_keys)) {
    stop("Bundle manifest has duplicate fields.", call. = FALSE)
  }
  connection <- textConnection(text, open = "r")
  on.exit(close(connection), add = TRUE)
  dcf <- tryCatch(
    read.dcf(connection, all = TRUE),
    error = function(error) {
      stop("Bundle manifest is not a single fixed DCF record.", call. = FALSE)
    }
  )
  if (!is.data.frame(dcf) || nrow(dcf) != 1L) {
    stop("Bundle manifest is not a single fixed DCF record.", call. = FALSE)
  }
  keys <- names(dcf)
  if (
    anyDuplicated(keys) ||
      length(keys) != length(.acquisition_io_manifest_fields) ||
      !setequal(keys, .acquisition_io_manifest_fields)
  ) {
    stop(
      "Bundle manifest has missing, duplicate, or unknown fields.",
      call. = FALSE
    )
  }
  values <- unlist(dcf[1L, , drop = FALSE], use.names = TRUE)
  values <- values[.acquisition_io_manifest_fields]
  manifest <- as.list(values)
  if (!identical(manifest[["Format"]], "measure-acquisition")) {
    stop("Unsupported bundle format.", call. = FALSE)
  }
  if (!identical(manifest[["Version"]], "1")) {
    stop("Unsupported measure-acquisition bundle version.", call. = FALSE)
  }
  if (!(manifest[["Delimiter"]] %in% c("csv", "tsv"))) {
    stop("Unsupported bundle delimiter.", call. = FALSE)
  }
  for (table in names(.acquisition_io_table_specs)) {
    rows_key <- paste0(table, "-Rows")
    columns_key <- paste0(table, "-Columns")
    hash_key <- paste0(table, "-SHA256")
    manifest[[rows_key]] <- .record_io_parse_integer(
      manifest[[rows_key]],
      paste0(table, " rows"),
      allow_missing = FALSE,
      allow_zero = TRUE
    )
    manifest[[columns_key]] <- .record_io_parse_integer(
      manifest[[columns_key]],
      paste0(table, " columns"),
      allow_missing = FALSE,
      allow_zero = FALSE
    )
    if (
      manifest[[columns_key]] != length(.acquisition_io_table_specs[[table]])
    ) {
      stop("Bundle has an invalid field count for ", table, ".", call. = FALSE)
    }
    if (!grepl("\\A[0-9a-f]{64}\\z", manifest[[hash_key]], perl = TRUE)) {
      stop("Bundle manifest has an invalid SHA256 field.", call. = FALSE)
    }
  }
  manifest
}

.acquisition_io_read_cumulative_member <- function(path, remaining) {
  if (remaining > 0L) {
    return(.record_read_raw(path, remaining))
  }
  connection <- file(path, open = "rb")
  on.exit(close(connection), add = TRUE)
  probe <- readBin(connection, what = "raw", n = 1L)
  if (length(probe) > 0L) {
    stop("Bundle exceeds max_bytes while reading.", call. = FALSE)
  }
  raw()
}

.acquisition_io_parse_payload <- function(raw, table, delim, manifest) {
  parsed <- .record_parse_delim(raw, delim)
  expected <- .acquisition_io_table_specs[[table]]
  if (!identical(names(parsed), expected)) {
    stop(
      "Bundle payload has unexpected columns for ",
      table,
      ".",
      call. = FALSE
    )
  }
  if (nrow(parsed) != manifest[[paste0(table, "-Rows")]]) {
    stop(
      "Bundle payload row count does not match the manifest for ",
      table,
      ".",
      call. = FALSE
    )
  }
  if (ncol(parsed) != manifest[[paste0(table, "-Columns")]]) {
    stop(
      "Bundle payload column count does not match the manifest for ",
      table,
      ".",
      call. = FALSE
    )
  }
  if (!all(vapply(parsed, is.character, logical(1)))) {
    stop("Bundle payload columns must be parsed as character.", call. = FALSE)
  }
  parsed
}

.acquisition_io_parse_key <- function(x, field, allow_zero = FALSE) {
  parsed <- .record_io_parse_integer(
    x,
    field,
    allow_missing = FALSE,
    allow_zero = allow_zero
  )
  if (anyNA(parsed)) {
    stop("Invalid key in ", field, ".", call. = FALSE)
  }
  parsed
}

.acquisition_io_parse_optional_positive <- function(x, field) {
  if (!is.character(x) || anyNA(x)) {
    stop(field, " must be represented as text.", call. = FALSE)
  }
  out <- rep(NA_integer_, length(x))
  present <- which(x != "")
  if (length(present) > 0L) {
    out[present] <- .record_io_parse_integer(
      x[present],
      field,
      allow_missing = FALSE,
      allow_zero = FALSE
    )
  }
  out
}

.acquisition_io_parse_flags <- function(x, field) {
  if (!is.character(x) || anyNA(x) || !all(x %in% c("0", "1"))) {
    stop("Invalid flag in ", field, ".", call. = FALSE)
  }
  x == "1"
}

.acquisition_io_parse_mask <- function(x, field) {
  if (!is.character(x) || anyNA(x) || !all(x %in% c("", "0", "1"))) {
    stop("Invalid mask token in ", field, ".", call. = FALSE)
  }
  out <- rep(NA, length(x))
  out[x == "0"] <- FALSE
  out[x == "1"] <- TRUE
  out
}

.acquisition_io_restore_row_order <- function(table, field) {
  index <- .acquisition_io_parse_key(
    table$row_index,
    paste0(field, ".row_index")
  )
  expected <- seq_len(nrow(table))
  if (anyDuplicated(index) || !identical(sort(index), expected)) {
    stop(
      field,
      ".row_index must be unique and contiguous from 1.",
      call. = FALSE
    )
  }
  table <- table[order(index), , drop = FALSE]
  table$row_index <- NULL
  table
}

.acquisition_io_validate_text <- function(x, field, nullable = FALSE) {
  .record_validate_text_column(x, field, nullable = nullable)
  invisible(x)
}

.acquisition_io_validate_unique <- function(x, field) {
  if (anyDuplicated(x)) {
    stop(field, " values must be unique.", call. = FALSE)
  }
}

.acquisition_io_validate_curve_points <- function(points, curves) {
  point_index <- .acquisition_io_parse_key(
    points$point_index,
    "Points.point_index"
  )
  source_row <- .record_io_parse_integer(
    points$source_row,
    "Points.source_row",
    allow_missing = TRUE,
    allow_zero = FALSE
  )
  location <- .record_parse_double(points$location, "Points.location")
  value <- .record_parse_double(
    points$value,
    "Points.value",
    allow_missing = TRUE
  )
  if (
    any(.nd_io_is_negative_zero(location)) ||
      any(.nd_io_is_negative_zero(value))
  ) {
    stop("Scalar record numeric fields must use positive zero.", call. = FALSE)
  }
  if (!all(points$curve_id %in% curves$curve_id)) {
    stop("Bundle points contain an orphan curve ID.", call. = FALSE)
  }
  groups <- vector("list", nrow(curves))
  for (i in seq_len(nrow(curves))) {
    selected <- which(points$curve_id == curves$curve_id[[i]])
    if (length(selected) == 0L) {
      stop("Every bundle curve must have at least one point.", call. = FALSE)
    }
    ordered <- selected[order(point_index[selected])]
    if (!identical(point_index[ordered], seq_len(length(selected)))) {
      stop(
        "Bundle point indices must be unique and contiguous within each curve.",
        call. = FALSE
      )
    }
    known_rows <- source_row[ordered][!is.na(source_row[ordered])]
    if (anyDuplicated(known_rows)) {
      stop("Curve source rows must be unique within each curve.", call. = FALSE)
    }
    selected_locations <- location[ordered]
    if (length(selected_locations) > 1L) {
      increasing <- all(diff(selected_locations) > 0)
      decreasing <- all(diff(selected_locations) < 0)
      if (!increasing && !decreasing) {
        stop(
          "Curve locations must be strictly increasing or decreasing.",
          call. = FALSE
        )
      }
    }
    groups[[i]] <- ordered
  }
  list(
    point_index = point_index,
    source_row = source_row,
    location = location,
    value = value,
    groups = groups
  )
}

.acquisition_io_validate_record <- function(tables) {
  samples <- tables$Samples
  injections <- tables$Injections
  channels <- tables$Channels
  sources <- tables$Sources
  curves <- tables$Curves
  points <- tables$Points

  .acquisition_io_validate_text(samples$sample_id, "samples.sample_id")
  .acquisition_io_validate_text(
    injections$injection_id,
    "injections.injection_id"
  )
  .acquisition_io_validate_text(injections$sample_id, "injections.sample_id")
  .acquisition_io_validate_text(channels$channel_id, "channels.channel_id")
  .acquisition_io_validate_text(
    channels$channel_name,
    "channels.channel_name",
    TRUE
  )
  .acquisition_io_validate_text(sources$source_id, "sources.source_id")
  .acquisition_io_validate_text(
    sources$source_name,
    "sources.source_name",
    TRUE
  )
  .acquisition_io_validate_text(
    sources$source_sha256,
    "sources.source_sha256",
    TRUE
  )
  .acquisition_io_validate_text(curves$curve_id, "curves.curve_id")
  .acquisition_io_validate_text(curves$injection_id, "curves.injection_id")
  .acquisition_io_validate_text(curves$channel_id, "curves.channel_id")
  for (column in c(
    "source_id",
    "axis_name",
    "axis_unit",
    "value_name",
    "value_unit"
  )) {
    .acquisition_io_validate_text(
      curves[[column]],
      paste0("curves.", column),
      TRUE
    )
  }

  .acquisition_io_validate_unique(samples$sample_id, "samples.sample_id")
  .acquisition_io_validate_unique(
    injections$injection_id,
    "injections.injection_id"
  )
  .acquisition_io_validate_unique(channels$channel_id, "channels.channel_id")
  .acquisition_io_validate_unique(sources$source_id, "sources.source_id")
  .acquisition_io_validate_unique(curves$curve_id, "curves.curve_id")
  invalid_hash <- !is.na(sources$source_sha256) &
    !grepl("^[0-9a-f]{64}$", sources$source_sha256)
  if (any(invalid_hash)) {
    stop(
      "Known source SHA256 values must be 64 lowercase hexadecimal characters.",
      call. = FALSE
    )
  }
  if (
    nrow(injections) > 0L && !all(injections$sample_id %in% samples$sample_id)
  ) {
    stop("Every injection must refer to a sample.", call. = FALSE)
  }
  if (nrow(curves) > 0L) {
    if (!all(curves$injection_id %in% injections$injection_id)) {
      stop("Every curve must refer to an injection.", call. = FALSE)
    }
    if (!all(curves$channel_id %in% channels$channel_id)) {
      stop("Every curve must refer to a channel.", call. = FALSE)
    }
    known_source <- !is.na(curves$source_id)
    if (any(known_source & !curves$source_id %in% sources$source_id)) {
      stop("Every known curve source must refer to a source.", call. = FALSE)
    }
    pairs <- data.frame(
      injection_id = curves$injection_id,
      channel_id = curves$channel_id,
      stringsAsFactors = FALSE
    )
    if (anyDuplicated(pairs)) {
      stop(
        "Each injection and channel pair may identify only one curve.",
        call. = FALSE
      )
    }
  }

  point_data <- .acquisition_io_validate_curve_points(points, curves)
  list(
    point_index = point_data$point_index,
    source_row = point_data$source_row,
    location = point_data$location,
    value = point_data$value,
    groups = point_data$groups
  )
}

.acquisition_io_restore_point_groups <- function(
  table,
  owner_column,
  owner_ids,
  field,
  expected_counts = NULL
) {
  point_index <- .acquisition_io_parse_key(
    table$point_index,
    paste0(field, ".point_index")
  )
  if (!all(table[[owner_column]] %in% owner_ids)) {
    stop(field, " contains an unknown owner reference.", call. = FALSE)
  }
  groups <- vector("list", length(owner_ids))
  names(groups) <- owner_ids
  for (i in seq_along(owner_ids)) {
    selected <- which(table[[owner_column]] == owner_ids[[i]])
    ordered <- selected[order(point_index[selected])]
    if (!identical(point_index[ordered], seq_len(length(selected)))) {
      stop(
        field,
        " point_index must be unique and contiguous within each owner.",
        call. = FALSE
      )
    }
    if (!is.null(expected_counts) && length(selected) != expected_counts[[i]]) {
      stop(
        field,
        " rows disagree with the declared point_count.",
        call. = FALSE
      )
    }
    groups[[i]] <- ordered
  }
  list(index = point_index, groups = groups)
}

.acquisition_io_decode_tables <- function(tables) {
  for (table in c(
    "Samples",
    "Injections",
    "Channels",
    "Sources",
    "Curves",
    "Streams",
    "Scans",
    "Arrays"
  )) {
    tables[[table]] <- .acquisition_io_restore_row_order(
      tables[[table]],
      table
    )
  }

  for (column in "channel_name") {
    tables$Channels[[column]] <- .record_io_nullable_text(
      tables$Channels[[column]]
    )
  }
  for (column in c("source_name", "source_sha256")) {
    tables$Sources[[column]] <- .record_io_nullable_text(
      tables$Sources[[column]]
    )
  }
  for (column in c(
    "source_id",
    "axis_name",
    "axis_unit",
    "value_name",
    "value_unit"
  )) {
    tables$Curves[[column]] <- .record_io_nullable_text(
      tables$Curves[[column]]
    )
  }

  record_numeric <- .acquisition_io_validate_record(tables)
  streams <- tables$Streams
  for (column in c(
    "source_id",
    "time_unit",
    "mz_unit",
    "value_name",
    "value_unit"
  )) {
    streams[[column]] <- .record_io_nullable_text(streams[[column]])
  }
  .acquisition_io_validate_text(streams$stream_id, "streams.stream_id")
  .acquisition_io_validate_text(streams$injection_id, "streams.injection_id")
  .acquisition_io_validate_text(streams$channel_id, "streams.channel_id")
  .acquisition_io_validate_text(streams$kind, "streams.kind")
  .acquisition_io_validate_text(streams$source_id, "streams.source_id", TRUE)
  for (column in c("time_unit", "mz_unit", "value_name", "value_unit")) {
    .acquisition_io_validate_text(
      streams[[column]],
      paste0("streams.", column),
      TRUE
    )
  }
  .acquisition_io_validate_unique(streams$stream_id, "streams.stream_id")
  if (!all(streams$injection_id %in% tables$Injections$injection_id)) {
    stop("Every stream must refer to an injection.", call. = FALSE)
  }
  if (!all(streams$channel_id %in% tables$Channels$channel_id)) {
    stop("Every stream must refer to a channel.", call. = FALSE)
  }
  known_sources <- !is.na(streams$source_id)
  if (any(known_sources & !(streams$source_id %in% tables$Sources$source_id))) {
    stop("Every known stream source must refer to a source.", call. = FALSE)
  }
  if (!all(streams$kind %in% c("ms", "nd"))) {
    stop("streams.kind must be ms or nd.", call. = FALSE)
  }
  nd_stream <- streams$kind == "nd"
  if (
    any(
      nd_stream &
        (!is.na(.record_io_nullable_text(streams$time_unit)) |
          !is.na(.record_io_nullable_text(streams$mz_unit)))
    )
  ) {
    stop("ND stream time_unit and mz_unit must be missing.", call. = FALSE)
  }

  scan_table <- tables$Scans
  scan_table$source_scan_id <- .record_io_nullable_text(
    scan_table$source_scan_id
  )
  .acquisition_io_validate_text(scan_table$scan_id, "scans.scan_id")
  .acquisition_io_validate_text(scan_table$stream_id, "scans.stream_id")
  .acquisition_io_validate_text(scan_table$scan_status, "scans.scan_status")
  .acquisition_io_validate_text(
    scan_table$source_scan_id,
    "scans.source_scan_id",
    TRUE
  )
  .acquisition_io_validate_unique(scan_table$scan_id, "scans.scan_id")
  scan_index <- .acquisition_io_parse_key(
    scan_table$scan_index,
    "scans.scan_index"
  )
  retention_time <- .nd_io_parse_numeric(
    scan_table$retention_time,
    "double",
    "scans.retention_time"
  )
  if (any(is.nan(retention_time) | is.infinite(retention_time))) {
    stop("scans.retention_time must be finite or missing.", call. = FALSE)
  }
  ms_level <- .acquisition_io_parse_optional_positive(
    scan_table$ms_level,
    "scans.ms_level"
  )
  point_count <- .acquisition_io_parse_key(
    scan_table$point_count,
    "scans.point_count",
    allow_zero = TRUE
  )
  if (!all(scan_table$scan_status %in% c("acquired", "missing"))) {
    stop("scans.scan_status must be acquired or missing.", call. = FALSE)
  }
  if (!all(scan_table$polarity %in% c("", "positive", "negative"))) {
    stop(
      "scans.polarity must be positive, negative, or unknown.",
      call. = FALSE
    )
  }
  if (!all(scan_table$spectrum_type %in% c("", "centroid", "profile"))) {
    stop(
      "scans.spectrum_type must be centroid, profile, or unknown.",
      call. = FALSE
    )
  }
  if (!all(scan_table$location_type %in% c("integer", "double"))) {
    stop("scans.location_type must be integer or double.", call. = FALSE)
  }
  if (!all(scan_table$value_type %in% c("integer", "double"))) {
    stop("scans.value_type must be integer or double.", call. = FALSE)
  }
  stream_match <- match(scan_table$stream_id, streams$stream_id)
  if (anyNA(stream_match) || any(streams$kind[stream_match] != "ms")) {
    stop("Every scan must refer to an MS stream.", call. = FALSE)
  }
  scan_pairs <- data.frame(
    stream_id = scan_table$stream_id,
    scan_index = scan_index,
    stringsAsFactors = FALSE
  )
  if (anyDuplicated(scan_pairs)) {
    stop("scan_index values must be unique within each stream.", call. = FALSE)
  }
  if (any(scan_table$scan_status == "missing" & point_count != 0L)) {
    stop("Missing scans must have zero points.", call. = FALSE)
  }
  scan_points <- tables$ScanPoints
  .acquisition_io_validate_text(scan_points$scan_id, "scan_points.scan_id")
  scan_groups <- .acquisition_io_restore_point_groups(
    scan_points,
    "scan_id",
    scan_table$scan_id,
    "ScanPoints",
    point_count
  )
  scan_source_rows <- .acquisition_io_parse_optional_positive(
    scan_points$source_row,
    "scan_points.source_row"
  )
  scan_saturated <- .acquisition_io_parse_mask(
    scan_points$saturated,
    "scan_points.saturated"
  )
  scan_measurements <- vector("list", nrow(scan_table))
  scan_lineage <- vector("list", nrow(scan_table))
  scan_masks <- vector("list", nrow(scan_table))
  for (i in seq_len(nrow(scan_table))) {
    selected <- scan_groups$groups[[i]]
    scan_measurements[[i]] <- list(
      location = .nd_io_parse_numeric(
        scan_points$location[selected],
        scan_table$location_type[[i]],
        paste0("scan_points.location for ", scan_table$scan_id[[i]])
      ),
      value = .nd_io_parse_numeric(
        scan_points$value[selected],
        scan_table$value_type[[i]],
        paste0("scan_points.value for ", scan_table$scan_id[[i]])
      )
    )
    scan_lineage[[i]] <- scan_source_rows[selected]
    scan_masks[[i]] <- scan_saturated[selected]
  }

  arrays <- tables$Arrays
  .acquisition_io_validate_text(arrays$array_id, "arrays.array_id")
  .acquisition_io_validate_text(arrays$stream_id, "arrays.stream_id")
  .acquisition_io_validate_unique(arrays$array_id, "arrays.array_id")
  dim_names_present <- .acquisition_io_parse_flags(
    arrays$dim_names_present,
    "arrays.dim_names_present"
  )
  dim_units_present <- .acquisition_io_parse_flags(
    arrays$dim_units_present,
    "arrays.dim_units_present"
  )
  array_point_count <- .acquisition_io_parse_key(
    arrays$point_count,
    "arrays.point_count",
    allow_zero = TRUE
  )
  if (!all(arrays$value_type %in% c("integer", "double"))) {
    stop("arrays.value_type must be integer or double.", call. = FALSE)
  }
  array_stream_match <- match(arrays$stream_id, streams$stream_id)
  if (
    anyNA(array_stream_match) || any(streams$kind[array_stream_match] != "nd")
  ) {
    stop("Every array must refer to an ND stream.", call. = FALSE)
  }

  axes <- tables$ArrayAxes
  .acquisition_io_validate_text(axes$array_id, "array_axes.array_id")
  axis_index <- .acquisition_io_parse_key(
    axes$axis_index,
    "array_axes.axis_index"
  )
  if (!all(axes$storage_type %in% c("integer", "double"))) {
    stop("array_axes.storage_type must be integer or double.", call. = FALSE)
  }
  if (!all(axes$array_id %in% arrays$array_id)) {
    stop("ArrayAxes contains an unknown array ID.", call. = FALSE)
  }
  axis_pairs <- data.frame(
    array_id = axes$array_id,
    axis_index = axis_index,
    stringsAsFactors = FALSE
  )
  axis_owner <- match(axes$array_id, arrays$array_id)
  axis_keys <- (as.double(axis_owner) - 1) * 2 + as.double(axis_index)
  expected_axis_rows <- 2 * as.double(nrow(arrays))
  if (
    anyNA(axis_owner) ||
      anyDuplicated(axis_pairs) ||
      nrow(axis_pairs) != expected_axis_rows ||
      !identical(sort(axis_keys), as.double(seq_len(nrow(axis_pairs))))
  ) {
    stop(
      "ArrayAxes must contain exactly axes 1 and 2 for each array.",
      call. = FALSE
    )
  }
  array_axis_data <- vector("list", nrow(arrays))
  for (i in seq_len(nrow(arrays))) {
    selected <- which(axes$array_id == arrays$array_id[[i]])
    ordered <- selected[order(axis_index[selected])]
    axis_names <- axes$axis_name[ordered]
    axis_units <- axes$axis_unit[ordered]
    if (!dim_names_present[[i]] && any(axis_names != "")) {
      stop(
        "Absent dim_names metadata requires blank axis_name cells.",
        call. = FALSE
      )
    }
    if (!dim_units_present[[i]] && any(axis_units != "")) {
      stop(
        "Absent dim_units metadata requires blank axis_unit cells.",
        call. = FALSE
      )
    }
    dim_names <- if (dim_names_present[[i]]) {
      .record_io_nullable_text(axis_names)
    } else {
      NULL
    }
    dim_units <- if (dim_units_present[[i]]) {
      .record_io_nullable_text(axis_units)
    } else {
      NULL
    }
    .nd_check_dim_metadata(dim_names, "dim_names", 2L, unique = TRUE)
    .nd_check_dim_metadata(dim_units, "dim_units", 2L)
    array_axis_data[[i]] <- list(
      storage_type = axes$storage_type[ordered],
      dim_names = dim_names,
      dim_units = dim_units
    )
  }

  array_points <- tables$ArrayPoints
  .acquisition_io_validate_text(array_points$array_id, "array_points.array_id")
  array_groups <- .acquisition_io_restore_point_groups(
    array_points,
    "array_id",
    arrays$array_id,
    "ArrayPoints",
    array_point_count
  )
  array_source_rows <- .acquisition_io_parse_optional_positive(
    array_points$source_row,
    "array_points.source_row"
  )
  array_saturated <- .acquisition_io_parse_mask(
    array_points$saturated,
    "array_points.saturated"
  )
  array_measurements <- vector("list", nrow(arrays))
  array_lineage <- vector("list", nrow(arrays))
  array_masks <- vector("list", nrow(arrays))
  for (i in seq_len(nrow(arrays))) {
    selected <- array_groups$groups[[i]]
    types <- array_axis_data[[i]]$storage_type
    array_measurements[[i]] <- list(
      location_1 = .nd_io_parse_numeric(
        array_points$location_1[selected],
        types[[1L]],
        paste0("array_points.location_1 for ", arrays$array_id[[i]])
      ),
      location_2 = .nd_io_parse_numeric(
        array_points$location_2[selected],
        types[[2L]],
        paste0("array_points.location_2 for ", arrays$array_id[[i]])
      ),
      value = .nd_io_parse_numeric(
        array_points$value[selected],
        arrays$value_type[[i]],
        paste0("array_points.value for ", arrays$array_id[[i]])
      )
    )
    array_lineage[[i]] <- array_source_rows[selected]
    array_masks[[i]] <- array_saturated[selected]
  }

  list(
    record_payloads = list(
      Samples = tables$Samples,
      Injections = tables$Injections,
      Channels = tables$Channels,
      Sources = tables$Sources,
      Curves = tables$Curves,
      Points = tables$Points
    ),
    record_numeric = record_numeric,
    streams = streams,
    scans = list(
      table = scan_table,
      scan_index = scan_index,
      retention_time = retention_time,
      ms_level = ms_level,
      measurements = scan_measurements,
      source_rows = scan_lineage,
      saturated = scan_masks
    ),
    arrays = list(
      table = arrays,
      point_count = array_point_count,
      axes = array_axis_data,
      measurements = array_measurements,
      source_rows = array_lineage,
      saturated = array_masks
    )
  )
}

.acquisition_io_build_object <- function(decoded) {
  record <- .record_io_build_record(decoded$record_payloads)
  streams <- decoded$streams
  streams <- tibble::tibble(
    stream_id = streams$stream_id,
    injection_id = streams$injection_id,
    channel_id = streams$channel_id,
    kind = streams$kind,
    source_id = streams$source_id,
    time_unit = .record_io_nullable_text(streams$time_unit),
    mz_unit = .record_io_nullable_text(streams$mz_unit),
    value_name = .record_io_nullable_text(streams$value_name),
    value_unit = .record_io_nullable_text(streams$value_unit)
  )

  scan_table <- decoded$scans$table
  scan_measures <- lapply(decoded$scans$measurements, function(measurement) {
    new_measure_tbl(
      location = measurement$location,
      value = measurement$value
    )
  })
  scans <- tibble::tibble(
    scan_id = scan_table$scan_id,
    stream_id = scan_table$stream_id,
    scan_index = decoded$scans$scan_index,
    scan_status = scan_table$scan_status,
    retention_time = decoded$scans$retention_time,
    polarity = .record_io_nullable_text(scan_table$polarity),
    ms_level = decoded$scans$ms_level,
    spectrum_type = .record_io_nullable_text(scan_table$spectrum_type),
    source_scan_id = .record_io_nullable_text(scan_table$source_scan_id),
    .measures = scan_measures,
    .source_rows = decoded$scans$source_rows,
    .saturated = decoded$scans$saturated
  )

  array_table <- decoded$arrays$table
  array_measures <- lapply(seq_len(nrow(array_table)), function(i) {
    measurement <- decoded$arrays$measurements[[i]]
    axes <- decoded$arrays$axes[[i]]
    new_measure_nd_tbl(
      location_1 = measurement$location_1,
      location_2 = measurement$location_2,
      value = measurement$value,
      dim_names = axes$dim_names,
      dim_units = axes$dim_units
    )
  })
  arrays <- tibble::tibble(
    array_id = array_table$array_id,
    stream_id = array_table$stream_id,
    .measures = array_measures,
    .source_rows = decoded$arrays$source_rows,
    .saturated = decoded$arrays$saturated
  )
  out <- measure_acquisition(
    record = record,
    streams = streams,
    scans = scans,
    arrays = arrays
  )
  validate_measure_acquisition(out)
  out
}

.acquisition_io_measure_tbl_equal <- function(x, y) {
  identical(class(x), class(y)) &&
    identical(names(x), names(y)) &&
    nrow(x) == nrow(y) &&
    .nd_io_numeric_equal(x$location, y$location) &&
    .nd_io_numeric_equal(x$value, y$value)
}

.acquisition_io_fidelity_equal <- function(x, y) {
  if (
    !identical(attributes(x), attributes(y)) ||
      !identical(names(x), names(y)) ||
      !identical(x$record, y$record) ||
      !identical(x$streams, y$streams) ||
      !identical(
        x$scans[names(x$scans)[seq_len(4L)]],
        y$scans[names(y$scans)[seq_len(4L)]]
      ) ||
      !.nd_io_numeric_equal(x$scans$retention_time, y$scans$retention_time) ||
      !identical(x$scans[names(x$scans)[6:9]], y$scans[names(y$scans)[6:9]]) ||
      !identical(
        x$arrays[names(x$arrays)[1:2]],
        y$arrays[names(y$arrays)[1:2]]
      ) ||
      !identical(x$scans$.source_rows, y$scans$.source_rows) ||
      !identical(x$scans$.saturated, y$scans$.saturated) ||
      !identical(x$arrays$.source_rows, y$arrays$.source_rows) ||
      !identical(x$arrays$.saturated, y$arrays$.saturated) ||
      nrow(x$scans) != nrow(y$scans) ||
      nrow(x$arrays) != nrow(y$arrays)
  ) {
    return(FALSE)
  }
  if (
    nrow(x$scans) > 0L &&
      !all(vapply(
        seq_len(nrow(x$scans)),
        function(i) {
          .acquisition_io_measure_tbl_equal(
            x$scans$.measures[[i]],
            y$scans$.measures[[i]]
          )
        },
        logical(1)
      ))
  ) {
    return(FALSE)
  }
  if (
    nrow(x$arrays) > 0L &&
      !all(vapply(
        seq_len(nrow(x$arrays)),
        function(i) {
          .nd_io_tbl_equal(x$arrays$.measures[[i]], y$arrays$.measures[[i]])
        },
        logical(1)
      ))
  ) {
    return(FALSE)
  }
  TRUE
}
