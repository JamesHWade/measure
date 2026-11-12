#' Read and write native measurement record bundles
#'
#' A bundle is a new directory containing one fixed manifest.dcf and six
#' fixed UTF-8 CSV or TSV files. The manifest selects only documented v1 file
#' names; it cannot provide paths. Text values remain machine data, including
#' values that spreadsheet software may interpret as formulas. Hashes detect
#' accidental changes and do not authenticate a source.
#'
#' The reader checks the manifest, payload hashes, table schemas, row and
#' column counts, and point references before constructing a record. Its byte
#' limit applies to the actual total bytes read from all seven files.
#'
#' @section Bundle version 1:
#' The six fixed payload names have the selected `.csv` or `.tsv` extension.
#' Their ordered headers are:
#'
#' * `sources`: `source_id`, `source_name`, `source_sha256`.
#' * `samples`: `sample_id`.
#' * `injections`: `injection_id`, `sample_id`.
#' * `channels`: `channel_id`, `channel_name`.
#' * `curves`: `curve_id`, `injection_id`, `channel_id`, `source_id`,
#'   `axis_name`, `axis_unit`, `value_name`, `value_unit`.
#' * `points`: `curve_id`, `point_index`, `location`, `value`, `source_row`.
#'
#' The metadata tables retain the corresponding schema in [measure_record()].
#' The single DCF manifest declares `Format: measure-record`, `Version: 1`,
#' and `Delimiter: csv` or `Delimiter: tsv`. It also has `-Rows`, `-Columns`,
#' and `-SHA256` fields for each of the prefixes `Sources`, `Samples`,
#' `Injections`, `Channels`, `Curves`, and `Points`: for example,
#' `Points-Rows`. The reader accepts any order of this exact field set.
#'
#' Empty cells in nullable text fields mean missing; literal `NA` text remains
#' text. Numeric missing values are written blank, and the reader also accepts
#' `NA` in numeric fields. Positive contiguous `point_index` values restore
#' point order within each curve. Numeric fidelity is exact double equality
#' after positive-zero canonicalization; arbitrary R attributes are not
#' serialized.
#'
#' The writer validates and stages files beside the destination, reserves a
#' new destination directory, copies payloads without overwriting, then copies
#' the manifest last. A failed copy may leave an incomplete destination
#' without a manifest; the reader rejects it. Existing destinations are never
#' replaced.
#'
#' @param x A validated measure_record.
#' @param path A new directory path to write, or a directory containing a
#'   complete bundle to read. Existing writer destinations are rejected.
#' @param delim A comma or tab field delimiter.
#' @param max_bytes A positive whole-number byte limit for all files read.
#'
#' @return write_measure_record() returns the normalized destination path
#'   invisibly.
#'   read_measure_record() returns a validated measure_record.
#' @examples
#' record <- measure_record()
#' local({
#'   bundle <- tempfile("measure-record-")
#'   on.exit(unlink(bundle, recursive = TRUE), add = TRUE)
#'   write_measure_record(record, bundle)
#'   identical(record, read_measure_record(bundle))
#' })
#' @name record-bundle
NULL

#' @rdname record-bundle
#' @export
write_measure_record <- function(x, path, delim = c(",", "\t")) {
  delim <- match.arg(delim)
  x <- validate_measure_record(x)
  destination <- .record_io_destination(path)
  if (.record_io_path_exists(destination)) {
    stop("path already exists.", call. = FALSE)
  }

  stage <- .record_io_make_stage(dirname(destination), basename(destination))
  on.exit(unlink(stage, recursive = TRUE, force = TRUE), add = TRUE)
  prepared <- .record_io_prepare_tables(x)
  hashes <- character(length(.record_io_table_specs))
  names(hashes) <- names(.record_io_table_specs)

  for (table in names(.record_io_table_specs)) {
    filename <- .record_io_filename(table, delim)
    staged_file <- file.path(stage, filename)
    expected <- prepared$tables[[table]]
    .record_io_write_table(expected, staged_file, delim)
    bytes <- .record_io_staged_bytes(staged_file)
    hashes[[table]] <- digest::digest(bytes, algo = "sha256", serialize = FALSE)
    .record_io_verify_written_table(
      bytes,
      expected,
      table,
      delim,
      prepared$numeric
    )
  }

  manifest <- .record_io_make_manifest(prepared$tables, hashes, delim)
  manifest_stage <- file.path(stage, "manifest.dcf")
  .record_io_write_manifest(manifest, manifest_stage)

  if (!.record_io_reserve_destination(destination)) {
    stop(
      "Could not reserve the destination directory; it may have appeared during writing.",
      call. = FALSE
    )
  }
  for (table in names(.record_io_table_specs)) {
    .record_io_copy_file(
      file.path(stage, .record_io_filename(table, delim)),
      file.path(destination, .record_io_filename(table, delim))
    )
  }
  .record_io_copy_file(manifest_stage, file.path(destination, "manifest.dcf"))
  invisible(destination)
}

#' @rdname record-bundle
#' @export
read_measure_record <- function(path, max_bytes = 100 * 1024^2) {
  max_bytes <- .record_io_max_bytes(max_bytes)
  directory <- .record_io_read_directory(path)
  manifest_path <- file.path(directory, "manifest.dcf")
  .record_io_check_member(manifest_path)
  manifest_raw <- .record_read_raw(manifest_path, max_bytes)
  remaining <- max_bytes - length(manifest_raw)
  manifest <- .record_io_parse_manifest(manifest_raw)
  delim <- if (identical(manifest[["Delimiter"]], "csv")) "," else "\t"

  payloads <- vector("list", length(.record_io_table_specs))
  names(payloads) <- names(.record_io_table_specs)
  for (table in names(.record_io_table_specs)) {
    if (remaining <= 0L) {
      stop("Bundle exceeds max_bytes while reading.", call. = FALSE)
    }
    filename <- .record_io_filename(table, delim)
    member <- file.path(directory, filename)
    .record_io_check_member(member)
    bytes <- .record_read_raw(member, remaining)
    remaining <- remaining - length(bytes)
    actual_hash <- digest::digest(bytes, algo = "sha256", serialize = FALSE)
    if (!identical(actual_hash, manifest[[paste0(table, "-SHA256")]])) {
      stop(
        "Bundle payload hash does not match the manifest for ",
        filename,
        ".",
        call. = FALSE
      )
    }
    payloads[[table]] <- .record_io_parse_payload(bytes, table, delim, manifest)
  }
  .record_io_build_record(payloads)
}

.record_io_table_specs <- list(
  Sources = c("source_id", "source_name", "source_sha256"),
  Samples = c("sample_id"),
  Injections = c("injection_id", "sample_id"),
  Channels = c("channel_id", "channel_name"),
  Curves = c(
    "curve_id",
    "injection_id",
    "channel_id",
    "source_id",
    "axis_name",
    "axis_unit",
    "value_name",
    "value_unit"
  ),
  Points = c("curve_id", "point_index", "location", "value", "source_row")
)

.record_io_manifest_fields <- c(
  "Format",
  "Version",
  "Delimiter",
  unlist(
    lapply(names(.record_io_table_specs), function(table) {
      paste0(table, c("-Rows", "-Columns", "-SHA256"))
    }),
    use.names = FALSE
  )
)

.record_io_filename <- function(table, delim) {
  extension <- if (identical(delim, ",")) "csv" else "tsv"
  paste0(tolower(table), ".", extension)
}

.record_io_prepare_tables <- function(x) {
  curves <- x$curves
  measures <- curves$.measures
  measure_lengths <- vapply(measures, nrow, integer(1))
  point_data <- tibble::tibble(
    curve_id = rep(curves$curve_id, measure_lengths),
    point_index = as.integer(unlist(
      lapply(measure_lengths, seq_len),
      use.names = FALSE
    )),
    location = as.double(unlist(
      lapply(measures, function(measure) measure[["location"]]),
      use.names = FALSE
    )),
    value = as.double(unlist(
      lapply(measures, function(measure) measure[["value"]]),
      use.names = FALSE
    )),
    source_row = as.integer(unlist(curves$.source_rows, use.names = FALSE))
  )
  if (sum(measure_lengths) == 0L) {
    point_data <- tibble::tibble(
      curve_id = character(),
      point_index = integer(),
      location = double(),
      value = double(),
      source_row = integer()
    )
  }
  tables <- list(
    Sources = x$sources[.record_io_table_specs$Sources],
    Samples = x$samples[.record_io_table_specs$Samples],
    Injections = x$injections[.record_io_table_specs$Injections],
    Channels = x$channels[.record_io_table_specs$Channels],
    Curves = curves[.record_io_table_specs$Curves],
    Points = point_data
  )
  numeric <- list(
    location = point_data$location,
    value = point_data$value,
    source_row = point_data$source_row
  )
  for (table in names(tables)) {
    tables[[table]] <- .record_io_text_table(tables[[table]], table)
  }
  list(tables = tables, numeric = numeric)
}

.record_io_text_table <- function(x, table) {
  out <- lapply(names(x), function(column) {
    values <- x[[column]]
    if (table == "Points" && column %in% c("location", "value")) {
      return(.record_format_double(as.double(values)))
    }
    if (table == "Points" && column %in% c("source_row", "point_index")) {
      return(.record_io_format_integer(values, column))
    }
    if (!is.character(values)) {
      stop(
        "Internal bundle table contains an unexpected column type.",
        call. = FALSE
      )
    }
    values <- enc2utf8(values)
    values[is.na(values)] <- ""
    values
  })
  names(out) <- names(x)
  tibble::as_tibble(out, .name_repair = "minimal")
}

.record_io_format_integer <- function(x, field) {
  if (
    !(typeof(x) %in% c("integer", "double")) ||
      !is.null(dim(x)) ||
      any(!is.na(x) & (!is.finite(x) | x != trunc(x)))
  ) {
    stop(
      "Internal bundle field ",
      field,
      " is not integer-valued.",
      call. = FALSE
    )
  }
  out <- rep("", length(x))
  present <- which(!is.na(x))
  if (length(present) > 0L) {
    out[present] <- format(x[present], scientific = FALSE, trim = TRUE)
  }
  out
}

.record_io_write_table <- function(x, path, delim) {
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
  con <- file(path, open = "wb")
  on.exit(close(con), add = TRUE)
  writeLines(c(header, rows), con, sep = "\n", useBytes = TRUE)
  invisible(path)
}

.record_io_quote_fields <- function(x) {
  x <- enc2utf8(x)
  escaped <- gsub("\"", "\"\"", x, fixed = TRUE, useBytes = TRUE)
  paste0("\"", escaped, "\"")
}

.record_io_staged_bytes <- function(path) {
  size <- file.info(path)$size
  if (
    length(size) != 1L ||
      is.na(size) ||
      size <= 0 ||
      size > .Machine$integer.max - 1
  ) {
    stop("Could not read back staged bundle payload.", call. = FALSE)
  }
  .record_read_raw(path, as.integer(size))
}

.record_io_verify_written_table <- function(
  bytes,
  expected,
  table,
  delim,
  numeric
) {
  parsed <- .record_parse_delim(bytes, delim)
  if (
    !identical(names(parsed), names(expected)) || nrow(parsed) != nrow(expected)
  ) {
    stop(
      "Staged bundle payload did not preserve its table shape.",
      call. = FALSE
    )
  }
  for (column in names(expected)) {
    if (!identical(parsed[[column]], expected[[column]])) {
      stop(
        "Staged bundle payload did not preserve field ",
        column,
        ".",
        call. = FALSE
      )
    }
  }
  if (identical(table, "Points")) {
    for (column in c("location", "value")) {
      parsed_numeric <- .record_parse_double(
        parsed[[column]],
        paste0("points.", column),
        allow_missing = identical(column, "value")
      )
      expected_numeric <- numeric[[column]]
      expected_numeric[expected_numeric == 0] <- 0
      if (!identical(parsed_numeric, expected_numeric)) {
        stop(
          "Staged bundle payload changed numeric field ",
          column,
          ".",
          call. = FALSE
        )
      }
    }
    parsed_rows <- .record_io_parse_integer(
      parsed$source_row,
      "points.source_row",
      allow_missing = TRUE,
      allow_zero = FALSE
    )
    if (!identical(parsed_rows, numeric$source_row)) {
      stop("Staged bundle payload changed point source rows.", call. = FALSE)
    }
  }
  invisible(parsed)
}

.record_io_make_manifest <- function(tables, hashes, delim) {
  fields <- c(
    Format = "measure-record",
    Version = "1",
    Delimiter = if (identical(delim, ",")) "csv" else "tsv"
  )
  for (table in names(.record_io_table_specs)) {
    fields[paste0(table, "-Rows")] <- as.character(nrow(tables[[table]]))
    fields[paste0(table, "-Columns")] <- as.character(ncol(tables[[table]]))
    fields[paste0(table, "-SHA256")] <- hashes[[table]]
  }
  if (!identical(names(fields), .record_io_manifest_fields)) {
    stop("Internal bundle manifest schema is inconsistent.", call. = FALSE)
  }
  fields
}

.record_io_write_manifest <- function(fields, path) {
  if (!identical(names(fields), .record_io_manifest_fields)) {
    stop("Internal bundle manifest schema is inconsistent.", call. = FALSE)
  }
  lines <- paste0(names(fields), ": ", unname(fields))
  con <- file(path, open = "wb")
  on.exit(close(con), add = TRUE)
  writeBin(charToRaw(paste0(paste(lines, collapse = "\n"), "\n")), con)
  invisible(path)
}

.record_io_parse_manifest <- function(raw) {
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
      length(keys) != length(.record_io_manifest_fields) ||
      !setequal(keys, .record_io_manifest_fields)
  ) {
    stop(
      "Bundle manifest has missing, duplicate, or unknown fields.",
      call. = FALSE
    )
  }
  fields <- unlist(dcf[1L, , drop = FALSE], use.names = TRUE)
  fields <- fields[.record_io_manifest_fields]
  if (!identical(fields[["Format"]], "measure-record")) {
    stop("Unsupported bundle format.", call. = FALSE)
  }
  if (!identical(fields[["Version"]], "1")) {
    stop("Unsupported measure-record bundle version.", call. = FALSE)
  }
  if (!(fields[["Delimiter"]] %in% c("csv", "tsv"))) {
    stop("Unsupported bundle delimiter.", call. = FALSE)
  }
  for (table in names(.record_io_table_specs)) {
    rows_key <- paste0(table, "-Rows")
    columns_key <- paste0(table, "-Columns")
    fields[[rows_key]] <- .record_io_parse_integer(
      fields[[rows_key]],
      paste0(table, " rows"),
      FALSE,
      TRUE
    )
    fields[[columns_key]] <- .record_io_parse_integer(
      fields[[columns_key]],
      paste0(table, " columns"),
      FALSE,
      FALSE
    )
    if (
      !grepl(
        "\\A[0-9a-f]{64}\\z",
        fields[[paste0(table, "-SHA256")]],
        perl = TRUE
      )
    ) {
      stop("Bundle manifest has an invalid SHA256 field.", call. = FALSE)
    }
    if (fields[[columns_key]] != length(.record_io_table_specs[[table]])) {
      stop(
        "Bundle manifest has an invalid field count for ",
        table,
        ".",
        call. = FALSE
      )
    }
  }
  fields
}

.record_io_parse_integer <- function(x, field, allow_missing, allow_zero) {
  if (!is.character(x) || !is.null(dim(x))) {
    stop(field, " must be represented as text.", call. = FALSE)
  }
  missing <- if (allow_missing) x == "" | x == "NA" else rep(FALSE, length(x))
  missing[is.na(missing)] <- FALSE
  tokens <- x[!missing]
  grammar <- if (allow_zero) {
    "\\A(?:0|[1-9][0-9]*)\\z"
  } else {
    "\\A[1-9][0-9]*\\z"
  }
  valid <- !is.na(tokens) & grepl(grammar, tokens, perl = TRUE, useBytes = TRUE)
  if (!all(valid)) {
    stop("Invalid integer in ", field, ".", call. = FALSE)
  }
  values <- suppressWarnings(as.double(tokens))
  if (!all(is.finite(values)) || any(values > .Machine$integer.max)) {
    stop(
      "Integer in ",
      field,
      " is outside the supported range.",
      call. = FALSE
    )
  }
  out <- rep(NA_integer_, length(x))
  out[!missing] <- as.integer(values)
  out
}

.record_io_parse_payload <- function(raw, table, delim, manifest) {
  parsed <- .record_parse_delim(raw, delim)
  if (!identical(names(parsed), .record_io_table_specs[[table]])) {
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
  parsed
}

.record_io_nullable_text <- function(x) {
  x[x == ""] <- NA_character_
  x
}

.record_io_build_record <- function(payloads) {
  sources <- payloads$Sources
  sources$source_name <- .record_io_nullable_text(sources$source_name)
  sources$source_sha256 <- .record_io_nullable_text(sources$source_sha256)
  channels <- payloads$Channels
  channels$channel_name <- .record_io_nullable_text(channels$channel_name)
  curves <- payloads$Curves
  for (column in c(
    "source_id",
    "axis_name",
    "axis_unit",
    "value_name",
    "value_unit"
  )) {
    curves[[column]] <- .record_io_nullable_text(curves[[column]])
  }

  points <- payloads$Points
  point_index <- .record_io_parse_integer(
    points$point_index,
    "points.point_index",
    FALSE,
    FALSE
  )
  location <- .record_parse_double(points$location, "points.location")
  value <- .record_parse_double(
    points$value,
    "points.value",
    allow_missing = TRUE
  )
  source_row <- .record_io_parse_integer(
    points$source_row,
    "points.source_row",
    TRUE,
    FALSE
  )
  if (anyDuplicated(curves$curve_id)) {
    stop("Bundle curves contain duplicate curve IDs.", call. = FALSE)
  }
  if (!all(points$curve_id %in% curves$curve_id)) {
    stop("Bundle points contain an orphan curve ID.", call. = FALSE)
  }

  measures <- vector("list", nrow(curves))
  source_rows <- vector("list", nrow(curves))
  for (i in seq_len(nrow(curves))) {
    selected <- which(points$curve_id == curves$curve_id[[i]])
    if (length(selected) == 0L) {
      stop("Bundle curve has no points.", call. = FALSE)
    }
    ordered <- selected[order(point_index[selected])]
    if (!identical(point_index[ordered], seq_len(length(selected)))) {
      stop(
        "Bundle point indices must be unique and contiguous within each curve.",
        call. = FALSE
      )
    }
    measures[[i]] <- new_measure_tbl(
      location = location[ordered],
      value = value[ordered]
    )
    source_rows[[i]] <- source_row[ordered]
  }
  curves$.measures <- measures
  curves$.source_rows <- source_rows
  record <- measure_record(
    samples = payloads$Samples,
    injections = payloads$Injections,
    channels = channels,
    curves = curves,
    sources = sources
  )
  validate_measure_record(record)
  record
}

.record_io_max_bytes <- function(x) {
  if (
    !is.numeric(x) ||
      is.complex(x) ||
      length(x) != 1L ||
      !is.null(dim(x)) ||
      is.na(x) ||
      !is.finite(x) ||
      x <= 0 ||
      x != floor(x) ||
      x > .Machine$integer.max - 1
  ) {
    stop(
      "max_bytes must be a positive whole number safely representable for bounded reads.",
      call. = FALSE
    )
  }
  as.integer(x)
}

.record_io_destination <- function(path) {
  if (
    !is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)
  ) {
    stop("path must be one non-empty local directory path.", call. = FALSE)
  }
  path <- path.expand(path)
  leaf <- basename(path)
  if (leaf %in% c("", ".", "..")) {
    stop("path must name a new directory, not its parent.", call. = FALSE)
  }
  parent <- dirname(path)
  if (!dir.exists(parent)) {
    stop("The parent directory of path must already exist.", call. = FALSE)
  }
  file.path(normalizePath(parent, mustWork = TRUE), leaf)
}

.record_io_read_directory <- function(path) {
  if (
    !is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)
  ) {
    stop("path must be one non-empty bundle directory path.", call. = FALSE)
  }
  path <- path.expand(path)
  if (!dir.exists(path)) {
    stop("path must name an existing bundle directory.", call. = FALSE)
  }
  normalizePath(path, mustWork = TRUE)
}

.record_io_is_symlink <- function(path) {
  target <- suppressWarnings(Sys.readlink(path))
  length(target) == 1L && !is.na(target) && nzchar(target)
}

.record_io_path_exists <- function(path) {
  file.exists(path) || .record_io_is_symlink(path)
}

.record_io_check_member <- function(path) {
  if (.record_io_is_symlink(path)) {
    stop("Bundle members must not be symbolic links.", call. = FALSE)
  }
  if (
    !isTRUE(fs::file_info(path, follow = TRUE, fail = FALSE)$type == "file")
  ) {
    stop("Bundle member must be an existing regular file.", call. = FALSE)
  }
  invisible(path)
}

.record_io_make_stage <- function(parent, leaf) {
  for (attempt in seq_len(100L)) {
    candidate <- tempfile(
      pattern = paste0(".", leaf, "-stage-"),
      tmpdir = parent
    )
    if (dir.create(candidate, showWarnings = FALSE, recursive = FALSE)) {
      return(candidate)
    }
  }
  stop("Could not create a staging directory beside path.", call. = FALSE)
}

.record_io_reserve_destination <- function(path) {
  dir.create(path, showWarnings = FALSE, recursive = FALSE)
}

.record_io_copy_file <- function(from, to) {
  if (.record_io_path_exists(to)) {
    stop("A destination bundle member appeared during writing.", call. = FALSE)
  }
  copied <- file.copy(from, to, overwrite = FALSE, copy.mode = FALSE)
  if (!isTRUE(copied)) {
    stop(
      "Could not copy a staged bundle member to the destination.",
      call. = FALSE
    )
  }
  if (
    .record_io_is_symlink(to) ||
      !isTRUE(fs::file_info(to, follow = TRUE, fail = FALSE)$type == "file")
  ) {
    stop(
      "Copied bundle member is not a regular file at its fixed path.",
      call. = FALSE
    )
  }
  invisible(to)
}
