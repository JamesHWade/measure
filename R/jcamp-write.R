#' Write one measurement-record curve as a JCAMP-DX IR spectrum
#'
#' `measure_write_jcamp()` writes one explicitly selected one-dimensional curve
#' from a validated [measure_record()] as a JCAMP-DX 4.24 IR spectrum. The
#' output uses the human-readable AFFN `##XYPOINTS= (XY..XY)` representation,
#' with one pair per line. Point order, irregular spacing, descending axes,
#' finite doubles, and missing signal values are retained. Missing signal
#' values are represented by the JCAMP-DX `?` token.
#'
#' The title and source unit declarations must be printable ASCII single-line
#' values. `origin` and `owner` may contain multiple printable ASCII lines;
#' CRLF and LF line endings are normalized to LF. Values are written literally
#' and are not inferred from record identities. In particular, this export
#' contains only the selected curve and does not include sample, injection,
#' channel, source, or other-curve identities.
#'
#' The destination must be new. The file is staged beside `path` and committed
#' without replacing an existing file, so validation or serialization errors do
#' not leave a partial destination file.
#'
#' @param record A validated [measure_record()] containing one-dimensional
#'   curves.
#' @param path A new local file path, usually ending in `.jdx` or `.dx`.
#' @param curve_id The exact identifier of the one curve to export.
#' @param title A printable ASCII, single-line JCAMP title.
#' @param origin A printable ASCII JCAMP origin declaration. CRLF and LF may
#'   separate literal continuation lines.
#' @param owner A printable ASCII JCAMP owner declaration. CRLF and LF may
#'   separate literal continuation lines.
#'
#' @return The normalized destination path, invisibly.
#' @references
#'   [IUPAC JCAMP-DX specifications](https://iupac.org/what-we-do/digital-standards/jcamp-dx/)
#' @export
measure_write_jcamp <- function(
  record,
  path,
  curve_id,
  title,
  origin,
  owner
) {
  validate_measure_record(record)
  if (!rlang::is_string(curve_id) || !is.null(dim(curve_id))) {
    cli::cli_abort("{.arg curve_id} must identify a single curve.")
  }
  i <- match(curve_id, record$curves$curve_id)
  if (is.na(i)) {
    cli::cli_abort(
      "No curve with ID {.val {curve_id}} exists in {.arg record}."
    )
  }
  curve <- record$curves[i, ]
  spectrum <- curve$.measures[[1L]]
  n <- nrow(spectrum)
  if (n < 1L || n > 1000000L) {
    cli::cli_abort("JCAMP-DX export requires between 1 and 1,000,000 points.")
  }

  destination <- .record_io_destination(path)
  if (.record_io_path_exists(destination)) {
    cli::cli_abort("Destination {.path {destination}} already exists.")
  }
  labels <- c(
    if (!is.na(curve$axis_name)) {
      jcamp_header("XLABEL", curve$axis_name, "axis_name")
    },
    if (!is.na(curve$value_name)) {
      jcamp_header("YLABEL", curve$value_name, "value_name")
    }
  )
  x <- .record_format_double(spectrum$location)
  y <- .record_format_double(spectrum$value)
  y[is.na(spectrum$value)] <- "?"
  lines <- c(
    jcamp_header("TITLE", title, "title"),
    "##JCAMP-DX= 4.24",
    "##DATA TYPE= INFRARED SPECTRUM",
    jcamp_header("ORIGIN", origin, "origin", multiline = TRUE),
    jcamp_header("OWNER", owner, "owner", multiline = TRUE),
    paste0("##NPOINTS= ", n),
    jcamp_header("XUNITS", curve$axis_unit, "axis_unit"),
    jcamp_header("YUNITS", curve$value_unit, "value_unit"),
    labels,
    "##XFACTOR= 1",
    "##YFACTOR= 1",
    paste0("##FIRSTX= ", x[1L]),
    paste0("##LASTX= ", x[n]),
    paste0("##FIRSTY= ", y[1L]),
    "##XYPOINTS= (XY..XY)",
    paste0(x, ",", y),
    "##END="
  )

  # The hard link publishes the finished file only if the name is still free.
  stage <- .record_io_make_stage(dirname(destination), basename(destination))
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  file <- file.path(stage, "spectrum.jdx")
  writeBin(charToRaw(paste0(paste(lines, collapse = "\n"), "\n")), file)
  fs::link_create(file, destination, symbolic = FALSE)
  invisible(normalizePath(destination, mustWork = TRUE))
}

jcamp_header <- function(label, value, field, multiline = FALSE) {
  if (!rlang::is_string(value) || !is.null(dim(value)) || !nzchar(value)) {
    cli::cli_abort("{.field {field}} must be a nonempty string.")
  }
  value <- gsub("\r\n", "\n", value, fixed = TRUE)
  if (!multiline && grepl("\n", value, fixed = TRUE)) {
    cli::cli_abort("{.field {field}} must fit on one line.")
  }
  lines <- strsplit(value, "\n", fixed = TRUE)[[1L]]
  if (any(grepl("[^ -~]", lines, perl = TRUE, useBytes = TRUE))) {
    cli::cli_abort("{.field {field}} must contain printable ASCII.")
  }
  if (
    !all(nzchar(trimws(lines))) ||
      any(grepl("$$", lines, fixed = TRUE)) ||
      any(startsWith(trimws(lines), "##"))
  ) {
    cli::cli_abort(
      "{.field {field}} contains a blank line or a JCAMP-DX header or comment marker."
    )
  }
  lines[1L] <- paste0("##", label, "= ", lines[1L])
  if (any(nchar(lines, type = "bytes") > 80L)) {
    cli::cli_abort(
      "{.field {field}} exceeds the JCAMP-DX limit of 80 characters per line."
    )
  }
  lines
}
