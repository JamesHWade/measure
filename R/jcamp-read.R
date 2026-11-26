#' Read an infrared JCAMP-DX spectrum
#'
#' Import one continuous infrared spectrum into a [measure_record()]. Supply
#' the identities from your experiment. The file title is descriptive metadata
#' and may be shared by many acquisitions.
#'
#' @param path Path to a local JCAMP-DX text file.
#' @param sample_id,injection_id,channel_id,curve_id,source_id Single, nonblank
#'   character identifiers for the sample, injection, detector channel, curve,
#'   and original file, respectively.
#' @param max_bytes Maximum input size in bytes. Defaults to 64 MiB.
#' @details The reader supports a single infrared block with plain numeric
#'   (AFFN) data, in either `XYPOINTS=(XY..XY)` or `XYDATA=(X++(Y..Y))`
#'   form. Explicit pairs keep their individual coordinates. For a regular
#'   grid the first coordinate, last coordinate, and point count define the
#'   axis. Declared scale factors apply to the stored numbers. An ordinate
#'   marked `?` becomes `NA_real_`. The record keeps the declared units and
#'   any explicit `XLABEL` and `YLABEL` axis and response names.
#'   The maximum spectrum length is one million points.
#'
#'   Use `measure_jcamp_headers()` to inspect declarations such as title,
#'   origin, owner, acquisition settings, and instrument-specific fields.
#'   Keep the original file for the complete acquisition metadata. Use
#'   [measure_write_jcamp()] to exchange one curve and [write_measure_record()]
#'   to save all five record tables.
#' @return `measure_read_jcamp()` returns a validated `measure_record`. The
#'   source table contains the input filename and SHA-256 digest of the bytes
#'   parsed. The source ordinals count spectral points in acquisition order.
#' @examples
#' path <- system.file("extdata", "ir-film.jdx", package = "measure")
#' headers <- measure_jcamp_headers(path)
#' headers[headers$label %in% c("TITLE", "ORIGIN", "OWNER"), ]
#' record <- measure_read_jcamp(path, "film", "scan-01", "ir", "film-ir", "file-01")
#' spectrum <- record$curves$.measures[[1]]
#' spectrum[which.max(spectrum$value), ]
#' @references [IUPAC JCAMP-DX specifications](https://iupac.org/what-we-do/digital-standards/jcamp-dx/)
#' @export
measure_read_jcamp <- function(
  path,
  sample_id,
  injection_id,
  channel_id,
  curve_id,
  source_id,
  max_bytes = 64 * 1024^2
) {
  ids <- list(
    sample_id = sample_id,
    injection_id = injection_id,
    channel_id = channel_id,
    curve_id = curve_id,
    source_id = source_id
  )
  for (field in names(ids)) {
    id <- ids[[field]]
    if (!rlang::is_string(id) || !is.null(dim(id)) || !nzchar(trimws(id))) {
      cli::cli_abort("{.arg {field}} must be a single, nonblank character ID.")
    }
  }
  bytes <- .record_read_raw(path, max_bytes)
  decoded <- jcamp_decode(jcamp_block(jcamp_lines(bytes)))
  measure_record(
    samples = tibble::tibble(sample_id = sample_id),
    injections = tibble::tibble(
      injection_id = injection_id,
      sample_id = sample_id
    ),
    channels = tibble::tibble(
      channel_id = channel_id,
      channel_name = NA_character_
    ),
    curves = tibble::tibble(
      curve_id = curve_id,
      injection_id = injection_id,
      channel_id = channel_id,
      source_id = source_id,
      axis_name = decoded$x_label %||% NA_character_,
      axis_unit = decoded$x_unit,
      value_name = decoded$y_label %||% NA_character_,
      value_unit = decoded$y_unit,
      .measures = list(new_measure_tbl(decoded$x, decoded$y)),
      .source_rows = list(seq_along(decoded$x))
    ),
    sources = tibble::tibble(
      source_id = source_id,
      source_name = basename(path),
      source_sha256 = digest::digest(bytes, algo = "sha256", serialize = FALSE)
    )
  )
}

#' @rdname measure_read_jcamp
#' @return `measure_jcamp_headers()` returns a tibble of `label` and `value`
#'   in source order. It retains original label spellings and repeat entries.
#'   Comments, surrounding whitespace, and data rows are omitted. Multiline
#'   declarations use a newline between lines. Reading headers does not
#'   validate the numeric spectrum.
#' @export
measure_jcamp_headers <- function(path, max_bytes = 64 * 1024^2) {
  block <- jcamp_block(jcamp_lines(.record_read_raw(path, max_bytes)))
  tibble::tibble(label = block$labels, value = unname(block$headers))
}

jcamp_lines <- function(bytes) {
  if (any(bytes == as.raw(0L))) {
    cli::cli_abort("JCAMP-DX text contains a NUL byte.")
  }
  text <- rawToChar(bytes)
  if (
    grepl("[^\\x09\\x0a\\x0d\\x20-\\x7e]", text, perl = TRUE, useBytes = TRUE)
  ) {
    cli::cli_abort("Expected printable ASCII JCAMP-DX text.")
  }
  strsplit(text, "\r\n|\r|\n", perl = TRUE)[[1L]]
}

jcamp_block <- function(lines) {
  lines <- trimws(sub("\\$\\$.*$", "", lines))
  lines <- lines[nzchar(lines)]
  starts <- which(startsWith(lines, "##"))
  if (!length(starts) || starts[1L] != 1L) {
    cli::cli_abort("Expected a JCAMP-DX block starting with TITLE.")
  }
  records <- substring(lines[starts], 3L)
  eq <- regexpr("=", records, fixed = TRUE)
  if (any(eq < 2L)) {
    cli::cli_abort("Expected a label and '=' in each JCAMP-DX declaration.")
  }
  labels <- trimws(substr(records, 1L, eq - 1L))
  # JCAMP labels ignore case, spaces, hyphens and underscores.
  keys <- chartr(
    "abcdefghijklmnopqrstuvwxyz",
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
    gsub("[ _-]", "", labels)
  )
  if (!all(nzchar(keys))) {
    cli::cli_abort("JCAMP-DX declarations must have a nonempty label.")
  }
  values <- trimws(substring(records, eq + 1L))
  if (
    keys[1L] != "TITLE" ||
      sum(keys == "TITLE") != 1L ||
      keys[length(keys)] != "END" ||
      sum(keys == "END") != 1L ||
      starts[length(starts)] != length(lines)
  ) {
    cli::cli_abort("Expected exactly one JCAMP-DX block from TITLE to END.")
  }
  tab <- which(keys %in% c("XYPOINTS", "XYDATA"))
  if (
    length(tab) != 1L ||
      any(
        keys %in%
          c(
            "NTUPLES",
            "DATATABLE",
            "PEAKTABLE",
            "PEAKASSIGNMENTS",
            "BLOCKS",
            "RADATA"
          )
      )
  ) {
    cli::cli_abort("Expected one continuous XYPOINTS or XYDATA spectrum.")
  }

  ends <- c(starts[-1L] - 1L, length(lines))
  for (i in which(ends > starts & seq_along(starts) != tab)) {
    values[i] <- paste(
      c(values[i], lines[(starts[i] + 1L):ends[i]]),
      collapse = "\n"
    )
  }
  rows <- starts[tab] + seq_len(ends[tab] - starts[tab])
  list(
    headers = stats::setNames(values, keys),
    labels = labels,
    lines = lines[rows],
    mode = keys[tab]
  )
}
