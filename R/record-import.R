#' Import a measurement record from a long table
#'
#' `measure_record_from_long()` turns an already parsed long table into a
#' relational analytical record. Explicit column mappings define sample,
#' injection, detector channel, curve, axis, and signal identity. Identifier
#' and label columns must already be character vectors; values are not inferred
#' from their appearance, and units are retained only as declarations.
#'
#' Curves and related entities retain first-appearance order. Points retain
#' their occurrence order within each curve, including interleaved curves.
#' Each curve may have its own finite, strictly monotone location grid. Values
#' must be finite numeric values or `NA`; locations must be finite numeric
#' values. The function does not sort points, infer missing metadata from other
#' rows, infer units, or perform locale conversion. Conflicting declarations,
#' including missing-versus-known mixtures within an entity, are errors. Its
#' source record contains the caller-declared `source_id` only; this conversion
#' does not compute a source digest or claim a complete transformation history.
#'
#' Unmapped input columns are errors by default. Set `allow_extra_columns =
#' TRUE` to explicitly ignore them. This API handles one-dimensional
#' measurement curves. It does not import nD data, DAD/MS vendor structures, or
#' vendor-specific file formats.
#'
#' @param data A data frame containing one row per point in long form.
#' @param columns A named character vector mapping canonical fields to existing
#'   input column names. Required names are `sample_id`, `injection_id`,
#'   `channel_id`, `curve_id`, `location`, and `value`. Optional names are
#'   `channel_name`, `axis_name`, `axis_unit`, `value_name`, and `value_unit`.
#'   Input columns cannot be reused for different fields.
#' @param source_id A caller-declared, nonblank character source identifier.
#' @param allow_extra_columns Set to `TRUE` to ignore input columns not named
#'   in `columns`.
#'
#' @return A `measure_record` containing samples, injections, channels, curves,
#'   and one source declaration.
#' @examples
#' long_data <- tibble::tibble(
#'   sample_id = c("sample-01", "sample-01"),
#'   injection_id = c("injection-01", "injection-01"),
#'   channel_id = c("uv", "uv"),
#'   curve_id = c("curve-01", "curve-01"),
#'   location = c(250, 251),
#'   value = c(0.02, NA_real_)
#' )
#' long_columns <- c(
#'   sample_id = "sample_id",
#'   injection_id = "injection_id",
#'   channel_id = "channel_id",
#'   curve_id = "curve_id",
#'   location = "location",
#'   value = "value"
#' )
#' record <- measure_record_from_long(long_data, long_columns, "source-01")
#' record
#' @export
measure_record_from_long <- function(
  data,
  columns,
  source_id,
  allow_extra_columns = FALSE
) {
  .record_import_validate_source_id(source_id)
  .record_import_validate_allow_extra(allow_extra_columns)
  mapped <- .record_import_validate_columns(data, columns, allow_extra_columns)

  required_ids <- c("sample_id", "injection_id", "channel_id", "curve_id")
  id_values <- lapply(required_ids, function(field) {
    .record_import_character_field(data, mapped[[field]], field)
  })
  names(id_values) <- required_ids

  for (field in required_ids) {
    value <- id_values[[field]]
    if (anyNA(value) || !all(nzchar(trimws(value)))) {
      stop(
        "`",
        field,
        "` values must be non-missing and nonblank.",
        call. = FALSE
      )
    }
  }

  optional <- c(
    "channel_name",
    "axis_name",
    "axis_unit",
    "value_name",
    "value_unit"
  )
  text_values <- stats::setNames(
    lapply(optional, function(field) {
      if (is.na(mapped[[field]])) {
        rep(NA_character_, nrow(data))
      } else {
        .record_import_character_field(data, mapped[[field]], field)
      }
    }),
    optional
  )

  location <- .record_import_numeric_field(
    data[[mapped[["location"]]]],
    "location"
  )
  value <- .record_import_numeric_field(data[[mapped[["value"]]]], "value")
  if (anyNA(location)) {
    row <- which(is.na(location))[1L]
    stop(
      "`location` must be finite and non-missing at row ",
      row,
      ".",
      call. = FALSE
    )
  }
  if (any(is.nan(value) | is.infinite(value))) {
    row <- which(is.nan(value) | is.infinite(value))[1L]
    stop("`value` must be finite or missing at row ", row, ".", call. = FALSE)
  }

  sample_id <- id_values$sample_id
  injection_id <- id_values$injection_id
  channel_id <- id_values$channel_id
  curve_id <- id_values$curve_id
  sample_ids <- unique(sample_id)
  injection_ids <- unique(injection_id)
  channel_ids <- unique(channel_id)
  curve_ids <- unique(curve_id)

  injection_samples <- vapply(
    injection_ids,
    function(id) {
      rows <- which(injection_id == id)
      .record_import_constant(sample_id, rows, "sample_id", "injection", id)
    },
    character(1)
  )
  channel_names <- vapply(
    channel_ids,
    function(id) {
      rows <- which(channel_id == id)
      .record_import_constant(
        text_values$channel_name,
        rows,
        "channel_name",
        "channel",
        id
      )
    },
    character(1)
  )

  curve_rows <- lapply(curve_ids, function(id) which(curve_id == id))
  curve_injections <- vapply(
    seq_along(curve_ids),
    function(i) {
      .record_import_constant(
        injection_id,
        curve_rows[[i]],
        "injection_id",
        "curve",
        curve_ids[[i]]
      )
    },
    character(1)
  )
  curve_channels <- vapply(
    seq_along(curve_ids),
    function(i) {
      .record_import_constant(
        channel_id,
        curve_rows[[i]],
        "channel_id",
        "curve",
        curve_ids[[i]]
      )
    },
    character(1)
  )
  curve_metadata <- lapply(
    c("axis_name", "axis_unit", "value_name", "value_unit"),
    function(field) {
      vapply(
        seq_along(curve_ids),
        function(i) {
          .record_import_constant(
            text_values[[field]],
            curve_rows[[i]],
            field,
            "curve",
            curve_ids[[i]]
          )
        },
        character(1)
      )
    }
  )
  names(curve_metadata) <- c(
    "axis_name",
    "axis_unit",
    "value_name",
    "value_unit"
  )

  measures <- lapply(curve_rows, function(rows) {
    new_measure_tbl(location = location[rows], value = value[rows])
  })
  source_rows <- lapply(curve_rows, as.integer)

  samples <- tibble::tibble(sample_id = sample_ids)
  injections <- tibble::tibble(
    injection_id = injection_ids,
    sample_id = unname(injection_samples)
  )
  channels <- tibble::tibble(
    channel_id = channel_ids,
    channel_name = unname(channel_names)
  )
  curves <- tibble::tibble(
    curve_id = curve_ids,
    injection_id = unname(curve_injections),
    channel_id = unname(curve_channels),
    source_id = rep(source_id, length(curve_ids)),
    axis_name = unname(curve_metadata$axis_name),
    axis_unit = unname(curve_metadata$axis_unit),
    value_name = unname(curve_metadata$value_name),
    value_unit = unname(curve_metadata$value_unit),
    .measures = new_measure_list(measures),
    .source_rows = source_rows
  )
  sources <- tibble::tibble(
    source_id = source_id,
    source_name = NA_character_,
    source_sha256 = NA_character_
  )

  measure_record(
    samples = samples,
    injections = injections,
    channels = channels,
    curves = curves,
    sources = sources
  )
}

#' Read one CSV or TSV file into a measurement record
#'
#' `read_measure_record_delim()` reads a single local CSV or TSV long table
#' with strict UTF-8 and fixed column names, then delegates record construction
#' to [measure_record_from_long()]. It reads the source bytes once, enforces
#' `max_bytes` while reading, and records the caller-declared source ID, the
#' input basename, and a SHA-256 digest of those exact bytes. The digest helps
#' detect accidental changes; it does not authenticate the source or record
#' the complete mapping and transformation history.
#'
#' All fields are initially read as character. Identifiers such as `0007`,
#' `1e3`, `NA`, and formula-like strings remain literal text. Numeric fields
#' use a strict, locale-independent decimal grammar: locations must be finite,
#' and signal values may be finite or missing. Blank optional metadata cells
#' become unknown (`NA_character_`); literal `NA` remains text. Mappings,
#' units, and identities are never inferred. Curves may have independent
#' finite, monotone grids and are not sorted or interpolated.
#'
#' Unmapped columns are rejected unless `allow_extra_columns = TRUE`, which
#' explicitly opts into ignoring them. This reader covers one-dimensional
#' curves in CSV/TSV text; it does not parse nD data, DAD/MS vendor structures,
#' or vendor-specific formats.
#'
#' @param file A local regular CSV or TSV file path.
#' @param columns A named character vector mapping canonical fields to exact
#'   input header names. See [measure_record_from_long()] for the required and
#'   optional canonical names.
#' @param source_id A caller-declared, nonblank character source identifier.
#' @param delim A comma or tab field delimiter. No delimiter inference is
#'   performed.
#' @param allow_extra_columns Set to `TRUE` to ignore columns not named in
#'   `columns`.
#' @param max_bytes A positive whole-number byte limit for the single source
#'   file.
#'
#' @return A `measure_record` with one source row containing the input basename
#'   and SHA-256 digest.
#' @examples
#' csv_file <- tempfile(fileext = ".csv")
#' writeLines(
#'   c(
#'     "sample_id,injection_id,channel_id,curve_id,location,value",
#'     "sample-01,injection-01,uv,curve-01,250,0.02",
#'     "sample-01,injection-01,uv,curve-01,251,0.04"
#'   ),
#'   csv_file,
#'   useBytes = TRUE
#' )
#' csv_columns <- c(
#'   sample_id = "sample_id",
#'   injection_id = "injection_id",
#'   channel_id = "channel_id",
#'   curve_id = "curve_id",
#'   location = "location",
#'   value = "value"
#' )
#' imported <- read_measure_record_delim(csv_file, csv_columns, "source-01")
#' imported
#' unlink(csv_file)
#' @export
read_measure_record_delim <- function(
  file,
  columns,
  source_id,
  delim = c(",", "\t"),
  allow_extra_columns = FALSE,
  max_bytes = 100 * 1024^2
) {
  .record_import_validate_source_id(source_id)
  .record_import_validate_allow_extra(allow_extra_columns)
  delim <- match.arg(delim)
  raw <- .record_read_raw(file, max_bytes)
  data <- .record_parse_delim(raw, delim)
  mapped <- .record_import_validate_columns(data, columns, allow_extra_columns)

  for (field in c("location", "value")) {
    input_name <- mapped[[field]]
    data[[input_name]] <- .record_parse_double(
      data[[input_name]],
      field,
      allow_missing = identical(field, "value")
    )
  }
  for (field in c(
    "channel_name",
    "axis_name",
    "axis_unit",
    "value_name",
    "value_unit"
  )) {
    input_name <- mapped[[field]]
    if (!is.na(input_name)) {
      blank <- !is.na(data[[input_name]]) & data[[input_name]] == ""
      data[[input_name]][blank] <- NA_character_
    }
  }

  out <- measure_record_from_long(
    data = data,
    columns = columns,
    source_id = source_id,
    allow_extra_columns = allow_extra_columns
  )
  out$sources$source_name <- basename(file)
  out$sources$source_sha256 <- digest::digest(
    raw,
    algo = "sha256",
    serialize = FALSE
  )
  out <- validate_measure_record(out)
  out
}

.record_import_validate_columns <- function(
  data,
  columns,
  allow_extra_columns
) {
  .record_import_validate_allow_extra(allow_extra_columns)
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  input_names <- names(data)
  if (is.null(input_names) || anyNA(input_names) || !all(nzchar(input_names))) {
    stop("Input columns must have non-empty, non-missing names.", call. = FALSE)
  }
  if (anyDuplicated(input_names)) {
    stop("Input column names must be unique.", call. = FALSE)
  }
  if (
    !is.character(columns) ||
      !is.null(dim(columns)) ||
      is.object(columns) ||
      is.null(names(columns))
  ) {
    stop("`columns` must be a named character vector.", call. = FALSE)
  }
  canonical <- names(columns)
  if (anyNA(canonical) || !all(nzchar(canonical)) || anyDuplicated(canonical)) {
    stop(
      "`columns` must have unique, non-empty canonical names.",
      call. = FALSE
    )
  }

  required <- c(
    "sample_id",
    "injection_id",
    "channel_id",
    "curve_id",
    "location",
    "value"
  )
  optional <- c(
    "channel_name",
    "axis_name",
    "axis_unit",
    "value_name",
    "value_unit"
  )
  unknown <- setdiff(canonical, c(required, optional))
  if (length(unknown) > 0L) {
    stop(
      "Unknown canonical mapping name: `",
      unknown[[1L]],
      "`.",
      call. = FALSE
    )
  }
  missing <- setdiff(required, canonical)
  if (length(missing) > 0L) {
    stop("Missing required mapping: `", missing[[1L]], "`.", call. = FALSE)
  }
  if (anyNA(columns) || !all(nzchar(columns))) {
    stop("Mapped input column names must be non-empty strings.", call. = FALSE)
  }
  if (anyDuplicated(unname(columns))) {
    stop(
      "An input column cannot be mapped to more than one canonical field.",
      call. = FALSE
    )
  }
  absent <- setdiff(unname(columns), input_names)
  if (length(absent) > 0L) {
    stop(
      "Mapped input column does not exist: `",
      absent[[1L]],
      "`.",
      call. = FALSE
    )
  }
  unmapped <- setdiff(input_names, unname(columns))
  if (length(unmapped) > 0L && !allow_extra_columns) {
    stop(
      "Unmapped input columns require `allow_extra_columns = TRUE`: ",
      paste0("`", unmapped, "`", collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  out <- stats::setNames(
    rep(NA_character_, length(required) + length(optional)),
    c(required, optional)
  )
  out[canonical] <- unname(columns)
  out
}

.record_import_validate_source_id <- function(source_id) {
  if (
    !is.character(source_id) ||
      length(source_id) != 1L ||
      is.na(source_id) ||
      !nzchar(trimws(source_id))
  ) {
    stop("`source_id` must be one nonblank character string.", call. = FALSE)
  }
  invisible(source_id)
}

.record_import_validate_allow_extra <- function(allow_extra_columns) {
  if (
    !is.logical(allow_extra_columns) ||
      length(allow_extra_columns) != 1L ||
      is.na(allow_extra_columns)
  ) {
    stop("`allow_extra_columns` must be TRUE or FALSE.", call. = FALSE)
  }
  invisible(allow_extra_columns)
}

.record_import_character_field <- function(data, input_name, field) {
  value <- data[[input_name]]
  if (!is.character(value) || !is.null(dim(value))) {
    stop(
      "Mapped field `",
      field,
      "` must be a character vector; factors and inferred labels are not supported.",
      call. = FALSE
    )
  }
  unname(value)
}

.record_import_numeric_field <- function(x, field) {
  if (is.object(x) || (!is.integer(x) && !is.double(x)) || !is.null(dim(x))) {
    stop("Mapped field `", field, "` must already be numeric.", call. = FALSE)
  }
  unname(as.double(x))
}

.record_import_constant <- function(x, rows, field, entity, id) {
  values <- x[rows]
  first <- values[[1L]]
  agrees <- if (is.na(first)) {
    all(is.na(values))
  } else {
    all(!is.na(values) & values == first)
  }
  if (!agrees) {
    stop(
      "Conflicting `",
      field,
      "` declarations for ",
      entity,
      " `",
      id,
      "`, including missing-versus-known mixtures.",
      call. = FALSE
    )
  }
  first
}
