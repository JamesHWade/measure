#' Create and validate an analytical measurement record
#'
#' A `measure_record` connects samples, injections, channels, sources, and
#' independent one-dimensional curves. Curve payloads remain ordinary
#' `measure_list` data (see [new_measure_list()]); the record adds explicit identities and declared axis
#' and signal metadata without changing or aligning the points.
#'
#' Each table has a fixed schema. `samples` has one row per `sample_id`;
#' `injections` has one row per `injection_id` and refers to a sample;
#' `channels` has one row per `channel_id`; `sources` has one row per
#' `source_id`; and `curves` has one row per `curve_id` and refers to one
#' injection and channel. A curve may also refer to a declared source.
#' Unused sample, injection, channel, and source rows are retained.
#'
#' IDs are explicit UTF-8 character strings. They are not trimmed, inferred,
#' or generated. A missing `source_id` means that the curve has no declared
#' source; nonmissing source IDs must be present in `sources`. Units and names
#' are opaque declarations: this function does not infer or convert units.
#'
#' Each curve payload must contain only numeric `location` and `value`
#' columns. Locations must be finite and strictly monotonic in either
#' direction. Values may be finite or `NA_real_`. Curves may have different
#' lengths, spacing, and direction. The `.source_rows` list column stores
#' optional positive parsed-record ordinals aligned to each point; missing
#' ordinals are represented by `NA_integer_` and are not physical text line
#' numbers. The constructor converts integer location and value vectors to
#' double and normalizes negative zero to positive zero. Revalidation rejects
#' a record mutated to contain negative zero.
#'
#' `NULL` tables create typed empty tables. When a table is supplied, required
#' columns must be present; omitted nullable columns are filled with typed
#' missing values. Unknown and duplicate columns are rejected. The input
#' objects are not modified.
#'
#' @param samples A data frame with the required `sample_id` column, or
#'   `NULL` for an empty table.
#' @param injections A data frame with required `injection_id` and `sample_id`
#'   columns, or `NULL` for an empty table.
#' @param channels A data frame with required `channel_id` and optional
#'   `channel_name` columns, or `NULL` for an empty table.
#' @param curves A data frame with required `curve_id`, `injection_id`,
#'   `channel_id`, and `.measures` columns, or `NULL` for an empty table.
#'   Nullable metadata columns and `.source_rows` may be omitted.
#' @param sources A data frame with required `source_id` and optional
#'   `source_name` and `source_sha256` columns, or `NULL` for an empty table.
#'
#' @return A plain S3 list with class `measure_record` and exactly five
#'   canonical tibble components: `samples`, `injections`, `channels`,
#'   `curves`, and `sources`.
#'
#' @examples
#' curve <- new_measure_tbl(
#'   location = c(250, 251, 252),
#'   value = c(0.02, NA_real_, 0.04)
#' )
#' record <- measure_record(
#'   samples = tibble::tibble(sample_id = "sample-01"),
#'   injections = tibble::tibble(
#'     injection_id = "injection-01",
#'     sample_id = "sample-01"
#'   ),
#'   channels = tibble::tibble(channel_id = "uv", channel_name = "UV"),
#'   curves = tibble::tibble(
#'     curve_id = "curve-01",
#'     injection_id = "injection-01",
#'     channel_id = "uv",
#'     .measures = list(curve)
#'   )
#' )
#' record
#'
#' @export
measure_record <- function(
  samples = NULL,
  injections = NULL,
  channels = NULL,
  curves = NULL,
  sources = NULL
) {
  samples <- .record_canonical_table(
    samples,
    table = "samples",
    columns = c("sample_id"),
    required = c("sample_id")
  )
  injections <- .record_canonical_table(
    injections,
    table = "injections",
    columns = c("injection_id", "sample_id"),
    required = c("injection_id", "sample_id")
  )
  channels <- .record_canonical_table(
    channels,
    table = "channels",
    columns = c("channel_id", "channel_name"),
    required = c("channel_id")
  )
  curves <- .record_canonical_table(
    curves,
    table = "curves",
    columns = c(
      "curve_id",
      "injection_id",
      "channel_id",
      "source_id",
      "axis_name",
      "axis_unit",
      "value_name",
      "value_unit",
      ".measures",
      ".source_rows"
    ),
    required = c("curve_id", "injection_id", "channel_id", ".measures")
  )
  sources <- .record_canonical_table(
    sources,
    table = "sources",
    columns = c("source_id", "source_name", "source_sha256"),
    required = c("source_id")
  )

  out <- structure(
    list(
      samples = samples,
      injections = injections,
      channels = channels,
      curves = curves,
      sources = sources
    ),
    class = c("measure_record", "list")
  )

  .record_validate(out)
  out
}

#' Test whether an object is a measurement record
#'
#' @param x Object to test.
#'
#' @return `TRUE` if `x` inherits from `measure_record`; otherwise `FALSE`.
#'   This is a class check and does not validate mutable table contents.
#'
#' @examples
#' is_measure_record(measure_record())
#' is_measure_record(list())
#'
#' @export
is_measure_record <- function(x) {
  inherits(x, "measure_record")
}

#' Validate a measurement record
#'
#' Rechecks the full table schema, key and foreign-key constraints, curve
#' payloads, and source-row references. This is useful after editing a record's
#' mutable tibble components.
#'
#' @param x An object created by [measure_record()].
#'
#' @return `x`, invisibly, if valid. Invalid records raise a cli error.
#'
#' @examples
#' record <- measure_record()
#' validate_measure_record(record)
#'
#' @export
validate_measure_record <- function(x) {
  .record_validate(x)
  invisible(x)
}

#' @rdname measure_record
#' @param x A measurement record to print.
#' @param ... Additional arguments; currently unused.
#' @export
print.measure_record <- function(x, ...) {
  validate_measure_record(x)
  cat("<measure_record>\n")
  cat("  Samples: ", nrow(x$samples), "\n", sep = "")
  cat("  Injections: ", nrow(x$injections), "\n", sep = "")
  cat("  Channels: ", nrow(x$channels), "\n", sep = "")
  cat("  Curves: ", nrow(x$curves), "\n", sep = "")
  cat("  Sources: ", nrow(x$sources), "\n", sep = "")
  invisible(x)
}

.record_canonical_table <- function(x, table, columns, required) {
  if (is.null(x)) {
    return(.record_empty_table(table))
  }

  if (!is.data.frame(x)) {
    cli::cli_abort(
      "Table {.field {table}} must be a data frame or {.code NULL}."
    )
  }

  input_names <- names(x)
  if (
    is.null(input_names) ||
      length(input_names) != ncol(x) ||
      anyNA(input_names) ||
      any(input_names == "")
  ) {
    cli::cli_abort("Table {.field {table}} must have named columns.")
  }
  if (anyDuplicated(input_names)) {
    duplicate <- input_names[duplicated(input_names)][[1]]
    cli::cli_abort(
      "Table {.field {table}} has duplicate column {.field {duplicate}}."
    )
  }

  unknown <- setdiff(input_names, columns)
  if (length(unknown) > 0) {
    cli::cli_abort(
      "Table {.field {table}} has unknown column{?s}: {.field {unknown}}."
    )
  }
  missing <- setdiff(required, input_names)
  if (length(missing) > 0) {
    cli::cli_abort(
      "Table {.field {table}} is missing required column{?s}: {.field {missing}}."
    )
  }

  n <- nrow(x)
  out <- vector("list", length(columns))
  names(out) <- columns
  for (column in columns) {
    if (column %in% input_names) {
      out[[column]] <- x[[column]]
    } else if (
      identical(table, "curves") && identical(column, ".source_rows")
    ) {
      out[[column]] <- rep(list(NULL), n)
    } else if (identical(column, ".measures")) {
      out[[column]] <- new_measure_list()
    } else {
      out[[column]] <- rep(NA_character_, n)
    }
  }

  if (identical(table, "curves")) {
    out$.measures <- .record_canonical_measures(out$.measures)
    if (!".source_rows" %in% input_names) {
      out$.source_rows <- lapply(
        seq_along(out$.measures),
        function(i) rep(NA_integer_, nrow(out$.measures[[i]]))
      )
    }
  }

  if (identical(table, "samples")) {
    out$sample_id <- .record_text(out$sample_id, "samples$sample_id")
  } else if (identical(table, "injections")) {
    out$injection_id <- .record_text(
      out$injection_id,
      "injections$injection_id"
    )
    out$sample_id <- .record_text(out$sample_id, "injections$sample_id")
  } else if (identical(table, "channels")) {
    out$channel_id <- .record_text(out$channel_id, "channels$channel_id")
    out$channel_name <- .record_text(
      out$channel_name,
      "channels$channel_name",
      nullable = TRUE
    )
  } else if (identical(table, "sources")) {
    out$source_id <- .record_text(out$source_id, "sources$source_id")
    out$source_name <- .record_text(
      out$source_name,
      "sources$source_name",
      nullable = TRUE
    )
    out$source_sha256 <- .record_text(
      out$source_sha256,
      "sources$source_sha256",
      nullable = TRUE
    )
  } else {
    out$curve_id <- .record_text(out$curve_id, "curves$curve_id")
    out$injection_id <- .record_text(out$injection_id, "curves$injection_id")
    out$channel_id <- .record_text(out$channel_id, "curves$channel_id")
    for (column in c(
      "source_id",
      "axis_name",
      "axis_unit",
      "value_name",
      "value_unit"
    )) {
      out[[column]] <- .record_text(
        out[[column]],
        paste0("curves$", column),
        nullable = TRUE
      )
    }
    out$.source_rows <- .record_canonical_source_rows(
      out$.source_rows,
      out$.measures
    )
  }

  out <- tibble::as_tibble(out, .name_repair = "minimal")
  out
}

.record_empty_table <- function(table) {
  if (identical(table, "samples")) {
    return(tibble::tibble(sample_id = character()))
  }
  if (identical(table, "injections")) {
    return(tibble::tibble(
      injection_id = character(),
      sample_id = character()
    ))
  }
  if (identical(table, "channels")) {
    return(tibble::tibble(
      channel_id = character(),
      channel_name = character()
    ))
  }
  if (identical(table, "curves")) {
    return(tibble::tibble(
      curve_id = character(),
      injection_id = character(),
      channel_id = character(),
      source_id = character(),
      axis_name = character(),
      axis_unit = character(),
      value_name = character(),
      value_unit = character(),
      .measures = new_measure_list(),
      .source_rows = list()
    ))
  }
  tibble::tibble(
    source_id = character(),
    source_name = character(),
    source_sha256 = character()
  )
}

.record_text <- function(x, field, nullable = FALSE) {
  if (typeof(x) != "character" || is.object(x) || !is.null(dim(x))) {
    cli::cli_abort(
      "{.field {field}} must be a one-dimensional character vector."
    )
  }
  if (length(x) == 0L) {
    return(character())
  }

  x <- unname(enc2utf8(x))
  if (any(Encoding(x) == "bytes")) {
    cli::cli_abort(
      "{.field {field}} must contain UTF-8 strings, not byte-marked text."
    )
  }
  if (any(!is.na(x) & !validUTF8(x))) {
    cli::cli_abort("{.field {field}} must contain valid UTF-8 strings.")
  }
  if (!nullable && anyNA(x)) {
    cli::cli_abort("{.field {field}} must not contain missing values.")
  }
  blank <- vapply(
    x,
    function(value) {
      !is.na(value) && .record_is_blank(value)
    },
    logical(1)
  )
  if (any(blank)) {
    cli::cli_abort("{.field {field}} must not contain blank strings.")
  }
  x
}

.record_canonical_measures <- function(x) {
  if (inherits(x, "measure_nd_list") || inherits(x, "measure_nd_tbl")) {
    cli::cli_abort(
      "Column {.field curves$.measures} does not support nD payloads."
    )
  }
  if (!is.list(x) || is.data.frame(x) || !is.null(dim(x))) {
    cli::cli_abort(
      "Column {.field curves$.measures} must be a {.cls measure_list} or a list of 1D data frames."
    )
  }

  payloads <- lapply(seq_along(x), function(i) {
    payload <- x[[i]]
    if (inherits(payload, "measure_nd_tbl")) {
      cli::cli_abort(
        "Curve {i} has an nD payload; only 1D curves are supported."
      )
    }
    if (!is.data.frame(payload)) {
      cli::cli_abort(
        "Curve {i} payload must be a data frame with location and value columns."
      )
    }
    payload_names <- names(payload)
    if (
      is.null(payload_names) ||
        anyNA(payload_names) ||
        any(payload_names == "") ||
        anyDuplicated(payload_names)
    ) {
      cli::cli_abort("Curve {i} payload must have unique named columns.")
    }
    if (!identical(payload_names, c("location", "value"))) {
      cli::cli_abort(
        "Curve {i} payload must contain only {.field location} and {.field value} columns."
      )
    }
    location <- .record_numeric(
      payload$location,
      paste0("curve ", i, " location"),
      allow_missing = FALSE,
      canonicalize_integer = TRUE
    )
    value <- .record_numeric(
      payload$value,
      paste0("curve ", i, " value"),
      allow_missing = TRUE,
      canonicalize_integer = TRUE
    )
    if (length(location) != length(value)) {
      cli::cli_abort(
        "Curve {i} location and value columns must have equal lengths."
      )
    }
    if (length(location) == 0L) {
      cli::cli_abort("Curve {i} must contain at least one point.")
    }
    if (length(location) > 1L) {
      increasing <- all(location[-1L] > location[-length(location)])
      decreasing <- all(location[-1L] < location[-length(location)])
      if (!increasing && !decreasing) {
        cli::cli_abort(
          "Curve {i} locations must be strictly increasing or strictly decreasing."
        )
      }
    }
    new_measure_tbl(location = location, value = value)
  })

  new_measure_list(payloads)
}

.record_numeric <- function(
  x,
  field,
  allow_missing,
  canonicalize_integer = FALSE
) {
  if (
    !(typeof(x) %in% c("integer", "double")) || is.object(x) || !is.null(dim(x))
  ) {
    cli::cli_abort("{.field {field}} must be an unclassed numeric vector.")
  }
  if (typeof(x) == "integer" && !canonicalize_integer) {
    cli::cli_abort("{.field {field}} must be stored as double.")
  }
  x <- as.double(unname(x))
  if (!allow_missing && anyNA(x)) {
    cli::cli_abort("{.field {field}} must not contain missing or NaN values.")
  }
  if (any(is.nan(x))) {
    cli::cli_abort("{.field {field}} must not contain NaN values.")
  }
  reciprocal <- suppressWarnings(1 / x)
  negative_zero <- !is.na(x) & x == 0 & reciprocal < 0
  if (any(negative_zero)) {
    if (canonicalize_integer) {
      x[which(negative_zero)] <- 0
    } else {
      cli::cli_abort("{.field {field}} must use canonical positive zero.")
    }
  }
  present <- !is.na(x)
  if (!all(is.finite(x[present]))) {
    cli::cli_abort(
      "{.field {field}} must contain only finite values or allowed missing values."
    )
  }
  x
}

.record_is_blank <- function(x) {
  codepoints <- utf8ToInt(x)
  whitespace <- c(
    0x0009:0x000D,
    0x0020,
    0x0085,
    0x00A0,
    0x1680,
    0x2000:0x200A,
    0x2028,
    0x2029,
    0x202F,
    0x205F,
    0x3000
  )
  all(codepoints %in% whitespace)
}

.record_canonical_source_rows <- function(x, measures) {
  is_plain_input <- is.list(x) &&
    is.null(dim(x)) &&
    (!is.object(x) || inherits(x, "AsIs"))
  if (!is_plain_input || length(x) != length(measures)) {
    cli::cli_abort(
      "Column {.field curves$.source_rows} must be a list with one vector per curve."
    )
  }
  lapply(seq_along(x), function(i) {
    rows <- x[[i]]
    if (
      !(typeof(rows) %in% c("integer", "double")) ||
        is.object(rows) ||
        !is.null(dim(rows))
    ) {
      cli::cli_abort("Curve {i} source rows must be an integer vector.")
    }
    rows <- unname(rows)
    if (typeof(rows) == "double") {
      if (any(is.nan(rows))) {
        cli::cli_abort("Curve {i} source rows cannot contain NaN values.")
      }
      if (
        any(
          !is.na(rows) &
            (!is.finite(rows) |
              rows != trunc(rows) |
              rows > .Machine$integer.max |
              rows < 1)
        )
      ) {
        cli::cli_abort(
          "Curve {i} source rows must be positive integers or missing."
        )
      }
      rows <- as.integer(rows)
    }
    if (length(rows) != nrow(measures[[i]])) {
      cli::cli_abort(
        "Curve {i} source rows must align with its {nrow(measures[[i]])} points."
      )
    }
    if (any(!is.na(rows) & rows < 1L)) {
      cli::cli_abort("Curve {i} source rows must be positive or missing.")
    }
    known <- rows[!is.na(rows)]
    if (anyDuplicated(known)) {
      cli::cli_abort("Curve {i} source rows must be unique within the curve.")
    }
    rows
  })
}

.record_validate <- function(x) {
  if (!is.list(x) || !identical(class(x), c("measure_record", "list"))) {
    cli::cli_abort("{.arg x} must be a {.cls measure_record} object.")
  }
  components <- c("samples", "injections", "channels", "curves", "sources")
  if (!identical(names(x), components)) {
    cli::cli_abort(
      "A {.cls measure_record} must have exactly these components in order: {.field {components}}."
    )
  }
  schemas <- list(
    samples = c("sample_id"),
    injections = c("injection_id", "sample_id"),
    channels = c("channel_id", "channel_name"),
    curves = c(
      "curve_id",
      "injection_id",
      "channel_id",
      "source_id",
      "axis_name",
      "axis_unit",
      "value_name",
      "value_unit",
      ".measures",
      ".source_rows"
    ),
    sources = c("source_id", "source_name", "source_sha256")
  )
  for (table in components) {
    current <- x[[table]]
    if (
      !tibble::is_tibble(current) ||
        !identical(names(current), schemas[[table]])
    ) {
      cli::cli_abort(
        "Component {.field {table}} must be a tibble with canonical columns {.field {schemas[[table]]}}."
      )
    }
  }

  .record_validate_text_column(x$samples$sample_id, "samples$sample_id")
  .record_validate_text_column(
    x$injections$injection_id,
    "injections$injection_id"
  )
  .record_validate_text_column(x$injections$sample_id, "injections$sample_id")
  .record_validate_text_column(x$channels$channel_id, "channels$channel_id")
  .record_validate_text_column(
    x$channels$channel_name,
    "channels$channel_name",
    TRUE
  )
  .record_validate_text_column(x$sources$source_id, "sources$source_id")
  .record_validate_text_column(
    x$sources$source_name,
    "sources$source_name",
    TRUE
  )
  .record_validate_text_column(
    x$sources$source_sha256,
    "sources$source_sha256",
    TRUE
  )
  .record_validate_text_column(x$curves$curve_id, "curves$curve_id")
  .record_validate_text_column(x$curves$injection_id, "curves$injection_id")
  .record_validate_text_column(x$curves$channel_id, "curves$channel_id")
  for (column in c(
    "source_id",
    "axis_name",
    "axis_unit",
    "value_name",
    "value_unit"
  )) {
    .record_validate_text_column(
      x$curves[[column]],
      paste0("curves$", column),
      TRUE
    )
  }

  .record_unique_key(x$samples$sample_id, "samples$sample_id")
  .record_unique_key(x$injections$injection_id, "injections$injection_id")
  .record_unique_key(x$channels$channel_id, "channels$channel_id")
  .record_unique_key(x$curves$curve_id, "curves$curve_id")
  .record_unique_key(x$sources$source_id, "sources$source_id")

  hashes <- x$sources$source_sha256
  invalid_hash <- !is.na(hashes) & !grepl("^[0-9a-f]{64}$", hashes)
  if (any(invalid_hash)) {
    cli::cli_abort(
      "Known {.field sources$source_sha256} values must be 64 lowercase hexadecimal characters."
    )
  }

  if (
    nrow(x$injections) > 0L &&
      !all(x$injections$sample_id %in% x$samples$sample_id)
  ) {
    cli::cli_abort("Every injection must refer to a row in {.field samples}.")
  }
  if (
    nrow(x$curves) > 0L &&
      !all(x$curves$injection_id %in% x$injections$injection_id)
  ) {
    cli::cli_abort("Every curve must refer to a row in {.field injections}.")
  }
  if (
    nrow(x$curves) > 0L &&
      !all(x$curves$channel_id %in% x$channels$channel_id)
  ) {
    cli::cli_abort("Every curve must refer to a row in {.field channels}.")
  }
  known_source <- !is.na(x$curves$source_id)
  if (
    any(
      known_source &
        !x$curves$source_id %in% x$sources$source_id
    )
  ) {
    cli::cli_abort(
      "Every known curve source ID must refer to a row in {.field sources}."
    )
  }
  if (
    nrow(x$curves) > 0L &&
      anyDuplicated(
        data.frame(
          injection_id = x$curves$injection_id,
          channel_id = x$curves$channel_id,
          stringsAsFactors = FALSE
        )
      )
  ) {
    cli::cli_abort(
      "Each {.field injection_id} and {.field channel_id} pair may identify only one curve."
    )
  }

  if (inherits(x$curves$.measures, "measure_nd_list")) {
    cli::cli_abort(
      "Column {.field curves$.measures} does not support nD payloads."
    )
  }
  if (!is.null(dim(x$curves$.measures))) {
    cli::cli_abort(
      "Column {.field curves$.measures} must be a one-dimensional measure_list."
    )
  }
  if (
    !is_measure_list(x$curves$.measures) ||
      length(x$curves$.measures) != nrow(x$curves)
  ) {
    cli::cli_abort(
      "Column {.field curves$.measures} must be a measure_list aligned with curves."
    )
  }
  if (
    !is.list(x$curves$.source_rows) ||
      is.object(x$curves$.source_rows) ||
      !is.null(dim(x$curves$.source_rows)) ||
      length(x$curves$.source_rows) != nrow(x$curves)
  ) {
    cli::cli_abort(
      "Column {.field curves$.source_rows} must be a list aligned with curves."
    )
  }
  for (i in seq_len(nrow(x$curves))) {
    .record_validate_payload(x$curves$.measures[[i]], i)
    .record_validate_source_rows(
      x$curves$.source_rows[[i]],
      nrow(x$curves$.measures[[i]]),
      i
    )
  }

  invisible(x)
}

.record_validate_text_column <- function(x, field, nullable = FALSE) {
  if (typeof(x) != "character" || is.object(x) || !is.null(dim(x))) {
    cli::cli_abort(
      "{.field {field}} must be a one-dimensional character vector."
    )
  }
  if (any(Encoding(x) == "bytes")) {
    cli::cli_abort(
      "{.field {field}} must contain UTF-8 strings, not byte-marked text."
    )
  }
  if (!nullable && anyNA(x)) {
    cli::cli_abort("{.field {field}} must not contain missing values.")
  }
  if (any(!is.na(x) & !validUTF8(x))) {
    cli::cli_abort("{.field {field}} must contain valid UTF-8 strings.")
  }
  blank <- vapply(
    x,
    function(value) {
      !is.na(value) && .record_is_blank(value)
    },
    logical(1)
  )
  if (any(blank)) {
    cli::cli_abort("{.field {field}} must not contain blank strings.")
  }
  invisible(x)
}

.record_unique_key <- function(x, field) {
  if (anyDuplicated(x)) {
    cli::cli_abort("{.field {field}} values must be unique.")
  }
}

.record_validate_payload <- function(x, curve) {
  if (inherits(x, "measure_nd_tbl")) {
    cli::cli_abort(
      "Curve {curve} has an nD payload; only 1D curves are supported."
    )
  }
  if (
    !is_measure_tbl(x) ||
      !tibble::is_tibble(x) ||
      !identical(names(x), c("location", "value"))
  ) {
    cli::cli_abort(
      "Curve {curve} payload must be a measure_tbl with only location and value columns."
    )
  }
  location <- .record_numeric(
    x$location,
    paste0("curve ", curve, " location"),
    allow_missing = FALSE
  )
  value <- .record_numeric(
    x$value,
    paste0("curve ", curve, " value"),
    allow_missing = TRUE
  )
  if (length(location) != length(value)) {
    cli::cli_abort(
      "Curve {curve} location and value columns must have equal lengths."
    )
  }
  if (length(location) == 0L) {
    cli::cli_abort("Curve {curve} must contain at least one point.")
  }
  if (length(location) > 1L) {
    increasing <- all(location[-1L] > location[-length(location)])
    decreasing <- all(location[-1L] < location[-length(location)])
    if (!increasing && !decreasing) {
      cli::cli_abort(
        "Curve {curve} locations must be strictly increasing or strictly decreasing."
      )
    }
  }
  invisible(x)
}

.record_validate_source_rows <- function(x, n_points, curve) {
  if (typeof(x) != "integer" || is.object(x) || !is.null(dim(x))) {
    cli::cli_abort("Curve {curve} source rows must be stored as integer.")
  }
  if (length(x) != n_points) {
    cli::cli_abort(
      "Curve {curve} source rows must align with its {n_points} points."
    )
  }
  if (any(!is.na(x) & x < 1L)) {
    cli::cli_abort("Curve {curve} source rows must be positive or missing.")
  }
  known <- x[!is.na(x)]
  if (anyDuplicated(known)) {
    cli::cli_abort("Curve {curve} source rows must be unique within the curve.")
  }
  invisible(x)
}
