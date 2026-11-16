#' Create a bounded mass-spectrometry and array acquisition
#'
#' An acquisition composes a validated [measure_record()] with declared
#' streams, mass-spectrometry scans, and two-dimensional arrays. Raw values and
#' point order are retained without interpreting, aligning, or serializing the
#' measurements.
#'
#' @param record A `measure_record` describing samples, injections, channels,
#'   sources, and scalar curves.
#' @param streams A data frame with stream declarations, or `NULL` for an
#'   empty table. The first four columns are `stream_id`, `injection_id`,
#'   `channel_id`, and `kind`; remaining columns are nullable text metadata.
#' @param scans A data frame with scan declarations and raw one-dimensional
#'   spectra, or `NULL` for an empty table.
#' @param arrays A data frame with array declarations and raw two-dimensional
#'   measurements, or `NULL` for an empty table.
#'
#' @details
#' All supplied tables use named columns. Unknown fields, duplicate fields,
#' extension attributes and malformed row sizes are rejected. The constructor
#' retains table order; it does not generate identifiers or infer units. Omitted
#' nullable metadata becomes typed missing values.
#'
#' @section Stream declarations:
#' Each stream requires `stream_id`, `injection_id`, `channel_id`, and `kind`
#' (`"ms"` or `"nd"`). Injection and channel IDs refer to the embedded
#' record; stream IDs are unique within the acquisition. Optional columns
#' are `source_id`, `time_unit`, `mz_unit`, `value_name`, and `value_unit`.
#' A nonmissing source ID refers to the record's source table. ND stream axis
#' metadata belongs to its arrays, so its `time_unit` and `mz_unit` must be
#' missing. Units are declarations; no conversion is performed.
#'
#' @section Scan declarations:
#' Each scan requires `scan_id`, `stream_id`, `scan_index`, `scan_status`, and
#' `.measures`. Its stream must have kind `"ms"`. The explicit positive
#' `scan_index` is unique within a stream and may have gaps; gaps do not create
#' missing scans. `scan_status` is `"acquired"` or `"missing"`; a declared
#' missing scan has zero points and differs from an acquired-empty scan.
#'
#' Optional columns are `retention_time`, `polarity`, `ms_level`,
#' `spectrum_type`, `source_scan_id`, `.source_rows`, and `.saturated`.
#' Retention time becomes double, with finite or missing values. MS level
#' becomes a positive integer or missing. Polarity is `"positive"`,
#' `"negative"`, or missing; spectrum type is `"centroid"`, `"profile"`,
#' or missing. A source scan ID is opaque text, not a parsed-row ordinal.
#'
#' `.measures` is an unnamed list of standard [new_measure_tbl()] payloads:
#' location stores m/z and value stores intensity. Raw integer/double storage,
#' point order, duplicate or unsorted coordinates, signed zero, nonfinite
#' states and typed empty vectors are retained. Storage validation does not
#' establish suitability for a calculation.
#'
#' @section Array declarations and point metadata:
#' Each array requires `array_id`, `stream_id`, and `.measures`; its stream
#' has kind `"nd"`. Payloads are an unnamed list of standard two-dimensional
#' [new_measure_nd_tbl()] objects. Each array retains its own coordinates,
#' dimension metadata and numeric storage, including sparse or repeated grids.
#'
#' Scans and arrays may provide `.source_rows` and `.saturated` as unnamed
#' lists aligned to their payload points. Source rows are positive integer
#' parsed-record ordinals or missing, with duplicates allowed; they are not
#' physical file lines. Saturation is logical: `TRUE` means declared saturated,
#' `FALSE` declared not saturated, and `NA` not assessed. Omitted masks and row
#' references are unknown, with typed empty vectors for zero-point payloads.
#' Missing signal values do not supply a saturation assessment.
#'
#' This container does not define a vendor parser, physical instrument
#' qualification, precursor annotation, method schema or processing graph.
#' It preserves the supported explicit declarations without authenticating
#' their source. The embedded scalar record keeps its independent contract.
#'
#' @examples
#' context <- measure_record(
#'   samples = tibble::tibble(sample_id = "sample-1"),
#'   injections = tibble::tibble(
#'     injection_id = "injection-1", sample_id = "sample-1"
#'   ),
#'   channels = tibble::tibble(channel_id = "ms")
#' )
#' acquisition <- measure_acquisition(
#'   context,
#'   streams = tibble::tibble(
#'     stream_id = "ms-1", injection_id = "injection-1",
#'     channel_id = "ms", kind = "ms", time_unit = "s", mz_unit = "m/z"
#'   ),
#'   scans = tibble::tibble(
#'     scan_id = "scan-1", stream_id = "ms-1", scan_index = 1L,
#'     scan_status = "acquired", retention_time = 1,
#'     polarity = "positive", ms_level = 1L, spectrum_type = "centroid",
#'     .measures = list(new_measure_tbl(c(100, 101), c(4, 8)))
#'   )
#' )
#' acquisition
#' summary(acquisition)
#'
#' @return A list with class `measure_acquisition` and components `record`,
#'   `streams`, `scans`, and `arrays`.
#'
#' @export
measure_acquisition <- function(
  record,
  streams = NULL,
  scans = NULL,
  arrays = NULL
) {
  streams <- .acq_canonical_streams(streams)
  scans <- .acq_canonical_scans(scans)
  arrays <- .acq_canonical_arrays(arrays)

  out <- structure(
    list(
      record = record,
      streams = streams,
      scans = scans,
      arrays = arrays
    ),
    class = c("measure_acquisition", "list")
  )

  .acq_validate(out)
  out
}

#' Test whether an object is a measurement acquisition
#'
#' @param x Object to test.
#'
#' @return `TRUE` when `x` inherits from `measure_acquisition`; otherwise
#'   `FALSE`. This is a class check and does not validate mutable contents.
#'
#' @export
is_measure_acquisition <- function(x) {
  inherits(x, "measure_acquisition")
}

#' Validate a measurement acquisition
#'
#' Rechecks the canonical tables, embedded record, payloads, masks, and
#' foreign-key relationships after edits to the mutable object.
#'
#' @param x An object created by [measure_acquisition()].
#'
#' @return `x`, invisibly, when valid. Invalid acquisitions raise an error.
#'
#' @export
validate_measure_acquisition <- function(x) {
  .acq_validate(x)
  invisible(x)
}

#' @rdname measure_acquisition
#' @param x A measurement acquisition to print.
#' @param ... Additional arguments; currently unused.
#' @export
print.measure_acquisition <- function(x, ...) {
  validate_measure_acquisition(x)
  cat("<measure_acquisition>\n")
  cat("  Samples: ", nrow(x$record$samples), "\n", sep = "")
  cat("  Injections: ", nrow(x$record$injections), "\n", sep = "")
  cat("  Channels: ", nrow(x$record$channels), "\n", sep = "")
  cat("  Sources: ", nrow(x$record$sources), "\n", sep = "")
  cat("  Curves: ", nrow(x$record$curves), "\n", sep = "")
  cat("  Streams: ", nrow(x$streams), "\n", sep = "")
  cat("  Scans: ", nrow(x$scans), "\n", sep = "")
  cat("  Arrays: ", nrow(x$arrays), "\n", sep = "")
  invisible(x)
}

#' @rdname measure_acquisition
#' @param object A measurement acquisition to summarize.
#' @export
summary.measure_acquisition <- function(object, ...) {
  validate_measure_acquisition(object)
  counts <- c(
    samples = nrow(object$record$samples),
    injections = nrow(object$record$injections),
    channels = nrow(object$record$channels),
    sources = nrow(object$record$sources),
    curves = nrow(object$record$curves),
    streams = nrow(object$streams),
    scans = nrow(object$scans),
    arrays = nrow(object$arrays)
  )
  structure(list(counts = counts), class = "summary_measure_acquisition")
}

#' @export
print.summary_measure_acquisition <- function(x, ...) {
  cat("<summary_measure_acquisition>\n")
  for (name in names(x$counts)) {
    cat("  ", name, ": ", x$counts[[name]], "\n", sep = "")
  }
  invisible(x)
}

.acq_schemas <- list(
  streams = c(
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
  scans = c(
    "scan_id",
    "stream_id",
    "scan_index",
    "scan_status",
    "retention_time",
    "polarity",
    "ms_level",
    "spectrum_type",
    "source_scan_id",
    ".measures",
    ".source_rows",
    ".saturated"
  ),
  arrays = c(
    "array_id",
    "stream_id",
    ".measures",
    ".source_rows",
    ".saturated"
  )
)

.acq_table_class <- c("tbl_df", "tbl", "data.frame")
.acq_record_table_class <- c("tbl_df", "tbl", "data.frame")
.acq_measure_tbl_class <- c(
  "measure_tbl",
  "tbl_df",
  "tbl",
  "data.frame"
)
.acq_measure_list_class <- c(
  "measure_list",
  "vctrs_list_of",
  "vctrs_vctr",
  "list"
)

.acq_canonical_streams <- function(x) {
  x <- .acq_input_table(
    x,
    table = "streams",
    columns = .acq_schemas$streams,
    required = c("stream_id", "injection_id", "channel_id", "kind")
  )
  if (is.null(x)) {
    return(tibble::tibble(
      stream_id = character(),
      injection_id = character(),
      channel_id = character(),
      kind = character(),
      source_id = character(),
      time_unit = character(),
      mz_unit = character(),
      value_name = character(),
      value_unit = character()
    ))
  }

  out <- list()
  for (column in .acq_schemas$streams) {
    value <- x[[column]]
    if (is.null(value)) {
      value <- rep(NA_character_, nrow(x))
    }
    out[[column]] <- .acq_text(value, paste0("streams$", column))
  }
  tibble::as_tibble(out, .name_repair = "minimal")
}

.acq_canonical_scans <- function(x) {
  x <- .acq_input_table(
    x,
    table = "scans",
    columns = .acq_schemas$scans,
    required = c(
      "scan_id",
      "stream_id",
      "scan_index",
      "scan_status",
      ".measures"
    )
  )
  if (is.null(x)) {
    return(tibble::tibble(
      scan_id = character(),
      stream_id = character(),
      scan_index = integer(),
      scan_status = character(),
      retention_time = double(),
      polarity = character(),
      ms_level = integer(),
      spectrum_type = character(),
      source_scan_id = character(),
      .measures = list(),
      .source_rows = list(),
      .saturated = list()
    ))
  }

  measures <- .acq_scan_measures(x$.measures)
  n <- nrow(x)
  if (length(measures) != n) {
    cli::cli_abort("scans$.measures must have one payload per scan row.")
  }
  source_rows <- if (".source_rows" %in% names(x)) {
    x$.source_rows
  } else {
    lapply(measures, function(measure) rep(NA_integer_, nrow(measure)))
  }
  saturated <- if (".saturated" %in% names(x)) {
    x$.saturated
  } else {
    lapply(measures, function(measure) rep(NA, nrow(measure)))
  }
  source_rows <- .acq_canonical_rows(
    source_rows,
    measures,
    "scans$.source_rows"
  )
  saturated <- .acq_canonical_masks(saturated, measures, "scans$.saturated")

  out <- list(
    scan_id = .acq_text(x$scan_id, "scans$scan_id"),
    stream_id = .acq_text(x$stream_id, "scans$stream_id"),
    scan_index = .acq_integer(
      x$scan_index,
      "scans$scan_index",
      nullable = FALSE,
      positive = TRUE
    ),
    scan_status = .acq_choice(
      x$scan_status,
      "scans$scan_status",
      c("acquired", "missing"),
      nullable = FALSE
    ),
    retention_time = .acq_double(
      if ("retention_time" %in% names(x)) {
        x$retention_time
      } else {
        rep(NA_real_, n)
      },
      "scans$retention_time"
    ),
    polarity = .acq_choice(
      if ("polarity" %in% names(x)) {
        x$polarity
      } else {
        rep(NA_character_, n)
      },
      "scans$polarity",
      c("positive", "negative")
    ),
    ms_level = .acq_integer(
      if ("ms_level" %in% names(x)) {
        x$ms_level
      } else {
        rep(NA_integer_, n)
      },
      "scans$ms_level",
      nullable = TRUE,
      positive = TRUE
    ),
    spectrum_type = .acq_choice(
      if ("spectrum_type" %in% names(x)) {
        x$spectrum_type
      } else {
        rep(NA_character_, n)
      },
      "scans$spectrum_type",
      c("centroid", "profile")
    ),
    source_scan_id = .acq_text(
      if ("source_scan_id" %in% names(x)) {
        x$source_scan_id
      } else {
        rep(NA_character_, n)
      },
      "scans$source_scan_id"
    ),
    .measures = measures,
    .source_rows = source_rows,
    .saturated = saturated
  )
  tibble::as_tibble(out, .name_repair = "minimal")
}

.acq_canonical_arrays <- function(x) {
  x <- .acq_input_table(
    x,
    table = "arrays",
    columns = .acq_schemas$arrays,
    required = c("array_id", "stream_id", ".measures")
  )
  if (is.null(x)) {
    return(tibble::tibble(
      array_id = character(),
      stream_id = character(),
      .measures = list(),
      .source_rows = list(),
      .saturated = list()
    ))
  }

  measures <- .acq_array_measures(x$.measures)
  n <- nrow(x)
  if (length(measures) != n) {
    cli::cli_abort("arrays$.measures must have one payload per array row.")
  }
  source_rows <- if (".source_rows" %in% names(x)) {
    x$.source_rows
  } else {
    lapply(measures, function(measure) rep(NA_integer_, nrow(measure)))
  }
  saturated <- if (".saturated" %in% names(x)) {
    x$.saturated
  } else {
    lapply(measures, function(measure) rep(NA, nrow(measure)))
  }
  source_rows <- .acq_canonical_rows(
    source_rows,
    measures,
    "arrays$.source_rows"
  )
  saturated <- .acq_canonical_masks(saturated, measures, "arrays$.saturated")

  tibble::tibble(
    array_id = .acq_text(x$array_id, "arrays$array_id"),
    stream_id = .acq_text(x$stream_id, "arrays$stream_id"),
    .measures = measures,
    .source_rows = source_rows,
    .saturated = saturated
  )
}

.acq_input_table <- function(x, table, columns, required) {
  if (is.null(x)) {
    return(NULL)
  }
  if (!is.data.frame(x)) {
    cli::cli_abort(
      "Table {.field {table}} must be a data frame or {.code NULL}."
    )
  }
  .acq_check_attributes(
    x,
    c("names", "row.names", "class"),
    paste0("table ", table)
  )
  if (
    !identical(attr(x, "class", exact = TRUE), "data.frame") &&
      !identical(class(x), .acq_table_class)
  ) {
    cli::cli_abort(
      "Table {.field {table}} must be a plain data frame or ordinary tibble."
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
  .acq_check_table_shape(x, paste0("input table ", table))
  unknown <- setdiff(input_names, columns)
  if (length(unknown) > 0L) {
    cli::cli_abort(
      "Table {.field {table}} has unknown column{?s}: {.field {unknown}}."
    )
  }
  missing <- setdiff(required, input_names)
  if (length(missing) > 0L) {
    cli::cli_abort(
      "Table {.field {table}} is missing required column{?s}: {.field {missing}}."
    )
  }
  x
}

.acq_text <- function(x, field) {
  .acq_plain_vector(x, field)
  .record_text(x, field, nullable = TRUE)
}

.acq_choice <- function(x, field, choices, nullable = TRUE) {
  x <- .acq_text(x, field)
  if (!nullable && anyNA(x)) {
    cli::cli_abort("{.field {field}} must not contain missing values.")
  }
  invalid <- !is.na(x) & !x %in% choices
  if (any(invalid)) {
    cli::cli_abort(
      "{.field {field}} must contain only {.val {choices}} or, where allowed, missing values."
    )
  }
  x
}

.acq_plain_vector <- function(x, field) {
  if (!is.null(attributes(x))) {
    cli::cli_abort("{.field {field}} must not have vector attributes.")
  }
  if (!is.atomic(x) || !is.null(dim(x))) {
    cli::cli_abort("{.field {field}} must be a plain one-dimensional vector.")
  }
  invisible(x)
}

.acq_double <- function(x, field) {
  .acq_plain_vector(x, field)
  if (!(typeof(x) %in% c("integer", "double"))) {
    cli::cli_abort("{.field {field}} must be an integer or double vector.")
  }
  out <- as.double(x)
  if (any(is.nan(out)) || any(!is.na(out) & !is.finite(out))) {
    cli::cli_abort("{.field {field}} must contain finite values or NA.")
  }
  out
}

.acq_integer <- function(x, field, nullable = FALSE, positive = FALSE) {
  .acq_plain_vector(x, field)
  if (!(typeof(x) %in% c("integer", "double"))) {
    cli::cli_abort("{.field {field}} must be an integer or double vector.")
  }
  if (typeof(x) == "integer") {
    out <- x
  } else {
    if (any(is.nan(x))) {
      cli::cli_abort("{.field {field}} cannot contain NaN values.")
    }
    missing <- is.na(x)
    present <- !missing
    if (
      any(
        present &
          (!is.finite(x) |
            x != trunc(x) |
            x > .Machine$integer.max |
            x < -.Machine$integer.max)
      )
    ) {
      cli::cli_abort(
        "{.field {field}} must contain whole representable integers."
      )
    }
    out <- as.integer(x)
  }
  if (!nullable && anyNA(out)) {
    cli::cli_abort("{.field {field}} must not contain missing values.")
  }
  if (positive && any(!is.na(out) & out < 1L)) {
    cli::cli_abort(
      "{.field {field}} must contain positive integers or missing values."
    )
  }
  out
}

.acq_plain_list <- function(x, field) {
  if (
    !is.list(x) ||
      is.data.frame(x) ||
      !is.null(dim(x)) ||
      !is.null(attributes(x))
  ) {
    cli::cli_abort("{.field {field}} must be an ordinary unnamed list.")
  }
  x
}

.acq_scan_measures <- function(x) {
  if (identical(class(x), .acq_measure_list_class)) {
    .acq_check_attributes(x, c("class", "ptype"), "scans$.measures")
    if (!is.list(x) || !is.null(dim(x))) {
      cli::cli_abort("Column {.field scans$.measures} must be one-dimensional.")
    }
    ptype <- attr(x, "ptype", exact = TRUE)
    if (!identical(ptype, new_measure_tbl())) {
      cli::cli_abort(
        "Column {.field scans$.measures} must use the standard default measure_list ptype."
      )
    }
  } else {
    x <- .acq_plain_list(x, "scans$.measures")
  }
  lapply(seq_along(x), function(i) {
    .acq_validate_measure_tbl(x[[i]], paste0("scan ", i, " payload"))
    x[[i]]
  })
}

.acq_validate_measure_tbl <- function(x, field) {
  if (
    !identical(class(x), .acq_measure_tbl_class) ||
      !tibble::is_tibble(x) ||
      !identical(names(x), c("location", "value"))
  ) {
    cli::cli_abort(
      "{.field {field}} must be a standard measure_tbl with only location and value columns."
    )
  }
  .acq_check_attributes(
    x,
    c("row.names", "names", "class"),
    field
  )
  if (!identical(row.names(x), as.character(seq_len(nrow(x))))) {
    cli::cli_abort("{.field {field}} must have ordinary automatic row names.")
  }
  for (column in c("location", "value")) {
    values <- x[[column]]
    .acq_plain_vector(values, paste0(field, "$", column))
    if (!(typeof(values) %in% c("integer", "double"))) {
      cli::cli_abort(
        "{.field {field}${column}} must be stored as integer or double."
      )
    }
    if (length(values) != nrow(x)) {
      cli::cli_abort(
        "Columns in {.field {field}} must each have a length matching its row count."
      )
    }
  }
  invisible(x)
}

.acq_array_measures <- function(x) {
  x <- .acq_plain_list(x, "arrays$.measures")
  lapply(seq_along(x), function(i) {
    measure <- x[[i]]
    .nd_io_validate_tbl(measure, paste0("array ", i, " payload"))
    if (!identical(attr(measure, "ndim", exact = TRUE), 2L)) {
      cli::cli_abort("Array {i} payload must be exactly two-dimensional.")
    }
    measure
  })
}

.acq_canonical_rows <- function(x, measures, field) {
  x <- .acq_plain_list(x, field)
  if (length(x) != length(measures)) {
    cli::cli_abort("{.field {field}} must have one vector per measurement.")
  }
  lapply(seq_along(x), function(i) {
    rows <- x[[i]]
    .acq_plain_vector(rows, paste0(field, "[[", i, "]]"))
    if (typeof(rows) != "integer") {
      cli::cli_abort("{.field {field}[[{i}]]} must be an integer vector.")
    }
    if (length(rows) != nrow(measures[[i]])) {
      cli::cli_abort(
        "{.field {field}[[{i}]]} must align with its measurement points."
      )
    }
    if (any(!is.na(rows) & rows < 1L)) {
      cli::cli_abort(
        "{.field {field}[[{i}]]} must contain positive integers or missing values."
      )
    }
    rows
  })
}

.acq_canonical_masks <- function(x, measures, field) {
  x <- .acq_plain_list(x, field)
  if (length(x) != length(measures)) {
    cli::cli_abort("{.field {field}} must have one vector per measurement.")
  }
  lapply(seq_along(x), function(i) {
    mask <- x[[i]]
    .acq_plain_vector(mask, paste0(field, "[[", i, "]]"))
    if (typeof(mask) != "logical") {
      cli::cli_abort("{.field {field}[[{i}]]} must be a logical vector.")
    }
    if (length(mask) != nrow(measures[[i]])) {
      cli::cli_abort(
        "{.field {field}[[{i}]]} must align with its measurement points."
      )
    }
    mask
  })
}

.acq_validate_record_attributes <- function(record) {
  .acq_check_attributes(record, c("names", "class"), "record")
  if (
    !identical(class(record), c("measure_record", "list")) ||
      !identical(
        names(record),
        c("samples", "injections", "channels", "curves", "sources")
      )
  ) {
    cli::cli_abort("{.field record} must be a standard measure_record.")
  }
  for (table in names(record)) {
    current <- record[[table]]
    .acq_check_attributes(
      current,
      c("row.names", "names", "class"),
      paste0("record$", table)
    )
    if (
      !tibble::is_tibble(current) ||
        !identical(class(current), .acq_record_table_class) ||
        !identical(row.names(current), as.character(seq_len(nrow(current))))
    ) {
      cli::cli_abort(
        "Component {.field record${table}} must be a standard tibble with automatic row names."
      )
    }
    .acq_check_table_shape(current, paste0("record table ", table))
    for (column in names(current)) {
      value <- current[[column]]
      if (identical(table, "curves") && identical(column, ".measures")) {
        .acq_validate_record_measure_list(value)
      } else if (
        identical(table, "curves") && identical(column, ".source_rows")
      ) {
        .acq_plain_list(value, "record$curves$.source_rows")
        for (i in seq_along(value)) {
          .acq_plain_vector(
            value[[i]],
            paste0("record$curves$.source_rows[[", i, "]]")
          )
        }
      } else {
        .acq_plain_vector(value, paste0("record$", table, "$", column))
      }
    }
  }
  invisible(record)
}

.acq_validate_record_measure_list <- function(x) {
  if (!identical(class(x), .acq_measure_list_class)) {
    cli::cli_abort("record$curves$.measures must be a standard measure_list.")
  }
  .acq_check_attributes(x, c("class", "ptype"), "record$curves$.measures")
  ptype <- attr(x, "ptype", exact = TRUE)
  if (!identical(ptype, new_measure_tbl())) {
    cli::cli_abort(
      "record$curves$.measures must use the standard default measure_list ptype."
    )
  }
  for (i in seq_along(x)) {
    .acq_validate_measure_tbl(x[[i]], paste0("record curve ", i, " payload"))
  }
  invisible(x)
}

.acq_validate_table <- function(x, table) {
  schema <- .acq_schemas[[table]]
  if (
    !tibble::is_tibble(x) ||
      !identical(class(x), .acq_table_class) ||
      !identical(names(x), schema)
  ) {
    cli::cli_abort(
      "Component {.field {table}} must be a canonical tibble with columns {.field {schema}}."
    )
  }
  .acq_check_attributes(
    x,
    c("row.names", "names", "class"),
    paste0("component ", table)
  )
  if (!identical(row.names(x), as.character(seq_len(nrow(x))))) {
    cli::cli_abort(
      "Component {.field {table}} must have ordinary automatic row names."
    )
  }
  .acq_check_table_shape(x, paste0("component ", table))
  invisible(x)
}

.acq_check_table_shape <- function(x, field) {
  if (any(lengths(x) != nrow(x))) {
    cli::cli_abort(
      "Columns in {.field {field}} must each have a length matching its row count."
    )
  }
  invisible(x)
}

.acq_validate <- function(x) {
  if (!is.list(x) || !identical(class(x), c("measure_acquisition", "list"))) {
    cli::cli_abort("{.arg x} must be a {.cls measure_acquisition} object.")
  }
  .acq_check_attributes(x, c("names", "class"), "acquisition")
  components <- c("record", "streams", "scans", "arrays")
  if (!identical(names(x), components)) {
    cli::cli_abort(
      "A {.cls measure_acquisition} must have exactly these components in order: {.field {components}}."
    )
  }

  .acq_validate_record_attributes(x$record)
  validate_measure_record(x$record)
  for (table in c("streams", "scans", "arrays")) {
    .acq_validate_table(x[[table]], table)
  }

  for (column in .acq_schemas$streams) {
    .acq_plain_vector(x$streams[[column]], paste0("streams$", column))
    if (typeof(x$streams[[column]]) != "character") {
      cli::cli_abort("{.field streams${column}} must be character.")
    }
    .record_validate_text_column(
      x$streams[[column]],
      paste0("streams$", column),
      nullable = !(column %in%
        c("stream_id", "injection_id", "channel_id", "kind"))
    )
  }
  .record_unique_key(x$streams$stream_id, "streams$stream_id")
  if (!all(x$streams$kind %in% c("ms", "nd"))) {
    cli::cli_abort("streams$kind must contain only `ms` or `nd`.")
  }
  if (
    nrow(x$streams) > 0L &&
      !all(x$streams$injection_id %in% x$record$injections$injection_id)
  ) {
    cli::cli_abort("Every stream must refer to a row in record injections.")
  }
  if (
    nrow(x$streams) > 0L &&
      !all(x$streams$channel_id %in% x$record$channels$channel_id)
  ) {
    cli::cli_abort("Every stream must refer to a row in record channels.")
  }
  known_sources <- !is.na(x$streams$source_id)
  if (
    any(known_sources & !x$streams$source_id %in% x$record$sources$source_id)
  ) {
    cli::cli_abort("Every known stream source ID must refer to record sources.")
  }
  nd <- x$streams$kind == "nd"
  if (any(nd & (!is.na(x$streams$time_unit) | !is.na(x$streams$mz_unit)))) {
    cli::cli_abort("ND stream time_unit and mz_unit must be missing.")
  }

  for (column in c(
    "scan_id",
    "stream_id",
    "scan_status",
    "polarity",
    "spectrum_type",
    "source_scan_id"
  )) {
    .acq_plain_vector(x$scans[[column]], paste0("scans$", column))
    if (typeof(x$scans[[column]]) != "character") {
      cli::cli_abort("{.field scans${column}} must be character.")
    }
    .record_validate_text_column(
      x$scans[[column]],
      paste0("scans$", column),
      nullable = !(column %in% c("scan_id", "stream_id", "scan_status"))
    )
  }
  .record_unique_key(x$scans$scan_id, "scans$scan_id")
  if (!all(x$scans$scan_status %in% c("acquired", "missing"))) {
    cli::cli_abort("scans$scan_status must be `acquired` or `missing`.")
  }
  if (
    any(
      !is.na(x$scans$polarity) &
        !x$scans$polarity %in% c("positive", "negative")
    )
  ) {
    cli::cli_abort("scans$polarity must be `positive`, `negative`, or missing.")
  }
  if (
    any(
      !is.na(x$scans$spectrum_type) &
        !x$scans$spectrum_type %in% c("centroid", "profile")
    )
  ) {
    cli::cli_abort(
      "scans$spectrum_type must be `centroid`, `profile`, or missing."
    )
  }
  .acq_validate_integer_column(x$scans$scan_index, "scans$scan_index", TRUE)
  .acq_validate_double_column(x$scans$retention_time, "scans$retention_time")
  .acq_validate_integer_column(x$scans$ms_level, "scans$ms_level", FALSE)

  stream_rows <- match(x$scans$stream_id, x$streams$stream_id)
  if (anyNA(stream_rows) || any(x$streams$kind[stream_rows] != "ms")) {
    cli::cli_abort("Every scan must refer to an existing MS stream.")
  }
  if (nrow(x$scans) > 0L) {
    keys <- data.frame(
      stream_id = x$scans$stream_id,
      scan_index = x$scans$scan_index,
      stringsAsFactors = FALSE
    )
    if (anyDuplicated(keys)) {
      cli::cli_abort("scans$scan_index must be unique within each stream.")
    }
  }

  for (i in seq_len(nrow(x$scans))) {
    measure <- x$scans$.measures[[i]]
    .acq_validate_measure_tbl(measure, paste0("scan ", i, " payload"))
  }
  .acq_validate_payload_columns(
    x$scans$.measures,
    x$scans$.source_rows,
    x$scans$.saturated,
    "scans",
    nrow(x$scans)
  )
  for (i in seq_len(nrow(x$scans))) {
    measure <- x$scans$.measures[[i]]
    if (x$scans$scan_status[[i]] == "missing" && nrow(measure) != 0L) {
      cli::cli_abort("Missing scan {i} must have zero raw points.")
    }
  }
  for (column in c("array_id", "stream_id")) {
    .acq_plain_vector(x$arrays[[column]], paste0("arrays$", column))
    if (typeof(x$arrays[[column]]) != "character") {
      cli::cli_abort("{.field arrays${column}} must be character.")
    }
    .record_validate_text_column(
      x$arrays[[column]],
      paste0("arrays$", column),
      nullable = FALSE
    )
  }
  .record_unique_key(x$arrays$array_id, "arrays$array_id")
  array_streams <- match(x$arrays$stream_id, x$streams$stream_id)
  if (anyNA(array_streams) || any(x$streams$kind[array_streams] != "nd")) {
    cli::cli_abort("Every array must refer to an existing ND stream.")
  }
  for (i in seq_len(nrow(x$arrays))) {
    measure <- x$arrays$.measures[[i]]
    .nd_io_validate_tbl(measure, paste0("array ", i, " payload"))
    if (!identical(attr(measure, "ndim", exact = TRUE), 2L)) {
      cli::cli_abort("Array {i} payload must be exactly two-dimensional.")
    }
  }
  .acq_validate_payload_columns(
    x$arrays$.measures,
    x$arrays$.source_rows,
    x$arrays$.saturated,
    "arrays",
    nrow(x$arrays)
  )
  invisible(x)
}

.acq_validate_integer_column <- function(x, field, positive) {
  .acq_plain_vector(x, field)
  if (typeof(x) != "integer" || (positive && anyNA(x))) {
    cli::cli_abort("{.field {field}} must be a canonical integer vector.")
  }
  if (positive && any(x < 1L)) {
    cli::cli_abort("{.field {field}} must contain positive integers.")
  }
  if (!positive && any(!is.na(x) & x < 1L)) {
    cli::cli_abort(
      "{.field {field}} must contain positive integers or missing values."
    )
  }
  invisible(x)
}

.acq_validate_double_column <- function(x, field) {
  .acq_plain_vector(x, field)
  if (typeof(x) != "double") {
    cli::cli_abort("{.field {field}} must be stored as double.")
  }
  if (any(is.nan(x)) || any(!is.na(x) & !is.finite(x))) {
    cli::cli_abort("{.field {field}} must contain finite values or NA.")
  }
  invisible(x)
}

.acq_validate_payload_columns <- function(
  measures,
  source_rows,
  saturated,
  table,
  n_rows
) {
  .acq_plain_list(measures, paste0(table, "$.measures"))
  if (
    length(measures) != n_rows ||
      length(measures) != length(source_rows) ||
      length(measures) != length(saturated)
  ) {
    cli::cli_abort(
      "Measurement payloads, source rows, and masks must align with rows."
    )
  }
  source_rows <- .acq_canonical_rows(
    source_rows,
    measures,
    paste0(table, "$.source_rows")
  )
  saturated <- .acq_canonical_masks(
    saturated,
    measures,
    paste0(table, "$.saturated")
  )
  invisible(list(source_rows = source_rows, saturated = saturated))
}

.acq_check_attributes <- function(x, allowed, field) {
  object_attributes <- attributes(x)
  if (is.null(object_attributes)) {
    return(invisible(NULL))
  }
  attribute_names <- names(object_attributes)
  if (
    is.null(attribute_names) ||
      length(attribute_names) != length(object_attributes) ||
      anyNA(attribute_names) ||
      anyDuplicated(attribute_names) ||
      !all(attribute_names %in% allowed)
  ) {
    cli::cli_abort("{.field {field}} has unsupported attributes.")
  }
  invisible(NULL)
}
