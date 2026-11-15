#' Read and write native n-dimensional measurement bundles
#'
#' A native nD bundle is a directory containing a fixed manifest and three
#' UTF-8 CSV or TSV tables. It preserves the ordered coordinates and values,
#' dimension labels and units, list member order and labels, and the storage
#' type of each numeric column. Sparse and repeated coordinate tuples are
#' retained as observed.
#'
#' The bundle format is a bounded numerical interchange profile. It does not
#' store acquisition provenance, detector or sample identity, signal units,
#' source row lineage, masks, or transformation history. The writer accepts
#' only the standard core nD classes with attribute-free numeric and metadata
#' vectors; unsupported attributes are rejected instead of being discarded.
#' See the package's `measure-nd-v1.md` profile specification for the exact
#' table and manifest schema.
#'
#' The reader selects only the fixed payload names from the manifest. Other
#' directory entries are ignored and are not protected by the manifest. Hashes
#' detect payload changes but do not authenticate a bundle.
#'
#' @param x A structurally valid `measure_nd_tbl` or `measure_nd_list` using
#'   the standard core classes and supported attributes.
#' @param path For writing, a new local directory path whose parent already
#'   exists. For reading, a directory containing a complete bundle.
#' @param delim A comma or tab field delimiter used for all three payloads.
#' @param max_bytes A positive whole-number limit for the actual cumulative
#'   bytes read from the manifest and the three selected payloads.
#'
#' @return `write_measure_nd()` returns the normalized destination path
#'   invisibly. `read_measure_nd()` returns a `measure_nd_tbl` or
#'   `measure_nd_list` matching the root kind stored in the bundle.
#'
#' @section Bundle version 1:
#' The fixed payloads are `members.csv`, `axes.csv`, and `points.csv`, or the
#' corresponding `.tsv` files. The manifest has the exact fields `Format`,
#' `Version`, `Delimiter`, `Object-Type`, `Dimensions`, `List-Names`, and
#' `-Rows`, `-Columns`, and `-SHA256` fields for each payload. The tables store
#' member descriptors (including a list prototype at index 0), per-member axis
#' metadata and storage types, and ordered numerical points. Member, axis, and
#' point indices are explicit keys, so readers may restore order after table
#' row reordering. Double tokens preserve NA, NaN, infinities and signed zero.
#' The writer emits canonical finite decimal tokens; the reader also accepts
#' equivalent finite spellings allowed by the scalar numeric parser.
#'
#' Dimensions are limited to 2 through 1024. Existing objects with unsupported
#' attributes, non-automatic row names, or attributed numeric or metadata
#' vectors are outside this profile. The writer validates the full object
#' before creating staging files, verifies its staged bundle with the strict
#' reader, and never replaces an existing destination. A failed copy can leave
#' an incomplete destination without a manifest; the reader rejects it.
#'
#' @examples
#' measurement <- new_measure_nd_tbl(
#'   location_1 = c(1, 2, 2),
#'   location_2 = c(254L, 280L, 280L),
#'   value = c(0, -0, NA_real_),
#'   dim_names = c("time", "wavelength"),
#'   dim_units = c("min", "nm")
#' )
#' local({
#'   bundle <- tempfile("measure-nd-")
#'   on.exit(unlink(bundle, recursive = TRUE), add = TRUE)
#'   write_measure_nd(measurement, bundle)
#'   restored <- read_measure_nd(bundle)
#'   identical(measurement$location_2, restored$location_2)
#' })
#'
#' @name measure-nd-bundle
NULL

#' @rdname measure-nd-bundle
#' @export
write_measure_nd <- function(x, path, delim = c(",", "\t")) {
  delim <- match.arg(delim)
  prepared <- .nd_io_prepare(x)
  destination <- .record_io_destination(path)
  if (.record_io_path_exists(destination)) {
    stop("path already exists.", call. = FALSE)
  }

  stage <- .record_io_make_stage(dirname(destination), basename(destination))
  on.exit(unlink(stage, recursive = TRUE, force = TRUE), add = TRUE)

  hashes <- character(length(prepared$tables))
  names(hashes) <- names(prepared$tables)
  for (table in names(prepared$tables)) {
    filename <- .nd_io_filename(table, delim)
    staged_file <- file.path(stage, filename)
    .record_io_write_table(prepared$tables[[table]], staged_file, delim)
    bytes <- .record_io_staged_bytes(staged_file)
    hashes[[table]] <- digest::digest(bytes, algo = "sha256", serialize = FALSE)
  }

  manifest <- .nd_io_make_manifest(prepared$tables, hashes, delim, prepared)
  manifest_stage <- file.path(stage, "manifest.dcf")
  .nd_io_write_manifest(manifest, manifest_stage)

  staged_bytes <- sum(
    file.info(file.path(
      stage,
      c(
        "manifest.dcf",
        vapply(
          names(prepared$tables),
          .nd_io_filename,
          character(1),
          delim = delim
        )
      )
    ))$size
  )
  if (!is.finite(staged_bytes) || staged_bytes > .Machine$integer.max - 1) {
    stop("Staged nD bundle is too large to verify safely.", call. = FALSE)
  }
  staged <- read_measure_nd(stage, max_bytes = max(1, staged_bytes))
  if (!.nd_io_fidelity_equal(staged, x)) {
    stop(
      "Staged nD bundle did not preserve the supplied object.",
      call. = FALSE
    )
  }

  if (!.record_io_reserve_destination(destination)) {
    stop(
      "Could not reserve the destination directory; it may have appeared during writing.",
      call. = FALSE
    )
  }
  for (table in names(prepared$tables)) {
    .record_io_copy_file(
      file.path(stage, .nd_io_filename(table, delim)),
      file.path(destination, .nd_io_filename(table, delim))
    )
  }
  .record_io_copy_file(manifest_stage, file.path(destination, "manifest.dcf"))
  invisible(destination)
}

#' @rdname measure-nd-bundle
#' @export
read_measure_nd <- function(path, max_bytes = 100 * 1024^2) {
  max_bytes <- .record_io_max_bytes(max_bytes)
  directory <- .record_io_read_directory(path)
  manifest_path <- file.path(directory, "manifest.dcf")
  .record_io_check_member(manifest_path)
  manifest_raw <- .record_read_raw(manifest_path, max_bytes)
  remaining <- max_bytes - length(manifest_raw)
  manifest <- .nd_io_parse_manifest(manifest_raw)
  delim <- if (identical(manifest[["Delimiter"]], "csv")) "," else "\t"

  raw_tables <- vector("list", 3L)
  names(raw_tables) <- c("Members", "Axes", "Points")
  for (table in names(raw_tables)) {
    filename <- .nd_io_filename(table, delim)
    member <- file.path(directory, filename)
    .record_io_check_member(member)
    bytes <- .nd_io_read_cumulative_member(member, remaining)
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
    raw_tables[[table]] <- bytes
  }

  tables <- lapply(names(raw_tables), function(table) {
    .nd_io_parse_payload(raw_tables[[table]], table, delim, manifest)
  })
  names(tables) <- names(raw_tables)
  .nd_io_build_object(tables, manifest)
}

.nd_io_tbl_class <- c(
  "measure_nd_tbl",
  "measure_tbl",
  "tbl_df",
  "tbl",
  "data.frame"
)

.nd_io_list_class <- c(
  "measure_nd_list",
  "measure_list",
  "vctrs_list_of",
  "vctrs_vctr",
  "list"
)

.nd_io_table_specs <- list(
  Members = c(
    "member_index",
    "member_name",
    "member_name_missing",
    "dim_names_present",
    "dim_units_present",
    "value_type",
    "point_count"
  ),
  Axes = c(
    "member_index",
    "axis_index",
    "storage_type",
    "axis_name",
    "axis_unit"
  )
)

.nd_io_manifest_fields <- c(
  "Format",
  "Version",
  "Delimiter",
  "Object-Type",
  "Dimensions",
  "List-Names",
  unlist(
    lapply(names(.nd_io_table_specs), function(table) {
      paste0(table, c("-Rows", "-Columns", "-SHA256"))
    }),
    use.names = FALSE
  ),
  "Points-Rows",
  "Points-Columns",
  "Points-SHA256"
)

.nd_io_filename <- function(table, delim) {
  extension <- if (identical(delim, ",")) "csv" else "tsv"
  paste0(tolower(table), ".", extension)
}

.nd_io_table_columns <- function(table, ndim) {
  if (identical(table, "Points")) {
    return(c(
      "member_index",
      "point_index",
      paste0("location_", seq_len(ndim)),
      "value"
    ))
  }
  .nd_io_table_specs[[table]]
}

.nd_io_check_attributes <- function(x, allowed, arg) {
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
    stop(
      arg,
      " has unsupported attributes for native nD interchange.",
      call. = FALSE
    )
  }
  invisible(NULL)
}

.nd_io_check_plain_vector <- function(x, type, arg) {
  if (
    typeof(x) != type ||
      !is.null(dim(x)) ||
      !is.null(attributes(x)) ||
      is.object(x)
  ) {
    stop(
      arg,
      " must be an attribute-free plain ",
      type,
      " vector.",
      call. = FALSE
    )
  }
  invisible(x)
}

.nd_io_validate_tbl <- function(x, arg = "x") {
  if (!identical(class(x), .nd_io_tbl_class)) {
    stop(
      arg,
      " must use the standard measure_nd_tbl class vector.",
      call. = FALSE
    )
  }
  .nd_io_check_attributes(
    x,
    c(
      "class",
      "names",
      "row.names",
      "ndim",
      "dim_order",
      "dim_names",
      "dim_units"
    ),
    arg
  )
  .nd_validate_tbl(x, arg = arg)

  ndim <- attr(x, "ndim", exact = TRUE)
  if (ndim > 1024L) {
    stop(
      "Dimensions must not exceed 1024 for native nD interchange.",
      call. = FALSE
    )
  }
  if (!identical(row.names(x), as.character(seq_len(nrow(x))))) {
    stop(arg, " must have ordinary automatic row names.", call. = FALSE)
  }

  for (column in names(x)) {
    .nd_io_check_plain_vector(
      x[[column]],
      typeof(x[[column]]),
      paste0(arg, "$", column)
    )
  }
  for (attribute in c("dim_names", "dim_units")) {
    metadata <- attr(x, attribute, exact = TRUE)
    if (!is.null(metadata)) {
      .nd_io_check_plain_vector(
        metadata,
        "character",
        paste0(arg, " attribute ", attribute)
      )
    }
  }
  invisible(x)
}

.nd_io_validate_list <- function(x, arg = "x") {
  if (!identical(class(x), .nd_io_list_class)) {
    stop(
      arg,
      " must use the standard measure_nd_list class vector.",
      call. = FALSE
    )
  }
  .nd_io_check_attributes(x, c("class", "names", "ptype"), arg)
  if (!is.list(x) || is.data.frame(x) || !is.null(dim(x))) {
    stop(arg, " must be a one-dimensional measure_nd_list.", call. = FALSE)
  }

  member_names <- attr(x, "names", exact = TRUE)
  if (!is.null(member_names)) {
    .nd_io_check_plain_vector(member_names, "character", paste0(arg, " names"))
    if (length(member_names) != length(x)) {
      stop(arg, " names must have one entry per member.", call. = FALSE)
    }
  }

  ptype <- attr(x, "ptype", exact = TRUE)
  if (is.null(ptype)) {
    stop(arg, " must have a validated zero-row nD ptype.", call. = FALSE)
  }
  .nd_io_validate_tbl(ptype, paste0(arg, " ptype"))
  if (nrow(ptype) != 0L) {
    stop(arg, " ptype must have zero rows.", call. = FALSE)
  }

  for (i in seq_len(length(x))) {
    .nd_io_validate_tbl(x[[i]], paste0(arg, " member ", i))
  }
  .nd_validate_list(x)
  ptype_ndim <- attr(ptype, "ndim", exact = TRUE)
  if (length(x) > 0L) {
    member_ndims <- vapply(
      seq_len(length(x)),
      function(i) attr(x[[i]], "ndim", exact = TRUE),
      integer(1)
    )
    if (any(member_ndims != ptype_ndim)) {
      stop(arg, " members must match the ptype dimensionality.", call. = FALSE)
    }
  }
  invisible(x)
}

.nd_io_prepare <- function(x) {
  if (identical(class(x), .nd_io_tbl_class)) {
    .nd_io_validate_tbl(x)
    descriptors <- list(list(index = 1L, object = x))
    object_type <- "tbl"
    list_names <- "absent"
    member_names <- NULL
    ndim <- attr(x, "ndim", exact = TRUE)
  } else if (identical(class(x), .nd_io_list_class)) {
    .nd_io_validate_list(x)
    ptype <- attr(x, "ptype", exact = TRUE)
    ndim <- attr(ptype, "ndim", exact = TRUE)
    member_names <- attr(x, "names", exact = TRUE)
    list_names <- if (is.null(member_names)) "absent" else "present"
    object_type <- "list"
    descriptors <- c(
      list(list(index = 0L, object = ptype)),
      lapply(seq_len(length(x)), function(i) {
        list(index = as.integer(i), object = x[[i]])
      })
    )
  } else {
    stop(
      "x must be a standard measure_nd_tbl or measure_nd_list.",
      call. = FALSE
    )
  }

  members <- lapply(descriptors, function(descriptor) {
    measurement <- descriptor$object
    is_prototype <- object_type == "list" && descriptor$index == 0L
    member_name <- if (object_type == "tbl" || is_prototype) {
      ""
    } else if (is.null(member_names)) {
      ""
    } else {
      .nd_io_text(member_names[[descriptor$index]])
    }
    member_name_missing <- object_type == "tbl" ||
      is_prototype ||
      is.null(member_names) ||
      is.na(member_names[[descriptor$index]])
    data.frame(
      member_index = as.character(descriptor$index),
      member_name = member_name,
      member_name_missing = if (member_name_missing) "1" else "0",
      dim_names_present = if (
        is.null(attr(measurement, "dim_names", exact = TRUE))
      ) {
        "0"
      } else {
        "1"
      },
      dim_units_present = if (
        is.null(attr(measurement, "dim_units", exact = TRUE))
      ) {
        "0"
      } else {
        "1"
      },
      value_type = typeof(measurement$value),
      point_count = as.character(nrow(measurement)),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })
  members <- do.call(rbind, members)
  rownames(members) <- NULL
  members <- tibble::as_tibble(members, .name_repair = "minimal")

  axes <- lapply(descriptors, function(descriptor) {
    measurement <- descriptor$object
    dim_names <- attr(measurement, "dim_names", exact = TRUE)
    dim_units <- attr(measurement, "dim_units", exact = TRUE)
    data.frame(
      member_index = rep(as.character(descriptor$index), ndim),
      axis_index = as.character(seq_len(ndim)),
      storage_type = vapply(
        seq_len(ndim),
        function(axis) typeof(measurement[[paste0("location_", axis)]]),
        character(1)
      ),
      axis_name = if (is.null(dim_names)) {
        rep("", ndim)
      } else {
        .nd_io_metadata_text(dim_names)
      },
      axis_unit = if (is.null(dim_units)) {
        rep("", ndim)
      } else {
        .nd_io_metadata_text(dim_units)
      },
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })
  axes <- do.call(rbind, axes)
  rownames(axes) <- NULL
  axes <- tibble::as_tibble(axes, .name_repair = "minimal")

  real_descriptors <- if (object_type == "tbl") {
    descriptors
  } else {
    descriptors[-1L]
  }
  point_columns <- .nd_io_table_columns("Points", ndim)
  point_parts <- lapply(real_descriptors, function(descriptor) {
    measurement <- descriptor$object
    n <- nrow(measurement)
    values <- list(
      member_index = rep(as.character(descriptor$index), n),
      point_index = as.character(seq_len(n))
    )
    for (axis in seq_len(ndim)) {
      column <- paste0("location_", axis)
      values[[column]] <- .nd_io_format_numeric(measurement[[column]])
    }
    values$value <- .nd_io_format_numeric(measurement$value)
    values
  })
  points <- if (length(point_parts) == 0L) {
    stats::setNames(
      lapply(point_columns, function(column) character()),
      point_columns
    )
  } else {
    stats::setNames(
      lapply(point_columns, function(column) {
        unlist(lapply(point_parts, `[[`, column), use.names = FALSE)
      }),
      point_columns
    )
  }
  points <- tibble::as_tibble(points, .name_repair = "minimal")

  list(
    tables = list(Members = members, Axes = axes, Points = points),
    object_type = object_type,
    list_names = list_names,
    ndim = ndim
  )
}

.nd_io_text <- function(x) {
  if (is.na(x)) "" else enc2utf8(x)
}

.nd_io_metadata_text <- function(x) {
  out <- enc2utf8(x)
  out[is.na(x)] <- ""
  out
}

.nd_io_make_manifest <- function(tables, hashes, delim, prepared) {
  fields <- c(
    Format = "measure-nd",
    Version = "1",
    Delimiter = if (identical(delim, ",")) "csv" else "tsv",
    `Object-Type` = prepared$object_type,
    Dimensions = as.character(prepared$ndim),
    `List-Names` = prepared$list_names
  )
  for (table in names(tables)) {
    fields[paste0(table, "-Rows")] <- as.character(nrow(tables[[table]]))
    fields[paste0(table, "-Columns")] <- as.character(ncol(tables[[table]]))
    fields[paste0(table, "-SHA256")] <- hashes[[table]]
  }
  if (!identical(names(fields), .nd_io_manifest_fields)) {
    stop("Internal nD bundle manifest schema is inconsistent.", call. = FALSE)
  }
  fields
}

.nd_io_write_manifest <- function(fields, path) {
  if (!identical(names(fields), .nd_io_manifest_fields)) {
    stop("Internal nD bundle manifest schema is inconsistent.", call. = FALSE)
  }
  lines <- paste0(names(fields), ": ", unname(fields))
  con <- file(path, open = "wb")
  on.exit(close(con), add = TRUE)
  writeBin(charToRaw(paste0(paste(lines, collapse = "\n"), "\n")), con)
  invisible(path)
}

.nd_io_parse_manifest <- function(raw) {
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
      length(keys) != length(.nd_io_manifest_fields) ||
      !setequal(keys, .nd_io_manifest_fields)
  ) {
    stop(
      "Bundle manifest has missing, duplicate, or unknown fields.",
      call. = FALSE
    )
  }
  fields <- unlist(dcf[1L, , drop = FALSE], use.names = TRUE)
  fields <- as.list(fields[.nd_io_manifest_fields])
  if (!identical(fields[["Format"]], "measure-nd")) {
    stop("Unsupported bundle format.", call. = FALSE)
  }
  if (!identical(fields[["Version"]], "1")) {
    stop("Unsupported measure-nd bundle version.", call. = FALSE)
  }
  if (!(fields[["Delimiter"]] %in% c("csv", "tsv"))) {
    stop("Unsupported bundle delimiter.", call. = FALSE)
  }
  if (!(fields[["Object-Type"]] %in% c("tbl", "list"))) {
    stop("Unsupported nD object type.", call. = FALSE)
  }
  if (!(fields[["List-Names"]] %in% c("absent", "present"))) {
    stop("Invalid List-Names manifest value.", call. = FALSE)
  }
  fields[["Dimensions"]] <- .record_io_parse_integer(
    fields[["Dimensions"]],
    "Dimensions",
    allow_missing = FALSE,
    allow_zero = FALSE
  )
  if (fields[["Dimensions"]] < 2L || fields[["Dimensions"]] > 1024L) {
    stop("Dimensions must be between 2 and 1024.", call. = FALSE)
  }
  if (
    identical(fields[["Object-Type"]], "tbl") &&
      !identical(fields[["List-Names"]], "absent")
  ) {
    stop("A tbl bundle must declare List-Names: absent.", call. = FALSE)
  }
  for (table in c("Members", "Axes", "Points")) {
    rows_key <- paste0(table, "-Rows")
    columns_key <- paste0(table, "-Columns")
    fields[[rows_key]] <- .record_io_parse_integer(
      fields[[rows_key]],
      paste0(table, " rows"),
      allow_missing = FALSE,
      allow_zero = TRUE
    )
    fields[[columns_key]] <- .record_io_parse_integer(
      fields[[columns_key]],
      paste0(table, " columns"),
      allow_missing = FALSE,
      allow_zero = FALSE
    )
    expected_columns <- length(.nd_io_table_columns(
      table,
      fields[["Dimensions"]]
    ))
    if (fields[[columns_key]] != expected_columns) {
      stop(
        "Bundle manifest has an invalid field count for ",
        table,
        ".",
        call. = FALSE
      )
    }
    hash_key <- paste0(table, "-SHA256")
    if (!grepl("\\A[0-9a-f]{64}\\z", fields[[hash_key]], perl = TRUE)) {
      stop("Bundle manifest has an invalid SHA256 field.", call. = FALSE)
    }
  }
  if (
    (identical(fields[["Object-Type"]], "tbl") &&
      fields[["Members-Rows"]] != 1L) ||
      (identical(fields[["Object-Type"]], "list") &&
        fields[["Members-Rows"]] < 1L)
  ) {
    stop(
      "Bundle manifest has an invalid member descriptor count.",
      call. = FALSE
    )
  }
  fields
}

.nd_io_read_cumulative_member <- function(path, remaining) {
  if (remaining > 0L) {
    return(.record_read_raw(path, remaining))
  }
  con <- file(path, open = "rb")
  on.exit(close(con), add = TRUE)
  probe <- readBin(con, what = "raw", n = 1L)
  if (length(probe) > 0L) {
    stop("Bundle exceeds max_bytes while reading.", call. = FALSE)
  }
  raw()
}

.nd_io_parse_payload <- function(raw, table, delim, manifest) {
  parsed <- .record_parse_delim(raw, delim)
  expected_columns <- .nd_io_table_columns(table, manifest[["Dimensions"]])
  if (!identical(names(parsed), expected_columns)) {
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

.nd_io_parse_flag <- function(x, field) {
  if (!is.character(x) || anyNA(x) || !all(x %in% c("0", "1"))) {
    stop("Invalid flag in ", field, ".", call. = FALSE)
  }
  x == "1"
}

.nd_io_parse_key <- function(x, field, allow_zero = FALSE) {
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

.nd_io_build_object <- function(tables, manifest) {
  ndim <- manifest[["Dimensions"]]
  members <- tables$Members
  axes <- tables$Axes
  points <- tables$Points
  object_type <- manifest[["Object-Type"]]
  is_list <- identical(object_type, "list")

  member_index <- .nd_io_parse_key(
    members$member_index,
    "members.member_index",
    allow_zero = TRUE
  )
  member_name_missing <- .nd_io_parse_flag(
    members$member_name_missing,
    "members.member_name_missing"
  )
  dim_names_present <- .nd_io_parse_flag(
    members$dim_names_present,
    "members.dim_names_present"
  )
  dim_units_present <- .nd_io_parse_flag(
    members$dim_units_present,
    "members.dim_units_present"
  )
  point_count <- .nd_io_parse_key(
    members$point_count,
    "members.point_count",
    allow_zero = TRUE
  )
  if (
    !all(members$value_type %in% c("integer", "double")) ||
      anyNA(members$value_type)
  ) {
    stop("members.value_type must be integer or double.", call. = FALSE)
  }
  if (any(member_name_missing & members$member_name != "")) {
    stop(
      "Missing member names must have blank member_name cells.",
      call. = FALSE
    )
  }
  prototype_row <- if (is_list) which(member_index == 0L) else integer()
  real_rows <- which(member_index > 0L)
  n_real <- length(real_rows)
  if (is_list) {
    if (
      length(prototype_row) != 1L ||
        !member_name_missing[[prototype_row]] ||
        members$member_name[[prototype_row]] != "" ||
        point_count[[prototype_row]] != 0L ||
        n_real != nrow(members) - 1L
    ) {
      stop("Invalid list prototype descriptor in members table.", call. = FALSE)
    }
    expected_member_index <- seq_len(n_real)
    if (!identical(sort(member_index[real_rows]), expected_member_index)) {
      stop("List member indices must be contiguous from 1.", call. = FALSE)
    }
    descriptor_order <- c(
      prototype_row,
      real_rows[order(member_index[real_rows])]
    )
  } else {
    if (
      nrow(members) != 1L ||
        !identical(member_index, 1L) ||
        !member_name_missing[[1L]] ||
        members$member_name[[1L]] != ""
    ) {
      stop(
        "A tbl bundle must have one unnamed member descriptor at index 1.",
        call. = FALSE
      )
    }
    descriptor_order <- 1L
  }
  members <- members[descriptor_order, , drop = FALSE]
  member_index <- member_index[descriptor_order]
  member_name_missing <- member_name_missing[descriptor_order]
  dim_names_present <- dim_names_present[descriptor_order]
  dim_units_present <- dim_units_present[descriptor_order]
  point_count <- point_count[descriptor_order]

  if (!is_list && manifest[["List-Names"]] != "absent") {
    stop("A tbl bundle must not declare list names.", call. = FALSE)
  }
  if (is_list && identical(manifest[["List-Names"]], "absent")) {
    if (
      !all(member_name_missing[-1L]) ||
        !all(members$member_name[-1L] == "")
    ) {
      stop(
        "An unnamed list must have missing blank member names.",
        call. = FALSE
      )
    }
  }

  axis_member_index <- .nd_io_parse_key(
    axes$member_index,
    "axes.member_index",
    allow_zero = TRUE
  )
  axis_index <- .nd_io_parse_key(axes$axis_index, "axes.axis_index")
  if (
    !all(axes$storage_type %in% c("integer", "double")) ||
      anyNA(axes$storage_type)
  ) {
    stop("axes.storage_type must be integer or double.", call. = FALSE)
  }
  n_descriptors <- nrow(members)
  expected_axes_rows <- as.double(n_descriptors) * as.double(ndim)
  if (expected_axes_rows > .Machine$integer.max) {
    stop("Axis reference count exceeds the supported range.", call. = FALSE)
  }
  if (nrow(axes) != expected_axes_rows) {
    stop(
      "Axes table does not contain one row per descriptor and dimension.",
      call. = FALSE
    )
  }
  axis_keys <- paste(axis_member_index, axis_index, sep = ":")
  expected_axis_keys <- unlist(
    lapply(member_index, function(index) {
      paste(index, seq_len(ndim), sep = ":")
    }),
    use.names = FALSE
  )
  if (
    anyDuplicated(axis_keys) ||
      length(axis_keys) != length(expected_axis_keys) ||
      !setequal(axis_keys, expected_axis_keys)
  ) {
    stop(
      "Axes table has missing, duplicate, or orphan axis references.",
      call. = FALSE
    )
  }
  axes$member_index <- axis_member_index
  axes$axis_index <- axis_index

  axis_data <- vector("list", n_descriptors)
  for (i in seq_len(n_descriptors)) {
    descriptor_index <- member_index[[i]]
    selected <- which(axes$member_index == descriptor_index)
    selected <- selected[order(axes$axis_index[selected])]
    axis_names <- axes$axis_name[selected]
    axis_units <- axes$axis_unit[selected]
    if (!dim_names_present[[i]] && any(axis_names != "")) {
      stop(
        "Absent dim_names metadata must have blank axes.axis_name cells.",
        call. = FALSE
      )
    }
    if (!dim_units_present[[i]] && any(axis_units != "")) {
      stop(
        "Absent dim_units metadata must have blank axes.axis_unit cells.",
        call. = FALSE
      )
    }
    dim_names <- if (dim_names_present[[i]]) {
      .nd_io_blank_to_missing(axis_names)
    } else {
      NULL
    }
    dim_units <- if (dim_units_present[[i]]) {
      .nd_io_blank_to_missing(axis_units)
    } else {
      NULL
    }
    .nd_check_dim_metadata(dim_names, "dim_names", ndim, unique = TRUE)
    .nd_check_dim_metadata(dim_units, "dim_units", ndim)
    axis_data[[i]] <- list(
      storage_type = axes$storage_type[selected],
      dim_names = dim_names,
      dim_units = dim_units
    )
  }

  point_member_index <- .nd_io_parse_key(
    points$member_index,
    "points.member_index"
  )
  point_index <- .nd_io_parse_key(points$point_index, "points.point_index")
  real_member_indices <- member_index[
    if (is_list) -1L else seq_along(member_index)
  ]
  if (!all(point_member_index %in% real_member_indices)) {
    stop(
      "Points table contains orphan or prototype point references.",
      call. = FALSE
    )
  }
  if (
    sum(as.double(point_count[if (is_list) -1L else seq_along(point_count)])) !=
      nrow(points)
  ) {
    stop(
      "Member point_count values do not sum to the points row count.",
      call. = FALSE
    )
  }

  descriptor_start <- if (is_list) 2L else 1L
  output_positions <- if (descriptor_start > n_descriptors) {
    integer()
  } else {
    seq.int(descriptor_start, n_descriptors)
  }
  point_groups <- vector("list", length(output_positions))
  for (position in seq_along(output_positions)) {
    i <- output_positions[[position]]
    expected_index <- member_index[[i]]
    selected <- which(point_member_index == expected_index)
    if (length(selected) != point_count[[i]]) {
      stop(
        "members.point_count does not match point references.",
        call. = FALSE
      )
    }
    ordered <- selected[order(point_index[selected])]
    actual_indices <- point_index[ordered]
    if (!identical(actual_indices, seq_len(length(selected)))) {
      stop(
        "Point indices must be unique and contiguous within each member.",
        call. = FALSE
      )
    }
    point_groups[[position]] <- ordered
  }

  prototype <- NULL
  if (is_list) {
    prototype_data <- axis_data[[1L]]
    prototype <- .nd_io_new_tbl(
      lapply(prototype_data$storage_type, .nd_io_empty_numeric),
      value = .nd_io_empty_numeric(members$value_type[[1L]]),
      dim_names = prototype_data$dim_names,
      dim_units = prototype_data$dim_units
    )
  }

  output_members <- vector("list", length(output_positions))
  output_names <- if (is_list && manifest[["List-Names"]] == "present") {
    rep(NA_character_, n_descriptors - 1L)
  } else {
    NULL
  }
  for (position in seq_along(output_positions)) {
    i <- output_positions[[position]]
    output_index <- position
    expected_index <- member_index[[i]]
    ordered <- point_groups[[position]]
    numeric_columns <- vector("list", ndim + 1L)
    for (axis in seq_len(ndim)) {
      column <- paste0("location_", axis)
      type <- axis_data[[i]]$storage_type[[axis]]
      numeric_columns[[axis]] <- .nd_io_parse_numeric(
        points[[column]][ordered],
        type,
        paste0("points.", column, " for member ", expected_index)
      )
    }
    numeric_columns[[ndim + 1L]] <- .nd_io_parse_numeric(
      points$value[ordered],
      members$value_type[[i]],
      paste0("points.value for member ", expected_index)
    )
    measurement <- .nd_io_new_tbl(
      numeric_columns[seq_len(ndim)],
      value = numeric_columns[[ndim + 1L]],
      dim_names = axis_data[[i]]$dim_names,
      dim_units = axis_data[[i]]$dim_units
    )
    output_members[[output_index]] <- measurement
    if (!is.null(output_names)) {
      if (member_name_missing[[i]]) {
        output_names[[output_index]] <- NA_character_
      } else {
        output_names[[output_index]] <- enc2utf8(members$member_name[[i]])
      }
    }
  }

  if (!is_list) {
    return(output_members[[1L]])
  }
  out <- vctrs::new_list_of(
    output_members,
    ptype = prototype,
    class = c("measure_nd_list", "measure_list")
  )
  if (!is.null(output_names)) {
    attr(out, "names") <- output_names
  }
  out
}

.nd_io_blank_to_missing <- function(x) {
  x[x == ""] <- NA_character_
  x
}

.nd_io_empty_numeric <- function(type) {
  if (identical(type, "integer")) integer() else double()
}

.nd_io_new_tbl <- function(locations, value, dim_names, dim_units) {
  names(locations) <- paste0("location_", seq_along(locations))
  arguments <- c(
    locations,
    list(value = value, dim_names = dim_names, dim_units = dim_units)
  )
  do.call(new_measure_nd_tbl, arguments)
}

.nd_io_numeric_equal <- function(x, y) {
  if (!identical(typeof(x), typeof(y)) || length(x) != length(y)) {
    return(FALSE)
  }
  if (typeof(x) == "integer") {
    return(identical(x, y))
  }
  if (typeof(x) != "double") {
    return(FALSE)
  }
  state <- function(value) {
    out <- rep("finite", length(value))
    out[is.na(value) & !is.nan(value)] <- "NA"
    out[is.nan(value)] <- "NaN"
    out[is.infinite(value) & value > 0] <- "Inf"
    out[is.infinite(value) & value < 0] <- "-Inf"
    out
  }
  if (!identical(state(x), state(y))) {
    return(FALSE)
  }
  finite <- is.finite(x)
  if (!any(finite)) {
    return(TRUE)
  }
  finite_bytes <- function(value) {
    connection <- rawConnection(raw(), open = "wb")
    on.exit(close(connection), add = TRUE)
    writeBin(value, connection, size = 8L, endian = "little")
    rawConnectionValue(connection)
  }
  identical(finite_bytes(x[finite]), finite_bytes(y[finite]))
}

.nd_io_character_equal <- function(x, y) {
  if (is.null(x) || is.null(y)) {
    return(is.null(x) && is.null(y))
  }
  if (!is.character(x) || !is.character(y) || length(x) != length(y)) {
    return(FALSE)
  }
  missing_x <- is.na(x)
  missing_y <- is.na(y)
  identical(missing_x, missing_y) &&
    identical(enc2utf8(x[!missing_x]), enc2utf8(y[!missing_y]))
}

.nd_io_tbl_equal <- function(x, y) {
  if (
    !identical(class(x), class(y)) ||
      !identical(names(x), names(y)) ||
      nrow(x) != nrow(y) ||
      !identical(
        attr(x, "ndim", exact = TRUE),
        attr(y, "ndim", exact = TRUE)
      ) ||
      !.nd_io_character_equal(
        attr(x, "dim_names", exact = TRUE),
        attr(y, "dim_names", exact = TRUE)
      ) ||
      !.nd_io_character_equal(
        attr(x, "dim_units", exact = TRUE),
        attr(y, "dim_units", exact = TRUE)
      )
  ) {
    return(FALSE)
  }
  all(vapply(
    names(x),
    function(column) {
      .nd_io_numeric_equal(x[[column]], y[[column]])
    },
    logical(1)
  ))
}

.nd_io_fidelity_equal <- function(x, y) {
  if (
    identical(class(x), .nd_io_tbl_class) &&
      identical(class(y), .nd_io_tbl_class)
  ) {
    return(.nd_io_tbl_equal(x, y))
  }
  if (
    !identical(class(x), .nd_io_list_class) ||
      !identical(class(y), .nd_io_list_class) ||
      length(x) != length(y) ||
      !.nd_io_character_equal(
        attr(x, "names", exact = TRUE),
        attr(y, "names", exact = TRUE)
      )
  ) {
    return(FALSE)
  }
  x_ptype <- attr(x, "ptype", exact = TRUE)
  y_ptype <- attr(y, "ptype", exact = TRUE)
  if (!.nd_io_tbl_equal(x_ptype, y_ptype)) {
    return(FALSE)
  }
  all(vapply(
    seq_len(length(x)),
    function(i) {
      .nd_io_tbl_equal(x[[i]], y[[i]])
    },
    logical(1)
  ))
}
