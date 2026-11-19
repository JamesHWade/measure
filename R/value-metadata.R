# Measurement value metadata

# Metadata is deliberately stored on the measure_list vector (the data-frame
# column), not on each measure_tbl member or its value vector. This keeps the
# declaration about one homogeneous column and means that a transformation
# which rebuilds a plain list does not accidentally inherit it.

.measure_value_metadata_attr <- "measure_value_metadata"

.measure_is_nonempty_string <- function(x) {
  is.character(x) &&
    length(x) == 1L &&
    is.null(attr(x, "class", exact = TRUE)) &&
    is.null(dim(x)) &&
    !is.na(x) &&
    nzchar(trimws(x))
}

.measure_validate_value_metadata <- function(metadata) {
  is.list(metadata) &&
    identical(names(metadata), c("quantity", "unit")) &&
    .measure_is_nonempty_string(metadata$quantity) &&
    (is.null(metadata$unit) ||
      .measure_is_nonempty_string(metadata$unit))
}

.measure_value_metadata_from_attr <- function(x) {
  has_metadata <- .measure_value_metadata_attr %in% names(attributes(x))
  if (!has_metadata) {
    return(NULL)
  }

  metadata <- attr(x, .measure_value_metadata_attr, exact = TRUE)
  if (!.measure_validate_value_metadata(metadata)) {
    cli::cli_abort(
      "Attribute {.field measure_value_metadata} on a {.cls measure_list} must be a named list with {.field quantity} and {.field unit}."
    )
  }

  list(
    quantity = unname(metadata$quantity),
    unit = if (is.null(metadata$unit)) NULL else unname(metadata$unit)
  )
}

#' Get measurement value metadata
#'
#' Read the declaration for the values in a measurement column. The
#' declaration describes the one homogeneous `measure_list` column, not the
#' individual observations inside each `measure_tbl` member.
#'
#' @param x A `measure_list` column.
#'
#' @return `NULL` when `x` is not a `measure_list` or has no declaration;
#'   otherwise a named list with `quantity` and `unit` elements. `unit` may be
#'   `NULL` when it was not declared. A malformed declaration errors rather
#'   than being treated as valid metadata.
#'
#' @examples
#' measures <- new_measure_list(list(
#'   new_measure_tbl(1:3, c(1, 2, 3)),
#'   new_measure_tbl(1:3, c(2, 3, 4))
#' ))
#' measure_value_metadata(measures)
#' measures <- set_measure_value_metadata(
#'   measures,
#'   quantity = "absorbance",
#'   unit = "AU"
#' )
#' measure_value_metadata(measures)
#'
#' @export
measure_value_metadata <- function(x) {
  if (!is_measure_list(x) || is_measure_nd_list(x)) {
    return(NULL)
  }

  .measure_value_metadata_from_attr(x)
}

#' Set measurement value metadata
#'
#' Declare the quantity and optional unit for one homogeneous 1D
#' `measure_list` column. The declaration is stored on the column itself. Row
#' slicing, filtering, and homogeneous binding preserve it; a value-changing
#' function that rebuilds a plain list must explicitly call this function on
#' the new result, or leave the result untagged when its quantity or unit is
#' unknown.
#'
#' @param x An ordinary 1D `measure_list` column.
#' @param quantity A non-empty, length-one character string naming the value
#'   quantity (for example, `"absorbance"` or `"intensity"`).
#' @param unit `NULL` or a non-empty, length-one character string declaring the
#'   value unit (for example, `"AU"`).
#'
#' @return `x` with a `measure_value_metadata` declaration attached to the
#'   column.
#'
#' @examples
#' measures <- new_measure_list(list(
#'   new_measure_tbl(1:3, c(1, 2, 3)),
#'   new_measure_tbl(1:3, c(2, 3, 4))
#' ))
#' measures <- set_measure_value_metadata(
#'   measures,
#'   quantity = "absorbance",
#'   unit = "AU"
#' )
#' measure_value_metadata(measures)
#'
#' # A transformed quantity gets a new explicit declaration.
#' transformed <- new_measure_list(lapply(measures, function(measure) {
#'   measure$value <- measure$value * 1000
#'   measure
#' }))
#' transformed <- set_measure_value_metadata(
#'   transformed,
#'   quantity = "signal",
#'   unit = "mAU"
#' )
#'
#' @export
set_measure_value_metadata <- function(x, quantity, unit = NULL) {
  if (!is_measure_list(x) || is_measure_nd_list(x)) {
    cli::cli_abort(
      "{.arg x} must be an ordinary 1D {.cls measure_list} column."
    )
  }
  if (!.measure_is_nonempty_string(quantity)) {
    cli::cli_abort("{.arg quantity} must be a non-empty string.")
  }
  if (!is.null(unit) && !.measure_is_nonempty_string(unit)) {
    cli::cli_abort("{.arg unit} must be NULL or a non-empty string.")
  }

  quantity <- unname(quantity)
  unit <- if (is.null(unit)) NULL else unname(unit)

  attr(x, .measure_value_metadata_attr) <- list(
    quantity = quantity,
    unit = unit
  )
  x
}

.measure_attach_value_metadata <- function(x, source) {
  metadata <- measure_value_metadata(source)
  if (!is.null(metadata)) {
    attr(x, .measure_value_metadata_attr) <- metadata
  }

  x
}

.measure_validate_common_value_metadata <- function(sources) {
  metadata <- lapply(sources, measure_value_metadata)
  declared <- which(!vapply(metadata, is.null, logical(1)))
  if (length(declared) == 0L) {
    return(NULL)
  }

  reference <- metadata[[declared[[1L]]]]
  if (length(declared) > 1L) {
    for (index in declared[-1L]) {
      if (!identical(reference, metadata[[index]])) {
        cli::cli_abort(
          "Cannot combine measure columns with incompatible value metadata."
        )
      }
    }
  }

  untagged <- which(vapply(metadata, is.null, logical(1)))
  if (length(untagged) > 0L) {
    nonempty <- vapply(
      sources[untagged],
      function(source) !is.null(source) && length(source) > 0L,
      logical(1)
    )
    if (any(nonempty)) {
      cli::cli_abort(
        "Cannot combine tagged and untagged measure columns."
      )
    }
  }

  reference
}
