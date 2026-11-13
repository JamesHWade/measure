# ==============================================================================
# N-Dimensional Measurement Validation
#
# This file provides validation functions for n-dimensional measurement data:
# - Grid regularity checks
# - Dimension validation
# - Structure validation for recipe steps
# ==============================================================================

.nd_is_plain_numeric_vector <- function(x) {
  typeof(x) %in%
    c("integer", "double") &&
    is.null(dim(x)) &&
    !is.object(x) &&
    all(names(attributes(x)) %in% "names")
}

.nd_check_numeric_vector <- function(x, arg) {
  if (!.nd_is_plain_numeric_vector(x)) {
    cli::cli_abort(
      "{.arg {arg}} must be numeric, specifically a plain one-dimensional integer or double vector, not {.obj_type_friendly {x}}."
    )
  }

  invisible(x)
}

.nd_check_dim_metadata <- function(x, arg, ndim, unique = FALSE) {
  if (is.null(x)) {
    return(invisible(x))
  }

  if (!is.character(x) || is.object(x) || !is.null(dim(x))) {
    cli::cli_abort(
      "{.arg {arg}} must be NULL or an unclassed one-dimensional character vector."
    )
  }

  if (length(x) != ndim) {
    cli::cli_abort(
      "{.arg {arg}} must have length {ndim} (one per dimension), not {length(x)}."
    )
  }

  known <- x[!is.na(x)]
  if (!all(nzchar(trimws(known, whitespace = "[\\h\\v]")))) {
    cli::cli_abort(
      "Known values in {.arg {arg}} must be nonblank; use {.code NA_character_} for unknown values."
    )
  }

  if (unique && anyDuplicated(known) > 0L) {
    cli::cli_abort(
      "Known values in {.arg {arg}} must be unique; missing values are allowed."
    )
  }

  invisible(x)
}

# Validate the mutable structure of an nD measurement payload. This deliberately
# does not impose analytical constraints on coordinate tuples or numeric values.
.nd_validate_tbl <- function(x, arg = "x") {
  if (!inherits(x, "measure_nd_tbl") || !is.data.frame(x)) {
    cli::cli_abort(
      "{.arg {arg}} must be a data frame inheriting from {.cls measure_nd_tbl}."
    )
  }

  ndim <- attr(x, "ndim")
  if (
    !is.integer(ndim) ||
      length(ndim) != 1L ||
      is.na(ndim) ||
      ndim < 2L ||
      !is.null(attributes(ndim))
  ) {
    cli::cli_abort(
      "{.arg {arg}} must have an unclassed integer {.code ndim} attribute of at least 2."
    )
  }

  if (ndim != ncol(x) - 1L) {
    cli::cli_abort(
      "The {.code ndim} attribute in {.arg {arg}} must equal the number of location columns ({ncol(x) - 1L})."
    )
  }

  expected_columns <- c(paste0("location_", seq_len(ndim)), "value")
  if (!identical(names(x), expected_columns)) {
    cli::cli_abort(
      "Columns in {.arg {arg}} must be exactly {.val {expected_columns}} in canonical order."
    )
  }

  if (!identical(attr(x, "dim_order"), seq_len(ndim))) {
    cli::cli_abort(
      "{.arg {arg}} must have canonical {.code dim_order} equal to {.val {seq_len(ndim)}}."
    )
  }

  for (column in expected_columns) {
    .nd_check_numeric_vector(x[[column]], paste0(arg, "$", column))
    if (length(x[[column]]) != nrow(x)) {
      cli::cli_abort(
        "Column {.arg {column}} in {.arg {arg}} must have length {.val {nrow(x)}}."
      )
    }
  }

  .nd_check_dim_metadata(attr(x, "dim_names"), "dim_names", ndim, unique = TRUE)
  .nd_check_dim_metadata(attr(x, "dim_units"), "dim_units", ndim)

  invisible(x)
}

.nd_validate_list <- function(x) {
  if (!is.list(x) || is.data.frame(x) || !is.null(dim(x))) {
    cli::cli_abort(
      "{.arg x} must be a one-dimensional list of nD measurements."
    )
  }
  ndims <- vapply(
    x,
    function(el) {
      .nd_validate_tbl(el, arg = "each measure_nd_tbl element")
      attr(el, "ndim")
    },
    integer(1)
  )
  if (length(unique(ndims)) > 1L) {
    cli::cli_abort(
      "All elements of measure_nd_list must have the same dimensionality.
       Found: {.val {unique(ndims)}}."
    )
  }
  invisible(x)
}

#' Check if an n-dimensional measurement has a regular grid
#'
#' A regular grid means all combinations of unique coordinate values exist
#' exactly once (i.e., it forms a complete rectangular grid).
#' A missing coordinate makes regularity `FALSE`: an unknown coordinate
#' cannot establish a complete grid. This is a structural check, not a
#' finite-value or physical-validity assessment.
#'
#' @param x A `measure_nd_tbl` object.
#'
#' @return Logical indicating if the measurement has a regular grid.
#'
#' @examples
#' # Regular grid (all combinations present)
#' regular <- new_measure_nd_tbl(
#'   location_1 = rep(1:3, each = 2),
#'   location_2 = rep(1:2, times = 3),
#'   value = rnorm(6)
#' )
#' measure_is_regular(regular)  # TRUE
#'
#' # Irregular grid (missing combinations)
#' irregular <- new_measure_nd_tbl(
#'   location_1 = c(1, 1, 2, 3),
#'   location_2 = c(1, 2, 1, 2),
#'   value = rnorm(4)
#' )
#' measure_is_regular(irregular)  # FALSE
#'
#' @export
measure_is_regular <- function(x) {
  UseMethod("measure_is_regular")
}


#' @export
measure_is_regular.measure_nd_tbl <- function(x) {
  .nd_validate_tbl(x)
  ndim <- measure_ndim(x)

  coord_cols <- paste0("location_", seq_len(ndim))
  if (any(vapply(x[coord_cols], anyNA, logical(1)))) {
    return(FALSE)
  }

  # Get unique values for each dimension
  unique_counts <- vapply(
    seq_len(ndim),
    function(i) length(unique(x[[paste0("location_", i)]])),
    integer(1)
  )

  # For a regular grid, nrow should equal product of unique values
  expected_rows <- prod(unique_counts)
  actual_rows <- nrow(x)

  # Also check for duplicate coordinate tuples
  n_unique <- nrow(unique(x[coord_cols]))

  actual_rows == expected_rows && n_unique == actual_rows
}


#' @export
measure_is_regular.measure_nd_list <- function(x) {
  .nd_validate_list(x)
  if (length(x) == 0) {
    return(TRUE)
  }
  all(vapply(x, measure_is_regular, logical(1)))
}


#' @export
measure_is_regular.default <- function(x) {
  cli::cli_abort(
    "{.arg x} must be a measure_nd object, not {.obj_type_friendly {x}}."
  )
}


#' Get grid information for an n-dimensional measurement
#'
#' Returns detailed information about the coordinate grid, including
#' unique values per dimension, grid shape, and regularity status.
#'
#' @param x A `measure_nd_tbl` object.
#'
#' @return A list with components:
#'   - `ndim`: Number of dimensions
#'   - `dim_names`: Semantic dimension names (if set)
#'   - `dim_units`: Dimension units (if set)
#'   - `unique_values`: List of unique coordinate values per dimension
#'   - `shape`: Integer vector of unique value counts per dimension
#'   - `n_points`: Total number of data points
#'   - `is_regular`: Whether the grid is regular
#'   - `has_na`: Whether any values are NA
#'
#' Missing coordinate states are retained in `unique_values` and counted in
#' `shape`; they make `is_regular` false. `has_na` describes missing signal
#' values only. The reported shape does not imply that every grid cell was
#' observed, and numeric payloads are not checked for physical validity.
#' For a collection, inspect each entry of `sample_info` for its own metadata;
#' collection-level names and units describe the first measurement.
#'
#' @examples
#' m2d <- new_measure_nd_tbl(
#'   location_1 = rep(seq(0, 10, by = 2), each = 4),
#'   location_2 = rep(c(254, 280, 320, 350), times = 6),
#'   value = rnorm(24),
#'   dim_names = c("time", "wavelength"),
#'   dim_units = c("min", "nm")
#' )
#' measure_grid_info(m2d)
#'
#' @export
measure_grid_info <- function(x) {
  UseMethod("measure_grid_info")
}


#' @export
measure_grid_info.measure_nd_tbl <- function(x) {
  .nd_validate_tbl(x)
  ndim <- measure_ndim(x)

  # Get unique values for each dimension
  unique_values <- lapply(
    seq_len(ndim),
    function(i) sort(unique(x[[paste0("location_", i)]]), na.last = TRUE)
  )
  names(unique_values) <- paste0("dim_", seq_len(ndim))

  # Shape is the count of unique values per dimension
  shape <- lengths(unique_values)
  names(shape) <- paste0("dim_", seq_len(ndim))

  list(
    ndim = ndim,
    dim_names = measure_dim_names(x),
    dim_units = measure_dim_units(x),
    unique_values = unique_values,
    shape = shape,
    n_points = nrow(x),
    is_regular = measure_is_regular(x),
    has_na = anyNA(x$value)
  )
}


#' @export
measure_grid_info.measure_nd_list <- function(x) {
  .nd_validate_list(x)
  if (length(x) == 0) {
    return(list(
      n_samples = 0L,
      ndim = 2L,
      sample_info = list()
    ))
  }

  sample_info <- lapply(x, measure_grid_info.measure_nd_tbl)

  list(
    n_samples = length(x),
    ndim = sample_info[[1]]$ndim,
    dim_names = sample_info[[1]]$dim_names,
    dim_units = sample_info[[1]]$dim_units,
    sample_info = sample_info,
    all_regular = all(vapply(sample_info, `[[`, logical(1), "is_regular")),
    any_na = any(vapply(sample_info, `[[`, logical(1), "has_na"))
  )
}


#' @export
measure_grid_info.default <- function(x) {
  cli::cli_abort(
    "{.arg x} must be a measure_nd object, not {.obj_type_friendly {x}}."
  )
}


# ------------------------------------------------------------------------------
# Validation helpers for recipe steps
# ------------------------------------------------------------------------------

#' Find n-dimensional measure columns in a data frame
#'
#' Returns the names of columns that contain `measure_nd_list` objects.
#'
#' @param data A data frame.
#'
#' @return Character vector of column names.
#'
#' @examples
#' # After using step_measure_input_long with multiple location columns
#' # find_measure_nd_cols(result)
#'
#' @export
find_measure_nd_cols <- function(data) {
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame.")
  }

  is_nd_meas <- vapply(data, is_measure_nd_list, logical(1))
  names(data)[is_nd_meas]
}
