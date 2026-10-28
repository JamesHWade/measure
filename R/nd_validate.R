# ==============================================================================
# N-Dimensional Measurement Validation
#
# This file provides validation functions for n-dimensional measurement data:
# - Grid regularity checks
# - Dimension validation
# - Structure validation for recipe steps
# ==============================================================================

#' Check if an n-dimensional measurement has a regular grid
#'
#' A regular grid means all combinations of unique coordinate values exist
#' exactly once (i.e., it forms a complete rectangular grid).
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
  ndim <- measure_ndim(x)

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
  coord_cols <- paste0("location_", seq_len(ndim))
  n_unique <- nrow(unique(x[coord_cols]))

  actual_rows == expected_rows && n_unique == actual_rows
}


#' @export
measure_is_regular.measure_nd_list <- function(x) {
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
  ndim <- measure_ndim(x)

  # Get unique values for each dimension
  unique_values <- lapply(
    seq_len(ndim),
    function(i) sort(unique(x[[paste0("location_", i)]]))
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
