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
  shape <- vapply(unique_values, length, integer(1))
  names(shape) <- paste0("dim_", seq_len(ndim))

  list(
    ndim = ndim,
    dim_names = measure_dim_names(x),
    dim_units = measure_dim_units(x),
    unique_values = unique_values,
    shape = shape,
    n_points = nrow(x),
    is_regular = measure_is_regular(x),
    has_na = any(is.na(x$value))
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

#' Validate n-dimensional measure data for processing
#'
#' Internal function to validate measure_nd data before operations that
#' require specific properties (e.g., regular grids for tensor conversion).
#'
#' @param x A `measure_nd_tbl` or `measure_nd_list` object.
#' @param require_regular If `TRUE`, abort if grid is not regular.
#' @param require_no_na If `TRUE`, abort if any values are NA.
#' @param min_ndim Minimum number of dimensions required.
#' @param max_ndim Maximum number of dimensions allowed.
#' @param call The calling environment for error reporting.
#'
#' @return Invisibly returns `x` if validation passes.
#'
#' @noRd
validate_measure_nd <- function(
  x,
  require_regular = FALSE,
  require_no_na = FALSE,
  min_ndim = 2L,
  max_ndim = NULL,
  call = rlang::caller_env()
) {
  # Check class
 if (!is_measure_nd_tbl(x) && !is_measure_nd_list(x)) {
    cli::cli_abort(
      "{.arg x} must be a {.cls measure_nd_tbl} or {.cls measure_nd_list},
       not {.obj_type_friendly {x}}.",
      call = call
    )
  }

  ndim <- measure_ndim(x)

  # Check minimum dimensions
  if (ndim < min_ndim) {
    cli::cli_abort(
      "At least {min_ndim} dimensions required, but got {ndim}.",
      call = call
    )
  }

  # Check maximum dimensions
  if (!is.null(max_ndim) && ndim > max_ndim) {
    cli::cli_abort(
      "At most {max_ndim} dimensions allowed, but got {ndim}.",
      call = call
    )
  }

  # Check regularity
  if (require_regular && !measure_is_regular(x)) {
    cli::cli_abort(
      c(
        "A regular grid is required for this operation.",
        "i" = "Use {.fn step_measure_regularize} to interpolate to a regular grid first."
      ),
      call = call
    )
  }

  # Check for NA values
  if (require_no_na) {
    has_na <- if (is_measure_nd_tbl(x)) {
      any(is.na(x$value))
    } else {
      any(vapply(x, function(el) any(is.na(el$value)), logical(1)))
    }

    if (has_na) {
      cli::cli_abort(
        c(
          "NA values are not allowed for this operation.",
          "i" = "Use {.fn step_measure_impute} to handle missing values first."
        ),
        call = call
      )
    }
  }

  invisible(x)
}


#' Check that all samples in a measure_nd_list have consistent dimensions
#'
#' @param x A `measure_nd_list` object.
#' @param col Column name (for error messages).
#' @param call The calling environment for error reporting.
#'
#' @return Invisibly returns the number of dimensions.
#'
#' @noRd
check_measure_nd_dims <- function(x, col = ".measures", call = rlang
::caller_env()) {
  if (!is_measure_nd_list(x)) {
    cli::cli_abort(
      "Column {.field {col}} must be a {.cls measure_nd_list}.",
      call = call
    )
  }

  if (length(x) == 0) {
    return(invisible(2L))
  }

  ndims <- vapply(x, measure_ndim, integer(1))
  if (length(unique(ndims)) > 1) {
    cli::cli_abort(
      c(
        "All samples in {.field {col}} must have the same dimensionality.",
        "i" = "Found dimensions: {.val {unique(ndims)}}."
      ),
      call = call
    )
  }

  invisible(ndims[1])
}


#' Check for n-dimensional measure columns in a data frame
#'
#' Validates that a data frame contains at least one n-dimensional
#' measure column and returns the column names.
#'
#' @param data A data frame.
#' @param call The calling environment for error reporting.
#'
#' @return Character vector of n-dimensional measure column names.
#'
#' @noRd
check_for_nd_measure <- function(data, call = rlang::caller_env()) {
  nd_cols <- find_measure_nd_cols(data)

  if (length(nd_cols) == 0) {
    cli::cli_abort(
      c(
        "No n-dimensional measure column found in the data.",
        "i" = "Use {.fn step_measure_input_long} with multiple location columns first."
      ),
      call = call
    )
  }

  nd_cols
}


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


#' Check if data frame has any measure columns (1D or nD)
#'
#' Updated version that detects both `measure_list` and `measure_nd_list`.
#'
#' @param data A data frame.
#'
#' @return Character vector of all measure column names (both 1D and nD).
#'
#' @noRd
find_all_measure_cols <- function(data) {
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame.")
  }

  is_1d <- vapply(data, is_measure_list, logical(1))
  is_nd <- vapply(data, is_measure_nd_list, logical(1))

  # Note: measure_nd_list inherits from measure_list, so is_1d will be TRUE
  # for nD columns too. Use is_nd to specifically identify nD columns.
  names(data)[is_1d | is_nd]
}
