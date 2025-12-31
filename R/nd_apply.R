# ==============================================================================
# N-Dimensional Measurement Apply Dispatcher
#
# This file provides the central measure_apply() function that enables
# existing 1D preprocessing steps to work on n-dimensional data by
# applying them along specified dimensions.
# ==============================================================================

#' Apply a function to measurement data along dimensions
#'
#' Central dispatcher that enables 1D preprocessing operations to work
#' on n-dimensional measurement data. For 1D data, it applies the function
#' directly. For nD data, it slices along the specified dimensions, applies
#' the function to each 1D slice, and rebuilds the nD structure.
#'
#' @param x A `measure_tbl`, `measure_nd_tbl`, `measure_list`, or
#'   `measure_nd_list` object.
#' @param fn A function that accepts a `measure_tbl` and returns a
#'   `measure_tbl`. The function signature should be `fn(x, ...)`.
#' @param along Integer vector specifying which dimensions to apply the
#'   function along. For 2D data, `along = 1` applies along dimension 1
#'   (e.g., time in LC-DAD), treating dimension 2 slices as independent.
#'   Default is `1L` (apply along the first dimension).
#' @param ... Additional arguments passed to `fn`.
#'
#' @return An object of the same class as the input, with the function
#'   applied to each 1D slice.
#'
#' @details
#' The `measure_apply()` function is the workhorse for making 1D
#' preprocessing steps work on nD data. It handles:
#'
#' - **1D data**: Direct function application
#' - **nD data**: Slice-apply-rebuild pattern
#'
#' For nD data, the function extracts 1D slices along the specified
#' dimension(s), applies the transformation function to each slice,
#' and reassembles the result into the original nD structure.
#'
#' @examples
#' # Create a simple 2D measurement
#' m2d <- new_measure_nd_tbl(
#'   location_1 = rep(1:10, each = 3),
#'   location_2 = rep(1:3, times = 10),
#'   value = rnorm(30)
#' )
#'
#' # Define a simple smoothing function for 1D data
#' smooth_1d <- function(x) {
#'   x$value <- stats::filter(x$value, rep(1/3, 3), sides = 2)
#'   x[!is.na(x$value), ]
#' }
#'
#' # Apply smoothing along dimension 1
#' result <- measure_apply(m2d, smooth_1d, along = 1)
#'
#' @export
measure_apply <- function(x, fn, along = 1L, ...) {
  UseMethod("measure_apply")
}


#' @export
measure_apply.measure_tbl <- function(x, fn, along = 1L, ...) {
  # 1D case: apply directly
  result <- fn(x, ...)

  if (!is_measure_tbl(result)) {
    cli::cli_abort(
      "Function {.arg fn} must return a {.cls measure_tbl},
       got {.obj_type_friendly {result}}."
    )
  }

  result
}


#' @export
measure_apply.measure_nd_tbl <- function(x, fn, along = 1L, ...) {
  ndim <- measure_ndim(x)

  # Validate 'along' parameter
  if (!is.numeric(along) || length(along) == 0) {
    cli::cli_abort("{.arg along} must be a non-empty integer vector.")
  }
  if (any(along < 1) || any(along > ndim)) {
    cli::cli_abort(
      "{.arg along} must be between 1 and {ndim}, got {.val {along}}."
    )
  }

  # Determine the 'other' dimensions (the ones we slice by)
  slice_dims <- setdiff(seq_len(ndim), along)

  if (length(slice_dims) == 0) {
    # Applying along all dimensions = single application
    # Convert to 1D-like format and apply
    m1d <- as_measure_tbl_from_nd(x, along = along)
    result_1d <- fn(m1d, ...)
    return(as_measure_nd_from_1d(result_1d, x, along = along))
  }

  # Split data by the slice dimensions
  slice_cols <- paste0("location_", slice_dims)
  sliced_data <- split_by_dims(x, slice_cols)

  # Apply function to each slice
  results <- lapply(sliced_data$data, function(slice) {
    # Convert slice to 1D measure_tbl
    along_col <- paste0("location_", along)
    m1d <- new_measure_tbl(
      location = slice[[along_col]],
      value = slice$value
    )

    # Apply the function
    result_1d <- fn(m1d, ...)

    if (!is_measure_tbl(result_1d)) {
      cli::cli_abort(
        "Function {.arg fn} must return a {.cls measure_tbl}."
      )
    }

    # Reconstruct with updated values
    tibble::tibble(
      !!along_col := result_1d$location,
      value = result_1d$value
    )
  })

  # Rebuild the nD structure
  rebuild_nd_from_slices(results, sliced_data$keys, slice_dims, along, x)
}


#' @export
measure_apply.measure_list <- function(x, fn, along = 1L, ...) {
  results <- lapply(x, function(m) {
    measure_apply(m, fn, along = along, ...)
  })
  new_measure_list(results)
}


#' @export
measure_apply.measure_nd_list <- function(x, fn, along = 1L, ...) {
  results <- lapply(x, function(m) {
    measure_apply(m, fn, along = along, ...)
  })
  new_measure_nd_list(results)
}


# ------------------------------------------------------------------------------
# Internal helper functions
# ------------------------------------------------------------------------------

#' Split nD data by dimension columns
#' @noRd
split_by_dims <- function(x, slice_cols) {
  # Create a key from the slice columns
  keys <- unique(x[slice_cols])

  # Split data by keys
  data_list <- lapply(seq_len(nrow(keys)), function(i) {
    key_vals <- keys[i, , drop = FALSE]
    mask <- rep(TRUE, nrow(x))
    for (col in slice_cols) {
      mask <- mask & (x[[col]] == key_vals[[col]])
    }
    x[mask, ]
  })

  list(keys = keys, data = data_list)
}


#' Rebuild nD structure from slices
#' @noRd
rebuild_nd_from_slices <- function(results, keys, slice_dims, along, original) {
  ndim <- measure_ndim(original)
  slice_cols <- paste0("location_", slice_dims)
  along_col <- paste0("location_", along)

  # Combine results with their keys
  rebuilt <- lapply(seq_along(results), function(i) {
    result <- results[[i]]
    key_row <- keys[i, , drop = FALSE]

    # Add the slice dimension columns back
    for (col in slice_cols) {
      result[[col]] <- key_row[[col]]
    }

    result
  })

  combined <- dplyr::bind_rows(rebuilt)

  # Reorder columns to canonical order: location_1, location_2, ..., value
  loc_cols <- paste0("location_", seq_len(ndim))
  combined <- combined[c(loc_cols, "value")]

  # Sort by canonical order (location_1, then location_2, etc.)
  combined <- dplyr::arrange(combined, dplyr::across(dplyr::all_of(loc_cols)))

  # Create new measure_nd_tbl with original metadata
  args <- stats::setNames(
    lapply(loc_cols, function(col) combined[[col]]),
    loc_cols
  )
  args$value <- combined$value
  args$dim_names <- measure_dim_names(original)
  args$dim_units <- measure_dim_units(original)

  do.call(new_measure_nd_tbl, args)
}


#' Convert nD to 1D-like for single dimension processing
#' @noRd
as_measure_tbl_from_nd <- function(x, along = 1L) {
  # When all dimensions are in 'along', treat as 1D
  # Order by the 'along' dimensions and flatten
  along_col <- paste0("location_", along[1])

  # For now, simple case: create 1D from flattened data
  # Location is from the first 'along' dimension
  new_measure_tbl(
    location = x[[along_col]],
    value = x$value
  )
}


#' Reconstruct nD from 1D result
#' @noRd
as_measure_nd_from_1d <- function(result_1d, original, along = 1L) {
  ndim <- measure_ndim(original)
  along_col <- paste0("location_", along[1])

  # Match back to original structure
  # This is a simplified version - assumes same ordering
  loc_cols <- paste0("location_", seq_len(ndim))
  args <- stats::setNames(
    lapply(loc_cols, function(col) {
      if (col == along_col) {
        result_1d$location
      } else {
        original[[col]]
      }
    }),
    loc_cols
  )
  args$value <- result_1d$value
  args$dim_names <- measure_dim_names(original)
  args$dim_units <- measure_dim_units(original)

  do.call(new_measure_nd_tbl, args)
}


#' Apply function along multiple dimensions sequentially
#'
#' For operations that need to be applied along each dimension in turn
#' (e.g., 2D smoothing via separable 1D filters).
#'
#' @param x A `measure_nd_tbl` or `measure_nd_list` object.
#' @param fn A function that accepts and returns a `measure_tbl`.
#' @param dims Integer vector of dimensions to apply along sequentially.
#'   Default is all dimensions.
#' @param ... Additional arguments passed to `fn`.
#'
#' @return An object of the same class as the input.
#'
#' @noRd
measure_apply_sequential <- function(x, fn, dims = NULL, ...) {
  if (is.null(dims)) {
    dims <- seq_len(measure_ndim(x))
  }

  result <- x
  for (d in dims) {
    result <- measure_apply(result, fn, along = d, ...)
  }
  result
}


#' Apply function to all dimensions simultaneously (parallel/joint)
#'
#' For operations that need the full nD context (e.g., 2D convolution).
#' This simply passes the entire nD object to the function.
#'
#' @param x A `measure_nd_tbl` or `measure_nd_list` object.
#' @param fn A function that accepts and returns a `measure_nd_tbl`.
#' @param ... Additional arguments passed to `fn`.
#'
#' @return An object of the same class as the input.
#'
#' @noRd
measure_apply_joint <- function(x, fn, ...) {
  if (is_measure_nd_list(x)) {
    results <- lapply(x, function(m) fn(m, ...))
    return(new_measure_nd_list(results))
  }

  fn(x, ...)
}
