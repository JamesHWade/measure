# ==============================================================================
# N-Dimensional Measurement Reduction Operations
#
# This file provides functions for reducing dimensionality of nD measurements:
# - measure_unfold(): Convert nD to 1D with fold metadata
# - measure_fold(): Reconstruct nD from unfolded 1D
# - measure_slice(): Extract slices at specific coordinates
# - measure_project(): Aggregate across dimensions
# ==============================================================================

# ------------------------------------------------------------------------------
# Unfold: nD -> 1D
# ------------------------------------------------------------------------------

#' Unfold n-dimensional measurement to 1D
#'
#' Converts an n-dimensional measurement to a 1D vector by flattening
#' according to a specified dimension order. Stores metadata needed to
#' reconstruct the original nD structure via [measure_fold()].
#'
#' @param x A `measure_nd_tbl` or `measure_nd_list` object.
#' @param order Integer vector specifying the order of dimensions for
#'   unfolding. Default is `NULL`, which uses the natural order (1, 2, ..., n).
#'   The first dimension varies fastest.
#'
#' @return A `measure_tbl` or `measure_list` with fold metadata. Each unfolded
#'   list member carries its own `"fold_info"` attribute.
#'
#' @details
#' Unfolding is useful for:
#' - Applying 1D modeling techniques (PCA, PLS) to nD data

#' - Exporting to formats that expect 1D vectors
#' - Visualization as a single trace
#'
#' The fold metadata includes:
#' - `ndim`: Original number of dimensions
#' - `dim_names`, `dim_units`: Original dimension metadata
#' - `coordinates`: The original coordinate values for each dimension
#' - `order`: The unfolding order used
#'
#' Unfolding sorts points into the selected canonical dimension order. Folding
#' restores that sorted order; it does not restore the input's acquisition row
#' order.
#'
#' @seealso [measure_fold()] to reconstruct the nD structure
#'
#' @examples
#' # Create a 2D measurement (3 x 4 grid)
#' m2d <- new_measure_nd_tbl(
#'   location_1 = rep(1:3, each = 4),
#'   location_2 = rep(1:4, times = 3),
#'   value = 1:12,
#'   dim_names = c("time", "wavelength")
#' )
#'
#' # Unfold to 1D
#' m1d <- measure_unfold(m2d)
#' m1d
#'
#' # Reconstruct
#' m2d_restored <- measure_fold(m1d)
#'
#' @export
measure_unfold <- function(x, order = NULL) {
  UseMethod("measure_unfold")
}


#' @export
measure_unfold.measure_nd_tbl <- function(x, order = NULL) {
  .nd_validate_tbl(x)
  ndim <- measure_ndim(x)

  if (is.null(order)) {
    order <- seq_len(ndim)
  }

  order <- .nd_validate_fold_order(order, ndim, arg = "order")

  # Sort by the specified dimension order
  loc_cols <- paste0("location_", order)
  sorted <- dplyr::arrange(x, dplyr::across(dplyr::all_of(rev(loc_cols))))

  # Create sequential location (1, 2, 3, ...)
  n <- nrow(sorted)

  # Store fold metadata
  fold_info <- list(
    ndim = ndim,
    dim_names = measure_dim_names(x),
    dim_units = measure_dim_units(x),
    order = order,
    coordinates = lapply(seq_len(ndim), function(i) {
      sorted[[paste0("location_", i)]]
    })
  )
  names(fold_info$coordinates) <- paste0("dim_", seq_len(ndim))

  # Create 1D measure_tbl
  result <- new_measure_tbl(
    location = seq_len(n),
    value = sorted$value
  )

  attr(result, "fold_info") <- fold_info
  result
}


#' @export
measure_unfold.measure_nd_list <- function(x, order = NULL) {
  .nd_validate_list(x)
  if (length(x) == 0L) {
    ndim <- measure_ndim(x)
    if (is.null(order)) {
      order <- seq_len(ndim)
    }
    .nd_validate_fold_order(order, ndim, arg = "order")
    return(new_measure_list(list()))
  }

  results <- lapply(x, function(m) measure_unfold(m, order = order))
  new_measure_list(results)
}


# ------------------------------------------------------------------------------
# Fold: 1D -> nD (inverse of unfold)
# ------------------------------------------------------------------------------

#' Fold 1D measurement back to n-dimensional
#'
#' Reconstructs an n-dimensional measurement from a 1D vector that was
#' created by [measure_unfold()]. Requires the fold metadata attribute.
#'
#' @param x A `measure_tbl` or `measure_list` with `"fold_info"` attribute.
#'
#' @return A `measure_nd_tbl` or `measure_nd_list` with the original
#'   dimensional structure restored.
#'
#' @details
#' Each 1D payload must retain its own `fold_info` mapping. When any member
#' of a collection has this metadata, every member must have its own valid
#' mapping. New list unfolds never attach the first member's grid to the
#' collection. For legacy objects with no member mappings, a list-level
#' descriptor is accepted only as a caller's explicit shared-grid declaration.
#'
#' The 1D `location` column contains point indices, not physical coordinates.
#' It must contain each integer from 1 to the saved point count exactly once.
#' Reordering rows is supported: values are matched back by point index. Signal
#' values may be edited, but deleting points, duplicating indices, or losing
#' part of the mapping is an error. Folding restores canonical unfold order,
#' rather than the original acquisition row order.
#' To edit a collection while retaining its mappings, transform the extracted
#' members with `lapply()` and rebuild with [new_measure_list()]. List-column
#' assignment or other operations that remove attributes can lose a mapping.
#'
#' @seealso [measure_unfold()] to create foldable 1D data
#'
#' @examples
#' # Create, unfold, then fold back
#' m2d <- new_measure_nd_tbl(
#'   location_1 = rep(1:3, each = 4),
#'   location_2 = rep(1:4, times = 3),
#'   value = 1:12
#' )
#'
#' m1d <- measure_unfold(m2d)
#' m2d_restored <- measure_fold(m1d)
#'
#' # Values are preserved
#' all.equal(m2d$value, m2d_restored$value)
#'
#' @export
measure_fold <- function(x) {
  UseMethod("measure_fold")
}


#' @export
measure_fold.measure_tbl <- function(x) {
  if (!is.data.frame(x) || !identical(names(x), c("location", "value"))) {
    cli::cli_abort(
      "Fold input must have exactly {.field location} and {.field value} columns."
    )
  }
  fold_info <- attr(x, "fold_info", exact = TRUE)

  if (is.null(fold_info)) {
    cli::cli_abort(
      c(
        "Cannot fold: no fold metadata found.",
        "i" = "Use {.fn measure_unfold} to create foldable data."
      )
    )
  }

  .nd_validate_fold_info(fold_info)

  ndim <- as.integer(fold_info$ndim)
  n <- nrow(x)
  .nd_check_numeric_vector(x$value, "x$value")
  if (length(x$value) != n) {
    cli::cli_abort(
      "Fold input {.field value} must have one entry per point index."
    )
  }

  # Validate length matches
  expected_n <- length(fold_info$coordinates[[1]])
  if (n != expected_n) {
    cli::cli_abort(
      c(
        "Length mismatch: data has {n} rows but fold info expects {expected_n}.",
        "i" = "The data may have been modified after unfolding."
      )
    )
  }

  point_index <- x$location
  valid_point_index <-
    typeof(point_index) %in%
    c("integer", "double") &&
    !is.object(point_index) &&
    is.null(dim(point_index)) &&
    length(point_index) == n &&
    !anyNA(point_index) &&
    all(is.finite(point_index)) &&
    all(point_index == floor(point_index)) &&
    !anyDuplicated(point_index) &&
    all(point_index >= 1 & point_index <= n)

  if (!valid_point_index) {
    cli::cli_abort(
      "The 1D {.field location} must be a complete point index from 1 to {n}."
    )
  }

  # Reconstruct nD structure in canonical unfold order, following the point
  # indices if rows were reordered after unfolding.
  value_order <- order(point_index)
  loc_cols <- paste0("location_", seq_len(ndim))
  args <- stats::setNames(
    lapply(seq_len(ndim), function(i) fold_info$coordinates[[i]]),
    loc_cols
  )
  args$value <- x$value[value_order]
  args$dim_names <- fold_info$dim_names
  args$dim_units <- fold_info$dim_units

  do.call(new_measure_nd_tbl, args)
}


#' @export
measure_fold.measure_list <- function(x) {
  if (!is.list(x) || is.data.frame(x) || !is.null(dim(x))) {
    cli::cli_abort("Fold input must be a one-dimensional list of measurements.")
  }
  if (length(x) == 0L) {
    return(new_measure_nd_list(list()))
  }

  member_info <- lapply(x, attr, which = "fold_info", exact = TRUE)
  has_member_info <- !vapply(member_info, is.null, logical(1))

  if (any(has_member_info) && !all(has_member_info)) {
    cli::cli_abort(
      c(
        "Cannot fold a list with partial member metadata.",
        "i" = "Every member must carry its own {.field fold_info} attribute."
      )
    )
  }

  if (all(has_member_info)) {
    # Per-member metadata is authoritative. Ignore any stale list-level copy.
    results <- lapply(x, measure_fold)
  } else {
    # Older list unfolds stored one shared grid on the list. Use it only when
    # none of the members has its own metadata.
    fold_info <- attr(x, "fold_info", exact = TRUE)
    if (is.null(fold_info)) {
      results <- lapply(x, measure_fold)
    } else {
      .nd_validate_fold_info(fold_info)
      results <- lapply(x, function(m) {
        attr(m, "fold_info") <- fold_info
        measure_fold(m)
      })
    }
  }

  new_measure_nd_list(results)
}


# Validate an unfolding permutation without coercing lossy or ambiguous input.
# @noRd
.nd_validate_fold_order <- function(order, ndim, arg = "order") {
  valid <-
    typeof(order) %in%
    c("integer", "double") &&
    !is.object(order) &&
    is.null(dim(order)) &&
    length(order) == ndim &&
    !anyNA(order) &&
    all(is.finite(order)) &&
    all(order == floor(order)) &&
    all(order >= 1 & order <= ndim) &&
    !anyDuplicated(order)

  if (!valid) {
    cli::cli_abort(
      "{.arg {arg}} must be a nonempty integral complete permutation of 1:{ndim}."
    )
  }

  # A complete set of unique integers within 1:ndim is the full permutation.
  as.integer(order)
}


# Validate the private metadata written by measure_unfold(). Older single
# measurements use the same fields and remain valid when those fields pass.
# @noRd
.nd_validate_fold_info <- function(fold_info) {
  required <- c("ndim", "dim_names", "dim_units", "order", "coordinates")
  valid_container <-
    is.list(fold_info) &&
    !is.object(fold_info) &&
    is.null(dim(fold_info)) &&
    !is.null(names(fold_info)) &&
    !anyDuplicated(names(fold_info)) &&
    all(required %in% names(fold_info))

  if (!valid_container) {
    cli::cli_abort(
      "Fold metadata must be a one-dimensional named list containing {.field ndim}, {.field dim_names}, {.field dim_units}, {.field order}, and {.field coordinates}."
    )
  }

  ndim <- fold_info$ndim
  valid_ndim <-
    typeof(ndim) %in%
    c("integer", "double") &&
    !is.object(ndim) &&
    is.null(dim(ndim)) &&
    length(ndim) == 1L &&
    !is.na(ndim) &&
    is.finite(ndim) &&
    ndim == floor(ndim) &&
    ndim >= 2 &&
    ndim <= .Machine$integer.max

  if (!valid_ndim) {
    cli::cli_abort(
      "Fold metadata {.field ndim} must be one integer of at least 2."
    )
  }

  ndim <- as.integer(ndim)

  coordinates <- fold_info$coordinates
  if (
    !is.list(coordinates) ||
      is.object(coordinates) ||
      !is.null(dim(coordinates)) ||
      length(coordinates) != ndim
  ) {
    cli::cli_abort(
      "Fold metadata {.field coordinates} must contain exactly one vector per dimension."
    )
  }

  .nd_validate_fold_order(fold_info$order, ndim, arg = "fold_info$order")

  for (field in c("dim_names", "dim_units")) {
    values <- fold_info[[field]]
    valid_values <-
      is.null(values) ||
      (is.character(values) &&
        !is.object(values) &&
        is.null(dim(values)) &&
        length(values) == ndim)

    if (!valid_values) {
      cli::cli_abort(
        "Fold metadata {.field {field}} must be NULL or a character vector of length {ndim}."
      )
    }

    if (is.null(values)) {
      next
    }

    known <- !is.na(values)
    if (any(known & !nzchar(trimws(values, whitespace = "[\\h\\v]")))) {
      cli::cli_abort(
        "Fold metadata {.field {field}} cannot contain blank values."
      )
    }

    if (identical(field, "dim_names") && anyDuplicated(values[known])) {
      cli::cli_abort(
        "Known fold metadata {.field dim_names} must be unique."
      )
    }
  }

  expected_coordinate_names <- paste0("dim_", seq_len(ndim))
  if (
    !is.list(coordinates) ||
      is.object(coordinates) ||
      length(coordinates) != ndim ||
      !identical(names(coordinates), expected_coordinate_names)
  ) {
    cli::cli_abort(
      "Fold metadata {.field coordinates} must be a named list with exactly {.val {expected_coordinate_names}} in dimension order."
    )
  }

  valid_coordinates <- vapply(
    coordinates,
    function(values) {
      typeof(values) %in%
        c("integer", "double") &&
        !is.object(values) &&
        is.null(dim(values))
    },
    logical(1)
  )

  if (!all(valid_coordinates)) {
    cli::cli_abort(
      "Every fold metadata coordinate must be a plain one-dimensional numeric vector."
    )
  }

  if (length(unique(lengths(coordinates))) > 1L) {
    cli::cli_abort(
      "Fold metadata coordinate vectors must have equal lengths."
    )
  }

  invisible(fold_info)
}


# ------------------------------------------------------------------------------
# Slice: Extract at specific coordinates
# ------------------------------------------------------------------------------

#' Extract slices from n-dimensional measurement
#'
#' Fixes one or more dimensions at specific coordinate values or ranges,
#' returning a lower-dimensional result.
#'
#' @param x A `measure_nd_tbl` or `measure_nd_list` object.
#' @param ... Named arguments specifying slice conditions. Names should be
#'   dimension numbers (e.g., `dim_1 = 5`) or dimension names if set
#'   (e.g., `time = 5`). Values can be:
#'   - A single value: exact match
#'   - A numeric vector: match any of these values
#'   - A function: applied to coordinates, should return logical
#' @param drop Logical. If `TRUE` (default), dimensions with a single
#'   value are dropped from the result. If `FALSE`, they are retained.
#'
#' @return A `measure_tbl`, `measure_nd_tbl`, `measure_list`, or
#'   `measure_nd_list` depending on the number of remaining dimensions.
#'
#' @examples
#' # Create a 3D measurement (2 x 3 x 4)
#' m3d <- new_measure_nd_tbl(
#'   location_1 = rep(1:2, each = 12),
#'   location_2 = rep(rep(1:3, each = 4), 2),
#'   location_3 = rep(1:4, 6),
#'   value = 1:24,
#'   dim_names = c("sample", "time", "wavelength")
#' )
#'
#' # Extract slice at sample = 1
#' slice_2d <- measure_slice(m3d, dim_1 = 1)
#' measure_ndim(slice_2d)  # 2D
#'
#' # Extract at specific time points
#' slice_subset <- measure_slice(m3d, dim_2 = c(1, 3))
#'
#' # Use dimension names
#' slice_wl <- measure_slice(m3d, wavelength = 2)
#'
#' @export
measure_slice <- function(x, ..., drop = TRUE) {
  UseMethod("measure_slice")
}


#' @export
measure_slice.measure_nd_tbl <- function(x, ..., drop = TRUE) {
  ndim <- measure_ndim(x)
  dim_names <- measure_dim_names(x)
  conditions <- list(...)

  if (length(conditions) == 0) {
    return(x)
  }

  # Resolve dimension references (by name or number)
  resolved <- resolve_dim_conditions(conditions, ndim, dim_names)

  # Apply filters
  mask <- rep(TRUE, nrow(x))
  for (dim_num in names(resolved)) {
    col_name <- paste0("location_", dim_num)
    condition <- resolved[[dim_num]]

    if (is.function(condition)) {
      mask <- mask & condition(x[[col_name]])
    } else {
      mask <- mask & (x[[col_name]] %in% condition)
    }
  }

  filtered <- x[mask, ]

  if (nrow(filtered) == 0) {
    cli::cli_warn("Slice returned no data points.")
  }

  # Determine which dimensions to drop
  if (drop) {
    dims_to_drop <- integer(0)
    for (i in seq_len(ndim)) {
      col_name <- paste0("location_", i)
      if (length(unique(filtered[[col_name]])) == 1) {
        dims_to_drop <- c(dims_to_drop, i)
      }
    }

    remaining_dims <- setdiff(seq_len(ndim), dims_to_drop)
    n_remaining <- length(remaining_dims)

    if (n_remaining == 0) {
      # All dimensions dropped - return single value
      return(filtered$value)
    } else if (n_remaining == 1) {
      # 1D result
      return(new_measure_tbl(
        location = filtered[[paste0("location_", remaining_dims)]],
        value = filtered$value
      ))
    } else {
      # nD result with fewer dimensions
      return(renumber_dims(
        filtered,
        remaining_dims,
        dim_names,
        measure_dim_units(x)
      ))
    }
  }

  # No dropping - return as nD with same structure
  rebuild_nd_tbl(filtered, ndim, dim_names, measure_dim_units(x))
}


#' @export
measure_slice.measure_nd_list <- function(x, ..., drop = TRUE) {
  results <- lapply(x, function(m) measure_slice(m, ..., drop = drop))

  # Determine result type based on first element
  if (length(results) == 0) {
    return(new_measure_nd_list(list()))
  }

  first <- results[[1]]
  if (is_measure_tbl(first) && !is_measure_nd_tbl(first)) {
    new_measure_list(results)
  } else if (is_measure_nd_tbl(first)) {
    new_measure_nd_list(results)
  } else {
    # Scalar results
    unlist(results)
  }
}


# ------------------------------------------------------------------------------
# Project: Aggregate across dimensions
# ------------------------------------------------------------------------------

#' Project n-dimensional measurement by aggregating across dimensions
#'
#' Reduces dimensionality by applying an aggregation function across
#' one or more dimensions.
#'
#' @param x A `measure_nd_tbl` or `measure_nd_list` object.
#' @param along Integer or character specifying which dimension(s) to
#'   aggregate across. Can use dimension numbers or names.
#' @param fn Aggregation function. Default is `mean`.
#' @param na_rm Logical. Remove NA values before aggregation? Default `TRUE`.
#' @param ... Additional arguments passed to `fn`.
#'
#' @return A `measure_tbl`, `measure_nd_tbl`, `measure_list`, or
#'   `measure_nd_list` with reduced dimensionality.
#'
#' @examples
#' # Create 2D measurement (time x wavelength)
#' m2d <- new_measure_nd_tbl(
#'   location_1 = rep(1:5, each = 3),
#'   location_2 = rep(c(254, 280, 320), times = 5),
#'   value = rnorm(15, mean = 100),
#'   dim_names = c("time", "wavelength")
#' )
#'
#' # Project across wavelength (average spectrum at each time)
#' time_trace <- measure_project(m2d, along = 2)
#'
#' # Project across time (average time profile at each wavelength)
#' wavelength_profile <- measure_project(m2d, along = 1)
#'
#' # Use sum instead of mean
#' total <- measure_project(m2d, along = 2, fn = sum)
#'
#' @export
measure_project <- function(x, along, fn = mean, na_rm = TRUE, ...) {
  UseMethod("measure_project")
}


#' @export
measure_project.measure_nd_tbl <- function(
  x,
  along,
  fn = mean,
  na_rm = TRUE,
  ...
) {
  ndim <- measure_ndim(x)
  dim_names_orig <- measure_dim_names(x)
  dim_units_orig <- measure_dim_units(x)

  # Resolve 'along' to dimension number(s)
  along <- resolve_dim_ref(along, ndim, dim_names_orig)

  if (any(along < 1) || any(along > ndim)) {
    cli::cli_abort(
      "{.arg along} must be between 1 and {ndim}."
    )
  }

  # Dimensions to keep
  keep_dims <- setdiff(seq_len(ndim), along)

  if (length(keep_dims) == 0) {
    # Projecting across all dimensions - return scalar
    if (na_rm) {
      return(fn(x$value[!is.na(x$value)], ...))
    } else {
      return(fn(x$value, ...))
    }
  }

  # Group by the kept dimensions and aggregate
  keep_cols <- paste0("location_", keep_dims)

  aggregated <- x |>
    dplyr::group_by(dplyr::across(dplyr::all_of(keep_cols))) |>
    dplyr::summarise(
      value = if (na_rm) fn(value[!is.na(value)], ...) else fn(value, ...),
      .groups = "drop"
    )

  # Build result
  n_remaining <- length(keep_dims)

  if (n_remaining == 1) {
    # 1D result
    new_measure_tbl(
      location = aggregated[[keep_cols]],
      value = aggregated$value
    )
  } else {
    # nD result
    renumber_dims(
      aggregated,
      keep_dims,
      dim_names_orig,
      dim_units_orig
    )
  }
}


#' @export
measure_project.measure_nd_list <- function(
  x,
  along,
  fn = mean,
  na_rm = TRUE,
  ...
) {
  results <- lapply(x, function(m) {
    measure_project(m, along = along, fn = fn, na_rm = na_rm, ...)
  })

  # Determine result type
  if (length(results) == 0) {
    return(new_measure_list(list()))
  }

  first <- results[[1]]
  if (is.numeric(first) && length(first) == 1) {
    # Scalar results
    unlist(results)
  } else if (is_measure_tbl(first) && !is_measure_nd_tbl(first)) {
    new_measure_list(results)
  } else {
    new_measure_nd_list(results)
  }
}


# ------------------------------------------------------------------------------
# Internal helpers
# ------------------------------------------------------------------------------

#' Resolve dimension conditions to numbered dimensions
#' @noRd
resolve_dim_conditions <- function(conditions, ndim, dim_names) {
  resolved <- list()

  for (name in names(conditions)) {
    # Try to match as dim_N format
    if (grepl("^dim_[0-9]+$", name)) {
      dim_num <- as.integer(sub("dim_", "", name, fixed = TRUE))
    } else if (!is.null(dim_names) && name %in% dim_names) {
      # Match by semantic name
      dim_num <- which(dim_names == name)
    } else {
      # Try as plain number
      dim_num <- suppressWarnings(as.integer(name))
      if (is.na(dim_num)) {
        cli::cli_abort(
          "Unknown dimension reference: {.val {name}}. Use dim_1, dim_2, ... or dimension names."
        )
      }
    }

    if (dim_num < 1 || dim_num > ndim) {
      cli::cli_abort(
        "Dimension {dim_num} is out of range (1 to {ndim})."
      )
    }

    resolved[[as.character(dim_num)]] <- conditions[[name]]
  }

  resolved
}


#' Resolve a dimension reference to number(s)
#' @noRd
resolve_dim_ref <- function(ref, ndim, dim_names) {
  if (is.numeric(ref)) {
    return(as.integer(ref))
  }

  if (is.character(ref)) {
    if (!is.null(dim_names)) {
      matches <- match(ref, dim_names)
      if (anyNA(matches)) {
        cli::cli_abort(
          "Unknown dimension name(s): {.val {ref[is.na(matches)]}}."
        )
      }
      return(matches)
    } else {
      cli::cli_abort(
        "Cannot use dimension names: no dim_names set on this measurement."
      )
    }
  }

  cli::cli_abort(
    "{.arg along} must be numeric or character."
  )
}


#' Renumber dimensions after dropping some
#' @noRd
renumber_dims <- function(
  data,
  remaining_dims,
  dim_names_orig,
  dim_units_orig
) {
  n_new <- length(remaining_dims)
  old_cols <- paste0("location_", remaining_dims)
  new_cols <- paste0("location_", seq_len(n_new))

  # Build args for new_measure_nd_tbl
  args <- stats::setNames(
    lapply(old_cols, function(col) data[[col]]),
    new_cols
  )
  args$value <- data$value

  # Update dimension names/units if present
  if (!is.null(dim_names_orig)) {
    args$dim_names <- dim_names_orig[remaining_dims]
  }
  if (!is.null(dim_units_orig)) {
    args$dim_units <- dim_units_orig[remaining_dims]
  }

  do.call(new_measure_nd_tbl, args)
}


#' Rebuild nD tbl from filtered data
#' @noRd
rebuild_nd_tbl <- function(data, ndim, dim_names, dim_units) {
  loc_cols <- paste0("location_", seq_len(ndim))
  args <- stats::setNames(
    lapply(loc_cols, function(col) data[[col]]),
    loc_cols
  )
  args$value <- data$value
  args$dim_names <- dim_names
  args$dim_units <- dim_units

  do.call(new_measure_nd_tbl, args)
}
