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
#' @param drop One non-missing logical value. If `TRUE` (default), dimensions
#'   with a single observed coordinate are dropped from the result. If `FALSE`,
#'   all original dimensions are retained.
#'
#' @return For one measurement, a numeric vector when all dimensions are
#'   dropped, a `measure_tbl` when one dimension remains, or a
#'   `measure_nd_tbl` when two or more dimensions remain. For a list input,
#'   homogeneous results are returned as a numeric vector, `measure_list`, or
#'   `measure_nd_list`, respectively.
#'
#' @details
#' Coordinate selections use exact numeric membership. A numeric selection
#' can include `NA` to select rows whose stored coordinate is unknown. A
#' predicate must return one non-missing logical value for every input row.
#' Empty slices warn and remain n-dimensional with the original axes because
#' no axis has a single observed coordinate. Slicing retains the input point
#' order. If list members would have different result dimensionalities or
#' retained axis schemas, an error recommends `drop = FALSE` or mapping over
#' members separately.
#' The dropping rule counts a stored `NA` coordinate as one distinct state;
#' it does not infer a physical coordinate for it. Repeated observations at
#' one coordinate remain a numeric vector for a single measurement. A list
#' cannot flatten such multi-value results into one scalar per member.
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
  .nd_validate_tbl(x)
  .nd_validate_flag(drop, "drop")
  conditions <- list(...)
  .nd_slice_tbl(x, conditions, drop)$result
}


#' @export
measure_slice.measure_nd_list <- function(x, ..., drop = TRUE) {
  .nd_validate_list(x)
  .nd_validate_flag(drop, "drop")
  conditions <- list(...)

  if (length(x) == 0L) {
    # Validate selector syntax against the documented 2D empty-list prototype.
    resolve_dim_conditions(conditions, measure_ndim(x), measure_dim_names(x))
    return(new_measure_nd_list(list()))
  }

  extracted <- lapply(x, .nd_slice_tbl, conditions = conditions, drop = drop)
  selector_dims <- lapply(extracted, `[[`, "selector_dims")
  if (!all(vapply(selector_dims, identical, logical(1), selector_dims[[1]]))) {
    cli::cli_abort(
      c(
        "Slice selectors resolve to different dimensions across {.cls measure_nd_list} members.",
        "i" = "Use numeric dimension selectors or map the slice over members separately."
      )
    )
  }

  .nd_assemble_list_results(extracted, operation = "slice", drop = drop)
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
#' @param along A nonempty vector of unique whole-number dimension indices or
#'   known semantic dimension names to aggregate across.
#' @param fn Function that returns one plain numeric scalar for each group.
#'   Default is `mean`.
#' @param na_rm One non-missing logical value. Remove `NA` values before
#'   aggregation? Default `TRUE`.
#' @param ... Additional arguments passed to `fn`.
#'
#' @return A numeric scalar when all dimensions are projected, a
#'   `measure_tbl` when one dimension remains, or a `measure_nd_tbl` when two
#'   or more dimensions remain. For a list input, homogeneous results are
#'   returned as a numeric vector, `measure_list`, or `measure_nd_list`,
#'   respectively.
#'
#' @details
#' Projection groups rows by the retained coordinate columns. `dplyr` sorts
#' these groups in ascending coordinate order (with missing coordinates last),
#' so the result order describes coordinate groups rather than acquisition
#' timing. With `na_rm = TRUE`, `NA` values are removed before `fn` is called.
#' An all-missing group therefore calls `fn(numeric(0))`; for example, `mean`
#' returns `NaN` and `sum` returns `0`. An empty input with retained axes
#' yields an empty measurement; projecting all axes applies `fn` to
#' `numeric(0)`. List members must retain the same axis positions, names, and
#' units or projection errors and recommends mapping over members separately.
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
  .nd_validate_tbl(x)
  .nd_validate_flag(na_rm, "na_rm")
  .nd_validate_project_fn(fn)
  .nd_project_tbl(x, along, fn, na_rm, list(...))$result
}


#' @export
measure_project.measure_nd_list <- function(
  x,
  along,
  fn = mean,
  na_rm = TRUE,
  ...
) {
  .nd_validate_list(x)
  .nd_validate_flag(na_rm, "na_rm")
  .nd_validate_project_fn(fn)

  if (length(x) == 0L) {
    resolve_dim_ref(along, measure_ndim(x), measure_dim_names(x))
    return(new_measure_list(list()))
  }

  extracted <- lapply(
    x,
    .nd_project_tbl,
    along = along,
    fn = fn,
    na_rm = na_rm,
    dots = list(...)
  )
  along_dims <- lapply(extracted, `[[`, "along_dims")
  if (!all(vapply(along_dims, identical, logical(1), along_dims[[1]]))) {
    cli::cli_abort(
      c(
        "Projection dimensions resolve differently across {.cls measure_nd_list} members.",
        "i" = "Use numeric dimension references or map the projection over members separately."
      )
    )
  }

  .nd_assemble_list_results(extracted, operation = "project")
}


# Apply one slice to a validated nD table and retain its axis schema privately.
# The schema is used by list methods because public 1D measure_tbl values do
# not carry nD axis-name or unit metadata.
# @noRd
.nd_slice_tbl <- function(x, conditions, drop) {
  ndim <- attr(x, "ndim")
  dim_names_orig <- measure_dim_names(x)
  dim_units_orig <- measure_dim_units(x)
  resolved <- resolve_dim_conditions(conditions, ndim, dim_names_orig)

  if (length(conditions) == 0L) {
    return(list(
      result = x,
      type = "nd",
      schema = .nd_axis_schema(seq_len(ndim), dim_names_orig, dim_units_orig),
      selector_dims = integer()
    ))
  }

  mask <- rep(TRUE, nrow(x))
  resolved_dims <- as.integer(names(resolved))
  for (dim_num in resolved_dims) {
    col_name <- paste0("location_", dim_num)
    condition <- resolved[[as.character(dim_num)]]

    if (is.function(condition)) {
      selected <- condition(x[[col_name]])
      selector <- names(conditions)[match(dim_num, resolved_dims)]
      .nd_validate_predicate(selected, nrow(x), selector)
      mask <- mask & selected
    } else {
      mask <- mask & (x[[col_name]] %in% condition)
    }
  }

  filtered <- x[mask, , drop = FALSE]
  if (nrow(filtered) == 0L) {
    cli::cli_warn("Slice returned no data points.")
  }

  if (drop) {
    dims_to_drop <- which(vapply(
      seq_len(ndim),
      function(i) length(unique(filtered[[paste0("location_", i)]])) == 1L,
      logical(1)
    ))
    remaining_dims <- setdiff(seq_len(ndim), dims_to_drop)
  } else {
    remaining_dims <- seq_len(ndim)
  }

  n_remaining <- length(remaining_dims)
  if (n_remaining == 0L) {
    result <- filtered$value
    type <- "scalar"
  } else if (n_remaining == 1L) {
    result <- new_measure_tbl(
      location = filtered[[paste0("location_", remaining_dims)]],
      value = filtered$value
    )
    type <- "1d"
  } else if (identical(remaining_dims, seq_len(ndim))) {
    result <- rebuild_nd_tbl(filtered, ndim, dim_names_orig, dim_units_orig)
    type <- "nd"
  } else {
    result <- renumber_dims(
      filtered,
      remaining_dims,
      dim_names_orig,
      dim_units_orig
    )
    type <- "nd"
  }

  list(
    result = result,
    type = type,
    schema = .nd_axis_schema(remaining_dims, dim_names_orig, dim_units_orig),
    selector_dims = resolved_dims
  )
}


# Apply one projection to a validated nD table.
# @noRd
.nd_project_tbl <- function(x, along, fn, na_rm, dots) {
  ndim <- attr(x, "ndim")
  dim_names_orig <- measure_dim_names(x)
  dim_units_orig <- measure_dim_units(x)
  along_dims <- resolve_dim_ref(along, ndim, dim_names_orig)
  keep_dims <- setdiff(seq_len(ndim), along_dims)

  if (length(keep_dims) == 0L) {
    result <- .nd_project_group(x$value, fn, na_rm, dots)
    type <- "scalar"
  } else {
    keep_cols <- paste0("location_", keep_dims)
    aggregated <- x |>
      dplyr::group_by(dplyr::across(dplyr::all_of(keep_cols))) |>
      dplyr::summarise(
        value = .nd_project_group(value, fn, na_rm, dots),
        .groups = "drop"
      )

    if (length(keep_dims) == 1L) {
      result <- new_measure_tbl(
        location = aggregated[[keep_cols]],
        value = aggregated$value
      )
      type <- "1d"
    } else {
      result <- renumber_dims(
        aggregated,
        keep_dims,
        dim_names_orig,
        dim_units_orig
      )
      type <- "nd"
    }
  }

  list(
    result = result,
    type = type,
    schema = .nd_axis_schema(keep_dims, dim_names_orig, dim_units_orig),
    along_dims = along_dims
  )
}


# Validate and call the projection function on one group's signal values.
# @noRd
.nd_project_group <- function(values, fn, na_rm, dots) {
  if (na_rm) {
    values <- values[!is.na(values)]
  }

  result <- do.call(fn, c(list(values), dots))
  if (!.nd_is_plain_numeric_vector(result) || length(result) != 1L) {
    cli::cli_abort(
      "{.arg fn} must return one plain numeric scalar for each projection group."
    )
  }

  result
}


# Validate scalar function arguments shared by extraction methods.
# @noRd
.nd_validate_flag <- function(x, arg) {
  valid <-
    typeof(x) == "logical" &&
    !is.object(x) &&
    is.null(dim(x)) &&
    length(x) == 1L &&
    !is.na(x) &&
    all(names(attributes(x)) %in% "names")

  if (!valid) {
    cli::cli_abort("{.arg {arg}} must be one plain, non-missing logical value.")
  }

  invisible(x)
}


# Validate the exact shape required from a predicate result.
# @noRd
.nd_validate_predicate <- function(x, n, selector) {
  valid <-
    typeof(x) == "logical" &&
    !is.object(x) &&
    is.null(dim(x)) &&
    length(x) == n &&
    !anyNA(x) &&
    all(names(attributes(x)) %in% "names")

  if (!valid) {
    cli::cli_abort(
      "Predicate for {.arg {selector}} must return a plain logical vector of length {n} with no missing values."
    )
  }

  invisible(x)
}


# Validate a projection function before applying it to any groups.
# @noRd
.nd_validate_project_fn <- function(fn) {
  if (!is.function(fn)) {
    cli::cli_abort("{.arg fn} must be a function.")
  }
  invisible(fn)
}


# Record retained nD axes privately; 1D measure_tbl intentionally has no such
# public metadata.
# @noRd
.nd_axis_schema <- function(axes, dim_names, dim_units) {
  if (length(axes) == 0L) {
    return(list(
      positions = integer(),
      dim_names = NULL,
      dim_units = NULL
    ))
  }

  list(
    positions = as.integer(axes),
    dim_names = if (is.null(dim_names)) NULL else dim_names[axes],
    dim_units = if (is.null(dim_units)) NULL else dim_units[axes]
  )
}


# Return an internal class category for every extraction result.
# @noRd
.nd_result_type <- function(x) {
  if (is_measure_nd_tbl(x)) {
    return("nd")
  }
  if (is_measure_tbl(x)) {
    return("1d")
  }
  if (.nd_is_plain_numeric_vector(x)) {
    return("scalar")
  }
  NA_character_
}


# Check every member's class and retained schema before constructing the list
# result, then preserve the existing scalar/1D/nD return conventions.
# @noRd
.nd_assemble_list_results <- function(extracted, operation, drop = TRUE) {
  if (length(extracted) == 0L) {
    if (identical(operation, "slice")) {
      return(new_measure_nd_list(list()))
    }
    return(new_measure_list(list()))
  }

  results <- lapply(extracted, `[[`, "result")
  observed_types <- vapply(results, .nd_result_type, character(1))
  if (anyNA(observed_types)) {
    cli::cli_abort(
      "Every {.cls measure_nd_list} member must produce a numeric, 1D, or nD extraction result."
    )
  }

  if (length(unique(observed_types)) != 1L) {
    if (identical(operation, "slice") && isTRUE(drop)) {
      cli::cli_abort(
        c(
          "Slicing {.cls measure_nd_list} members produced mixed scalar, 1D, and nD results.",
          "i" = "Use {.code drop = FALSE} to retain all axes, or map the slice over members separately."
        )
      )
    }
    cli::cli_abort(
      "Projection results across {.cls measure_nd_list} members have incompatible dimensionality; map the operation over members separately."
    )
  }

  schemas <- lapply(extracted, `[[`, "schema")
  if (!all(vapply(schemas, identical, logical(1), schemas[[1]]))) {
    if (identical(operation, "slice") && isTRUE(drop)) {
      cli::cli_abort(
        c(
          "Slicing {.cls measure_nd_list} members retained different axis positions, names, or units.",
          "i" = "Use {.code drop = FALSE} when the difference comes from dropped axes, or map the slice over members separately."
        )
      )
    }
    cli::cli_abort(
      c(
        "Extraction results across {.cls measure_nd_list} members retained different axis positions, names, or units.",
        "i" = "Map the operation over members separately."
      )
    )
  }

  if (identical(observed_types[[1]], "scalar")) {
    if (any(lengths(results) != 1L)) {
      cli::cli_abort(
        c(
          "Dropping all axes left multiple observations in a list member.",
          "i" = "Use {.code drop = FALSE} or map the slice over members separately."
        )
      )
    }
    return(unlist(results))
  }
  if (identical(observed_types[[1]], "1d")) {
    return(new_measure_list(results))
  }
  new_measure_nd_list(results)
}


# ------------------------------------------------------------------------------
# Internal helpers
# ------------------------------------------------------------------------------

#' Resolve dimension conditions to numbered dimensions
#' @noRd
resolve_dim_conditions <- function(conditions, ndim, dim_names) {
  if (length(conditions) == 0L) {
    return(list())
  }

  condition_names <- names(conditions)
  if (
    is.null(condition_names) ||
      length(condition_names) != length(conditions) ||
      anyNA(condition_names) ||
      !all(nzchar(trimws(condition_names)))
  ) {
    cli::cli_abort("Every slice condition must have a nonempty dimension name.")
  }
  if (anyDuplicated(condition_names)) {
    cli::cli_abort("Slice condition names must be unique.")
  }

  resolved_dims <- vapply(
    condition_names,
    .nd_resolve_slice_dim,
    integer(1),
    ndim = ndim,
    dim_names = dim_names
  )
  if (anyDuplicated(resolved_dims)) {
    cli::cli_abort(
      "Slice conditions must not select the same dimension through multiple aliases."
    )
  }

  for (i in seq_along(conditions)) {
    condition <- conditions[[i]]
    valid_condition <- is.function(condition) ||
      .nd_is_plain_numeric_vector(condition)
    if (!valid_condition) {
      cli::cli_abort(
        "Slice condition {.arg {condition_names[[i]]}} must be a function or a plain one-dimensional numeric vector."
      )
    }
  }

  names(conditions) <- as.character(resolved_dims)
  conditions
}


# Resolve a slice selector string, rejecting reserved-name collisions.
# @noRd
.nd_resolve_slice_dim <- function(name, ndim, dim_names) {
  candidates <- numeric()
  reserved <- character()
  if (grepl("\\Adim_[0-9]+\\z", name, perl = TRUE)) {
    reserved <- c(reserved, sub("^dim_", "", name))
  }
  if (grepl("\\A[0-9]+\\z", name, perl = TRUE)) {
    reserved <- c(reserved, name)
  }
  if (length(reserved) > 0L) {
    candidates <- c(candidates, suppressWarnings(as.double(reserved)))
  }

  if (!is.null(dim_names)) {
    semantic <- which(!is.na(dim_names) & dim_names == name)
    candidates <- c(candidates, semantic)
  }
  candidates <- unique(candidates)

  if (length(candidates) == 0L) {
    cli::cli_abort(
      "Unknown dimension reference: {.val {name}}. Use dim_1, dim_2, an integer dimension string, or a known dimension name."
    )
  }
  if (length(candidates) > 1L) {
    cli::cli_abort(
      "Dimension reference {.val {name}} is ambiguous between a reserved dimension and a semantic axis name."
    )
  }
  if (
    !is.finite(candidates[[1]]) ||
      candidates[[1]] < 1 ||
      candidates[[1]] > ndim ||
      candidates[[1]] != floor(candidates[[1]])
  ) {
    cli::cli_abort(
      "Dimension reference {.val {name}} is out of range (1 to {ndim})."
    )
  }

  as.integer(candidates[[1]])
}


#' Resolve a dimension reference to number(s)
#' @noRd
resolve_dim_ref <- function(ref, ndim, dim_names) {
  if (
    typeof(ref) %in%
      c("integer", "double") &&
      !is.object(ref) &&
      is.null(dim(ref)) &&
      all(names(attributes(ref)) %in% "names")
  ) {
    valid <-
      length(ref) > 0L &&
      !anyNA(ref) &&
      all(is.finite(ref)) &&
      all(ref == floor(ref)) &&
      all(ref >= 1 & ref <= ndim)
    if (!valid) {
      cli::cli_abort(
        "{.arg along} must contain nonempty, unique whole-number dimensions between 1 and {ndim}."
      )
    }
    if (anyDuplicated(ref)) {
      cli::cli_abort("{.arg along} must not contain duplicate dimensions.")
    }
    return(as.integer(ref))
  }

  if (
    typeof(ref) == "character" &&
      !is.object(ref) &&
      is.null(dim(ref)) &&
      all(names(attributes(ref)) %in% "names")
  ) {
    valid_names <- length(ref) > 0L && !anyNA(ref) && all(nzchar(trimws(ref)))
    if (!valid_names) {
      cli::cli_abort(
        "{.arg along} must contain nonempty, non-missing dimension names."
      )
    }
    if (is.null(dim_names)) {
      cli::cli_abort(
        "Cannot use dimension names: no dim_names set on this measurement."
      )
    }
    matches <- match(ref, dim_names)
    if (anyNA(matches)) {
      cli::cli_abort(
        "Unknown dimension name(s): {.val {ref[is.na(matches)]}}."
      )
    }
    if (anyDuplicated(matches)) {
      cli::cli_abort("{.arg along} must not contain duplicate dimensions.")
    }
    return(as.integer(matches))
  }

  cli::cli_abort(
    "{.arg along} must be a plain numeric or character vector."
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
