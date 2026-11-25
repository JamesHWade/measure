.nd_list_to_array <- function(
  measure_nd_list,
  grid_info = NULL,
  supported_dims = 2:3,
  method = "Multiway decomposition"
) {
  n_samples <- length(measure_nd_list)
  if (is.null(grid_info) && n_samples == 0L) {
    cli::cli_abort(
      "{method} needs at least one sample to learn its coordinate grid."
    )
  }

  ndim <- if (is.null(grid_info)) {
    measure_ndim(measure_nd_list[[1]])
  } else {
    length(grid_info)
  }
  if (!ndim %in% supported_dims) {
    cli::cli_abort(
      "{method} requires {paste(supported_dims, collapse = ' or ')} measurement axes per sample. Got {ndim}."
    )
  }

  loc_cols <- paste0("location_", seq_len(ndim))
  if (is.null(grid_info)) {
    first <- measure_nd_list[[1L]]
    grid_info <- lapply(first[loc_cols], function(axis) sort(unique(axis)))
  }
  grid_sizes <- lengths(grid_info)
  indices <- vector("list", n_samples)
  n_observed <- 0

  for (i in seq_len(n_samples)) {
    sample <- measure_nd_list[[i]]
    if (measure_ndim(sample) != ndim) {
      cli::cli_abort(
        "{method} sample {i} must have {ndim} measurement axes to match the reference grid."
      )
    }
    axis_indices <- lapply(loc_cols, function(axis) {
      locations <- sample[[axis]]
      index <- match(locations, grid_info[[axis]])
      if (
        !length(locations) ||
          !all(is.finite(locations)) ||
          anyNA(index) ||
          length(unique(index)) != length(grid_info[[axis]])
      ) {
        cli::cli_abort(
          "{method} sample {i} has coordinates on {.field {axis}} that do not match the reference grid."
        )
      }
      index
    })
    point_index <- do.call(cbind, axis_indices)
    if (anyDuplicated(point_index, MARGIN = 1L)) {
      cli::cli_abort(
        "{method} sample {i} contains duplicate coordinate tuples."
      )
    }
    indices[[i]] <- cbind(i, point_index)
    n_observed <- n_observed + sum(!is.na(sample$value))
  }

  # Check occupancy before allocating a potentially large Cartesian product.
  dimensions <- c(n_samples, grid_sizes)
  n_cells <- prod(dimensions)
  n_missing <- n_cells - n_observed
  if (n_missing > 0) {
    pct_missing <- 100 * n_missing / n_cells
    if (pct_missing > 10) {
      cli::cli_abort(c(
        "Array has {.val {sprintf('%.1f%%', pct_missing)}} missing values.",
        "i" = "All samples must have the same regular grid."
      ))
    }
    cli::cli_warn(
      "Array has {n_missing} missing value{?s} ({sprintf('%.1f%%', pct_missing)})."
    )
  }

  arr <- array(NA_real_, dim = dimensions)
  for (i in seq_len(n_samples)) {
    arr[indices[[i]]] <- measure_nd_list[[i]]$value
  }
  list(array = arr, grid_info = grid_info)
}

.fit_multiway_preprocessing <- function(X, center, scale) {
  content_modes <- seq_along(dim(X))[-1L]
  center_values <- if (center) apply(X, content_modes, mean, na.rm = TRUE)
  scale_values <- if (scale) {
    sds <- apply(X, content_modes, stats::sd, na.rm = TRUE)
    sds[sds == 0] <- 1
    sds
  }
  list(
    array = .apply_multiway_preprocessing(X, center_values, scale_values),
    center_values = center_values,
    scale_values = scale_values
  )
}

.apply_multiway_preprocessing <- function(X, center_values, scale_values) {
  content_modes <- seq_along(dim(X))[-1L]
  if (!is.null(center_values)) {
    X <- sweep(X, content_modes, center_values, "-")
  }
  if (!is.null(scale_values)) {
    X <- sweep(X, content_modes, scale_values, "/")
  }
  X
}
