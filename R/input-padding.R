# Use the observed tuples as keys, preserving their order across the samples.
pad_measure_nd_dims <- function(measures, loc_cols) {
  coordinates <- lapply(measures, \(member) member[loc_cols])
  for (i in seq_along(coordinates)) {
    axes <- coordinates[[i]]
    if (!all(vapply(axes, \(axis) all(is.finite(axis)), logical(1)))) {
      cli::cli_abort(
        "Cannot pad sample {i} with missing or non-finite coordinates."
      )
    }
    if (vctrs::vec_duplicate_any(axes)) {
      cli::cli_abort("Cannot pad sample {i} with duplicate coordinate tuples.")
    }
  }

  grid <- vctrs::vec_unique(dplyr::bind_rows(coordinates))
  lapply(seq_along(measures), function(i) {
    index <- vctrs::vec_match(grid, coordinates[[i]])
    result <- grid
    result$value <- vctrs::vec_slice(measures[[i]]$value, index)
    result
  })
}
