.prominence_boundary_band <- function(location, value, baseline, tolerance) {
  if (is.null(baseline) && is.null(tolerance)) {
    return(NULL)
  }
  if (
    !is.numeric(tolerance) ||
      !is.null(dim(tolerance)) ||
      length(tolerance) != 1L ||
      !is.finite(tolerance) ||
      tolerance <= 0
  ) {
    cli::cli_abort(
      "{.arg boundary_tolerance} must be a positive finite number in signal units."
    )
  }
  if (
    !all(is.finite(location)) ||
      !all(is.finite(value)) ||
      (is.unsorted(location, strictly = TRUE) &&
        is.unsorted(rev(location), strictly = TRUE))
  ) {
    cli::cli_abort(
      "Baseline-return bounds require a finite signal and a finite, strictly monotonic location axis."
    )
  }

  if (is.function(baseline)) {
    baseline <- baseline(location)
  } else if (length(baseline) != 1L) {
    cli::cli_abort(
      "Supply {.arg boundary_baseline} as a reference level or a function of location."
    )
  }
  if (
    !is.numeric(baseline) ||
      !is.null(dim(baseline)) ||
      !length(baseline) %in% c(1L, length(location)) ||
      !all(is.finite(baseline))
  ) {
    cli::cli_abort(
      "{.arg boundary_baseline} must provide one finite level or one finite value per location."
    )
  }
  band <- as.double(baseline) + tolerance
  if (!all(is.finite(band))) {
    cli::cli_abort(
      "The sum of {.arg boundary_baseline} and {.arg boundary_tolerance} must be finite."
    )
  }
  band
}

.prominence_return_bounds <- function(location, value, apex, limits, band) {
  at_baseline <- value <= band
  if (any(at_baseline[apex])) {
    i <- which(at_baseline[apex])[1L]
    cli::cli_abort(
      "Peak at {.val {location[apex[i]]}} does not rise above the supplied baseline band."
    )
  }

  # Find the nearest sampled return on either side in acquisition order.
  returns <- which(at_baseline)
  preceding <- findInterval(apex, returns)
  left <- c(NA_integer_, returns)[preceding + 1L]
  right <- returns[preceding + 1L]
  unresolved <- is.na(left) |
    is.na(right) |
    left < limits$left |
    right > limits$right
  if (any(unresolved)) {
    i <- which(unresolved)[1L]
    cli::cli_abort(
      "Peak at {.val {location[apex[i]]}} has no sampled return to the baseline band on both sides within its prominence bounds."
    )
  }
  list(left = left, right = right)
}
