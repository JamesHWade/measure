# ==============================================================================
# Axis Semantics & Validation Utilities
#
# This file provides validation checks and axis metadata inference for
# measure data. These utilities help ensure data quality and enable
# technique-aware processing.
# ==============================================================================

# ------------------------------------------------------------------------------
# Main validation function
# ------------------------------------------------------------------------------

#' Validate measure data
#'
#' Performs comprehensive validation checks on measure data, including
#' axis monotonicity, duplicate detection, missing value detection, and
#' spacing regularity.
#'
#' @param x A `measure_tbl`, `measure_list`, or data frame with measure column.
#' @param checks Character vector of checks to perform. Default is all checks:
#'   `"monotonic"`, `"duplicates"`, `"missing"`, `"spacing"`.
#' @param tolerance Numeric tolerance for spacing regularity check.
#'   Default is 1e-6.
#' @param action What to do when validation fails: `"error"` (default),
#'   `"warn"`, or `"message"`.
#'
#' @return Invisibly returns a list with validation results. Each element
#'   is a list with `valid` (logical), `message` (character), and `details`.
#'
#' @examples
#' # Create valid measure data
#' spec <- new_measure_tbl(location = 1:100, value = sin(1:100 / 10))
#' validate_measure(spec)
#'
#' # Data with issues
#' spec_dup <- new_measure_tbl(location = c(1, 2, 2, 3), value = c(1, 2, 3, 4))
#' try(validate_measure(spec_dup))
#'
#' # Only check specific issues
#' validate_measure(spec, checks = c("monotonic", "missing"))
#'
#' @export
validate_measure <- function(x,
                             checks = c("monotonic", "duplicates", "missing", "spacing"),
                             tolerance = 1e-6,
                             action = c("error", "warn", "message")) {
  action <- match.arg(action)
  checks <- match.arg(checks, several.ok = TRUE)

  # Handle different input types
  if (is_measure_tbl(x)) {
    measures <- list(x)
    names(measures) <- "1"
  } else if (is_measure_list(x)) {
    measures <- x
    names(measures) <- seq_along(x)
  } else if (is.data.frame(x)) {
    meas_cols <- find_measure_cols(x)
    if (length(meas_cols) == 0) {
      cli::cli_abort(
        "No measure column found in data frame.",
        "i" = "Use {.fn step_measure_input_wide} or {.fn step_measure_input_long} first."
      )
    }
    measures <- x[[meas_cols[1]]]
    names(measures) <- seq_along(measures)
  } else {
    cli::cli_abort(
      "{.arg x} must be a {.cls measure_tbl}, {.cls measure_list}, or data frame."
    )
  }

  results <- list()

  # Run requested checks
  if ("monotonic" %in% checks) {
    results$monotonic <- check_monotonic(measures)
  }
  if ("duplicates" %in% checks) {
    results$duplicates <- check_no_duplicates(measures)
  }
  if ("missing" %in% checks) {
    results$missing <- check_no_missing(measures)
  }
  if ("spacing" %in% checks) {
    results$spacing <- check_regular_spacing(measures, tolerance = tolerance)
  }

  # Report issues based on action
  failed <- purrr::keep(results, ~ !.x$valid)
  if (length(failed) > 0) {
    msgs <- purrr::map_chr(failed, ~ .x$message)
    combined_msg <- paste(msgs, collapse = "\n")

    if (action == "error") {
      cli::cli_abort(c(
        "Measure validation failed:",
        "x" = combined_msg
      ))
    } else if (action == "warn") {
      cli::cli_warn(c(
        "Measure validation issues detected:",
        "!" = combined_msg
      ))
    } else {
      cli::cli_inform(c(
        "Measure validation notes:",
        "i" = combined_msg
      ))
    }
  }

  invisible(results)
}

# ------------------------------------------------------------------------------
# Individual validation checks
# ------------------------------------------------------------------------------

#' Check for monotonic axis
#'
#' Validates that location values are monotonically increasing or decreasing.
#'
#' @param measures A list of `measure_tbl` objects.
#'
#' @return A list with `valid`, `message`, and `details`.
#'
#' @examples
#' spec <- new_measure_tbl(location = 1:10, value = rnorm(10))
#' measure:::check_monotonic(list(spec))
#'
#' @noRd
check_monotonic <- function(measures) {
  issues <- character()

  for (i in seq_along(measures)) {
    loc <- measures[[i]]$location

    # Skip if too few points or all NA
    loc_clean <- loc[!is.na(loc)]
    if (length(loc_clean) < 2) next

    diffs <- diff(loc_clean)

    # Check for weakly monotonic (allowing zeros for duplicates, caught separately)
    # Strictly increasing: all diffs >= 0 and not all zeros
    # Strictly decreasing: all diffs <= 0 and not all zeros
    all_nonneg <- all(diffs >= 0, na.rm = TRUE)
    all_nonpos <- all(diffs <= 0, na.rm = TRUE)

    if (!all_nonneg && !all_nonpos) {
      # Find first direction change (ignoring zeros)
      nonzero_diffs <- diffs[diffs != 0]
      if (length(nonzero_diffs) >= 2) {
        sign_changes <- which(diff(sign(nonzero_diffs)) != 0)
        if (length(sign_changes) > 0) {
          # Find position in original sequence
          first_change <- sign_changes[1]
          issues <- c(issues, sprintf(
            "Sample %s: non-monotonic (direction changes detected)",
            names(measures)[i]
          ))
        }
      }
    }
  }

  list(
    valid = length(issues) == 0,
    message = if (length(issues) > 0) {
      sprintf("Axis is not monotonic in %d sample(s)", length(issues))
    } else {
      "Axis is monotonic"
    },
    details = issues
  )
}

#' Check for duplicate locations
#'
#' Validates that there are no duplicate location values.
#'
#' @param measures A list of `measure_tbl` objects.
#'
#' @return A list with `valid`, `message`, and `details`.
#'
#' @noRd
check_no_duplicates <- function(measures) {
  issues <- character()

  for (i in seq_along(measures)) {
    loc <- measures[[i]]$location
    dups <- loc[duplicated(loc)]

    if (length(dups) > 0) {
      issues <- c(issues, sprintf(
        "Sample %s: %d duplicate location(s) found (e.g., %.4g)",
        names(measures)[i],
        length(dups),
        dups[1]
      ))
    }
  }

  list(
    valid = length(issues) == 0,
    message = if (length(issues) > 0) {
      sprintf("Duplicate locations found in %d sample(s)", length(issues))
    } else {
      "No duplicate locations"
    },
    details = issues
  )
}

#' Check for missing values
#'
#' Validates that there are no NA values in location or value columns.
#'
#' @param measures A list of `measure_tbl` objects.
#'
#' @return A list with `valid`, `message`, and `details`.
#'
#' @noRd
check_no_missing <- function(measures) {
  issues <- character()

  for (i in seq_along(measures)) {
    m <- measures[[i]]
    loc_na <- sum(is.na(m$location))
    val_na <- sum(is.na(m$value))

    if (loc_na > 0 || val_na > 0) {
      issues <- c(issues, sprintf(
        "Sample %s: %d missing location(s), %d missing value(s)",
        names(measures)[i],
        loc_na,
        val_na
      ))
    }
  }

  list(
    valid = length(issues) == 0,
    message = if (length(issues) > 0) {
      sprintf("Missing values found in %d sample(s)", length(issues))
    } else {
      "No missing values"
    },
    details = issues
  )
}

#' Check for regular spacing
#'
#' Validates that location values have regular (uniform) spacing.
#'
#' @param measures A list of `measure_tbl` objects.
#' @param tolerance Numeric tolerance for spacing comparison.
#'
#' @return A list with `valid`, `message`, and `details`.
#'
#' @noRd
check_regular_spacing <- function(measures, tolerance = 1e-6) {
  issues <- character()

  for (i in seq_along(measures)) {
    loc <- measures[[i]]$location
    if (length(loc) < 3) next

    diffs <- diff(loc)
    median_diff <- stats::median(diffs)

    # Check if all differences are approximately equal
    rel_diffs <- abs(diffs - median_diff) / abs(median_diff)
    irregular <- which(rel_diffs > tolerance)

    if (length(irregular) > 0) {
      max_dev <- max(rel_diffs)
      issues <- c(issues, sprintf(
        "Sample %s: irregular spacing (max deviation: %.2f%%, %d irregular intervals)",
        names(measures)[i],
        max_dev * 100,
        length(irregular)
      ))
    }
  }

  list(
    valid = length(issues) == 0,
    message = if (length(issues) > 0) {
      sprintf("Irregular spacing found in %d sample(s)", length(issues))
    } else {
      "Spacing is regular"
    },
    details = issues
  )
}

# ------------------------------------------------------------------------------
# Axis metadata inference
# ------------------------------------------------------------------------------

#' Get axis information from measure data
#'
#' Extracts metadata about the axis (location dimension) of measure data,
#' including range, spacing, direction, and inferred axis type.
#'
#' @param x A `measure_tbl`, `measure_list`, or data frame with measure column.
#' @param sample Integer index of sample to analyze (for `measure_list`).
#'   Default is 1.
#'
#' @return A list with:
#'   - `min`, `max`: Range of location values
#'   - `n_points`: Number of data points
#'   - `spacing`: Median spacing between points (NA if irregular)
#'   - `direction`: "increasing", "decreasing", or "mixed"
#'   - `regular`: Logical indicating if spacing is regular
#'   - `axis_type`: Inferred axis type (see [infer_axis_type()])
#'
#' @examples
#' # NIR spectrum
#' spec <- new_measure_tbl(
#'   location = seq(1000, 2500, by = 2),
#'   value = rnorm(751)
#' )
#' measure_axis_info(spec)
#'
#' # Chromatogram
#' chrom <- new_measure_tbl(
#'   location = seq(0, 30, by = 0.01),
#'   value = rnorm(3001)
#' )
#' measure_axis_info(chrom)
#'
#' @export
measure_axis_info <- function(x, sample = 1L) {
  # Get measure_tbl
  if (is_measure_tbl(x)) {
    m <- x
  } else if (is_measure_list(x)) {
    if (sample > length(x)) {
      cli::cli_abort("Sample {sample} does not exist (only {length(x)} samples).")
    }
    m <- x[[sample]]
  } else if (is.data.frame(x)) {
    meas_cols <- find_measure_cols(x)
    if (length(meas_cols) == 0) {
      cli::cli_abort("No measure column found in data frame.")
    }
    measures <- x[[meas_cols[1]]]
    if (sample > length(measures)) {
      cli::cli_abort("Sample {sample} does not exist (only {length(measures)} samples).")
    }
    m <- measures[[sample]]
  } else {
    cli::cli_abort("{.arg x} must be measure_tbl, measure_list, or data frame.")
  }

  loc <- m$location
  n <- length(loc)

  if (n == 0) {
    return(list(
      min = NA_real_,
      max = NA_real_,
      n_points = 0L,
      spacing = NA_real_,
      direction = NA_character_,
      regular = NA,
      axis_type = NA_character_
    ))
  }

  # Calculate spacing
  diffs <- if (n > 1) diff(loc) else NA_real_
  median_spacing <- if (n > 1) stats::median(diffs) else NA_real_

  # Determine direction
  direction <- if (n < 2) {
    NA_character_
  } else if (all(diffs > 0)) {
    "increasing"
  } else if (all(diffs < 0)) {
    "decreasing"
  } else {
    "mixed"
  }

  # Check regularity
  regular <- if (n < 3) {
    TRUE
  } else {
    rel_diffs <- abs(diffs - median_spacing) / abs(median_spacing)
    all(rel_diffs < 1e-6)
  }

  list(
    min = min(loc, na.rm = TRUE),
    max = max(loc, na.rm = TRUE),
    n_points = n,
    spacing = abs(median_spacing),
    direction = direction,
    regular = regular,
    axis_type = infer_axis_type(loc)
  )
}

#' Infer axis type from location values
#'
#' Attempts to infer the type of measurement axis based on the range and
#' characteristics of location values. This is a heuristic that helps
#' guide appropriate preprocessing choices.
#'
#' @param location Numeric vector of location values.
#'
#' @return Character string indicating inferred axis type:
#'   - `"wavelength_nm"`: Visible/NIR wavelengths (typically 300-2500 nm)
#'   - `"wavenumber"`: Mid-IR wavenumbers (typically 400-4000 cm^-1)
#'   - `"retention_time"`: Chromatography retention time (typically 0-60 min)
#'   - `"mass_charge"`: Mass spectrometry m/z (typically 50-2000+)
#'   - `"ppm"`: NMR chemical shift (typically -2 to 14 ppm)
#'   - `"two_theta"`: XRD diffraction angle (typically 5-90 degrees)
#'   - `"temperature"`: Thermal analysis (typically 20-1000 C)
#'   - `"unknown"`: Could not determine axis type
#'
#' @examples
#' # NIR wavelengths
#' infer_axis_type(seq(1000, 2500, by = 2))
#'

#' # Mid-IR wavenumbers
#' infer_axis_type(seq(4000, 400, by = -4))
#'
#' # Retention time (minutes)
#' infer_axis_type(seq(0, 30, by = 0.01))
#'
#' # NMR chemical shift
#' infer_axis_type(seq(0, 12, by = 0.001))
#'
#' @export
infer_axis_type <- function(location) {
  if (length(location) == 0 || all(is.na(location))) {
    return("unknown")
  }

  loc_min <- min(location, na.rm = TRUE)
  loc_max <- max(location, na.rm = TRUE)
  loc_range <- loc_max - loc_min
  n_points <- sum(!is.na(location))

  # NMR chemical shift (ppm): typically -2 to 14
  if (loc_min >= -5 && loc_max <= 20 && loc_range < 30) {
    return("ppm")
  }

  # Retention time (minutes): typically 0 to 60, starts near 0
  if (loc_min >= -1 && loc_min < 5 && loc_max <= 120 && loc_range > 5 && loc_range < 150) {
    return("retention_time")
  }

  # Wavelength (nm): typically 200-2500 for UV-Vis-NIR

  # Check wavelength BEFORE temperature since they overlap
  # Key distinguishing factor: wavelength data typically has many points (high resolution)
  # and range values between 200-3000
  if (loc_min >= 100 && loc_max <= 3000) {
    # Typical visible/NIR wavelengths (300-2500 nm)
    if (loc_min >= 200 && loc_max <= 2500) {
      return("wavelength_nm")
    }
    # Broader range still likely wavelength
    if (loc_range > 50 && loc_range < 2500) {
      return("wavelength_nm")
    }
  }

  # Wavenumber (cm^-1): typically 400-4000 for mid-IR
  # Distinguished by larger max values (often > 1000) and larger range (> 500)
  if (loc_min >= 200 && loc_max <= 8000 && loc_range > 500) {
    if (loc_max > 1000) {
      return("wavenumber")
    }
  }

  # XRD two-theta (degrees): typically 5 to 90
  if (loc_min >= 0 && loc_max <= 180 && loc_range > 20 && loc_range < 180) {
    # Integer-like steps typical for XRD
    if (mean(abs(location %% 1), na.rm = TRUE) < 0.3) {
      return("two_theta")
    }
  }

  # Temperature (Celsius): typically 20 to 1000
  # Usually has wide range (>200) and starts at higher values
  if (loc_min >= -200 && loc_max <= 1500 && loc_range > 200) {
    return("temperature")
  }

  # Mass-to-charge (m/z): typically 50-2000+
  if (loc_min >= 0 && loc_max > 100 && loc_max <= 100000) {
    return("mass_charge")
  }

  "unknown"
}

# ------------------------------------------------------------------------------
# Axis consistency checks across samples
# ------------------------------------------------------------------------------

#' Check axis consistency across samples
#'
#' Validates that all samples in a `measure_list` have consistent axes
#' (same locations). This is important for matrix operations that assume

#' aligned data.
#'
#' @param x A `measure_list` or data frame with measure column.
#' @param tolerance Numeric tolerance for location comparison. Default is 1e-10.
#' @param action What to do when validation fails: `"error"` (default),
#'   `"warn"`, or `"message"`.
#'
#' @return Invisibly returns a list with:
#'   - `consistent`: Logical indicating if axes are consistent
#'   - `reference_locations`: The reference locations (from first sample)
#'   - `inconsistent_samples`: Indices of samples with different axes
#'   - `max_deviation`: Maximum deviation from reference locations
#'
#' @examples
#' # Consistent axes
#' specs <- new_measure_list(list(
#'   new_measure_tbl(location = 1:10, value = rnorm(10)),
#'   new_measure_tbl(location = 1:10, value = rnorm(10))
#' ))
#' check_axis_consistency(specs)
#'
#' # Inconsistent axes
#' specs_bad <- new_measure_list(list(
#'   new_measure_tbl(location = 1:10, value = rnorm(10)),
#'   new_measure_tbl(location = 1:11, value = rnorm(11))
#' ))
#' try(check_axis_consistency(specs_bad))
#'
#' @export
check_axis_consistency <- function(x,
                                   tolerance = 1e-10,
                                   action = c("error", "warn", "message")) {
  action <- match.arg(action)

  # Get measure_list
  if (is_measure_list(x)) {
    measures <- x
  } else if (is.data.frame(x)) {
    meas_cols <- find_measure_cols(x)
    if (length(meas_cols) == 0) {
      cli::cli_abort("No measure column found in data frame.")
    }
    measures <- x[[meas_cols[1]]]
  } else {
    cli::cli_abort("{.arg x} must be measure_list or data frame.")
  }

  if (length(measures) < 2) {
    return(invisible(list(
      consistent = TRUE,
      reference_locations = if (length(measures) == 1) measures[[1]]$location else NULL,
      inconsistent_samples = integer(),
      max_deviation = 0
    )))
  }

  ref_loc <- measures[[1]]$location
  ref_n <- length(ref_loc)
  inconsistent <- integer()
  max_dev <- 0

  for (i in 2:length(measures)) {
    loc <- measures[[i]]$location

    # Check length
    if (length(loc) != ref_n) {
      inconsistent <- c(inconsistent, i)
      next
    }

    # Check values
    dev <- max(abs(loc - ref_loc))
    max_dev <- max(max_dev, dev)
    if (dev > tolerance) {
      inconsistent <- c(inconsistent, i)
    }
  }

  result <- list(
    consistent = length(inconsistent) == 0,
    reference_locations = ref_loc,
    inconsistent_samples = inconsistent,
    max_deviation = max_dev
  )

  if (!result$consistent) {
    msg <- sprintf(
      "%d of %d samples have inconsistent axes",
      length(inconsistent),
      length(measures)
    )

    if (action == "error") {
      cli::cli_abort(c(
        msg,
        "i" = "Use {.fn step_measure_resample} or {.fn step_measure_interpolate} to align axes."
      ))
    } else if (action == "warn") {
      cli::cli_warn(msg)
    } else {
      cli::cli_inform(msg)
    }
  }

  invisible(result)
}

#' Summarize measure data quality
#'
#' Provides a comprehensive quality summary for measure data, including
#' axis information and validation results.
#'
#' @param x A `measure_tbl`, `measure_list`, or data frame with measure column.
#' @param verbose Logical; if TRUE, prints summary to console. Default is TRUE.
#'
#' @return Invisibly returns a list containing axis info and validation results.
#'
#' @examples
#' specs <- new_measure_list(list(
#'   new_measure_tbl(location = seq(1000, 2500, by = 2), value = rnorm(751)),
#'   new_measure_tbl(location = seq(1000, 2500, by = 2), value = rnorm(751))
#' ))
#' measure_quality_summary(specs)
#'
#' @export
measure_quality_summary <- function(x, verbose = TRUE) {
  # Get measure_list
  if (is_measure_tbl(x)) {
    measures <- list(x)
    n_samples <- 1L
  } else if (is_measure_list(x)) {
    measures <- x
    n_samples <- length(x)
  } else if (is.data.frame(x)) {
    meas_cols <- find_measure_cols(x)
    if (length(meas_cols) == 0) {
      cli::cli_abort("No measure column found in data frame.")
    }
    measures <- x[[meas_cols[1]]]
    n_samples <- length(measures)
  } else {
    cli::cli_abort("{.arg x} must be measure_tbl, measure_list, or data frame.")
  }

  # Get axis info from first sample
  axis_info <- measure_axis_info(measures[[1]])

  # Run validation (suppress output)
  validation <- validate_measure(
    new_measure_list(measures),
    action = "message"
  )

  # Check consistency
  consistency <- if (n_samples > 1) {
    check_axis_consistency(new_measure_list(measures), action = "message")
  } else {
    list(consistent = TRUE)
  }

  result <- list(
    n_samples = n_samples,
    axis_info = axis_info,
    validation = validation,
    consistency = consistency
  )

  if (verbose) {
    cli::cli_h2("Measure Data Quality Summary")
    cli::cli_text("")

    cli::cli_h3("Overview")
    cli::cli_ul()
    cli::cli_li("Samples: {n_samples}")
    cli::cli_li("Points per sample: {axis_info$n_points}")
    cli::cli_li("Axis range: {round(axis_info$min, 4)} to {round(axis_info$max, 4)}")
    cli::cli_li("Axis type: {.field {axis_info$axis_type}}")
    cli::cli_li("Direction: {axis_info$direction}")
    cli::cli_li("Regular spacing: {if (axis_info$regular) 'Yes' else 'No'}")
    if (axis_info$regular && !is.na(axis_info$spacing)) {
      cli::cli_li("Spacing: {round(axis_info$spacing, 6)}")
    }
    cli::cli_end()

    cli::cli_text("")
    cli::cli_h3("Validation")
    cli::cli_ul()
    for (check_name in names(validation)) {
      v <- validation[[check_name]]
      status <- if (v$valid) cli::col_green("\u2714") else cli::col_red("\u2718")
      cli::cli_li("{status} {check_name}: {v$message}")
    }
    cli::cli_end()

    if (n_samples > 1) {
      cli::cli_text("")
      cli::cli_h3("Consistency")
      status <- if (consistency$consistent) {
        cli::col_green("\u2714 All samples have consistent axes")
      } else {
        cli::col_red(sprintf(
          "\u2718 %d sample(s) have inconsistent axes",
          length(consistency$inconsistent_samples)
        ))
      }
      cli::cli_text(status)
    }
  }

  invisible(result)
}
