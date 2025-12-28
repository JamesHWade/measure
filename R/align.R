# ==============================================================================
# Alignment & Registration Steps
#
# This file contains alignment preprocessing steps:
# - step_measure_align_shift: Simple cross-correlation shift
# - step_measure_align_reference: Align to a reference spectrum
# - step_measure_align_dtw: Dynamic Time Warping (requires dtw package)
# - step_measure_align_ptw: Parametric Time Warping (requires ptw package)
# - step_measure_align_cow: Correlation Optimized Warping (pure R implementation)
# ==============================================================================

# ==============================================================================
# step_measure_align_shift
# ==============================================================================

#' Shift Alignment via Cross-Correlation
#'
#' `step_measure_align_shift()` creates a *specification* of a recipe step that
#' aligns spectra by finding the optimal shift using cross-correlation.
#'
#' @param recipe A recipe object.
#' @param measures An optional character vector of measure column names.
#' @param max_shift Maximum shift (in points) to consider. Default is 10.
#'   Tunable via [align_max_shift()].
#' @param reference How to determine the reference:
#'   - `"mean"` (default): Use the mean spectrum from training
#'   - `"median"`: Use the median spectrum from training
#'   - `"first"`: Use the first sample
#' @param role Not used.
#' @param trained Logical indicating if the step has been trained.
#' @param skip Logical. Should the step be skipped when baking?
#' @param id Unique step identifier.
#'
#' @return An updated recipe with the new step added.
#'
#' @details
#' This step corrects for small linear shifts between spectra, which can occur
#' due to:
#' - Wavelength calibration drift
#' - Sample positioning differences
#' - Temperature effects on instrument
#'
#' The optimal shift is found by maximizing the cross-correlation between
#' each spectrum and the reference. After shifting, edge values are filled
#' by constant extrapolation.
#'
#' @family measure-align
#' @export
#'
#' @examples
#' library(recipes)
#'
#' rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_align_shift(max_shift = 5) |>
#'   prep()
#'
#' bake(rec, new_data = NULL)
step_measure_align_shift <- function(
    recipe,
    measures = NULL,
    max_shift = 10L,
    reference = c("mean", "median", "first"),
    role = NA,
    trained = FALSE,
    skip = FALSE,
    id = recipes::rand_id("measure_align_shift")) {
  reference <- match.arg(reference)

  recipes::add_step(
    recipe,
    step_measure_align_shift_new(
      measures = measures,
      max_shift = as.integer(max_shift),
      reference = reference,
      ref_spectrum = NULL,
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_measure_align_shift_new <- function(
    measures, max_shift, reference, ref_spectrum, role, trained, skip, id) {
  recipes::step(
    subclass = "measure_align_shift",
    measures = measures,
    max_shift = max_shift,
    reference = reference,
    ref_spectrum = ref_spectrum,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_align_shift <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  if (!is.numeric(x$max_shift) || x$max_shift < 1) {
    cli::cli_abort("{.arg max_shift} must be a positive integer.")
  }

  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  col <- measure_cols[1]

  # Compute reference spectrum from training data
  mat <- measure_to_matrix(training[[col]])

  ref_spectrum <- switch(
    x$reference,
    "mean" = colMeans(mat, na.rm = TRUE),
    "median" = apply(mat, 2, stats::median, na.rm = TRUE),
    "first" = mat[1, ]
  )

  step_measure_align_shift_new(
    measures = measure_cols,
    max_shift = as.integer(x$max_shift),
    reference = x$reference,
    ref_spectrum = ref_spectrum,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_align_shift <- function(object, new_data, ...) {
  for (col in object$measures) {
    result <- purrr::map(
      new_data[[col]],
      .align_shift_single,
      ref_spectrum = object$ref_spectrum,
      max_shift = object$max_shift
    )
    new_data[[col]] <- new_measure_list(result)
  }
  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_align_shift <- function(x, width = max(20, options()$width - 30), ...) {
  title <- paste0("Shift alignment (max_shift = ", x$max_shift, ") on ")
  if (x$trained) {
    cat(title, "<internal measurements>", sep = "")
  } else {
    cat(title)
  }
  cat("\n")
  invisible(x)
}

#' @rdname tidy.recipe
#' @export
#' @keywords internal
tidy.step_measure_align_shift <- function(x, ...) {
  tibble::tibble(
    terms = if (is_trained(x)) x$measures else "<all measure columns>",
    max_shift = x$max_shift,
    reference = x$reference,
    id = x$id
  )
}

.align_shift_single <- function(x, ref_spectrum, max_shift) {
  values <- x$value
  n <- length(values)

  if (n != length(ref_spectrum)) {
    cli::cli_warn("Spectrum length differs from reference. Skipping alignment.")
    return(x)
  }

  # Find optimal shift using cross-correlation
  best_shift <- 0
  best_corr <- -Inf

  for (shift in (-max_shift):max_shift) {
    if (shift < 0) {
      # Shift left
      idx_val <- (1 - shift):n
      idx_ref <- 1:(n + shift)
    } else if (shift > 0) {
      # Shift right
      idx_val <- 1:(n - shift)
      idx_ref <- (1 + shift):n
    } else {
      idx_val <- 1:n
      idx_ref <- 1:n
    }

    corr <- stats::cor(values[idx_val], ref_spectrum[idx_ref], use = "complete.obs")
    if (!is.na(corr) && corr > best_corr) {
      best_corr <- corr
      best_shift <- shift
    }
  }

  # Apply shift
  if (best_shift != 0) {
    new_values <- numeric(n)
    if (best_shift > 0) {
      # Shift right: values move to higher indices
      new_values[(best_shift + 1):n] <- values[1:(n - best_shift)]
      new_values[1:best_shift] <- values[1]  # Constant extrapolation
    } else {
      # Shift left: values move to lower indices
      new_values[1:(n + best_shift)] <- values[(1 - best_shift):n]
      new_values[(n + best_shift + 1):n] <- values[n]
    }
    x$value <- new_values
  }

  x
}


# ==============================================================================
# step_measure_align_reference
# ==============================================================================

#' Align to Reference Spectrum
#'
#' `step_measure_align_reference()` creates a *specification* of a recipe step
#' that aligns spectra to a user-provided reference spectrum using
#' cross-correlation.
#'
#' @param recipe A recipe object.
#' @param measures An optional character vector of measure column names.
#' @param ref_spectrum A numeric vector containing the reference spectrum.
#'   Must have the same length as the measurement spectra.
#' @param max_shift Maximum shift (in points) to consider. Default is 10.
#' @param role Not used.
#' @param trained Logical indicating if the step has been trained.
#' @param skip Logical. Should the step be skipped when baking?
#' @param id Unique step identifier.
#'
#' @return An updated recipe with the new step added.
#'
#' @details
#' Similar to [step_measure_align_shift()], but uses an externally provided
#' reference spectrum instead of computing one from training data. This is
#' useful when you have a known standard or calibration spectrum.
#'
#' @family measure-align
#' @export
#'
#' @examples
#' library(recipes)
#'
#' # Create a reference spectrum (in practice, this would be from calibration)
#' ref <- rep(1, 100)  # placeholder
#'
#' # Note: This example would need matching spectrum lengths to work
#' \dontrun{
#' rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_align_reference(ref_spectrum = ref) |>
#'   prep()
#' }
step_measure_align_reference <- function(
    recipe,
    measures = NULL,
    ref_spectrum,
    max_shift = 10L,
    role = NA,
    trained = FALSE,
    skip = FALSE,
    id = recipes::rand_id("measure_align_reference")) {
  if (missing(ref_spectrum) || !is.numeric(ref_spectrum)) {
    cli::cli_abort("{.arg ref_spectrum} must be a numeric vector.")
  }

  recipes::add_step(
    recipe,
    step_measure_align_reference_new(
      measures = measures,
      ref_spectrum = ref_spectrum,
      max_shift = as.integer(max_shift),
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_measure_align_reference_new <- function(
    measures, ref_spectrum, max_shift, role, trained, skip, id) {
  recipes::step(
    subclass = "measure_align_reference",
    measures = measures,
    ref_spectrum = ref_spectrum,
    max_shift = max_shift,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_align_reference <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  # Validate reference length matches data
  col <- measure_cols[1]
  first_spectrum <- training[[col]][[1]]

  if (length(x$ref_spectrum) != length(first_spectrum$value)) {
    cli::cli_warn(
      "Reference spectrum length ({length(x$ref_spectrum)}) differs from data ({length(first_spectrum$value)}). ",
      "Alignment may not work correctly."
    )
  }

  step_measure_align_reference_new(
    measures = measure_cols,
    ref_spectrum = x$ref_spectrum,
    max_shift = x$max_shift,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_align_reference <- function(object, new_data, ...) {
  for (col in object$measures) {
    result <- purrr::map(
      new_data[[col]],
      .align_shift_single,  # Reuse the same function
      ref_spectrum = object$ref_spectrum,
      max_shift = object$max_shift
    )
    new_data[[col]] <- new_measure_list(result)
  }
  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_align_reference <- function(x, width = max(20, options()$width - 30), ...) {
  title <- paste0("Reference alignment (max_shift = ", x$max_shift, ") on ")
  if (x$trained) {
    cat(title, "<internal measurements>", sep = "")
  } else {
    cat(title)
  }
  cat("\n")
  invisible(x)
}

#' @rdname tidy.recipe
#' @export
#' @keywords internal
tidy.step_measure_align_reference <- function(x, ...) {
  tibble::tibble(
    terms = if (is_trained(x)) x$measures else "<all measure columns>",
    max_shift = x$max_shift,
    id = x$id
  )
}


# ==============================================================================
# step_measure_align_dtw
# ==============================================================================

#' Dynamic Time Warping Alignment
#'
#' `step_measure_align_dtw()` creates a *specification* of a recipe step that
#' aligns spectra using Dynamic Time Warping (DTW). This method can handle
#' non-linear distortions in the x-axis.
#'
#' @param recipe A recipe object.
#' @param measures An optional character vector of measure column names.
#' @param reference How to determine the reference:
#'   - `"mean"` (default): Use the mean spectrum from training
#'   - `"median"`: Use the median spectrum from training
#'   - `"first"`: Use the first sample
#' @param window_type Windowing constraint for DTW. One of `"none"` (default),
#'   `"sakoechiba"`, or `"slantedband"`.
#' @param window_size Window size for constrained DTW. Default is 10.
#' @param role Not used.
#' @param trained Logical indicating if the step has been trained.
#' @param skip Logical. Should the step be skipped when baking?
#' @param id Unique step identifier.
#'
#' @return An updated recipe with the new step added.
#'
#' @details
#' DTW finds the optimal non-linear alignment between two sequences by
#' minimizing a distance measure while allowing warping of the time/x-axis.
#'
#' This is useful for:
#' - Chromatographic peak alignment
#' - Correcting non-linear retention time shifts
#' - Aligning spectra with complex distortions
#'
#' Requires the `dtw` package to be installed.
#'
#' @family measure-align
#' @export
#'
#' @examples
#' library(recipes)
#'
#' rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_align_dtw() |>
#'   prep()
#'
#' bake(rec, new_data = NULL)
step_measure_align_dtw <- function(
    recipe,
    measures = NULL,
    reference = c("mean", "median", "first"),
    window_type = c("none", "sakoechiba", "slantedband"),
    window_size = 10L,
    role = NA,
    trained = FALSE,
    skip = FALSE,
    id = recipes::rand_id("measure_align_dtw")) {
  reference <- match.arg(reference)
  window_type <- match.arg(window_type)

  recipes::add_step(
    recipe,
    step_measure_align_dtw_new(
      measures = measures,
      reference = reference,
      window_type = window_type,
      window_size = as.integer(window_size),
      ref_spectrum = NULL,
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_measure_align_dtw_new <- function(
    measures, reference, window_type, window_size, ref_spectrum,
    role, trained, skip, id) {
  recipes::step(
    subclass = "measure_align_dtw",
    measures = measures,
    reference = reference,
    window_type = window_type,
    window_size = window_size,
    ref_spectrum = ref_spectrum,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_align_dtw <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  col <- measure_cols[1]

  # Compute reference spectrum
  mat <- measure_to_matrix(training[[col]])

  ref_spectrum <- switch(
    x$reference,
    "mean" = colMeans(mat, na.rm = TRUE),
    "median" = apply(mat, 2, stats::median, na.rm = TRUE),
    "first" = mat[1, ]
  )

  step_measure_align_dtw_new(
    measures = measure_cols,
    reference = x$reference,
    window_type = x$window_type,
    window_size = x$window_size,
    ref_spectrum = ref_spectrum,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_align_dtw <- function(object, new_data, ...) {
  rlang::check_installed("dtw", reason = "for DTW alignment")

  for (col in object$measures) {
    result <- purrr::map(
      new_data[[col]],
      .align_dtw_single,
      ref_spectrum = object$ref_spectrum,
      window_type = object$window_type,
      window_size = object$window_size
    )
    new_data[[col]] <- new_measure_list(result)
  }
  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_align_dtw <- function(x, width = max(20, options()$width - 30), ...) {
  title <- "DTW alignment on "
  if (x$trained) {
    cat(title, "<internal measurements>", sep = "")
  } else {
    cat(title)
  }
  cat("\n")
  invisible(x)
}

#' @rdname tidy.recipe
#' @export
#' @keywords internal
tidy.step_measure_align_dtw <- function(x, ...) {
  tibble::tibble(
    terms = if (is_trained(x)) x$measures else "<all measure columns>",
    reference = x$reference,
    window_type = x$window_type,
    id = x$id
  )
}

#' @rdname required_pkgs.recipe
#' @export
required_pkgs.step_measure_align_dtw <- function(x, ...) {
  c("measure", "dtw")
}

.align_dtw_single <- function(x, ref_spectrum, window_type, window_size) {
  values <- x$value
  n <- length(values)

  if (n != length(ref_spectrum)) {
    cli::cli_warn("Spectrum length differs from reference. Skipping DTW alignment.")
    return(x)
  }

  # Set up window function - use "none" string, not NULL
  window_func <- if (window_type == "none") {
    "none"
  } else if (window_type == "sakoechiba") {
    dtw::sakoeChibaWindow
  } else {
    dtw::slantedBandWindow
  }

  # Compute DTW alignment
  tryCatch({
    alignment <- dtw::dtw(
      x = values,
      y = ref_spectrum,
      window.type = window_func,
      window.size = window_size,
      keep.internals = TRUE
    )

    # Warp the query to match reference indices
    # Use the warping path to remap values
    warped <- numeric(n)
    warp_idx <- alignment$index1

    # For each reference position, average the corresponding query values
    for (i in seq_len(n)) {
      matching <- which(alignment$index2 == i)
      if (length(matching) > 0) {
        warped[i] <- mean(values[warp_idx[matching]], na.rm = TRUE)
      } else {
        warped[i] <- NA_real_
      }
    }

    # Fill any remaining NAs by interpolation
    if (anyNA(warped)) {
      non_na <- which(!is.na(warped))
      if (length(non_na) >= 2) {
        warped <- stats::approx(non_na, warped[non_na], seq_len(n), rule = 2)$y
      }
    }

    x$value <- warped
  }, error = function(e) {
    cli::cli_warn("DTW alignment failed: {e$message}")
  })

  x
}


# ==============================================================================
# step_measure_align_ptw
# ==============================================================================

#' Parametric Time Warping Alignment
#'
#' `step_measure_align_ptw()` creates a *specification* of a recipe step that
#' aligns spectra using Parametric Time Warping (PTW). This method uses
#' polynomial warping functions to correct for shifts and distortions.
#'
#' @param recipe A recipe object.
#' @param measures An optional character vector of measure column names.
#' @param reference How to determine the reference:
#'   `"mean"` (default, mean spectrum from training),
#'   `"median"` (median spectrum from training), or
#'   `"first"` (first sample).
#' @param role Not used.
#' @param trained Logical indicating if the step has been trained.
#' @param skip Logical. Should the step be skipped when baking?
#' @param id Unique step identifier.
#'
#' @return An updated recipe with the new step added.
#'
#' @details
#' Parametric Time Warping optimizes polynomial warping coefficients to
#' maximize the correlation between each sample and the reference spectrum.
#' This corrects for smooth, continuous distortions in the x-axis.
#'
#' Requires the `ptw` package to be installed.
#'
#' @references
#' Eilers, P.H.C. (2004). Parametric Time Warping.
#' Analytical Chemistry, 76(2), 404-411.
#'
#' @family measure-align
#' @export
#'
#' @examples
#' library(recipes)
#'
#' rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_align_ptw() |>
#'   prep()
#'
#' bake(rec, new_data = NULL)
step_measure_align_ptw <- function(
    recipe,
    measures = NULL,
    reference = c("mean", "median", "first"),
    role = NA,
    trained = FALSE,
    skip = FALSE,
    id = recipes::rand_id("measure_align_ptw")) {
  reference <- match.arg(reference)

  recipes::add_step(
    recipe,
    step_measure_align_ptw_new(
      measures = measures,
      reference = reference,
      ref_spectrum = NULL,
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_measure_align_ptw_new <- function(
    measures, reference, ref_spectrum, role, trained, skip, id) {
  recipes::step(
    subclass = "measure_align_ptw",
    measures = measures,
    reference = reference,
    ref_spectrum = ref_spectrum,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_align_ptw <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  col <- measure_cols[1]

  # Compute reference spectrum
  mat <- measure_to_matrix(training[[col]])

  ref_spectrum <- switch(
    x$reference,
    "mean" = colMeans(mat, na.rm = TRUE),
    "median" = apply(mat, 2, stats::median, na.rm = TRUE),
    "first" = mat[1, ]
  )

  step_measure_align_ptw_new(
    measures = measure_cols,
    reference = x$reference,
    ref_spectrum = ref_spectrum,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_align_ptw <- function(object, new_data, ...) {

  rlang::check_installed("ptw", reason = "for PTW alignment")

  for (col in object$measures) {
    # Get locations from first spectrum
    locations <- new_data[[col]][[1]]$location

    # Convert to matrix for ptw
    mat <- measure_to_matrix(new_data[[col]])

    tryCatch({
      # Apply Parametric Time Warping alignment
      # ptw expects ref and samp as matrices (one signal per row)
      ref_mat <- matrix(object$ref_spectrum, nrow = 1)

      aligned <- ptw::ptw(
        ref = ref_mat,
        samp = mat,
        init.coef = c(0, 1, 0),  # Start with identity warp
        warp.type = "individual"
      )

      # Extract warped samples
      warped_mat <- aligned$warped.sample

      # Convert back to measure_list using the original locations
      result <- matrix_to_measure(warped_mat, locations)
      new_data[[col]] <- new_measure_list(result)
    }, error = function(e) {
      cli::cli_warn("PTW alignment failed: {e$message}")
    })
  }
  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_align_ptw <- function(x, width = max(20, options()$width - 30), ...) {
  title <- "PTW alignment on "
  if (x$trained) {
    cat(title, "<internal measurements>", sep = "")
  } else {
    cat(title)
  }
  cat("\n")
  invisible(x)
}

#' @rdname tidy.recipe
#' @export
#' @keywords internal
tidy.step_measure_align_ptw <- function(x, ...) {
  tibble::tibble(
    terms = if (is_trained(x)) x$measures else "<all measure columns>",
    reference = x$reference,
    id = x$id
  )
}

#' @rdname required_pkgs.recipe
#' @export
required_pkgs.step_measure_align_ptw <- function(x, ...) {
  c("measure", "ptw")
}


# ==============================================================================
# step_measure_align_cow
# ==============================================================================

#' Correlation Optimized Warping Alignment
#'
#' `step_measure_align_cow()` creates a *specification* of a recipe step that
#' aligns spectra using Correlation Optimized Warping (COW). This method uses
#' piecewise linear warping to correct for non-linear shifts.
#'
#' @param recipe A recipe object.
#' @param measures An optional character vector of measure column names.
#' @param reference How to determine the reference:
#'   `"mean"` (default, mean spectrum from training),
#'   `"median"` (median spectrum from training), or
#'   `"first"` (first sample).
#' @param segment_length Length of each segment for warping. Default is 30.
#'   Tunable via [align_segment_length()].
#' @param slack Maximum compression/expansion per segment in points. Default is 1.
#'   A slack of 1 means each segment can shrink or expand by 1 point.
#' @param role Not used.
#' @param trained Logical indicating if the step has been trained.
#' @param skip Logical. Should the step be skipped when baking?
#' @param id Unique step identifier.
#'
#' @return An updated recipe with the new step added.
#'
#' @details
#' Correlation Optimized Warping (COW) divides signals into segments and uses
#' dynamic programming to find the optimal piecewise linear warping that
#' maximizes correlation with the reference spectrum.
#'
#' Key parameters:
#' - `segment_length`: Controls the resolution of warping. Smaller segments
#'   allow more local corrections but increase computation.
#' - `slack`: Controls how much each segment can stretch or compress.
#'   Larger values allow more flexibility but may introduce artifacts.
#'
#' This is a pure R implementation based on Nielsen et al. (1998).
#'
#' @references
#' Nielsen, N.P.V., Carstensen, J.M., and Smedsgaard, J. (1998).
#' Aligning of single and multiple wavelength chromatographic profiles
#' for chemometric data analysis using correlation optimised warping.
#' Journal of Chromatography A, 805, 17-35.
#'
#' @family measure-align
#' @export
#'
#' @examples
#' library(recipes)
#'
#' rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_align_cow(segment_length = 20, slack = 2) |>
#'   prep()
#'
#' bake(rec, new_data = NULL)
step_measure_align_cow <- function(
    recipe,
    measures = NULL,
    reference = c("mean", "median", "first"),
    segment_length = 30L,
    slack = 1L,
    role = NA,
    trained = FALSE,
    skip = FALSE,
    id = recipes::rand_id("measure_align_cow")) {
  reference <- match.arg(reference)

  recipes::add_step(
    recipe,
    step_measure_align_cow_new(
      measures = measures,
      reference = reference,
      segment_length = as.integer(segment_length),
      slack = as.integer(slack),
      ref_spectrum = NULL,
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_measure_align_cow_new <- function(
    measures, reference, segment_length, slack, ref_spectrum,
    role, trained, skip, id) {
  recipes::step(
    subclass = "measure_align_cow",
    measures = measures,
    reference = reference,
    segment_length = segment_length,
    slack = slack,
    ref_spectrum = ref_spectrum,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_align_cow <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  if (!is.numeric(x$segment_length) || x$segment_length < 2) {
    cli::cli_abort("{.arg segment_length} must be >= 2.")
  }
  if (!is.numeric(x$slack) || x$slack < 0) {
    cli::cli_abort("{.arg slack} must be >= 0.")
  }

  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  col <- measure_cols[1]

  # Compute reference spectrum
  mat <- measure_to_matrix(training[[col]])

  ref_spectrum <- switch(
    x$reference,
    "mean" = colMeans(mat, na.rm = TRUE),
    "median" = apply(mat, 2, stats::median, na.rm = TRUE),
    "first" = mat[1, ]
  )

  step_measure_align_cow_new(
    measures = measure_cols,
    reference = x$reference,
    segment_length = x$segment_length,
    slack = x$slack,
    ref_spectrum = ref_spectrum,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_align_cow <- function(object, new_data, ...) {
  for (col in object$measures) {
    result <- purrr::map(
      new_data[[col]],
      .align_cow_single,
      ref_spectrum = object$ref_spectrum,
      segment_length = object$segment_length,
      slack = object$slack
    )
    new_data[[col]] <- new_measure_list(result)
  }
  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_align_cow <- function(x, width = max(20, options()$width - 30), ...) {
  title <- paste0(
    "COW alignment (segment = ", x$segment_length,
    ", slack = ", x$slack, ") on "
  )
  if (x$trained) {
    cat(title, "<internal measurements>", sep = "")
  } else {
    cat(title)
  }
  cat("\n")
  invisible(x)
}

#' @rdname tidy.recipe
#' @export
#' @keywords internal
tidy.step_measure_align_cow <- function(x, ...) {
  tibble::tibble(
    terms = if (is_trained(x)) x$measures else "<all measure columns>",
    reference = x$reference,
    segment_length = x$segment_length,
    slack = x$slack,
    id = x$id
  )
}

#' @rdname tunable_measure
#' @export
tunable.step_measure_align_cow <- function(x, ...) {
 tibble::tibble(
    name = c("segment_length", "slack"),
    call_info = list(
      list(pkg = "measure", fun = "align_segment_length"),
      list(pkg = "dials", fun = "new_quant_param",
           args = list(type = "integer", range = c(0L, 5L), default = 1L, label = "Slack"))
    ),
    source = "recipe",
    component = "step_measure_align_cow",
    component_id = x$id
  )
}

# ------------------------------------------------------------------------------
# COW algorithm implementation
# ------------------------------------------------------------------------------

.align_cow_single <- function(x, ref_spectrum, segment_length, slack) {
  sample_values <- x$value
  n <- length(sample_values)
  n_ref <- length(ref_spectrum)

  if (n != n_ref) {
    cli::cli_warn("Spectrum length differs from reference. Skipping COW alignment.")
    return(x)
  }

  # Handle signals that are too short
  if (n < segment_length * 2) {
    cli::cli_warn("Signal too short for COW alignment. Skipping.")
    return(x)
  }

  # Compute number of segments
  n_segments <- floor(n / segment_length)
  if (n_segments < 2) {
    cli::cli_warn("Not enough segments for COW. Skipping.")
    return(x)
  }

  # Adjust segment length to fit signal exactly
  seg_len <- n / n_segments

  # Build cost matrix using dynamic programming
  # State: (segment_index, accumulated_position)
  # For each segment, we can warp by -slack to +slack

  # Warp options for each segment
  warp_options <- (-slack):slack
  n_warps <- length(warp_options)

  # Cost matrix: rows = segments, columns = cumulative warp state
  # We track the total accumulated warp from -n_segments*slack to +n_segments*slack
  max_total_warp <- n_segments * slack
  n_states <- 2 * max_total_warp + 1

  # Initialize DP tables
  cost <- matrix(Inf, nrow = n_segments, ncol = n_states)
  backtrack <- matrix(NA_integer_, nrow = n_segments, ncol = n_states)
  warp_choice <- matrix(NA_integer_, nrow = n_segments, ncol = n_states)

  # Helper to convert warp sum to state index
  state_idx <- function(w) w + max_total_warp + 1

  # Process first segment
  for (w in warp_options) {
    seg_cost <- .cow_segment_cost(
      sample_values, ref_spectrum,
      seg_start = 1,
      seg_len = round(seg_len),
      warp = w
    )
    cost[1, state_idx(w)] <- seg_cost
    warp_choice[1, state_idx(w)] <- w
  }

  # Dynamic programming for remaining segments
  for (seg in 2:n_segments) {
    for (w in warp_options) {
      # Try all previous states
      for (prev_w in warp_options) {
        prev_total <- (seg - 2) * 0  # Not tracking cumulative directly
        # This approach: track segment-by-segment, each warp is local

        prev_state <- state_idx(prev_w)
        if (cost[seg - 1, prev_state] == Inf) next

        # Compute cost for this segment with warp w
        seg_start <- round((seg - 1) * seg_len) + 1
        seg_cost <- .cow_segment_cost(
          sample_values, ref_spectrum,
          seg_start = seg_start,
          seg_len = round(seg_len),
          warp = w
        )

        new_cost <- cost[seg - 1, prev_state] + seg_cost
        curr_state <- state_idx(w)

        if (new_cost < cost[seg, curr_state]) {
          cost[seg, curr_state] <- new_cost
          backtrack[seg, curr_state] <- prev_state
          warp_choice[seg, curr_state] <- w
        }
      }
    }
  }

  # Find optimal endpoint
  final_costs <- cost[n_segments, ]
  best_end_state <- which.min(final_costs)

  if (!is.finite(final_costs[best_end_state])) {
    cli::cli_warn("COW optimization failed. Returning original signal.")
    return(x)
  }

  # Backtrack to get warp path
  warps <- integer(n_segments)
  state <- best_end_state
  for (seg in n_segments:1) {
    warps[seg] <- warp_choice[seg, state]
    if (seg > 1) {
      state <- backtrack[seg, state]
    }
  }

  # Apply warping
  warped <- .apply_cow_warps(sample_values, seg_len, warps)

  # Ensure output has same length as input
  if (length(warped) != n) {
    # Interpolate to exact length
    warped <- stats::approx(
      seq_along(warped),
      warped,
      n = n,
      rule = 2
    )$y
  }

  x$value <- warped
  x
}

.cow_segment_cost <- function(sample, reference, seg_start, seg_len, warp) {

  n <- length(sample)

  # Warped segment length
  warped_len <- seg_len + warp

  if (warped_len < 2) return(Inf)

  # Sample indices for this segment
  seg_end <- min(seg_start + seg_len - 1, n)
  if (seg_start > n || seg_start < 1) return(Inf)

  samp_seg <- sample[seg_start:seg_end]

  # Reference indices (fixed)
  ref_start <- seg_start
  ref_end <- min(ref_start + seg_len - 1, length(reference))
  if (ref_start > length(reference)) return(Inf)

  ref_seg <- reference[ref_start:ref_end]

  # Interpolate sample to match warped length then compare to reference
  if (length(samp_seg) < 2 || length(ref_seg) < 2) return(Inf)

  # Warp the sample segment to new length, then interpolate to ref length
  warped_samp <- stats::approx(
    seq_along(samp_seg),
    samp_seg,
    n = length(ref_seg),
    rule = 2
  )$y

  # Cost is negative correlation (we want to maximize correlation)
  corr <- stats::cor(warped_samp, ref_seg, use = "complete.obs")
  if (is.na(corr)) return(Inf)

  -corr
}

.apply_cow_warps <- function(sample, seg_len, warps) {
  n <- length(sample)
  n_segments <- length(warps)

  # Build the warped signal segment by segment
  result <- numeric(0)

  for (seg in seq_len(n_segments)) {
    seg_start <- round((seg - 1) * seg_len) + 1
    seg_end <- min(round(seg * seg_len), n)

    if (seg_start > n) break

    samp_seg <- sample[seg_start:seg_end]
    warp <- warps[seg]

    # Warped length
    target_len <- length(samp_seg) + warp
    target_len <- max(2, target_len)

    # Interpolate to warped length
    warped_seg <- stats::approx(
      seq_along(samp_seg),
      samp_seg,
      n = target_len,
      rule = 2
    )$y

    result <- c(result, warped_seg)
  }

  result
}
