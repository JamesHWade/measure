# ==============================================================================
# Wavelet Denoising Step
#
# This file contains the wavelet denoising preprocessing step:
# - step_measure_smooth_wavelet: Wavelet-based noise reduction
#
# Requires the wavethresh package.
# ==============================================================================

#' Wavelet Denoising
#'
#' `step_measure_smooth_wavelet()` creates a *specification* of a recipe step
#' that applies wavelet-based denoising to measurement data. This method is
#' particularly effective for signals with localized features like peaks.
#'
#' @param recipe A recipe object.
#' @param measures An optional character vector of measure column names.
#' @param wavelet The wavelet family to use. Default is `"DaubExPhase"`.
#'   Options include `"DaubExPhase"`, `"DaubLeAsworthy"`, `"Lawton"`.
#' @param filter_number The filter number within the wavelet family. Default
#'   is 4. Higher numbers give smoother wavelets.
#' @param threshold_type Type of thresholding: `"soft"` (default) or `"hard"`.
#'   Soft thresholding shrinks coefficients toward zero; hard thresholding
#'   sets small coefficients exactly to zero.
#' @param threshold_policy How to determine the threshold:
#'   - `"universal"` (default): Uses universal threshold sqrt(2*log(n))
#'   - `"sure"`: Stein's Unbiased Risk Estimate
#'   - `"cv"`: Cross-validation
#' @param levels Number of decomposition levels. Default is `NULL` (auto).
#' @param role Not used.
#' @param trained Logical indicating if the step has been trained.
#' @param skip Logical. Should the step be skipped when baking?
#' @param id Unique step identifier.
#'
#' @return An updated recipe with the new step added.
#'
#' @details
#' Wavelet denoising works by:
#' 1. Decomposing the signal into wavelet coefficients
#' 2. Thresholding small coefficients (presumed to be noise)
#' 3. Reconstructing the signal from remaining coefficients
#'
#' This approach is powerful because:
#' - It adapts to local signal characteristics
#' - It preserves sharp features like peaks
#' - It can separate noise from signal at multiple scales
#'
#' Requires the `wavethresh` package to be installed.
#'
#' @note
#' Wavelet transforms require signal lengths that are powers of 2. Signals
#' are automatically padded to the next power of 2 and trimmed after
#' processing.
#'
#' @family measure-smoothing
#' @export
#'
#' @examples
#' library(recipes)
#'
#' rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_smooth_wavelet() |>
#'   prep()
#'
#' bake(rec, new_data = NULL)
step_measure_smooth_wavelet <- function(
  recipe,
  measures = NULL,
  wavelet = "DaubExPhase",
  filter_number = 4L,
  threshold_type = c("soft", "hard"),
  threshold_policy = c("universal", "sure", "cv"),
  levels = NULL,
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_smooth_wavelet")
) {
  threshold_type <- match.arg(threshold_type)
  threshold_policy <- match.arg(threshold_policy)

  recipes::add_step(
    recipe,
    step_measure_smooth_wavelet_new(
      measures = measures,
      wavelet = wavelet,
      filter_number = as.integer(filter_number),
      threshold_type = threshold_type,
      threshold_policy = threshold_policy,
      levels = levels,
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_measure_smooth_wavelet_new <- function(
  measures,
  wavelet,
  filter_number,
  threshold_type,
  threshold_policy,
  levels,
  role,
  trained,
  skip,
  id
) {
  recipes::step(
    subclass = "measure_smooth_wavelet",
    measures = measures,
    wavelet = wavelet,
    filter_number = filter_number,
    threshold_type = threshold_type,
    threshold_policy = threshold_policy,
    levels = levels,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_smooth_wavelet <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  step_measure_smooth_wavelet_new(
    measures = measure_cols,
    wavelet = x$wavelet,
    filter_number = x$filter_number,
    threshold_type = x$threshold_type,
    threshold_policy = x$threshold_policy,
    levels = x$levels,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_smooth_wavelet <- function(object, new_data, ...) {
  rlang::check_installed("wavethresh", reason = "for wavelet denoising")

  for (col in object$measures) {
    result <- purrr::map(
      new_data[[col]],
      .smooth_wavelet_single,
      wavelet = object$wavelet,
      filter_number = object$filter_number,
      threshold_type = object$threshold_type,
      threshold_policy = object$threshold_policy,
      levels = object$levels
    )
    new_data[[col]] <- new_measure_list(result)
  }
  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_smooth_wavelet <- function(
  x,
  width = max(20, options()$width - 30),
  ...
) {
  title <- paste0("Wavelet denoising (", x$wavelet, ") on ")
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
tidy.step_measure_smooth_wavelet <- function(x, ...) {
  tibble::tibble(
    terms = if (is_trained(x)) x$measures else "<all measure columns>",
    wavelet = x$wavelet,
    filter_number = x$filter_number,
    threshold_type = x$threshold_type,
    id = x$id
  )
}

#' @rdname required_pkgs.recipe
#' @export
required_pkgs.step_measure_smooth_wavelet <- function(x, ...) {
  c("measure", "wavethresh")
}

.smooth_wavelet_single <- function(
  x,
  wavelet,
  filter_number,
  threshold_type,
  threshold_policy,
  levels
) {
  values <- x$value
  n <- length(values)

  if (n < 8) {
    cli::cli_warn(
      "Spectrum too short for wavelet denoising. Returning unchanged."
    )
    return(x)
  }

  # Handle NA values
  na_mask <- is.na(values)
  if (any(na_mask)) {
    # Interpolate NAs for wavelet transform
    non_na <- which(!na_mask)
    if (length(non_na) < 2) {
      return(x)
    }
    values <- stats::approx(non_na, values[non_na], seq_len(n), rule = 2)$y
  }

  # Pad to next power of 2
  orig_n <- n
  next_pow2 <- 2^ceiling(log2(n))
  if (n < next_pow2) {
    # Symmetric padding
    pad_length <- next_pow2 - n
    pad_left <- floor(pad_length / 2)
    pad_right <- pad_length - pad_left

    if (pad_left > 0) {
      left_pad <- rev(values[2:(pad_left + 1)])
      if (length(left_pad) < pad_left) {
        left_pad <- c(rep(values[1], pad_left - length(left_pad)), left_pad)
      }
    } else {
      left_pad <- numeric(0)
    }

    if (pad_right > 0) {
      right_pad <- rev(values[(n - pad_right):(n - 1)])
      if (length(right_pad) < pad_right) {
        right_pad <- c(right_pad, rep(values[n], pad_right - length(right_pad)))
      }
    } else {
      right_pad <- numeric(0)
    }

    values_padded <- c(left_pad, values, right_pad)
  } else {
    values_padded <- values
    pad_left <- 0
  }

  # Determine number of levels
  n_padded <- length(values_padded)
  max_levels <- floor(log2(n_padded)) - 2
  if (is.null(levels)) {
    levels <- max(1, min(max_levels, floor(log2(n_padded) / 2)))
  } else {
    levels <- min(levels, max_levels)
  }

  tryCatch(
    {
      # Discrete wavelet transform (standard, not stationary)
      wt <- wavethresh::wd(
        values_padded,
        filter.number = filter_number,
        family = wavelet
      )

      # Threshold the wavelet coefficients
      wt_thresh <- wavethresh::threshold(
        wt,
        type = threshold_type,
        policy = threshold_policy,
        levels = 0:(levels - 1)
      )

      # Inverse transform using wr (wavelet reconstruction)
      denoised <- wavethresh::wr(wt_thresh)

      # Remove padding
      if (pad_left > 0 || length(denoised) > orig_n) {
        denoised <- denoised[(pad_left + 1):(pad_left + orig_n)]
      }

      # Restore NAs if there were any
      if (any(na_mask)) {
        denoised[na_mask] <- NA_real_
      }

      x$value <- denoised
    },
    error = function(e) {
      cli::cli_warn("Wavelet denoising failed: {e$message}")
    }
  )

  x
}
