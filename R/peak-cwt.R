# ==============================================================================
# Continuous wavelet transform peak detection
#
# The method is inspired by Du et al. (2006), Bioinformatics 22(17), 2059-2065,
# doi:10.1093/bioinformatics/btl355, but this is not a reproduction of their
# algorithm. This implementation links maxima from smaller to larger scales,
# uses discrete zero-mean/unit-energy Mexican-hat (Ricker) kernels, and estimates
# local noise outside the fine-scale peak support. Each run is median-centered,
# and its precision floor is derived from a local signal window. Close ridge
# branches can
# produce separate raw-sample candidates; `min_distance` controls suppression.
# Ridge gap and persistence parameters have roles similar to those documented
# by scipy.signal.find_peaks_cwt:
# https://docs.scipy.org/doc/scipy/reference/generated/scipy.signal.find_peaks_cwt.html
#
# CWT widths and ridge distances are in samples, not location-axis units. Widths
# must be at least one sample and no more than half the finite run being
# analyzed; the built-in defaults are trimmed for short runs. To bound work,
# requests are limited to 256 scales, widths no larger than 10,000 samples,
# 10 million CWT coefficient cells, and 200 million convolution terms. The
# transform requires a finite, strictly monotone, uniformly spaced location
# axis. Descending axes are reversed internally and results are always returned
# in ascending physical-location order. Missing signal values split the input
# into independent finite runs; the transform never bridges an NA gap.
# ==============================================================================

.cwt_default_scales <- c(1, 2, 3, 4, 6, 8, 12)
.cwt_max_scales <- 256L
.cwt_max_scale <- 10000
.cwt_max_coefficient_cells <- 1e7
.cwt_max_convolution_terms <- 2e8

#' Detect peaks with a Mexican-hat continuous wavelet transform
#'
#' Performs a discrete, scale-normalized Mexican-hat CWT, links positive local
#' maxima across scales, and accepts ridges with enough persistence and local
#' wavelet-space signal-to-noise ratio. The approach is inspired by Du et al.
#' (2006), *Bioinformatics*, 22(17), 2059-2065,
#' <https://doi.org/10.1093/bioinformatics/btl355>. The CWT scale values are
#' widths in samples, from 1 through 10,000. Each width must also be no more
#' than half the finite run being analyzed. For short runs, the built-in default
#' widths are trimmed; explicitly requested unsupported widths error. Location
#' axes must be strictly monotone and uniformly spaced; irregular axes are
#' rejected rather than resampled. NA values divide a signal into independent
#' runs. Returned location, height, and bases are read from the original
#' signal samples. Nearby ridge branches can yield separate candidates; use
#' `min_distance` to suppress peaks that are too close for the analysis.
#'
#' @param location Numeric, strictly monotone location axis.
#' @param value Numeric signal. `NA` values split independent runs.
#' @param scales Strictly increasing CWT widths from 1 to 10,000 samples. At
#'   most 256 scales are supported. For each finite run, widths may not exceed
#'   half of its length; built-in defaults are trimmed to fit short runs.
#' @param min_ridge_length Minimum number of distinct scales represented in a
#'   ridge.
#' @param gap_thresh Maximum number of consecutive scales a ridge may skip.
#' @param min_snr Minimum ratio of ridge signal to the local fine-scale noise
#'   estimate. The estimate uses the requested quantile of absolute fine-scale
#'   CWT coefficients outside the peak's immediate support. SNR is capped at
#'   `1e6`; a relative machine-precision noise floor makes noiseless inputs
#'   finite and reproducible.
#' @param noise_window Window width in samples for local noise estimation.
#' @param noise_quantile Quantile in `(0, 1)` used for the local absolute
#'   fine-scale coefficient noise estimate.
#' @param min_height Minimum raw-signal height for returned peaks.
#' @param min_distance Minimum separation between returned peaks in location
#'   axis units.
#'
#' @return A `peaks_tbl` with original-sample apex locations, heights, and
#'   nearest observed bases. `algorithm_score` contains the bounded ridge SNR;
#'   `ridge_length` is the number of linked scale rows.
#' @noRd
.detect_peaks_cwt <- function(
  location,
  value,
  scales = .cwt_default_scales,
  min_ridge_length = 3L,
  gap_thresh = 1L,
  min_snr = 4.5,
  noise_window = 31L,
  noise_quantile = 0.95,
  min_height = 0,
  min_distance = 0
) {
  .validate_cwt_parameters(
    scales = scales,
    min_ridge_length = min_ridge_length,
    gap_thresh = gap_thresh,
    min_snr = min_snr,
    noise_window = noise_window,
    noise_quantile = noise_quantile,
    min_height = min_height,
    min_distance = min_distance
  )

  if (!is.numeric(location) || !is.numeric(value)) {
    cli::cli_abort(
      "CWT peak detection requires numeric {.arg location} and {.arg value} vectors."
    )
  }
  if (length(location) != length(value)) {
    cli::cli_abort(
      "CWT {.arg location} and {.arg value} must have equal lengths."
    )
  }
  if (anyNA(location) || !all(is.finite(location))) {
    cli::cli_abort(
      "CWT {.arg location} values must all be finite and non-missing."
    )
  }
  if (any(is.infinite(value))) {
    cli::cli_abort("CWT {.arg value} may contain NA, but not infinite values.")
  }

  if (length(location) < 2L) {
    return(new_peaks_tbl())
  }

  axis_diff <- diff(location)
  if (all(axis_diff > 0)) {
    descending <- FALSE
  } else if (all(axis_diff < 0)) {
    descending <- TRUE
  } else {
    cli::cli_abort(
      "CWT peak detection requires a strictly monotone location axis with no duplicates."
    )
  }

  if (length(location) < 3L) {
    return(new_peaks_tbl())
  }

  spacing <- stats::median(abs(axis_diff))
  axis_tolerance <- max(
    1e-6 * spacing,
    10 * .Machine$double.eps * max(abs(location))
  )
  if (max(abs(abs(axis_diff) - spacing)) > axis_tolerance) {
    cli::cli_abort(
      "CWT peak detection requires uniformly spaced locations; the supplied axis is irregular."
    )
  }

  if (descending) {
    location <- rev(location)
    value <- rev(value)
  }
  if (all(is.na(value))) {
    return(new_peaks_tbl())
  }

  finite_idx <- which(!is.na(value))
  run_id <- cumsum(c(1L, as.integer(diff(finite_idx) != 1L)))
  runs <- split(finite_idx, run_id)
  candidates <- list()

  for (run in runs) {
    if (length(run) < 3L) {
      next
    }

    run_scales <- scales
    max_run_scale <- floor(length(run) / 2)
    if (identical(scales, .cwt_default_scales)) {
      run_scales <- scales[scales <= max_run_scale]
    } else if (any(scales > max_run_scale)) {
      cli::cli_abort(
        "CWT {.arg scales} must not exceed half the number of finite samples in each run."
      )
    }
    if (length(run_scales) < min_ridge_length) {
      next
    }

    .cwt_check_resource_budget(length(run), run_scales)

    run_value <- value[run]
    centered_value <- run_value - stats::median(run_value)
    if (!all(is.finite(centered_value))) {
      cli::cli_abort(
        "Centering the CWT signal exceeded the supported numeric range."
      )
    }
    signal_scale <- max(abs(centered_value))
    if (!is.finite(signal_scale) || signal_scale == 0) {
      next
    }

    normalized_value <- centered_value / signal_scale
    cwt <- .cwt_mexican_hat(normalized_value, run_scales)
    raw_maxima <- .cwt_raw_local_maxima(normalized_value)
    if (length(raw_maxima) == 0L) {
      next
    }

    ridges <- .cwt_link_ridges(cwt, run_scales, gap_thresh = gap_thresh)
    if (length(ridges) == 0L) {
      next
    }

    ridge_rows <- lapply(ridges, function(ridge) {
      scale_rows <- which(!is.na(ridge$points))
      if (length(scale_rows) < min_ridge_length) {
        return(NULL)
      }

      cwt_values <- cwt[cbind(scale_rows, ridge$points[scale_rows])]
      signal <- max(cwt_values)
      if (!is.finite(signal) || signal <= 0) {
        return(NULL)
      }

      first_row <- scale_rows[1]
      ridge_position <- ridge$points[first_row]
      search_radius <- max(1L, ceiling(2 * run_scales[first_row]))
      local_candidates <- raw_maxima[
        abs(raw_maxima - ridge_position) <= search_radius
      ]
      if (length(local_candidates) == 0L) {
        return(NULL)
      }

      raw_peak <- local_candidates[
        order(
          abs(local_candidates - ridge_position),
          -normalized_value[local_candidates]
        )[1]
      ]
      noise <- .cwt_local_noise(
        cwt[1, ],
        raw_peak,
        run_scales[1],
        window = noise_window,
        quantile = noise_quantile
      )
      local_signal_scale <- .cwt_local_signal_scale(
        normalized_value,
        raw_peak,
        window = noise_window
      )
      noise_floor <- sqrt(.Machine$double.eps) *
        max(local_signal_scale, .Machine$double.xmin)
      snr <- min(signal / max(noise, noise_floor), 1e6)
      if (
        !is.finite(snr) || snr < min_snr || run_value[raw_peak] < min_height
      ) {
        return(NULL)
      }

      bases <- .cwt_peak_bases(run_value, raw_peak)
      list(
        index = as.integer(run[raw_peak]),
        left = as.integer(run[bases$left]),
        right = as.integer(run[bases$right]),
        snr = snr,
        ridge_length = as.integer(length(scale_rows))
      )
    })

    ridge_rows <- Filter(Negate(is.null), ridge_rows)
    if (length(ridge_rows) > 0L) {
      for (candidate in ridge_rows) {
        candidates[[length(candidates) + 1L]] <- candidate
      }
    }
  }

  if (length(candidates) == 0L) {
    return(new_peaks_tbl())
  }

  candidate_index <- vapply(candidates, `[[`, integer(1), "index")
  left_index <- vapply(candidates, `[[`, integer(1), "left")
  right_index <- vapply(candidates, `[[`, integer(1), "right")
  candidate_snr <- vapply(candidates, `[[`, numeric(1), "snr")
  candidate_length <- vapply(candidates, `[[`, integer(1), "ridge_length")

  # Several scale paths can describe one sampled apex. Keep the strongest path.
  candidate_order <- order(-candidate_snr, -candidate_length, candidate_index)
  candidate_index <- candidate_index[candidate_order]
  left_index <- left_index[candidate_order]
  right_index <- right_index[candidate_order]
  candidate_snr <- candidate_snr[candidate_order]
  candidate_length <- candidate_length[candidate_order]
  unique_candidate <- !duplicated(candidate_index)
  candidate_index <- candidate_index[unique_candidate]
  left_index <- left_index[unique_candidate]
  right_index <- right_index[unique_candidate]
  candidate_snr <- candidate_snr[unique_candidate]
  candidate_length <- candidate_length[unique_candidate]

  if (min_distance > 0 && length(candidate_index) > 1L) {
    distance_order <- order(-value[candidate_index], -candidate_snr)
    keep <- integer()
    for (candidate in distance_order) {
      if (
        length(keep) == 0L ||
          all(
            abs(
              location[candidate_index[candidate]] -
                location[candidate_index[keep]]
            ) >=
              min_distance
          )
      ) {
        keep <- c(keep, candidate)
      }
    }
    candidate_index <- candidate_index[keep]
    left_index <- left_index[keep]
    right_index <- right_index[keep]
    candidate_snr <- candidate_snr[keep]
    candidate_length <- candidate_length[keep]
  }

  order_index <- order(location[candidate_index])
  candidate_index <- candidate_index[order_index]
  left_index <- left_index[order_index]
  right_index <- right_index[order_index]
  candidate_snr <- candidate_snr[order_index]
  candidate_length <- candidate_length[order_index]

  new_peaks_tbl(
    peak_id = seq_along(candidate_index),
    location = location[candidate_index],
    height = value[candidate_index],
    left_base = location[left_index],
    right_base = location[right_index],
    area = rep(NA_real_, length(candidate_index)),
    algorithm_score = candidate_snr,
    ridge_length = candidate_length
  )
}

.validate_cwt_parameters <- function(
  scales,
  min_ridge_length,
  gap_thresh,
  min_snr,
  noise_window,
  noise_quantile,
  min_height,
  min_distance
) {
  if (
    !is.numeric(scales) ||
      is.logical(scales) ||
      length(scales) < 2L ||
      length(scales) > .cwt_max_scales ||
      !all(is.finite(scales)) ||
      any(scales < 1) ||
      any(scales > .cwt_max_scale) ||
      any(diff(scales) <= 0)
  ) {
    cli::cli_abort(
      "CWT {.arg scales} must contain 2 to 256 strictly increasing widths from 1 through 10,000 samples."
    )
  }
  if (
    !is.numeric(min_ridge_length) ||
      length(min_ridge_length) != 1L ||
      !is.finite(min_ridge_length) ||
      min_ridge_length != floor(min_ridge_length) ||
      min_ridge_length < 1 ||
      min_ridge_length > length(scales)
  ) {
    cli::cli_abort(
      "CWT {.arg min_ridge_length} must be an integer from 1 through the number of {.arg scales}."
    )
  }
  if (
    !is.numeric(gap_thresh) ||
      length(gap_thresh) != 1L ||
      !is.finite(gap_thresh) ||
      gap_thresh != floor(gap_thresh) ||
      gap_thresh < 0 ||
      gap_thresh >= length(scales)
  ) {
    cli::cli_abort(
      "CWT {.arg gap_thresh} must be an integer from 0 through one less than the number of {.arg scales}."
    )
  }
  if (!.cwt_is_finite_nonnegative_scalar(min_snr)) {
    cli::cli_abort(
      "CWT {.arg min_snr} must be one finite, non-negative number."
    )
  }
  if (
    !is.numeric(noise_window) ||
      length(noise_window) != 1L ||
      !is.finite(noise_window) ||
      noise_window != floor(noise_window) ||
      noise_window < 3 ||
      noise_window > .Machine$integer.max
  ) {
    cli::cli_abort(
      "CWT {.arg noise_window} must be an integer from 3 through the largest supported sample count."
    )
  }
  if (
    !is.numeric(noise_quantile) ||
      length(noise_quantile) != 1L ||
      !is.finite(noise_quantile) ||
      noise_quantile <= 0 ||
      noise_quantile >= 1
  ) {
    cli::cli_abort(
      "CWT {.arg noise_quantile} must be one number strictly between 0 and 1."
    )
  }
  if (!.cwt_is_finite_nonnegative_scalar(min_height)) {
    cli::cli_abort(
      "CWT {.arg min_height} must be one finite, non-negative number."
    )
  }
  if (!.cwt_is_finite_nonnegative_scalar(min_distance)) {
    cli::cli_abort(
      "CWT {.arg min_distance} must be one finite, non-negative number."
    )
  }

  invisible(NULL)
}

.cwt_is_finite_nonnegative_scalar <- function(x) {
  is.numeric(x) && length(x) == 1L && is.finite(x) && x >= 0
}

.cwt_mexican_hat <- function(value, scales) {
  n <- length(value)
  result <- matrix(0, nrow = length(scales), ncol = n)

  for (scale_index in seq_along(scales)) {
    scale <- scales[scale_index]
    radius <- max(1L, as.integer(ceiling(4 * scale)))
    offsets <- seq.int(-radius, radius)
    scaled_offset <- offsets / scale
    kernel <- (1 - scaled_offset^2) * exp(-0.5 * scaled_offset^2)
    kernel <- kernel - mean(kernel)
    kernel_norm <- sqrt(sum(kernel^2))
    if (!is.finite(kernel_norm) || kernel_norm == 0) {
      next
    }
    kernel <- kernel / kernel_norm

    extended_index <- .cwt_reflect_index(
      seq.int(1L - radius, n + radius),
      n
    )
    extended_value <- value[extended_index]
    filtered <- stats::filter(extended_value, filter = kernel, sides = 2)
    result[scale_index, ] <- as.numeric(
      filtered[seq.int(radius + 1L, radius + n)]
    )
  }

  result
}

.cwt_reflect_index <- function(index, n) {
  if (n <= 1L) {
    return(rep.int(1L, length(index)))
  }
  period <- 2L * n - 2L
  position <- (index - 1L) %% period
  ifelse(position >= n, period - position, position) + 1L
}

.cwt_local_maxima <- function(value) {
  n <- length(value)
  if (n < 2L) {
    return(integer())
  }

  left <- c(-Inf, value[-n])
  right <- c(value[-1L], -Inf)
  maxima <- which(
    is.finite(value) &
      value > 0 &
      value >= left &
      value >= right &
      (value > left | value > right)
  )
  if (length(maxima) < 2L) {
    return(maxima)
  }

  groups <- split(maxima, cumsum(c(1L, diff(maxima) > 1L)))
  vapply(groups, function(group) group[ceiling(length(group) / 2)], integer(1))
}

.cwt_link_ridges <- function(cwt, scales, gap_thresh) {
  n_scales <- nrow(cwt)
  maxima <- lapply(seq_len(n_scales), function(i) .cwt_local_maxima(cwt[i, ]))
  ridges <- list()

  for (scale_index in seq_len(n_scales)) {
    current <- maxima[[scale_index]]
    active <- which(vapply(ridges, `[[`, logical(1), "active"))
    matched_ridge <- integer()
    matched_maximum <- integer()

    if (length(active) > 0L && length(current) > 0L) {
      pair_rows <- vector("list", length(active))
      max_distance <- max(1, scales[scale_index])

      for (i in seq_along(active)) {
        ridge <- ridges[[active[i]]]
        gap <- scale_index - ridge$last_scale - 1L
        allowed <- max_distance * (gap + 1L)
        lower <- findInterval(
          ridge$last_index - allowed,
          current,
          left.open = TRUE
        ) +
          1L
        upper <- findInterval(ridge$last_index + allowed, current)
        if (lower <= upper) {
          maxima_index <- seq.int(lower, upper)
          ridge_scales <- which(!is.na(ridge$points))
          ridge_strength <- max(
            cwt[cbind(ridge_scales, ridge$points[ridge_scales])]
          )
          pair_rows[[i]] <- data.frame(
            ridge = rep.int(active[i], length(maxima_index)),
            maximum = maxima_index,
            distance = abs(current[maxima_index] - ridge$last_index),
            maximum_strength = cwt[scale_index, current[maxima_index]],
            ridge_strength = ridge_strength
          )
        }
      }

      available_pairs <- Filter(Negate(is.null), pair_rows)
      pairs <- if (length(available_pairs) > 0L) {
        do.call(rbind, available_pairs)
      } else {
        NULL
      }
      if (!is.null(pairs) && nrow(pairs) > 0L) {
        pairs <- pairs[
          # Prefer a stronger local maximum and an established ridge when
          # competing matches are equally close. Index order is only the final
          # deterministic fallback because it is not reflection-neutral.
          order(
            pairs$distance,
            -pairs$maximum_strength,
            -pairs$ridge_strength,
            pairs$ridge,
            pairs$maximum
          ),
          ,
          drop = FALSE
        ]
        used_ridge <- rep(FALSE, length(ridges))
        used_maximum <- rep(FALSE, length(current))
        for (i in seq_len(nrow(pairs))) {
          ridge_index <- pairs$ridge[i]
          maximum_index <- pairs$maximum[i]
          if (!used_ridge[ridge_index] && !used_maximum[maximum_index]) {
            matched_ridge <- c(matched_ridge, ridge_index)
            matched_maximum <- c(matched_maximum, maximum_index)
            used_ridge[ridge_index] <- TRUE
            used_maximum[maximum_index] <- TRUE
          }
        }
      }
    }

    if (length(active) > 0L) {
      unmatched <- setdiff(active, matched_ridge)
      for (ridge_index in unmatched) {
        ridges[[ridge_index]]$gap <- ridges[[ridge_index]]$gap + 1L
        if (ridges[[ridge_index]]$gap > gap_thresh) {
          ridges[[ridge_index]]$active <- FALSE
        }
      }
    }

    if (length(matched_ridge) > 0L) {
      for (i in seq_along(matched_ridge)) {
        ridge_index <- matched_ridge[i]
        maximum_index <- matched_maximum[i]
        ridges[[ridge_index]]$points[scale_index] <- current[maximum_index]
        ridges[[ridge_index]]$last_index <- current[maximum_index]
        ridges[[ridge_index]]$last_scale <- scale_index
        ridges[[ridge_index]]$gap <- 0L
      }
    }

    if (length(current) > 0L) {
      unmatched_maxima <- setdiff(seq_along(current), matched_maximum)
      for (maximum_index in unmatched_maxima) {
        points <- rep.int(NA_integer_, n_scales)
        points[scale_index] <- current[maximum_index]
        ridges[[length(ridges) + 1L]] <- list(
          points = points,
          last_index = current[maximum_index],
          last_scale = scale_index,
          gap = 0L,
          active = TRUE
        )
      }
    }
  }

  ridges
}

.cwt_raw_local_maxima <- function(value) {
  n <- length(value)
  if (n < 3L) {
    return(integer())
  }
  maxima <- integer()
  index <- 2L

  while (index < n) {
    end <- index
    while (end < n && value[end + 1L] == value[index]) {
      end <- end + 1L
    }
    if (
      end < n &&
        value[index] > value[index - 1L] &&
        value[end] > value[end + 1L]
    ) {
      maxima <- c(maxima, index + (end - index) %/% 2L)
    }
    index <- end + 1L
  }

  maxima
}

.cwt_check_resource_budget <- function(n, scales) {
  coefficient_cells <- as.double(n) * length(scales)
  convolution_terms <- as.double(n) * sum(2 * ceiling(4 * scales) + 1)
  if (
    coefficient_cells > .cwt_max_coefficient_cells ||
      convolution_terms > .cwt_max_convolution_terms
  ) {
    cli::cli_abort(
      c(
        "CWT request exceeds the supported computation budget.",
        "i" = paste0(
          "Requested ",
          format(coefficient_cells, scientific = FALSE),
          " coefficient cells and ",
          format(convolution_terms, scientific = FALSE),
          " convolution terms. Reduce the signal length or number and size of {.arg scales}."
        )
      )
    )
  }
  invisible(NULL)
}

.cwt_local_signal_scale <- function(signal, index, window) {
  n <- length(signal)
  half_window <- as.integer(floor(min(window, n) / 2))
  lower <- max(1L, index - half_window)
  upper <- min(n, index + half_window)
  max(abs(signal[seq.int(lower, upper)]))
}

.cwt_local_noise <- function(
  fine_scale,
  index,
  scale,
  window,
  quantile
) {
  n <- length(fine_scale)
  half_window <- as.integer(floor(min(window, n) / 2))
  lower <- max(1L, index - half_window)
  upper <- min(n, index + half_window)
  local_index <- seq.int(lower, upper)
  exclusion <- max(1L, as.integer(ceiling(4 * scale)))
  noise_index <- local_index[abs(local_index - index) > exclusion]
  noise_values <- abs(fine_scale[noise_index])

  if (length(noise_values) < 5L) {
    fallback <- which(abs(seq_len(n) - index) > exclusion)
    noise_values <- abs(fine_scale[fallback])
  }
  if (length(noise_values) == 0L) {
    noise_values <- abs(fine_scale)
  }
  noise_values <- noise_values[is.finite(noise_values)]
  if (length(noise_values) == 0L) {
    return(0)
  }

  stats::quantile(
    noise_values,
    probs = quantile,
    names = FALSE,
    type = 8
  )
}

.cwt_peak_bases <- function(value, index) {
  n <- length(value)
  left <- index - 1L
  while (left > 1L && value[left] >= value[left - 1L]) {
    left <- left - 1L
  }
  right <- index + 1L
  while (right < n && value[right] >= value[right + 1L]) {
    right <- right + 1L
  }
  list(left = as.integer(left), right = as.integer(right))
}
