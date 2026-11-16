.ms_trace_abort <- function(message) {
  cli::cli_abort("{message}", call = NULL)
}

.ms_trace_validate_text_selector <- function(x, field) {
  if (
    typeof(x) != "character" ||
      length(x) != 1L ||
      !is.null(attributes(x)) ||
      is.na(x) ||
      !nzchar(trimws(x)) ||
      Encoding(x) == "bytes" ||
      !validUTF8(x)
  ) {
    .ms_trace_abort(
      paste0("`", field, "` must be one nonblank UTF-8 character value.")
    )
  }
  invisible(x)
}

.ms_trace_validate_ms_level <- function(x) {
  if (
    !typeof(x) %in% c("integer", "double") ||
      length(x) != 1L ||
      !is.null(attributes(x)) ||
      is.na(x) ||
      !is.finite(x) ||
      x < 1 ||
      x != floor(x) ||
      x > .Machine$integer.max
  ) {
    .ms_trace_abort("`ms_level` must be one positive whole-number value.")
  }
  as.integer(x)
}

.ms_trace_validate_mz_range <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }

  if (
    !typeof(x) %in% c("integer", "double") ||
      length(x) != 2L ||
      !is.null(attributes(x)) ||
      !all(is.finite(x)) ||
      any(x <= 0) ||
      x[[1L]] > x[[2L]]
  ) {
    .ms_trace_abort(
      "`mz_range` must be a plain finite positive interval with lower <= upper."
    )
  }

  as.double(x)
}

#' Extract a centroid total-ion or selected-ion trace
#'
#' Extract one explicit polarity and MS level from one declared MS stream.
#' Centroid intensities are summed in their original point order. A closed
#' `mz_range` selects an inclusive extracted-ion window in the stream's
#' declared m/z unit; `NULL` extracts a total-ion trace.
#'
#' @param x A validated `measure_acquisition`.
#' @param stream_id One exact MS stream identifier.
#' @param polarity One of `"positive"` or `"negative"`.
#' @param ms_level One positive MS level.
#' @param mz_range `NULL` for a total-ion trace, or a two-value closed m/z
#'   interval in the stream's declared unit.
#'
#' @details
#' One stream, polarity, and MS level must be selected explicitly. A scan
#' whose unknown settings could match the requested combination is ambiguous
#' and causes an error. Unknown settings on scans excluded by another known
#' selector do not block the calculation. An empty selection is an error.
#'
#' Selected rows follow their declared `scan_index`, retaining repeated or
#' nonmonotonic retention times. Times must be known and finite and the stream
#' must declare a time unit. XIC bounds require a known declared m/z unit.
#' No units are converted and no samples, polarities or MS levels are pooled.
#'
#' Acquired selected scans must declare centroid representation. Every m/z in
#' those scans must be finite and positive, including points outside an XIC
#' window. Selected intensities must be finite and nonnegative or ordinary NA;
#' NaN, infinities, negative values and unrepresentable sums cause errors.
#' Unselected intensities do not contribute to an XIC. Duplicate m/z points
#' contribute separately, in their original order.
#'
#' @section Missingness and saturation:
#' The following statuses describe the declared centroid calculation:
#'
#' * `missing_scan`: an explicitly missing scan yields NA, with no points.
#' * `empty_scan`: an acquired-empty centroid spectrum yields zero.
#' * `empty_window`: a nonempty spectrum with no XIC matches yields zero.
#' * `saturated`: any selected point declared saturated yields NA.
#' * `missing_intensity`: any selected ordinary NA yields NA.
#' * `calculated`: the selected finite intensities were summed.
#'
#' Saturation takes precedence over ordinary missing intensity. Invalid selected
#' numerical states still cause errors before either policy is applied.
#' `saturation_unknown` independently records whether any selected mask entry
#' is unknown; it is not silently treated as FALSE. A calculated value does
#' not certify an instrument, method, absence of saturation, or analyte identity.
#'
#' @section Contributor links and processing:
#' `.point_indices` retains every selected original point position, including
#' those responsible for an NA result. Together with `scan_id`, these indices
#' link each output row to its retained acquisition input. They are not source
#' file line numbers. Source record ordinals remain in the raw scan's
#' `.source_rows` and original source scan identifiers remain opaque text.
#'
#' The `processing` attribute is a named list recording version 1, the operation,
#' package version, explicit stream/injection/channel/source IDs and available
#' source digest, selectors, bounds, units, centroid sum, propagation policies
#' and absence of synchronization. Unknown source evidence remains unknown.
#' Keep this metadata and contributor links with the raw acquisition. A scalar
#' record export alone does not preserve many-to-one extraction ancestry.
#'
#' Profile spectra may be stored but are not calculated here: an unweighted
#' profile-point sum depends on sampling density and is not physical integration.
#' This function performs no centroiding, interpolation, binning or annotation.
#'
#' @seealso [measure_acquisition()]
#'
#' @return An ordinary tibble with one row per selected scan. The
#'   columns are `scan_id`, `scan_index`, `source_scan_id`, `retention_time`,
#'   `value`, `status`, `selected_points`, `saturation_unknown`, and
#'   `.point_indices`. The `processing` attribute records the declared
#'   extraction parameters. The returned table does not require a monotonic
#'   time axis; converting it to a scalar record requires that record's checks.
#' @export
measure_ms_trace <- function(
  x,
  stream_id,
  polarity,
  ms_level,
  mz_range = NULL
) {
  validate_measure_acquisition(x)
  .ms_trace_validate_text_selector(stream_id, "stream_id")

  .ms_trace_validate_text_selector(polarity, "polarity")
  if (!polarity %in% c("positive", "negative")) {
    .ms_trace_abort("`polarity` must be exactly `positive` or `negative`.")
  }
  ms_level <- .ms_trace_validate_ms_level(ms_level)
  mz_range <- .ms_trace_validate_mz_range(mz_range)

  stream_index <- match(stream_id, x$streams$stream_id)
  if (is.na(stream_index) || x$streams$kind[[stream_index]] != "ms") {
    .ms_trace_abort("`stream_id` must identify exactly one declared MS stream.")
  }
  stream <- x$streams[stream_index, , drop = FALSE]

  if (!is.null(mz_range) && is.na(stream$mz_unit[[1L]])) {
    .ms_trace_abort("An XIC requires a known declared m/z unit for the stream.")
  }
  if (is.na(stream$time_unit[[1L]])) {
    .ms_trace_abort("A centroid trace requires a known declared time unit.")
  }

  stream_rows <- which(x$scans$stream_id == stream_id)
  scan_polarity <- x$scans$polarity[stream_rows]
  scan_level <- x$scans$ms_level[stream_rows]
  could_match <-
    (is.na(scan_polarity) | scan_polarity == polarity) &
    (is.na(scan_level) | scan_level == ms_level)
  has_unknown_selector <- is.na(scan_polarity) | is.na(scan_level)

  if (any(could_match & has_unknown_selector)) {
    .ms_trace_abort(
      "A scan with unknown polarity or MS level could match the requested selectors."
    )
  }

  selected <- stream_rows[
    scan_polarity == polarity & scan_level == ms_level
  ]
  if (length(selected) == 0L) {
    .ms_trace_abort(
      "No scans match the requested stream, polarity, and MS level."
    )
  }
  selected <- selected[order(x$scans$scan_index[selected], method = "radix")]

  selected_time <- x$scans$retention_time[selected]
  if (!all(is.finite(selected_time))) {
    .ms_trace_abort(
      "Every selected scan must have a known finite retention time."
    )
  }

  n_selected <- length(selected)
  trace_value <- rep(NA_real_, n_selected)
  status <- character(n_selected)
  selected_points <- integer(n_selected)
  saturation_unknown <- logical(n_selected)
  point_indices <- vector("list", n_selected)

  for (i in seq_len(n_selected)) {
    scan <- x$scans[selected[[i]], , drop = FALSE]

    if (scan$scan_status[[1L]] == "missing") {
      trace_value[[i]] <- NA_real_
      status[[i]] <- "missing_scan"
      selected_points[[i]] <- 0L
      saturation_unknown[[i]] <- FALSE
      point_indices[[i]] <- integer()
      next
    }

    if (
      is.na(scan$spectrum_type[[1L]]) ||
        scan$spectrum_type[[1L]] != "centroid"
    ) {
      .ms_trace_abort(
        paste0(
          "Acquired scan `",
          scan$scan_id[[1L]],
          "` must declare centroid spectrum type."
        )
      )
    }

    spectrum <- scan$.measures[[1L]]
    mz <- spectrum$location
    if (any(!is.finite(mz) | mz <= 0)) {
      .ms_trace_abort(
        paste0(
          "Every m/z in acquired selected scan `",
          scan$scan_id[[1L]],
          "` must be finite and positive."
        )
      )
    }

    if (is.null(mz_range)) {
      contributors <- seq_along(mz)
    } else {
      contributors <- which(mz >= mz_range[[1L]] & mz <= mz_range[[2L]])
    }

    values <- as.double(spectrum$value[contributors])
    invalid_value <-
      is.nan(values) |
      is.infinite(values) |
      (!is.na(values) & values < 0)
    if (any(invalid_value)) {
      .ms_trace_abort(
        paste0(
          "Selected centroid intensities in scan `",
          scan$scan_id[[1L]],
          "` must be finite and nonnegative, or ordinary NA."
        )
      )
    }

    finite_values <- values[!is.na(values)]
    sum_value <- 0
    for (j in seq_along(finite_values)) {
      sum_value <- sum_value + finite_values[[j]]
      if (!is.finite(sum_value)) {
        .ms_trace_abort(
          paste0(
            "Selected centroid intensities in scan `",
            scan$scan_id[[1L]],
            "` have an unrepresentable sum."
          )
        )
      }
    }

    mask <- scan$.saturated[[1L]][contributors]
    has_saturated <- any(!is.na(mask) & mask)
    has_missing_intensity <- anyNA(values)

    point_indices[[i]] <- as.integer(contributors)
    selected_points[[i]] <- as.integer(length(contributors))
    saturation_unknown[[i]] <- anyNA(mask)

    if (length(mz) == 0L) {
      trace_value[[i]] <- 0
      status[[i]] <- "empty_scan"
    } else if (length(contributors) == 0L) {
      trace_value[[i]] <- 0
      status[[i]] <- "empty_window"
    } else if (has_saturated) {
      trace_value[[i]] <- NA_real_
      status[[i]] <- "saturated"
    } else if (has_missing_intensity) {
      trace_value[[i]] <- NA_real_
      status[[i]] <- "missing_intensity"
    } else {
      trace_value[[i]] <- sum_value
      status[[i]] <- "calculated"
    }
  }

  source_id <- stream$source_id[[1L]]
  source_sha256 <- NA_character_
  if (!is.na(source_id)) {
    source_row <- match(source_id, x$record$sources$source_id)
    source_sha256 <- x$record$sources$source_sha256[[source_row]]
  }

  result <- tibble::tibble(
    scan_id = x$scans$scan_id[selected],
    scan_index = x$scans$scan_index[selected],
    source_scan_id = x$scans$source_scan_id[selected],
    retention_time = as.double(selected_time),
    value = trace_value,
    status = status,
    selected_points = selected_points,
    saturation_unknown = saturation_unknown,
    .point_indices = point_indices
  )

  attr(result, "processing") <- list(
    version = 1L,
    operation = if (is.null(mz_range)) "tic" else "xic",
    package = "measure",
    package_version = as.character(utils::packageVersion("measure")),
    stream_id = stream$stream_id[[1L]],
    injection_id = stream$injection_id[[1L]],
    channel_id = stream$channel_id[[1L]],
    source_id = source_id,
    source_sha256 = source_sha256,
    polarity = polarity,
    ms_level = ms_level,
    mz_range = mz_range,
    mz_unit = stream$mz_unit[[1L]],
    time_unit = stream$time_unit[[1L]],
    value_name = stream$value_name[[1L]],
    value_unit = stream$value_unit[[1L]],
    aggregation = "centroid_sum",
    missing_intensity = "propagate",
    saturation = "propagate",
    synchronization = "none"
  )

  result
}
