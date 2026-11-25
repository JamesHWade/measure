.peak_cwt_tunable_names <- function(step) {
  if (
    !identical(step$algorithm, "cwt") ||
      !identical(get_peak_algorithm("cwt")$algorithm_fn, .detect_peaks_cwt)
  ) {
    return(character())
  }
  intersect(
    c(
      "min_snr",
      "min_ridge_length",
      "gap_thresh",
      "noise_window",
      "noise_quantile"
    ),
    names(step$algorithm_params)
  )
}

#' @importFrom generics tune_args
#' @export
tune_args.step_measure_peaks_detect <- function(object, full = FALSE, ...) {
  # recipes discovers markers on individual step fields. Each supported CWT
  # field has a distinct name, so expose it directly on this temporary copy.
  tunable_names <- generics::tunable(object)$name
  for (name in .peak_cwt_tunable_names(object)) {
    object[[name]] <- .custom_baseline_normalize_tune_ids(
      object$algorithm_params[[name]],
      default_id = paste0("cwt_", name)
    )
  }
  object$algorithm_params <- NULL
  args <- NextMethod()
  args$tunable <- !is.na(args$id) & args$name %in% tunable_names
  args
}

#' @export
update.step_measure_peaks_detect <- function(object, ...) {
  nested <- intersect(names(list(...)), .peak_cwt_tunable_names(object))
  if (!length(nested)) {
    return(NextMethod())
  }
  # Let update.step() validate and apply all changes together, including
  # direct thresholds. Put the finalized CWT values back where bake reads them.
  object[nested] <- object$algorithm_params[nested]
  updated <- NextMethod()
  updated$algorithm_params[nested] <- updated[nested]
  updated[nested] <- NULL
  updated
}
