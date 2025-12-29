#' Parameter for measure steps
#'
#' `window_side()` and `differentiation_order()` are used with Savitzky-Golay
#' processing.
#'
#' @param range A two-element vector holding the _defaults_ for the smallest and
#' largest possible values, respectively. If a transformation is specified,
#' these values should be in the _transformed units_.
#'
#' @param trans A `trans` object from the `scales` package, such as
#' `scales::transform_log10()` or `scales::transform_reciprocal()`. If not provided,
#' the default is used which matches the units used in `range`. If no
#' transformation, `NULL`.
#'
#' @details
#' This parameter is often used to correct for zero-count data in tables or
#' proportions.
#'
#' @return A function with classes `"quant_param"` and `"param"`.
#' @examples
#' window_side()
#' differentiation_order()
#' @export
window_side <- function(range = c(1L, 5L), trans = NULL) {
  dials::new_quant_param(
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(window_side = "Window Size (one side)"),
    finalize = NULL
  )
}

#' @rdname window_side
#' @export
differentiation_order <- function(range = c(0L, 4L), trans = NULL) {
  dials::new_quant_param(
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(differentiation_order = "Differentiation Order"),
    finalize = NULL
  )
}

#' Parameters for baseline correction steps
#'
#' `baseline_lambda()` controls the smoothness penalty in ALS baseline correction.
#' `baseline_asymmetry()` controls the asymmetry parameter in ALS.
#' `baseline_degree()` controls the polynomial degree for baseline fitting.
#'
#' @inheritParams window_side
#'
#' @return A function with classes `"quant_param"` and `"param"`.
#' @examples
#' baseline_lambda()
#' baseline_asymmetry()
#' baseline_degree()
#' baseline_span()
#' @export
baseline_lambda <- function(
  range = c(2, 9),
  trans = scales::transform_log10()
) {
  dials::new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(baseline_lambda = "Baseline Smoothness (lambda)"),
    finalize = NULL
  )
}

#' @rdname baseline_lambda
#' @export
baseline_asymmetry <- function(range = c(0.001, 0.1), trans = NULL) {
  dials::new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(baseline_asymmetry = "Baseline Asymmetry (p)"),
    finalize = NULL
  )
}

#' @rdname baseline_lambda
#' @export
baseline_degree <- function(range = c(1L, 6L), trans = NULL) {
  dials::new_quant_param(
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(baseline_degree = "Baseline Polynomial Degree"),
    finalize = NULL
  )
}

#' @rdname baseline_lambda
#' @export
baseline_half_window <- function(range = c(5L, 100L), trans = NULL) {
  dials::new_quant_param(
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(baseline_half_window = "Baseline Half Window"),
    finalize = NULL
  )
}

#' @rdname baseline_lambda
#' @export
baseline_span <- function(range = c(0.1, 0.9), trans = NULL) {
  dials::new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(baseline_span = "LOESS Span"),
    finalize = NULL
  )
}

#' Parameters for peak normalization
#'
#' `peak_location_min()` and `peak_location_max()` define the bounds for
#' the reference region in peak normalization. These should be specified
#' in the same units as the location values in your measurement data.
#'
#' @inheritParams window_side
#'
#' @return A function with classes `"quant_param"` and `"param"`.
#' @examples
#' peak_location_min()
#' peak_location_max()
#' @export
peak_location_min <- function(range = c(0, 100), trans = NULL) {
  dials::new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(peak_location_min = "Peak Region Lower Bound"),
    finalize = NULL
  )
}

#' @rdname peak_location_min
#' @export
peak_location_max <- function(range = c(0, 100), trans = NULL) {
  dials::new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(peak_location_max = "Peak Region Upper Bound"),
    finalize = NULL
  )
}

#' Parameters for derivative steps
#'
#' `derivative_order()` controls the order of differentiation in
#' `step_measure_derivative()` (1 = first derivative, 2 = second derivative).
#' `derivative_gap()` and `derivative_segment()` control the gap derivative
#' (Norris-Williams) parameters in `step_measure_derivative_gap()`.
#'
#' @inheritParams window_side
#'
#' @return A function with classes `"quant_param"` and `"param"`.
#' @examples
#' derivative_order()
#' derivative_gap()
#' derivative_segment()
#' @export
derivative_order <- function(range = c(1L, 2L), trans = NULL) {
  dials::new_quant_param(
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(derivative_order = "Derivative Order"),
    finalize = NULL
  )
}

#' @rdname derivative_order
#' @export
derivative_gap <- function(range = c(1L, 10L), trans = NULL) {
  dials::new_quant_param(
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(derivative_gap = "Derivative Gap"),
    finalize = NULL
  )
}

#' @rdname derivative_order
#' @export
derivative_segment <- function(range = c(1L, 5L), trans = NULL) {
  dials::new_quant_param(
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(derivative_segment = "Derivative Segment"),
    finalize = NULL
  )
}

# ------------------------------------------------------------------------------
# Tunable methods

#' @importFrom generics tunable
#' @export
generics::tunable

#' tunable methods for measure
#'
#' These functions define what parameters _can_ be tuned for specific steps.
#' They also define the recommended objects from the `dials` package that can be
#' used to generate new parameter values and other characteristics.
#' @param x A recipe step object
#' @param ... Not used.
#' @name tunable_measure
#' @return A tibble object.
#' @keywords internal
#' @export
tunable.step_measure_savitzky_golay <- function(x, ...) {
  tibble::tibble(
    name = c("window_side", "differentiation_order", "degree"),
    call_info = list(
      list(pkg = "measure", fun = "window_side"),
      list(pkg = "measure", fun = "differentiation_order"),
      list(pkg = "dials", fun = "degree_int", range = c(1L, 5L))
    ),
    source = "recipe",
    component = "step_measure_savitzky_golay",
    component_id = x$id
  )
}

#' @rdname tunable_measure
#' @export
tunable.step_measure_baseline_als <- function(x, ...) {
  tibble::tibble(
    name = c("lambda", "p"),
    call_info = list(
      list(pkg = "measure", fun = "baseline_lambda"),
      list(pkg = "measure", fun = "baseline_asymmetry")
    ),
    source = "recipe",
    component = "step_measure_baseline_als",
    component_id = x$id
  )
}

#' @rdname tunable_measure
#' @export
tunable.step_measure_baseline_poly <- function(x, ...) {
  tibble::tibble(
    name = "degree",
    call_info = list(
      list(pkg = "measure", fun = "baseline_degree")
    ),
    source = "recipe",
    component = "step_measure_baseline_poly",
    component_id = x$id
  )
}

#' @rdname tunable_measure
#' @export
tunable.step_measure_baseline_rf <- function(x, ...) {
  tibble::tibble(
    name = "span",
    call_info = list(
      list(pkg = "measure", fun = "baseline_span")
    ),
    source = "recipe",
    component = "step_measure_baseline_rf",
    component_id = x$id
  )
}

#' @rdname tunable_measure
#' @export
tunable.step_measure_detrend <- function(x, ...) {
  tibble::tibble(
    name = "degree",
    call_info = list(
      list(pkg = "dials", fun = "degree_int", range = c(0L, 3L))
    ),
    source = "recipe",
    component = "step_measure_detrend",
    component_id = x$id
  )
}

#' @rdname tunable_measure
#' @export
tunable.step_measure_normalize_peak <- function(x, ...) {
  tibble::tibble(
    name = c("location_min", "location_max"),
    call_info = list(
      list(pkg = "measure", fun = "peak_location_min"),
      list(pkg = "measure", fun = "peak_location_max")
    ),
    source = "recipe",
    component = "step_measure_normalize_peak",
    component_id = x$id
  )
}

#' @rdname tunable_measure
#' @export
tunable.step_measure_derivative <- function(x, ...) {
  tibble::tibble(
    name = "order",
    call_info = list(
      list(pkg = "measure", fun = "derivative_order")
    ),
    source = "recipe",
    component = "step_measure_derivative",
    component_id = x$id
  )
}

#' @rdname tunable_measure
#' @export
tunable.step_measure_derivative_gap <- function(x, ...) {
  tibble::tibble(
    name = c("gap", "segment"),
    call_info = list(
      list(pkg = "measure", fun = "derivative_gap"),
      list(pkg = "measure", fun = "derivative_segment")
    ),
    source = "recipe",
    component = "step_measure_derivative_gap",
    component_id = x$id
  )
}

# ==============================================================================
# Parameters for smoothing steps
# ==============================================================================

#' Parameters for smoothing steps
#'
#' `smooth_window()` controls the window size for moving average and median
#' smoothing. `smooth_sigma()` controls the standard deviation for Gaussian
#' smoothing. `fourier_cutoff()` controls the frequency cutoff for Fourier
#' filtering.
#'
#' @inheritParams window_side
#'
#' @return A function with classes `"quant_param"` and `"param"`.
#' @examples
#' smooth_window()
#' smooth_sigma()
#' fourier_cutoff()
#' @export
smooth_window <- function(range = c(3L, 21L), trans = NULL) {
  dials::new_quant_param(
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(smooth_window = "Smoothing Window Size"),
    finalize = NULL
  )
}

#' @rdname smooth_window
#' @export
smooth_sigma <- function(range = c(0.5, 5), trans = NULL) {
  dials::new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(smooth_sigma = "Gaussian Sigma"),
    finalize = NULL
  )
}

#' @rdname smooth_window
#' @export
fourier_cutoff <- function(range = c(0.01, 0.5), trans = NULL) {
  dials::new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(fourier_cutoff = "Fourier Cutoff Frequency"),
    finalize = NULL
  )
}

#' @rdname smooth_window
#' @export
despike_threshold <- function(range = c(2, 10), trans = NULL) {
  dials::new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(despike_threshold = "Despike Threshold (MAD units)"),
    finalize = NULL
  )
}

# ==============================================================================
# Parameters for alignment steps
# ==============================================================================

#' Parameters for alignment steps
#'
#' `align_max_shift()` controls the maximum shift allowed in alignment.
#' `align_segment_length()` controls segment size for COW alignment.
#'
#' @inheritParams window_side
#'
#' @return A function with classes `"quant_param"` and `"param"`.
#' @examples
#' align_max_shift()
#' align_segment_length()
#' @export
align_max_shift <- function(range = c(1L, 50L), trans = NULL) {
  dials::new_quant_param(
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(align_max_shift = "Maximum Alignment Shift"),
    finalize = NULL
  )
}

#' @rdname align_max_shift
#' @export
align_segment_length <- function(range = c(10L, 100L), trans = NULL) {
  dials::new_quant_param(
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(align_segment_length = "Alignment Segment Length"),
    finalize = NULL
  )
}

# ==============================================================================
# Parameters for quality control steps
# ==============================================================================

#' Parameters for quality control steps
#'
#' `outlier_threshold()` controls the threshold for outlier detection
#' (in standard deviation or Mahalanobis distance units).
#'
#' @inheritParams window_side
#'
#' @return A function with classes `"quant_param"` and `"param"`.
#' @examples
#' outlier_threshold()
#' @export
outlier_threshold <- function(range = c(2, 5), trans = NULL) {
  dials::new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(outlier_threshold = "Outlier Threshold"),
    finalize = NULL
  )
}

# ==============================================================================
# Tunable methods for new steps
# ==============================================================================

#' @rdname tunable_measure
#' @export
tunable.step_measure_baseline_airpls <- function(x, ...) {
  tibble::tibble(
    name = "lambda",
    call_info = list(
      list(pkg = "measure", fun = "baseline_lambda")
    ),
    source = "recipe",
    component = "step_measure_baseline_airpls",
    component_id = x$id
  )
}

#' @rdname tunable_measure
#' @export
tunable.step_measure_baseline_arpls <- function(x, ...) {
  tibble::tibble(
    name = "lambda",
    call_info = list(
      list(pkg = "measure", fun = "baseline_lambda")
    ),
    source = "recipe",
    component = "step_measure_baseline_arpls",
    component_id = x$id
  )
}

#' @rdname tunable_measure
#' @export
tunable.step_measure_smooth_ma <- function(x, ...) {
  tibble::tibble(
    name = "window",
    call_info = list(
      list(pkg = "measure", fun = "smooth_window")
    ),
    source = "recipe",
    component = "step_measure_smooth_ma",
    component_id = x$id
  )
}

#' @rdname tunable_measure
#' @export
tunable.step_measure_smooth_median <- function(x, ...) {
  tibble::tibble(
    name = "window",
    call_info = list(
      list(pkg = "measure", fun = "smooth_window")
    ),
    source = "recipe",
    component = "step_measure_smooth_median",
    component_id = x$id
  )
}

#' @rdname tunable_measure
#' @export
tunable.step_measure_smooth_gaussian <- function(x, ...) {
  tibble::tibble(
    name = "sigma",
    call_info = list(
      list(pkg = "measure", fun = "smooth_sigma")
    ),
    source = "recipe",
    component = "step_measure_smooth_gaussian",
    component_id = x$id
  )
}

#' @rdname tunable_measure
#' @export
tunable.step_measure_filter_fourier <- function(x, ...) {
  tibble::tibble(
    name = "cutoff",
    call_info = list(
      list(pkg = "measure", fun = "fourier_cutoff")
    ),
    source = "recipe",
    component = "step_measure_filter_fourier",
    component_id = x$id
  )
}

#' @rdname tunable_measure
#' @export
tunable.step_measure_despike <- function(x, ...) {
  tibble::tibble(
    name = c("threshold", "window"),
    call_info = list(
      list(pkg = "measure", fun = "despike_threshold"),
      list(pkg = "measure", fun = "smooth_window")
    ),
    source = "recipe",
    component = "step_measure_despike",
    component_id = x$id
  )
}

#' @rdname tunable_measure
#' @export
tunable.step_measure_align_shift <- function(x, ...) {
  tibble::tibble(
    name = "max_shift",
    call_info = list(
      list(pkg = "measure", fun = "align_max_shift")
    ),
    source = "recipe",
    component = "step_measure_align_shift",
    component_id = x$id
  )
}

#' @rdname tunable_measure
#' @export
tunable.step_measure_qc_outlier <- function(x, ...) {
  tibble::tibble(
    name = "threshold",
    call_info = list(
      list(pkg = "measure", fun = "outlier_threshold")
    ),
    source = "recipe",
    component = "step_measure_qc_outlier",
    component_id = x$id
  )
}

# ==============================================================================
# Parameters for feature engineering and scatter correction steps
# ==============================================================================

#' Parameters for feature engineering and scatter correction
#'
#' `bin_width()` controls the width of bins in spectral binning.
#' `emsc_degree()` controls the polynomial degree for EMSC correction.
#' `osc_n_components()` controls the number of orthogonal components in OSC.
#'
#' @inheritParams window_side
#'
#' @return A function with classes `"quant_param"` and `"param"`.
#' @examples
#' bin_width()
#' emsc_degree()
#' osc_n_components()
#' @export
bin_width <- function(range = c(1, 20), trans = NULL) {
  dials::new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(bin_width = "Bin Width"),
    finalize = NULL
  )
}

#' @rdname bin_width
#' @export
emsc_degree <- function(range = c(0L, 4L), trans = NULL) {
  dials::new_quant_param(
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(emsc_degree = "EMSC Polynomial Degree"),
    finalize = NULL
  )
}

#' @rdname bin_width
#' @export
osc_n_components <- function(range = c(1L, 10L), trans = NULL) {
  dials::new_quant_param(
    type = "integer",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(osc_n_components = "Number of OSC Components"),
    finalize = NULL
  )
}
