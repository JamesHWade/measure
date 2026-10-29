#' Wavelet Peak Detection
#'
#' Use `step_measure_peaks_detect(algorithm = "cwt")` to detect peaks with
#' Mexican-hat wavelets at several widths. Positive wavelet maxima are linked
#' into ridges; ridge persistence and local wavelet-space signal-to-noise ratio
#' filter the candidates.
#'
#' @section Choosing widths:
#' `algorithm_params$scales` contains strictly increasing widths in **samples**,
#' rather than measurement-axis units. The default is `c(1, 2, 3, 4, 6, 8, 12)`.
#' Choose widths that cover the peaks of interest on the sampled grid. Widths
#' must be between 1 and 10,000 samples, with at most 256 scales. Each width
#' must also fit within half the length of the finite signal run. The default
#' width sequence is trimmed for short runs, including when explicitly supplied
#' as integers or doubles. Other unsupported width sequences are errors.
#' A run with fewer supported scales than the requested ridge length returns
#' no peaks. Work is limited to 10 million coefficient cells and 200 million
#' convolution terms per run; oversized requests fail before allocation.
#'
#' @section Algorithm parameters:
#' Pass CWT-specific controls through `algorithm_params`:
#'
#' * `min_ridge_length = 3L`: minimum number of linked scales.
#' * `gap_thresh = 1L`: maximum number of consecutive missing scales.
#' * `min_snr = 4.5`: minimum ridge SNR in wavelet space. Lower values admit
#'   weaker peaks and more noise candidates. This differs from the recipe's
#'   optional MAD-based `snr_threshold`, which is a separate filter.
#' * `noise_window = 31L`: local noise window in samples.
#' * `noise_quantile = 0.95`: quantile of absolute fine-scale coefficients
#'   outside the immediate peak support used to estimate local noise.
#'
#' `min_height` remains in original signal units; `min_distance` remains in
#' measurement-axis units. Nearby ridges can generate multiple candidates
#' within one broad or noisy feature. Set `min_distance` for the separation
#' appropriate to the measurement. Values nested in `algorithm_params` are
#' currently not extracted as recipe tuning parameters.
#'
#' @section Axes, missing data, and output:
#' The location axis must be finite, strictly monotonic, and uniformly spaced.
#' Irregular grids are rejected. Descending axes are reversed internally;
#' returned peaks are ordered by ascending location. Missing signal values
#' split the signal into independent runs, so detection does not bridge gaps.
#' Median centering and reflected boundaries reduce baseline and edge effects.
#'
#' Locations, heights, and detected bases refer to the original sampled signal.
#' The additional `algorithm_score` column is the ridge SNR (capped at `1e6`),
#' and `ridge_length` records the number of linked scales. A local numerical
#' noise floor keeps noiseless-input scores finite. These scores describe the
#' detector, rather than a probability or a false-discovery guarantee.
#'
#' @section Method:
#' This implementation is inspired by Du et al. (2006), using discrete
#' zero-mean, unit-energy Mexican-hat kernels and scale-linked maxima. Its
#' ridge linking and local noise implementation are specific to measure.
#'
#' @references
#' Du, P., Kibbe, W. A., and Lin, S. M. (2006). Improved peak detection in mass
#' spectrum by incorporating continuous wavelet transform-based pattern
#' matching. *Bioinformatics*, 22(17), 2059-2065.
#' <https://doi.org/10.1093/bioinformatics/btl355>
#'
#' @name peak_detection_cwt
#' @seealso [step_measure_peaks_detect()], [get_peak_algorithm()]
#' @examples
#' library(recipes)
#'
#' axis <- seq(0, 20, length.out = 201)
#' signal <- exp(-0.5 * ((axis - 5) / 0.3)^2) +
#'   0.7 * exp(-0.5 * ((axis - 14) / 0.8)^2)
#' data <- tibble::tibble(sample_id = "example")
#' data$signal <- new_measure_list(list(new_measure_tbl(axis, signal)))
#'
#' rec <- recipe(~ ., data = data) |>
#'   step_measure_peaks_detect(
#'     algorithm = "cwt",
#'     min_distance = 3,
#'     algorithm_params = list(scales = c(1, 2, 4, 8, 12))
#'   ) |>
#'   prep()
#' bake(rec, new_data = NULL)$.peaks[[1]]
NULL
