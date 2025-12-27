#' Subtract baseline using robust fitting method
#'
#' A standalone function for robust fitting baseline subtraction using
#' local regression with iterative reweighting. For use within a recipe
#' workflow, see [step_measure_baseline_rf()].
#'
#' @param data A dataframe containing the variable for baseline subtraction
#' @param yvar The name of the column for baseline subtraction
#' @param span Controls the amount of smoothing based on the fraction of data
#' to use in computing each fitted value, defaults to `2/3`.
#' @param maxit The number of iterations to use the robust fit, defaults to
#' `c(5, 5)` where the first value specifies iterations for asymmetric weighting
#' function and the second value for symmetric weighting function.
#'
#' @return A dataframe matching column in data plus `raw` and `baseline` columns
#' @export
#'
#' @seealso [step_measure_baseline_rf()] for the recipe step version.
#'
#' @examples
#' library(dplyr)
#' meats_long |>
#'   group_by(id) |>
#'   subtract_rf_baseline(yvar = transmittance)
subtract_rf_baseline <- function(data, yvar, span = 2 / 3, maxit = c(5, 5)) {
  data |>
    dplyr::mutate(
      raw = {{ yvar }},
      baseline = IDPmisc::rfbaseline(
        x = 1:length({{ yvar }}),
        y = {{ yvar }},
        span = span,
        maxit = maxit
      )$fit,
      {{ yvar }} := {{ yvar }} - baseline
    )
}
