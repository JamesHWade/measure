#' Title
#'
#' @param recipe A recipe object. The step will be added to the
#'  sequence of operations for this recipe.
#' @param ... One or more selector functions to choose variables
#'  for this step.
#' @param role Assign the role of new variables.
#'
step_baseline <- function(recipe,
                          ...,
                          role = "predictor") {
  cli::cli_alert_danger("Not yet implemented.")
}


#' Subtract baseline using robust fitting method
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
#' @examples
#' meats_long |> subtract_rf_baseline(yvar = transmittance)
subtract_rf_baseline <- function(data, yvar, span = 2/3, maxit = c(5, 5)){

  # rlang::arg_match0(as.character(rlang::enquo(yvar)), values = names(data))

  data |>
    dplyr::mutate(
      raw = {{ yvar }},
      baseline = IDPmisc::rfbaseline(x = 1:length({{ yvar }}),
                                     y = {{ yvar }},
                                     span = span,
                                     maxit = maxit)$fit,
      {{ yvar }} := {{ yvar }} - baseline
    )
}
