#' Fit and subtract a baseline from a measurement signal
#'
#' @inheritParams recipes::step_pca
#' @param recipe A recipe object. The step will be added to the
#'  sequence of operations for this recipe.
#' @param ... One or more selector functions to choose variables
#'  for this step.
#' @param role Assign the role of new variables.
#'
step_baseline <- function(recipe,
                          ...,
                          role = NA,
                          trained = FALSE,
                          options = NULL,
                          skip = FALSE,
                          id = recipes::rand_id("measure")) {
  terms <- recipes::ellipse_check(...)
  add_step(
    recipe,
    step_baseline_new(
      terms = terms,
      trained = trained,
      role = role,
      options = options,
      skip = skip,
      id = id
    )
  )
}

step_baseline_new <-
  function(terms, role, trained, options, skip, id) {
    step(
      subclass = "measure",
      terms = terms,
      role = role,
      trained = trained,
      options = options,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_baseline <- function(x, training, info = NULL, ...) {
  col_names <- recipes::recipes_eval_select(x$terms, training, info)
  recipes::check_type(x, quant = TRUE)
}

#' @export
print.step_baseline <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Subtract baseline "
    print_step(names(x$means), x$terms, x$trained, title, width)
    invisible(x)
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
#' meats_long %>% subtract_rf_baseline(yvar = transmittance)
subtract_rf_baseline <- function(data, yvar, span = 2/3, maxit = c(5, 5)){

  # rlang::arg_match0(as.character(rlang::enquo(yvar)), values = names(data))

  data %>%
    dplyr::mutate(
      raw = {{ yvar }},
      baseline = IDPmisc::rfbaseline(x = 1:length({{ yvar }}),
                                     y = {{ yvar }},
                                     span = span,
                                     maxit = maxit)$fit,
      {{ yvar }} := {{ yvar }} - baseline
    )
}

#' @export
bake.step_baseline <- function(object, new_data, ...) {
  cli::cli_alert_danger("Not yet implemented.")
}
