#' Reshape measurement data into
#'
#' @inheritParams recipes::step_pca
#' @param recipe A recipe object. The step will be added to the
#'  sequence of operations for this recipe.
#' @param ... One or more selector functions to choose variables
#'  for this step.
#' @param role Assign the role of new variables.
#' @param shape The starting shape of the data, either "long" or "wide" using
#' tidyr-style nomenclature.
#'
step_measure_collect <- function(recipe,
                          ...,
                          role = NA,
                          shape = c("long", "wide"),
                          trained = FALSE,
                          options = NULL,
                          skip = FALSE,
                          id = rand_id("measure")) {
  terms <- recipes::ellipse_check(...)
  warn("Not yet implemented.")
  add_step(
    recipe,
    step_measure_collect_new(
      terms = terms,
      trained = trained,
      role = role,
      options = options,
      skip = skip,
      id = id
    )
  )
}

step_measure_collect_new <-
  function(terms, role, trained, options, skip, id) {
    cli::cli_alert_danger("Not yet implemented.")
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

prep.step_measure_collect <- function(x, training, info = NULL, ...) {
  cli::cli_alert_danger("Not yet implemented.")
  col_names <- recipes::recipes_eval_select(x$terms, training, info)
  recipes::check_type(x, quant = TRUE)
}


#' Subtract baseline using robust fitting method
#'
#' @param data A dataframe containing the variable for baseline subtraction
#'
#' @return A dataframe matching column in data plus `raw` and `baseline` columns
#' @export
#'
#' @examples
#' meats_long |> subtract_rf_measure_collect(yvar = transmittance)
measure_collect <- function(data){

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

bake.step_measure_collect <- function(object, new_data, ...) {
  cli::cli_alert_danger("Not yet implemented.")
}
