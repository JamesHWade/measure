#' Subtract signal of a "blank" measurement from target signal response
#'
#' @param recipe A recipe object. The step will be added to the
#'  sequence of operations for this recipe.
#' @param ... One or more selector functions to choose variables
#'  for this step.
#' @param role Assign the role of new variables.
#'
step_subtract_blank <- function(recipe,
                          ...,
                          role = "predictor") {
  # terms <- recipes::ellipse_check(...)
  cli::cli_alert_danger("Not yet implemented.")
  # recipes::add_step(
  #   recipe,
  #   step_baseline_new()
  # )
}

step_subtract_blank_new <-
  function(terms, role, trained, ref_dist, options, skip, id) {
    cli::cli_alert_danger("Not yet implemented.")
    # step(
    #   subclass = "measure",
    #   terms = terms,
    #   role = role,
    #   trained = trained,
    #   ref_dist = ref_dist,
    #   options = options,
    #   skip = skip,
    #   id = id
    # )
  }
