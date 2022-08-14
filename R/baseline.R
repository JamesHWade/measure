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
  cli::cli_alert_info("Not yet implemented.")
  NULL
}
