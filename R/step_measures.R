#' #' Reshape measurement data into
#' #'
#' #' @inheritParams recipes::step_pca
#' #' @param recipe A recipe object. The step will be added to the
#' #'  sequence of operations for this recipe.
#' #' @param ... One or more selector functions to choose variables
#' #'  for this step.
#' #' @param role Assign the role of new variables.
#' #' @param shape The starting shape of the data, either "long" or "wide" using
#' #' tidyr-style nomenclature.
#' #'
#' #' @export
#' step_measure <- function(recipe,
#'                                  ...,
#'                                  role = "measure",
#'                                  shape = c("long", "wide"),
#'                                  trained = FALSE,
#'                                  options = NULL,
#'                                  skip = FALSE,
#'                                  id = rand_id("measure")) {
#'   terms <- ellipse_check(...)
#'   add_step(
#'     recipe,
#'     step_measure_new(
#'       terms = terms,
#'       trained = trained,
#'       role = role,
#'       shape = shape,
#'       options = options,
#'       skip = skip,
#'       id = id
#'     )
#'   )
#' }
#'
#' step_measure_new <-
#'   function(terms, role, trained, shape, options, skip, id) {
#'     step(
#'       subclass = "measure",
#'       terms = terms,
#'       role = role,
#'       trained = trained,
#'       shape = shape,
#'       options = options,
#'       skip = skip,
#'       id = id
#'     )
#'   }
#
# step_measure_collect_new <-
#   function(terms, role, shape, trained, options, skip, id) {
#     step(
#       subclass = "measure",
#       terms    = terms,
#       role     = role,
#       shape    = shape,
#       trained  = trained,
#       options  = options,
#       skip     = skip,
#       id       = id
#     )
#   }
#
#' prep.step_measure_collect <- function(x, training, info = NULL, ...) {
#'   col_names <- recipes_eval_select(x$terms, training, info)
#'   step_measure_collect_new(
#'     terms   = x$terms,
#'     role    = x$role,
#'     shape   = x$shape,
#'     trained = TRUE,
#'     options = x$options,
#'     skip    = x$skip,
#'     id      = x$id
#'   )
#' }
#'
#'
