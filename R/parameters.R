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
