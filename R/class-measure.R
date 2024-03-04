# ------------------------------------------------------------------------------
# Creation

new_measure <- function(x, labels, ..., subclass = NULL) {
  new_vctr(
    .data = x,
    ...,
    class = c(subclass, "measure")
  )
}


measure <- function() {
  which <- vec_cast(which, integer())

  # no duplicates allowed
  which <- unique(which)

  # which cannot go outside the range of the number of values in x
  if (length(which) > 0L && max(which) > length(x)) {
    msg <- paste0("The largest value of `which` can be ", length(x), ".")
    abort(msg)
  }

  labs <- levels(x)

  # Check for `equivocal` in labels. Not allowed.
  if (equivocal %in% labs) {
    msg <- paste0(
      "`\"", equivocal, "\"`",
      "is reserved for equivocal values",
      "and must not already be a level."
    )
    abort(msg)
  }

  # rip out the underlying integer structure
  # as.integer() also removes attributes
  x_int <- as.integer(unclass(x))

  # declare equivocal
  x_int[which] <- 0L

  new_class_pred(
    x = x_int,
    labels = labs,
    ordered = is.ordered(x),
    equivocal = equivocal
  )
}

# ------------------------------------------------------------------------------
# Printing

# Always return a character vector
# Rely on as.character.factor() for NA handling
# Used by data.frame() columns and general printing
#' @export
format.measure <- function(x, ...) {

}

# -------------------------------------------------------------------------
# Check that measures are the `measure_obj` class

is_measure <- function(x) {
  inherits(x, "measure_obj")
}

# ------------------------------------------------------------------------------
# Coercion

#' Coerce to a `measure` object
#'
#' `as_measure()` provides coercion to `measure` from other
#' existing objects.
#'
#' @examples
#'
#'
#' @export
as_measure <- function(x) {
  UseMethod("as_measure")
}

#' @export
as_measure.default <- function(x) {
  abort_default(x, "as_class_pred")
}
