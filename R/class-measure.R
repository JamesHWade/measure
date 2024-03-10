#' @import vctrs
#' @importFrom utils globalVariables
#' @importFrom rlang abort

# ------------------------------------------------------------------------------
# Creation

new_measure <- function(x = numeric(), location = numeric(), ..., subclass = NULL) {
  if (!is.numeric(x)) {
    abort("`x` must be a numeric vector.")
  }
  if (!is.numeric(location)) {
    abort("`location` must be a numeric vector.")
  }
  if (length(x) != length(location)) {
    abort("The lengths of `x` and `location` must be the same.")
  }

  new_rcrd(
    fields = purrr::compact(
      list(
        x = x,
        location = location
      )
    ),
    ...,
    class = c(subclass, "measure")
  )

}

measure <- function(x = numeric(), location = numeric()) {
  new_measure(x, location)
}

is_measure <- function(x) {
  inherits(x, "measure")
}

# ------------------------------------------------------------------------------
# Printing

#'@export
format.measure <- function(x) {
  format(field(x, "x"), trim = TRUE, digits = 2)
  # format(field(x, "location"), trim = TRUE)
}

#' @export
vec_ptype_abbr.measure <- function(x, ...) {
  "meas"
}

# ------------------------------------------------------------------------------
# Coercion

#' @export
as_measure <- function(x) {
  UseMethod("as_measure")
}

#' @export
as_measure.default <- function(x) {
  abort_default(x, "as_measure")
}

#' @export
as_measure.data.frame <- function(x) {
  if (ncol(x) != 2) {
    abort("A data frame must have exactly 2 columns to be converted to a measure.")
  }

  new_measure(x[[1]], x[[2]])
}

#' @export
as_measure.matrix <- function(x) {
  if (ncol(x) != 2) {
    abort("A matrix must have exactly 2 columns to be converted to a measure.")
  }

  new_measure(x[, 1], x[, 2])
}

# ------------------------------------------------------------------------------
# Casting

#' @export
vec_cast.measure.measure <- function(x, to, ...) x

#' @export
vec_cast.measure.data.frame <- function(x, to, ...) as_measure(x)

#' @export
vec_cast.measure.matrix <- function(x, to, ...) as_measure(x)

#' @export
vec_cast.data.frame.measure <- function(x, to, ...) {
  data.frame(x = field(x, "x"), location = field(x, "location"))
}

#' @export
vec_cast.matrix.measure <- function(x, to, ...) {
  matrix(c(field(x, "x"), field(x, "location")), ncol = 2)
}

# ------------------------------------------------------------------------------
# Equality and comparison

#' @export
vec_proxy_equal.measure <- function(x, ...) {
  data.frame(x = field(x, "x"), location = field(x, "location"))
}

# ------------------------------------------------------------------------------
# Arithmetic

#' @export
vec_arith.measure <- function(op, x, y, ...) {
  UseMethod("vec_arith.measure", y)
}

#' @export
vec_arith.measure.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}

#' @export
vec_arith.measure.numeric <- function(op, x, y, ...) {
  switch(
    op,
    "+" = ,
    "-" = new_measure(vec_arith_base(op, x$x, y), x$location),
    "/" = ,
    "*" = new_measure(vec_arith_base(op, x$x, y), x$location),
    stop_incompatible_op(op, x, y)
  )
}

#' @export
vec_arith.numeric.measure <- function(op, x, y, ...) {
  switch(
    op,
    "+" = ,
    "-" = ,
    "/" = new_measure(vec_arith_base(op, x, y$x), y$location),
    "*" = new_measure(vec_arith_base(op, x, y$x), y$location),
    stop_incompatible_op(op, x, y)
  )
}

#' @export
vec_arith.measure.measure <- function(op, x, y, ...) {
  switch(
    op,
    "+" = ,
    "-" = new_measure(vec_arith_base(op, x$x, y$x), x$location),
    "/" = vec_arith_base(op, x$x, y$x),
    "*" = stop_incompatible_op(op, x, y),
    stop_incompatible_op(op, x, y)
  )
}

#' @export
vec_arith.measure.MISSING <- function(op, x, y, ...) {
  switch(op,
         `-` = new_measure(-x$x, x$location),
         `+` = x,
         stop_incompatible_op(op, x, y)
  )
}

# ------------------------------------------------------------------------------
# Extract underlying data

#' @export
vec_data.measure <- function(x) {
  data.frame(x = field(x, "x"), location = field(x, "location"), stringsAsFactors = FALSE)
}
