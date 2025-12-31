# ==============================================================================
# Custom classes for measure package
#
# This file defines the S3 class infrastructure for measurement data:
# - measure_tbl: Individual measurement (tibble with location and value)
# - measure_list: Collection of measurements (list column in data frames)
#
# Using custom classes enables:
# - Robust detection via inherits() instead of column name
# - Nice printing in tibbles
# - Future support for multiple measure columns
# ==============================================================================

# ------------------------------------------------------------------------------
# measure_tbl: Individual measurement tibble
# ------------------------------------------------------------------------------

#' Create a new measure tibble
#'
#' Constructor for creating a single measurement object containing
#' location (e.g., wavelength, retention time) and value pairs.
#'
#' @param location Numeric vector of measurement locations (e.g., wavelengths,
#'   wavenumbers, retention times).
#' @param value Numeric vector of measurement values (e.g., absorbance,
#'   intensity, signal).
#'
#' @return A tibble with class `measure_tbl` containing `location` and `value`
#'   columns.
#'
#' @seealso [new_measure_list()] for creating collections of measurements,
#'   [is_measure_tbl()] for checking object class.
#'
#' @examples
#' # Create a simple spectrum
#' spec <- new_measure_tbl(
#'   location = seq(1000, 1100, by = 10),
#'   value = sin(seq(1000, 1100, by = 10) / 50)
#' )
#' spec
#'
#' @export
new_measure_tbl <- function(location = double(), value = double()) {
  if (!is.numeric(location)) {
    cli::cli_abort(
      "{.arg location} must be numeric, not {.obj_type_friendly {location}}."
    )
  }
  if (!is.numeric(value)) {
    cli::cli_abort(
      "{.arg value} must be numeric, not {.obj_type_friendly {value}}."
    )
  }
  if (length(location) != length(value)) {
    cli::cli_abort(
      "{.arg location} and {.arg value} must have the same length \\
      ({length(location)} vs {length(value)})."
    )
  }

  x <- tibble::tibble(location = location, value = value)
  class(x) <- c("measure_tbl", class(x))
  x
}

#' Coerce to measure tibble
#'
#' Converts a data frame with location and value columns to a measure_tbl.
#'
#' @param x A data frame with `location` and `value` columns.
#'
#' @return A tibble with class `measure_tbl`.
#' @noRd
as_measure_tbl <- function(x) {
  if (is_measure_tbl(x)) {
    return(x)
  }

  if (!is.data.frame(x)) {
    cli::cli_abort(
      "{.arg x} must be a data frame, not {.obj_type_friendly {x}}."
    )
  }

  required_cols <- c("location", "value")
  missing_cols <- setdiff(required_cols, names(x))

  if (length(missing_cols) > 0) {
    cli::cli_abort(
      "{.arg x} must have columns {.field {required_cols}}, \\
      missing: {.field {missing_cols}}."
    )
  }

  # Ensure it's a tibble with canonical column order (location, value)
  x <- tibble::as_tibble(x)
  x <- x[, c("location", "value")]
  class(x) <- c("measure_tbl", class(x))
  x
}

#' Test if object is a measure tibble
#'
#' @param x Object to test.
#'
#' @return Logical indicating if `x` inherits from `measure_tbl`.
#'
#' @examples
#' # Create a measure tibble
#' mt <- measure:::new_measure_tbl(location = 1:5, value = rnorm(5))
#' is_measure_tbl(mt)
#'
#' # Regular tibbles are not measure tibbles
#' is_measure_tbl(tibble::tibble(location = 1:5, value = rnorm(5)))
#'
#' @export
is_measure_tbl <- function(x) {
  inherits(x, "measure_tbl")
}

#' @rdname vctrs-methods
#' @export
print.measure_tbl <- function(x, ..., n = NULL, width = NULL) {
  cat("<measure_tbl [", nrow(x), " x ", ncol(x), "]>\n", sep = "")
  # Use tibble's print for the content
  NextMethod()
}

#' @rdname vctrs-methods
#' @exportS3Method vctrs::vec_ptype_abbr
vec_ptype_abbr.measure_tbl <- function(x, ...) {
  "msr_tbl"
}

# ------------------------------------------------------------------------------
# measure_list: List of measurements (column type)
# ------------------------------------------------------------------------------

#' Create a new measure list
#'
#' Constructor for creating a collection of measurements suitable
#' for use as a list column in a data frame. Each element should be
#' a `measure_tbl` or tibble with `location` and `value` columns.
#'
#' @param x A list of `measure_tbl` objects or tibbles with `location`
#'   and `value` columns.
#'
#' @return A list with class `measure_list`.
#'
#' @seealso [new_measure_tbl()] for creating individual measurements,
#'   [is_measure_list()] for checking object class.
#'
#' @examples
#' # Create individual spectra
#' spec1 <- new_measure_tbl(location = 1:10, value = rnorm(10))
#' spec2 <- new_measure_tbl(location = 1:10, value = rnorm(10))
#'
#' # Combine into a measure_list
#' specs <- new_measure_list(list(spec1, spec2))
#' specs
#'
#' @export
new_measure_list <- function(x = list()) {
  if (!is.list(x)) {
    cli::cli_abort("{.arg x} must be a list, not {.obj_type_friendly {x}}.")
  }

  # Coerce each element to measure_tbl if it isn't already
  x <- lapply(x, function(el) {
    if (is_measure_tbl(el)) {
      el
    } else if (is.data.frame(el)) {
      as_measure_tbl(el)
    } else {
      cli::cli_abort(
        "Each element of measure_list must be a data frame, \\
        not {.obj_type_friendly {el}}."
      )
    }
  })

  # Use vctrs for proper list-column behavior
  x <- vctrs::new_list_of(
    x,
    ptype = new_measure_tbl(),
    class = "measure_list"
  )

  x
}

#' Coerce to measure list
#'
#' Converts a plain list to a measure_list.
#'
#' @param x A list of data frames with `location` and `value` columns.
#'
#' @return A list with class `measure_list`.
#' @noRd
as_measure_list <- function(x) {
  if (is_measure_list(x)) {
    return(x)
  }
  new_measure_list(x)
}

#' Test if object is a measure list
#'
#' @param x Object to test.
#'
#' @return Logical indicating if `x` inherits from `measure_list`.
#'
#' @examples
#' # After using step_measure_input_*, the .measures column is a measure_list
#' library(recipes)
#'
#' rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   prep()
#'
#' result <- bake(rec, new_data = NULL)
#' is_measure_list(result$.measures)
#'
#' @export
is_measure_list <- function(x) {
  inherits(x, "measure_list")
}

#' @rdname vctrs-methods
#' @export
print.measure_list <- function(x, ...) {
  n <- length(x)
  cat(
    "A measure_list with",
    n,
    if (n == 1) "measurement\n" else "measurements\n"
  )

  if (n > 0) {
    sizes <- vapply(x, nrow, integer(1))
    cat("Sizes: ")
    if (n <= 6) {
      cat(paste(sizes, collapse = ", "))
    } else {
      cat(
        paste(utils::head(sizes, 3), collapse = ", "),
        ", ... ,",
        paste(utils::tail(sizes, 2), collapse = ", ")
      )
    }
    cat(" (", min(sizes), "-", max(sizes), " points)\n", sep = "")
  }

  invisible(x)
}

# vctrs methods for nice tibble printing

#' vctrs methods for measure classes
#'
#' These methods enable nice printing of measure objects in tibbles.
#'
#' @param x A measure_list or measure_tbl object.
#' @param ... Additional arguments (unused).
#'
#' @name vctrs-methods
#' @keywords internal
NULL

#' @rdname vctrs-methods
#' @exportS3Method vctrs::vec_ptype_abbr
vec_ptype_abbr.measure_list <- function(x, ...) {
  "meas"
}

#' @rdname vctrs-methods
#' @exportS3Method vctrs::vec_ptype_full
vec_ptype_full.measure_list <- function(x, ...) {
  "measure_list"
}

#' @rdname vctrs-methods
#' @export
format.measure_list <- function(x, ...) {
  if (length(x) == 0) {
    return(character())
  }
  sizes <- vapply(x, nrow, integer(1))
  paste0("<meas [", sizes, "]>")
}

#' @rdname vctrs-methods
#' @exportS3Method pillar::obj_print_data
obj_print_data.measure_list <- function(x, ...) {
  if (length(x) == 0) {
    return()
  }
  cat(format(x), sep = "\n")
  invisible(x)
}

# Subsetting should preserve class
#' @rdname vctrs-methods
#' @export
`[.measure_list` <- function(x, i, ...) {
  out <- NextMethod()
  new_measure_list(out)
}

#' @rdname vctrs-methods
#' @export
`[[.measure_list` <- function(x, i, ...) {
  # Single element extraction returns the measure_tbl
  NextMethod()
}

# Concatenation should preserve class
#' @rdname vctrs-methods
#' @export
c.measure_list <- function(...) {
  dots <- list(...)
  all_elements <- unlist(lapply(dots, unclass), recursive = FALSE)
  new_measure_list(all_elements)
}

# ------------------------------------------------------------------------------
# Discovery functions
# ------------------------------------------------------------------------------
#' Find measure columns in a data frame
#'
#' Finds all columns in a data frame that contain measurement data
#' (i.e., are of class `measure_list`).
#'
#' @param data A data frame.
#'
#' @return Character vector of column names containing measure data.
#'   Returns empty character vector if no measure columns found.
#'
#' @examples
#' library(recipes)
#'
#' rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   prep()
#'
#' result <- bake(rec, new_data = NULL)
#' find_measure_cols(result)  # ".measures"
#'
#' @export
find_measure_cols <- function(data) {
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame.")
  }

  # Note: This also detects measure_nd_list columns since they inherit
 # from measure_list. Use find_measure_nd_cols() for nD-only detection.
  is_meas <- vapply(data, is_measure_list, logical(1))
  names(data)[is_meas]
}


#' Get the dimensionality of a measure column
#'
#' Returns the number of dimensions (1 for `measure_list`, 2+ for
#' `measure_nd_list`) of a measure column in a data frame.
#'
#' @param data A data frame.
#' @param col Character string naming the measure column.
#'
#' @return Integer indicating the number of dimensions.
#'
#' @examples
#' library(recipes)
#'
#' rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   prep()
#'
#' result <- bake(rec, new_data = NULL)
#' get_measure_col_ndim(result, ".measures")  # 1
#'
#' @export
get_measure_col_ndim <- function(data, col) {
  if (!col %in% names(data)) {
    cli::cli_abort("Column {.field {col}} not found in data.")
  }

  x <- data[[col]]

  if (!is_measure_list(x)) {
    cli::cli_abort("Column {.field {col}} is not a measure column.")
  }

  # Check if it's an nD list (has measure_nd_list class)
  if (inherits(x, "measure_nd_list")) {
    if (length(x) == 0) {
      return(2L)  # Default for empty nD list
    }
    return(attr(x[[1]], "ndim") %||% 2L)
  }

  # Regular 1D measure_list
  1L
}

#' Check if data frame has measure column(s)
#'
#' Checks whether a data frame contains at least one measure column.
#' This is the recommended way to validate data in step functions.
#'
#' @param data A data frame.
#'
#' @return Invisibly returns the names of measure columns found.
#'   Throws an error if no measure columns are found.
#'
#' @examples
#' library(recipes)
#'
#' rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   prep()
#'
#' result <- bake(rec, new_data = NULL)
#' has_measure_col(result)  # TRUE (returns invisibly)
#'
#' @export
has_measure_col <- function(data) {
  # First check for measure_list class (preferred)
  meas_cols <- find_measure_cols(data)
  if (length(meas_cols) > 0) {
    return(invisible(meas_cols))
  }

  # Fallback: check for .measures column name (backwards compatibility)
  if (".measures" %in% names(data)) {
    return(invisible(".measures"))
  }

  cli::cli_abort(c(
    "No measure column found in the data.",
    "i" = "Use {.fn step_measure_input_wide} or {.fn step_measure_input_long} first."
  ))
}
