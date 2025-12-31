# ==============================================================================
# N-Dimensional Measurement Classes
#
# This file defines the S3 class infrastructure for n-dimensional measurement
# data:
# - measure_nd_tbl: Individual nD measurement (tibble with location_1..n, value)
# - measure_nd_list: Collection of nD measurements (list column in data frames)
#
# These extend the 1D classes (measure_tbl, measure_list) to support
# multi-dimensional analytical data like LC-DAD, 2D NMR, EEM fluorescence, etc.
# ==============================================================================

# ------------------------------------------------------------------------------
# measure_nd_tbl: Individual n-dimensional measurement tibble
# ------------------------------------------------------------------------------

#' Create a new n-dimensional measure tibble
#'
#' Constructor for creating a single n-dimensional measurement object
#' containing location coordinates (e.g., wavelength, retention time)
#' and values.
#'
#' @param ... Named location vectors. Names should follow the pattern
#'   `location_1`, `location_2`, etc. Each must be a numeric vector
#'   of the same length.
#' @param value Numeric vector of measurement values (e.g., absorbance,
#'   intensity, signal). Must have the same length as location vectors.
#' @param dim_names Optional character vector of semantic dimension names
#'   (e.g., `c("wavelength", "retention_time")`).
#' @param dim_units Optional character vector of dimension units
#'   (e.g., `c("nm", "min")`).
#'
#' @return A tibble with class `measure_nd_tbl` containing `location_1`,
#'   `location_2`, ..., `location_n`, and `value` columns. Attributes
#'   include `ndim`, `dim_names`, `dim_units`, and `dim_order`.
#'
#' @seealso [new_measure_nd_list()] for creating collections of nD measurements,
#'   [is_measure_nd_tbl()] for checking object class, [measure_ndim()] for
#'   getting dimensionality.
#'
#' @examples
#' # Create a 2D measurement (e.g., LC-UV: retention time x wavelength)
#' meas_2d <- new_measure_nd_tbl(
#'   location_1 = rep(seq(0, 10, length.out = 5), each = 3),
#'   location_2 = rep(c(254, 280, 320), times = 5),
#'   value = rnorm(15),
#'   dim_names = c("retention_time", "wavelength"),
#'   dim_units = c("min", "nm")
#' )
#' meas_2d
#'
#' @export
new_measure_nd_tbl <- function(
  ...,
  value = double(),
  dim_names = NULL,
  dim_units = NULL
) {
  dots <- rlang::list2(...)

  # Extract location vectors from dots
  loc_names <- names(dots)
  if (is.null(loc_names) || any(loc_names == "")) {
    cli::cli_abort(
      "All location arguments must be named (e.g., {.code location_1 = ...})."
    )
  }

  # Validate location naming pattern
  loc_pattern <- "^location_[0-9]+$"
  invalid_names <- loc_names[!grepl(loc_pattern, loc_names)]
  if (length(invalid_names) > 0) {
    cli::cli_abort(
      "Location arguments must be named {.code location_1}, {.code location_2}, etc.
       Invalid names: {.val {invalid_names}}."
    )
  }

  # Determine ndim and validate
  ndim <- length(dots)
  if (ndim < 2) {
    cli::cli_abort(
      "At least 2 location dimensions are required for {.fn new_measure_nd_tbl}.
       Use {.fn new_measure_tbl} for 1D data."
    )
  }

  # Validate all location vectors are numeric
  for (nm in loc_names) {
    if (!is.numeric(dots[[nm]])) {
      cli::cli_abort(
        "{.arg {nm}} must be numeric, not {.obj_type_friendly {dots[[nm]]}}."
      )
    }
  }

  # Validate value is numeric
  if (!is.numeric(value)) {
    cli::cli_abort(
      "{.arg value} must be numeric, not {.obj_type_friendly {value}}."
    )
  }

  # Validate all vectors have the same length
  lengths <- c(lengths(dots), length(value))
  if (length(unique(lengths)) != 1) {
    len_str <- paste(c(loc_names, "value"), "=", lengths, collapse = ", ")
    cli::cli_abort(
      "All location and value vectors must have the same length.
       Got: {len_str}."
    )
  }

  # Sort location columns by dimension number for consistent ordering
  dim_nums <- as.integer(sub("location_", "", loc_names, fixed = TRUE))
  sorted_idx <- order(dim_nums)
  dots <- dots[sorted_idx]
  loc_names <- loc_names[sorted_idx]

  # Validate dim_names length
  if (!is.null(dim_names) && length(dim_names) != ndim) {
    cli::cli_abort(
      "{.arg dim_names} must have length {ndim} (one per dimension), not {length(dim_names)}."
    )
  }

  # Validate dim_units length
  if (!is.null(dim_units) && length(dim_units) != ndim) {
    cli::cli_abort(
      "{.arg dim_units} must have length {ndim} (one per dimension), not {length(dim_units)}."
    )
  }

  # Build the tibble
  data <- c(dots, list(value = value))
  x <- tibble::as_tibble(data)

  # Add class and attributes
  class(x) <- c("measure_nd_tbl", "measure_tbl", class(x))
  attr(x, "ndim") <- ndim
  attr(x, "dim_names") <- dim_names
  attr(x, "dim_units") <- dim_units
  attr(x, "dim_order") <- seq_len(ndim)

  x
}


#' Coerce to n-dimensional measure tibble
#'
#' Converts a data frame with location_* and value columns to a measure_nd_tbl.
#'
#' @param x A data frame with `location_1`, `location_2`, ..., and `value`
#'   columns.
#' @param dim_names Optional character vector of semantic dimension names.
#' @param dim_units Optional character vector of dimension units.
#'
#' @return A tibble with class `measure_nd_tbl`.
#' @noRd
as_measure_nd_tbl <- function(x, dim_names = NULL, dim_units = NULL) {
  if (is_measure_nd_tbl(x)) {
    return(x)
  }

  if (!is.data.frame(x)) {
    cli::cli_abort(
      "{.arg x} must be a data frame, not {.obj_type_friendly {x}}."
    )
  }

  # Find location columns
  loc_cols <- grep("^location_[0-9]+$", names(x), value = TRUE)
  if (length(loc_cols) < 2) {
    cli::cli_abort(
      "{.arg x} must have at least 2 location columns ({.code location_1}, {.code location_2}, ...).
       Found: {.val {loc_cols}}."
    )
  }

  if (!"value" %in% names(x)) {
    cli::cli_abort("{.arg x} must have a {.code value} column.")
  }

  # Extract data in correct order
  dim_nums <- as.integer(sub("location_", "", loc_cols, fixed = TRUE))
  sorted_idx <- order(dim_nums)
  loc_cols <- loc_cols[sorted_idx]

  # Build args for new_measure_nd_tbl
  args <- stats::setNames(lapply(loc_cols, function(col) x[[col]]), loc_cols)
  args$value <- x[["value"]]
  args$dim_names <- dim_names %||% attr(x, "dim_names")
  args$dim_units <- dim_units %||% attr(x, "dim_units")

  do.call(new_measure_nd_tbl, args)
}


#' Test if object is an n-dimensional measure tibble
#'
#' @param x Object to test.
#'
#' @return Logical indicating if `x` inherits from `measure_nd_tbl`.
#'
#' @examples
#' # Create a 2D measure tibble
#' mt <- new_measure_nd_tbl(
#'   location_1 = 1:10,
#'   location_2 = rep(1:2, each = 5),
#'   value = rnorm(10)
#' )
#' is_measure_nd_tbl(mt)  # TRUE
#'
#' # Regular tibbles are not measure_nd_tbl
#' is_measure_nd_tbl(tibble::tibble(x = 1:5))  # FALSE
#'
#' @export
is_measure_nd_tbl <- function(x) {
  inherits(x, "measure_nd_tbl")
}


#' @rdname vctrs-methods
#' @export
print.measure_nd_tbl <- function(x, ..., n = NULL, width = NULL) {
  ndim <- attr(x, "ndim") %||% 2L
  dim_names <- attr(x, "dim_names")

  if (!is.null(dim_names)) {
    dim_str <- paste(dim_names, collapse = " x ")
  } else {
    dim_str <- paste0(ndim, "D")
  }

  cat(
    "<measure_nd_tbl [",
    nrow(x),
    " x ",
    ncol(x),
    "] ",
    dim_str,
    ">\n",
    sep = ""
  )
  # Use tibble's print for the content
  NextMethod()
}


#' @rdname vctrs-methods
#' @exportS3Method vctrs::vec_ptype_abbr
vec_ptype_abbr.measure_nd_tbl <- function(x, ...) {
  ndim <- attr(x, "ndim") %||% 2L
  paste0("msr", ndim, "d")
}


# ------------------------------------------------------------------------------
# measure_nd_list: List of n-dimensional measurements (column type)
# ------------------------------------------------------------------------------

#' Create a new n-dimensional measure list
#'
#' Constructor for creating a collection of n-dimensional measurements
#' suitable for use as a list column in a data frame. Each element should be
#' a `measure_nd_tbl` or tibble with `location_*` and `value` columns.
#'
#' @param x A list of `measure_nd_tbl` objects or tibbles with `location_*`
#'   and `value` columns.
#'
#' @return A list with class `measure_nd_list`.
#'
#' @seealso [new_measure_nd_tbl()] for creating individual nD measurements,
#'   [is_measure_nd_list()] for checking object class.
#'
#' @examples
#' # Create individual 2D measurements
#' meas1 <- new_measure_nd_tbl(
#'   location_1 = rep(1:5, each = 3),
#'   location_2 = rep(1:3, times = 5),
#'   value = rnorm(15)
#' )
#' meas2 <- new_measure_nd_tbl(
#'   location_1 = rep(1:5, each = 3),
#'   location_2 = rep(1:3, times = 5),
#'   value = rnorm(15)
#' )
#'
#' # Combine into a measure_nd_list
#' meas_list <- new_measure_nd_list(list(meas1, meas2))
#' meas_list
#'
#' @export
new_measure_nd_list <- function(x = list()) {
  if (!is.list(x)) {
    cli::cli_abort("{.arg x} must be a list, not {.obj_type_friendly {x}}.")
  }

  if (length(x) == 0) {
    # Empty list - create with default 2D prototype
    return(vctrs::new_list_of(
      list(),
      ptype = new_measure_nd_tbl(
        location_1 = double(),
        location_2 = double(),
        value = double()
      ),
      class = c("measure_nd_list", "measure_list")
    ))
  }

  # Coerce each element to measure_nd_tbl if it isn't already
  x <- lapply(x, function(el) {
    if (is_measure_nd_tbl(el)) {
      el
    } else if (is.data.frame(el)) {
      as_measure_nd_tbl(el)
    } else {
      cli::cli_abort(
        "Each element of measure_nd_list must be a data frame,
        not {.obj_type_friendly {el}}."
      )
    }
  })

  # Validate all elements have the same ndim
  ndims <- vapply(x, function(el) attr(el, "ndim") %||% 2L, integer(1))
  if (length(unique(ndims)) > 1) {
    cli::cli_abort(
      "All elements of measure_nd_list must have the same dimensionality.
       Found: {.val {unique(ndims)}}."
    )
  }

  # Use the first element as prototype for vctrs
  ptype <- new_measure_nd_tbl(
    location_1 = double(),
    location_2 = double(),
    value = double()
  )
  # Copy ndim attribute to match elements
  attr(ptype, "ndim") <- ndims[1]

  # Add location columns for higher dimensions if needed
  if (ndims[1] > 2) {
    for (i in 3:ndims[1]) {
      ptype[[paste0("location_", i)]] <- double()
    }
    # Reorder columns: location_1, location_2, ..., location_n, value
    loc_cols <- paste0("location_", seq_len(ndims[1]))
    ptype <- ptype[c(loc_cols, "value")]
  }

  # Use vctrs for proper list-column behavior
  x <- vctrs::new_list_of(
    x,
    ptype = ptype,
    class = c("measure_nd_list", "measure_list")
  )

  x
}


#' Coerce to n-dimensional measure list
#'
#' Converts a plain list to a measure_nd_list.
#'
#' @param x A list of data frames with `location_*` and `value` columns.
#'
#' @return A list with class `measure_nd_list`.
#' @noRd
as_measure_nd_list <- function(x) {
  if (is_measure_nd_list(x)) {
    return(x)
  }
  new_measure_nd_list(x)
}


#' Test if object is an n-dimensional measure list
#'
#' @param x Object to test.
#'
#' @return Logical indicating if `x` inherits from `measure_nd_list`.
#'
#' @examples
#' # Create and test a measure_nd_list
#' meas1 <- new_measure_nd_tbl(
#'   location_1 = 1:5,
#'   location_2 = rep(1, 5),
#'   value = rnorm(5)
#' )
#' ml <- new_measure_nd_list(list(meas1))
#' is_measure_nd_list(ml)  # TRUE
#'
#' @export
is_measure_nd_list <- function(x) {
  inherits(x, "measure_nd_list")
}


#' @rdname vctrs-methods
#' @export
print.measure_nd_list <- function(x, ...) {
  n <- length(x)
  ndim <- if (n > 0) attr(x[[1]], "ndim") %||% 2L else 2L

  cat(
    "A measure_nd_list with",
    n,
    if (n == 1) "measurement" else "measurements",
    paste0("(", ndim, "D)\n")
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


#' @rdname vctrs-methods
#' @exportS3Method vctrs::vec_ptype_abbr
vec_ptype_abbr.measure_nd_list <- function(x, ...) {
  if (length(x) > 0) {
    ndim <- attr(x[[1]], "ndim") %||% 2L
    paste0("meas", ndim, "d")
  } else {
    "measnd"
  }
}


#' @rdname vctrs-methods
#' @exportS3Method vctrs::vec_ptype_full
vec_ptype_full.measure_nd_list <- function(x, ...) {
  "measure_nd_list"
}


#' @rdname vctrs-methods
#' @export
format.measure_nd_list <- function(x, ...) {
  if (length(x) == 0) {
    return(character())
  }
  sizes <- vapply(x, nrow, integer(1))
  ndim <- if (length(x) > 0) attr(x[[1]], "ndim") %||% 2L else 2L
  paste0("<meas", ndim, "d [", sizes, "]>")
}


#' @rdname vctrs-methods
#' @exportS3Method pillar::obj_print_data
obj_print_data.measure_nd_list <- function(x, ...) {
  if (length(x) == 0) {
    return()
  }
  cat(format(x), sep = "\n")
  invisible(x)
}


# Subsetting should preserve class
#' @rdname vctrs-methods
#' @export
`[.measure_nd_list` <- function(x, i, ...) {
  out <- NextMethod()
  new_measure_nd_list(out)
}


#' @rdname vctrs-methods
#' @export
`[[.measure_nd_list` <- function(x, i, ...) {
  # Single element extraction returns the measure_nd_tbl
  NextMethod()
}


# Concatenation should preserve class
#' @rdname vctrs-methods
#' @export
c.measure_nd_list <- function(...) {
  dots <- list(...)
  all_elements <- unlist(lapply(dots, unclass), recursive = FALSE)
  new_measure_nd_list(all_elements)
}


# ------------------------------------------------------------------------------
# Dimension metadata accessors
# ------------------------------------------------------------------------------

#' Get the number of dimensions of a measurement
#'
#' Returns the dimensionality of a measurement object. For 1D measurements
#' (`measure_tbl`), returns 1. For n-dimensional measurements
#' (`measure_nd_tbl`), returns the number of location dimensions.
#'
#' @param x A `measure_tbl`, `measure_nd_tbl`, `measure_list`, or
#'   `measure_nd_list` object.
#'
#' @return Integer indicating the number of dimensions.
#'
#' @examples
#' # 1D measurement
#' m1d <- new_measure_tbl(location = 1:10, value = rnorm(10))
#' measure_ndim(m1d)  # 1
#'
#' # 2D measurement
#' m2d <- new_measure_nd_tbl(
#'   location_1 = rep(1:5, each = 3),
#'   location_2 = rep(1:3, times = 5),
#'   value = rnorm(15)
#' )
#' measure_ndim(m2d)  # 2
#'
#' @export
measure_ndim <- function(x) {
  UseMethod("measure_ndim")
}


#' @export
measure_ndim.measure_nd_tbl <- function(x) {
  attr(x, "ndim") %||% 2L
}


#' @export
measure_ndim.measure_nd_list <- function(x) {
  if (length(x) == 0) {
    return(2L) # Default for empty list
  }
  attr(x[[1]], "ndim") %||% 2L
}


#' @export
measure_ndim.measure_tbl <- function(x) {
  1L
}


#' @export
measure_ndim.measure_list <- function(x) {
  # Check if it's actually an nD list
  if (is_measure_nd_list(x)) {
    return(measure_ndim.measure_nd_list(x))
  }
  1L
}


#' @export
measure_ndim.default <- function(x) {
  cli::cli_abort(
    "{.arg x} must be a measure object, not {.obj_type_friendly {x}}."
  )
}


#' Get dimension names of an n-dimensional measurement
#'
#' Returns the semantic names for each dimension (e.g., "wavelength",
#' "retention_time").
#'
#' @param x A `measure_nd_tbl` or `measure_nd_list` object.
#'
#' @return Character vector of dimension names, or `NULL` if not set.
#'
#' @examples
#' m2d <- new_measure_nd_tbl(
#'   location_1 = 1:10,
#'   location_2 = rep(1:2, each = 5),
#'   value = rnorm(10),
#'   dim_names = c("retention_time", "wavelength")
#' )
#' measure_dim_names(m2d)
#'
#' @export
measure_dim_names <- function(x) {
  UseMethod("measure_dim_names")
}


#' @export
measure_dim_names.measure_nd_tbl <- function(x) {
  attr(x, "dim_names")
}


#' @export
measure_dim_names.measure_nd_list <- function(x) {
  if (length(x) == 0) {
    return(NULL)
  }
  attr(x[[1]], "dim_names")
}


#' @export
measure_dim_names.default <- function(x) {
  NULL
}


#' Get dimension units of an n-dimensional measurement
#'
#' Returns the units for each dimension (e.g., "nm", "min").
#'
#' @param x A `measure_nd_tbl` or `measure_nd_list` object.
#'
#' @return Character vector of dimension units, or `NULL` if not set.
#'
#' @examples
#' m2d <- new_measure_nd_tbl(
#'   location_1 = 1:10,
#'   location_2 = rep(1:2, each = 5),
#'   value = rnorm(10),
#'   dim_units = c("min", "nm")
#' )
#' measure_dim_units(m2d)
#'
#' @export
measure_dim_units <- function(x) {
  UseMethod("measure_dim_units")
}


#' @export
measure_dim_units.measure_nd_tbl <- function(x) {
  attr(x, "dim_units")
}


#' @export
measure_dim_units.measure_nd_list <- function(x) {
  if (length(x) == 0) {
    return(NULL)
  }
  attr(x[[1]], "dim_units")
}


#' @export
measure_dim_units.default <- function(x) {
  NULL
}
