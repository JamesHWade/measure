#' Ingest Measurements from a Single Column
#'
#' `step_measure_input_long` creates a *specification* of a recipe
#'  step that converts measures organized in a column for the analytical results
#'  (and one or more columns of numeric indices) into an internal format used by
#'  the package.
#'
#' @family input/output steps
#' @inheritParams recipes::step_center
#' @param ... One or more selector functions to choose which _single_ column
#'   contains the analytical measurements. The selection should be in the order
#'   of the measurement's profile.
#' @param location One or more selector functions to choose which column(s)
#'   have the locations of the analytical values. For 1D data (spectra,
#'   chromatograms), select a single location column. For 2D or higher
#'   dimensional data (LC-DAD, 2D NMR, EEM), select multiple location columns.
#'   Columns will be renamed to `location_1`, `location_2`, etc. in order.
#' @param col_name A single character string specifying the name of the output
#'   column that will contain the measure data. Defaults to `".measures"`. Use
#'   different names when creating multiple measure columns (e.g., `".uv_spectrum"`
#'   and `".ms_spectrum"`).
#' @param dim_names Optional character vector of semantic names for each
#'   dimension (e.g., `c("retention_time", "wavelength")`). Only used for
#'   multi-dimensional data.
#' @param dim_units Optional character vector of units for each dimension
#'   (e.g., `c("min", "nm")`). Only used for multi-dimensional data.
#' @param pad Whether to pad the measurements to ensure that they all have the
#'   same number of values. This is useful when there are missing values in the
#'   measurements.
#' @param columns A character vector of column names determined by the recipe.
#'
#' @details
#' This step is designed for data in a format where there is a column
#' for the analytical measurement (e.g., absorption, etc.) and one or more
#' columns with the location of the value (e.g., wave number, retention time,
#' wavelength, etc.).
#'
#' `step_measure_input_long()` will collect those data and put them into a
#' format used internally by this package. The data structure has a row for
#' each independent experimental unit and a nested tibble with that sample's
#' measure (measurement and location). It assumes that there are unique
#' combinations of the other columns in the data that define individual
#' patterns associated with the pattern. If this is not the case, the special
#' values might be inappropriately restructured.
#'
#' The best advice is to have a column of any type that indicates the unique
#' sample number for each measure. For example, if there are 200 values in the
#' measure and 7 samples, the input data (in long format) should have 1,400
#' rows. We advise having a column with 7 unique values indicating which of the
#' rows correspond to each sample.
#'
#' # Multi-Dimensional Data
#'
#' For 2D or higher dimensional data, provide multiple location columns:
#'
#' ```r
#' # LC-DAD data with retention time and wavelength
#' step_measure_input_long(
#'   absorbance,
#'   location = vars(retention_time, wavelength),
#'   dim_names = c("time", "wavelength"),
#'   dim_units = c("min", "nm")
#' )
#' ```
#'
#' The result will be a `measure_nd_list` column instead of a `measure_list`.
#'
#' # Missing Data
#'
#' Currently, \pkg{measure} assumes that there are equal numbers of values
#' within a sample. If there are missing values in the measurements, you'll
#' need to pad them with missing values (as opposed to an absent row in the
#' long format). If not, an error will occur.
#'
#' # Tidying
#'
#' When you [`tidy()`][recipes::tidy.recipe()] this step, a tibble indicating which of
#' the original columns were used to reformat the data.
#'
#' @examples
#' library(recipes)
#'
#' # 1D data (traditional usage)
#' rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   prep()
#'
#' bake(rec, new_data = NULL)
#'
#' @export
step_measure_input_long <-
  function(
    recipe,
    ...,
    location,
    col_name = ".measures",
    dim_names = NULL,
    dim_units = NULL,
    pad = FALSE,
    role = "measure",
    trained = FALSE,
    columns = NULL,
    skip = FALSE,
    id = rand_id("measure_input_long")
  ) {
    if (rlang::is_missing(location)) {
      location <- NULL
    }
    add_step(
      recipe,
      step_measure_input_long_new(
        terms = enquos(...),
        trained = trained,
        role = role,
        columns = columns,
        location = location,
        col_name = col_name,
        dim_names = dim_names,
        dim_units = dim_units,
        pad = pad,
        skip = skip,
        id = id
      )
    )
  }

step_measure_input_long_new <-
  function(
    terms,
    role,
    trained,
    columns,
    location,
    col_name,
    dim_names,
    dim_units,
    pad,
    skip,
    id
  ) {
    step(
      subclass = "measure_input_long",
      terms = terms,
      role = role,
      trained = trained,
      columns = columns,
      location = location,
      col_name = col_name,
      dim_names = dim_names,
      dim_units = dim_units,
      pad = pad,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_measure_input_long <- function(x, training, info = NULL, ...) {
  value_name <- recipes_eval_select(x$terms, training, info)
  check_single_selector(value_name, "...")
  check_type(training[, value_name], types = c("double", "integer"))

  if (length(x$location) > 0) {
    loc_names <- recipes_eval_select(x$location, training, info)
    # Allow multiple location columns for nD data
    if (length(loc_names) == 0) {
      rlang::abort("'location' is required for long input data")
    }
    # Check that value and location columns don't overlap
    overlap <- intersect(value_name, loc_names)
    if (length(overlap) > 0) {
      cli::cli_abort(
        c(
          "Value and location columns must not overlap.",
          "x" = "Column{?s} {.field {overlap}} selected for both."
        )
      )
    }
    for (loc_col in loc_names) {
      check_type(training[, loc_col], types = c("double", "integer"))
    }
  } else {
    rlang::abort("'location' is required for long input data")
  }

  # Validate dim_names and dim_units lengths if provided
  ndim <- length(loc_names)
  if (!is.null(x$dim_names) && length(x$dim_names) != ndim) {
    cli::cli_abort(
      "{.arg dim_names} must have length {ndim} (one per location column),
       not {length(x$dim_names)}."
    )
  }
  if (!is.null(x$dim_units) && length(x$dim_units) != ndim) {
    cli::cli_abort(
      "{.arg dim_units} must have length {ndim} (one per location column),
       not {length(x$dim_units)}."
    )
  }

  # Store value name as first column, then location names
  step_measure_input_long_new(
    terms = x$terms,
    role = "measure",
    trained = TRUE,
    columns = unname(c(value_name, loc_names)),
    location = x$location,
    col_name = x$col_name,
    dim_names = x$dim_names,
    dim_units = x$dim_units,
    pad = x$pad,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_input_long <- function(object, new_data, ...) {
  col_name <- object$col_name
  value_col <- object$columns[1]
  loc_cols <- object$columns[-1]
  ndim <- length(loc_cols)

  # Rename columns to canonical names
  new_data <- rename_long_cols_nd(new_data, value_col, loc_cols)

  # Get list of columns to exclude from grouping (value + all locations)
  if (ndim == 1) {
    exclude_cols <- c("value", "location")
  } else {
    exclude_cols <- c("value", paste0("location_", seq_len(ndim)))
  }

  # Nest by all other columns
  new_data <- new_data |>
    tidyr::nest(.by = -dplyr::all_of(exclude_cols), .key = ".measures_temp")

  # Rename to the user-specified column name
  if (col_name != ".measures_temp") {
    names(new_data)[names(new_data) == ".measures_temp"] <- col_name
  }

  if (rlang::is_true(object$pad)) {
    if (ndim == 1) {
      new_data <- pad_measure_dims(new_data, col = col_name)
    } else {
      # For nD, we don't enforce equal dimensions by default (flexible grids)
      # Padding would be more complex - skip for now
      cli::cli_warn(
        "Padding is not yet implemented for multi-dimensional data.
         Consider using {.fn step_measure_regularize} for grid alignment."
      )
    }
  }

  # Dimension validation
  if (ndim == 1) {
    check_measure_dims(new_data, col = col_name)
    # Wrap as measure_list for 1D data
    new_data[[col_name]] <- new_measure_list(new_data[[col_name]])
  } else {
    # For nD data, create measure_nd_list with metadata
    new_data[[col_name]] <- wrap_as_measure_nd_list(
      new_data[[col_name]],
      ndim = ndim,
      dim_names = object$dim_names,
      dim_units = object$dim_units
    )
  }

  new_data
}

#' @export
print.step_measure_input_long <-
  function(x, width = max(20, options()$width - 30), ...) {
    ndim <- length(x$columns) - 1
    if (ndim > 1) {
      title <- paste0("Collate long analytical measurements (", ndim, "D)")
    } else {
      title <- "Collate long analytical measurements"
    }
    print_step(x$columns, x$terms, x$trained, title, width)
    invisible(x)
  }

#' Tidiers for measure steps
#' @param x A recipe step.
#' @rdname tidy.recipe
#' @export
tidy.step_measure_input_long <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(
      terms = x$columns[!is.na(x$columns)],
      value = na_dbl
    )
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(
      terms = term_names,
      value = na_dbl
    )
  }
  res$id <- x$id
  res
}


# ------------------------------------------------------------------------------
# Internal helpers
# ------------------------------------------------------------------------------

#' Rename columns to canonical names (location or location_1, location_2, etc.)
#' @noRd
rename_long_cols_nd <- function(.data, value_col, loc_cols) {
  ndim <- length(loc_cols)

  if (ndim == 1) {
    # 1D: keep original behavior for backward compatibility
    if (!is.na(loc_cols[1])) {
      rename_map <- c(value = value_col, location = loc_cols[1])
    } else {
      rename_map <- c(value = value_col)
    }
  } else {
    # nD: rename to location_1, location_2, etc.
    loc_new_names <- paste0("location_", seq_len(ndim))
    rename_map <- stats::setNames(c(value_col, loc_cols), c("value", loc_new_names))
  }

  dplyr::rename(.data, dplyr::all_of(rename_map))
}


#' Wrap nested data as measure_nd_list with metadata
#' @noRd
wrap_as_measure_nd_list <- function(nested_data, ndim, dim_names, dim_units) {
  # Each element of nested_data is a tibble with location_1, location_2, ..., value
  result <- lapply(nested_data, function(tbl) {
    # Build the args for new_measure_nd_tbl
    loc_cols <- paste0("location_", seq_len(ndim))
    args <- stats::setNames(lapply(loc_cols, function(col) tbl[[col]]), loc_cols)
    args$value <- tbl[["value"]]
    args$dim_names <- dim_names
    args$dim_units <- dim_units

    do.call(new_measure_nd_tbl, args)
  })

  new_measure_nd_list(result)
}


# Keep original helper for backward compatibility
rename_long_cols <- function(.data, val_chr, loc_chr) {
  if (!is.na(loc_chr)) {
    res <- c(value = val_chr, location = loc_chr)
  } else {
    res <- c(value = unname(val_chr))
  }
  dplyr::rename(.data, dplyr::all_of(res))
}
