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
        group_cols = NULL,
        outcome_cols = NULL,
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
    group_cols,
    outcome_cols,
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
      group_cols = group_cols,
      outcome_cols = outcome_cols,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_measure_input_long <- function(x, training, info = NULL, ...) {
  value_name <- recipes_eval_select(x$terms, training, info)
  check_single_selector(value_name, "...")

  # Type checking: accept double/integer or list columns containing numerics
  # (list columns occur when a previous input step has already run)
  check_type_or_list_numeric(training[[value_name]], value_name)

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
      check_type_or_list_numeric(training[[loc_col]], loc_col)
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

  # Identify columns to group by for nesting

  # Strategy:
  # 1. If explicit ID columns (role = "id") or measure columns from previous
  #    input steps (role = "measure") exist, use those for grouping
  # 2. Otherwise, fall back to original behavior: exclude only value/location
  #
  # This allows multiple step_measure_input_long calls to work correctly
  # when ID columns are properly defined, while maintaining backward
  # compatibility with existing recipes that don't set ID roles
  consumed_cols <- c(value_name, loc_names)

  if (!is.null(info)) {
    id_cols <- info$variable[info$role == "id" & !is.na(info$role)]
    # Also include columns with role "measure" from previous input steps
    measure_cols <- info$variable[info$role == "measure" & !is.na(info$role)]

    if (length(id_cols) > 0 || length(measure_cols) > 0) {
      # Use explicit ID/measure columns
      group_cols <- c(id_cols, measure_cols)
    } else {
      # Fallback: group by all columns except value and location
      # This is the original behavior for backward compatibility
      group_cols <- setdiff(names(training), consumed_cols)
    }
  } else {
    # No info available - use original behavior
    group_cols <- setdiff(names(training), consumed_cols)
  }
  # Filter to only columns actually present in the training data
  group_cols <- intersect(group_cols, names(training))

  # Also identify outcome columns - these should be kept as scalar values,
  # not converted to list columns
  outcome_cols <- character(0)
  if (!is.null(info)) {
    outcome_cols <- info$variable[info$role == "outcome" & !is.na(info$role)]
    outcome_cols <- intersect(outcome_cols, names(training))
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
    group_cols = group_cols,
    outcome_cols = outcome_cols,
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
  group_cols <- object$group_cols
  outcome_cols <- object$outcome_cols %||% character(0)

  # Validate required columns exist in new_data
  required_cols <- c(value_col, loc_cols)
  missing_cols <- setdiff(required_cols, names(new_data))
  if (length(missing_cols) > 0) {
    cli::cli_abort(c(
      "Required column{?s} not found in data: {.field {missing_cols}}",
      "i" = "These columns were expected from recipe preparation.",
      "x" = "Ensure new_data has the same structure as training data."
    ))
  }

  # Filter group_cols and outcome_cols to only those present in new_data
  # (handles cases where columns may have been removed by prior steps)
  group_cols <- intersect(group_cols, names(new_data))
  outcome_cols <- intersect(outcome_cols, names(new_data))

  # Exclude consumed columns (value and location) from outcome_cols
  # since they're being nested, not grouped by
  consumed_cols <- c(value_col, loc_cols)
  outcome_cols <- setdiff(outcome_cols, consumed_cols)

  # Determine which outcome columns are actually constant per sample
  # (i.e., have only one unique value per group). Only those should be
  # treated as scalar values. Outcome columns that vary per measurement
  # point should be preserved as list columns.
  scalar_outcome_cols <- character(0)
  if (length(outcome_cols) > 0 && length(group_cols) > 0) {
    for (oc in outcome_cols) {
      if (oc %in% names(new_data) && !is.list(new_data[[oc]])) {
        # Check if this column has unique values per group
        unique_per_group <- new_data |>
          dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
          dplyr::summarize(
            n_unique = dplyr::n_distinct(.data[[oc]]),
            .groups = "drop"
          )
        if (all(unique_per_group$n_unique == 1)) {
          scalar_outcome_cols <- c(scalar_outcome_cols, oc)
        }
      }
    }
  }

  # Only constant outcome columns should be treated like group_cols for nesting
  nest_by_cols <- unique(c(group_cols, scalar_outcome_cols))

  # Variable outcome columns go into other_cols for list column preservation
  variable_outcome_cols <- setdiff(outcome_cols, scalar_outcome_cols)

  # Check if input columns are list columns (from a previous input step)
  # If so, unnest them first to recreate long format
  cols_to_unnest <- c(value_col, loc_cols)
  cols_to_unnest <- cols_to_unnest[cols_to_unnest %in% names(new_data)]
  is_list_col <- vapply(
    new_data[cols_to_unnest],
    is.list,
    logical(1)
  )

  if (any(is_list_col)) {
    # Validate all required columns are in same state (all lists or all non-lists)
    if (!all(is_list_col)) {
      list_cols <- cols_to_unnest[is_list_col]
      non_list_cols <- cols_to_unnest[!is_list_col]
      cli::cli_abort(c(
        "Mixed column types: cannot process data.",
        "i" = "List column{?s}: {.field {list_cols}}",
        "i" = "Non-list column{?s}: {.field {non_list_cols}}",
        "x" = "All value and location columns must be in the same format."
      ))
    }
    # Unnest the list columns to recreate long format
    # This enables multiple step_measure_input_long calls in sequence
    new_data <- tidyr::unnest(new_data, cols = dplyr::all_of(cols_to_unnest))
  }

  # Get all columns that are NOT nest_by_cols, value_col, or loc_cols
  # These will be preserved as list columns for subsequent steps
  # (scalar outcome_cols are in nest_by_cols; variable outcome_cols end up here)
  other_cols <- setdiff(names(new_data), c(nest_by_cols, value_col, loc_cols))

  # Build the nested measure tibble with canonical column names (location, value)
  # Also preserve location columns as list columns for subsequent input steps
  if (ndim == 1) {
    loc_col <- loc_cols[1]

    # Use tidyr::nest to create the measure column, then rename inside
    # First, create a temporary tibble with canonical names for nesting
    work_data <- new_data
    work_data$.measure_value <- work_data[[value_col]]
    work_data$.measure_location <- work_data[[loc_col]]

    new_data <- work_data |>
      tidyr::nest(
        .measures_temp = c(.measure_value, .measure_location),
        .by = dplyr::all_of(nest_by_cols)
      )

    # Rename columns inside nested tibbles to canonical names
    new_data$.measures_temp <- lapply(new_data$.measures_temp, function(df) {
      tibble::tibble(
        location = df$.measure_location,
        value = df$.measure_value
      )
    })

    # The location column is now inside the nested measure, but we also need
    # to preserve it separately for subsequent steps.
    # Extract from the first measure (all should have same locations)
    if (nrow(new_data) > 0 && length(new_data$.measures_temp) > 0) {
      # Create a list column with the location values for each row
      new_data[[loc_col]] <- lapply(
        new_data$.measures_temp,
        function(m) m$location
      )
    }

    # Also need to preserve other_cols as list columns
    # These get nested by tidyr::nest, so extract them back
    if (length(other_cols) > 0) {
      # Re-aggregate from original work_data
      for (other_col in other_cols) {
        col_is_list <- is.list(work_data[[other_col]])
        agg_data <- work_data |>
          dplyr::group_by(dplyr::across(dplyr::all_of(nest_by_cols))) |>
          dplyr::summarize(
            # If column is already a list (from previous input step), it was
            # replicated during unnest - each row in a group has the same list
            # element. Take the first to recreate the original list structure.
            # Otherwise, wrap in list() to preserve values for this group.
            !!other_col := if (col_is_list) {
              list(.data[[other_col]][[1]])
            } else {
              list(.data[[other_col]])
            },
            .groups = "drop"
          )
        new_data <- dplyr::left_join(new_data, agg_data, by = nest_by_cols)
      }
    }
  } else {
    # Multi-dimensional case
    work_data <- new_data
    work_data$.measure_value <- work_data[[value_col]]
    for (i in seq_along(loc_cols)) {
      work_data[[paste0(".measure_loc_", i)]] <- work_data[[loc_cols[i]]]
    }
    loc_temp_cols <- paste0(".measure_loc_", seq_along(loc_cols))

    new_data <- work_data |>
      tidyr::nest(
        .measures_temp = c(.measure_value, dplyr::all_of(loc_temp_cols)),
        .by = dplyr::all_of(nest_by_cols)
      )

    # Rename columns inside nested tibbles to canonical names
    new_data$.measures_temp <- lapply(new_data$.measures_temp, function(df) {
      result <- tibble::tibble(value = df$.measure_value)
      for (i in seq_along(loc_cols)) {
        result[[paste0("location_", i)]] <- df[[paste0(".measure_loc_", i)]]
      }
      result
    })

    # Preserve location columns for subsequent steps
    for (i in seq_along(loc_cols)) {
      new_data[[loc_cols[i]]] <- lapply(
        new_data$.measures_temp,
        function(m) m[[paste0("location_", i)]]
      )
    }

    # Preserve other_cols as list columns
    if (length(other_cols) > 0) {
      for (other_col in other_cols) {
        col_is_list <- is.list(work_data[[other_col]])
        agg_data <- work_data |>
          dplyr::group_by(dplyr::across(dplyr::all_of(nest_by_cols))) |>
          dplyr::summarize(
            # If column is already a list (from previous input step), it was
            # replicated during unnest - each row in a group has the same list
            # element. Take the first to recreate the original list structure.
            # Otherwise, wrap in list() to preserve values for this group.
            !!other_col := if (col_is_list) {
              list(.data[[other_col]][[1]])
            } else {
              list(.data[[other_col]])
            },
            .groups = "drop"
          )
        new_data <- dplyr::left_join(new_data, agg_data, by = nest_by_cols)
      }
    }
  }

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
    rename_map <- stats::setNames(
      c(value_col, loc_cols),
      c("value", loc_new_names)
    )
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
    args <- stats::setNames(
      lapply(loc_cols, function(col) tbl[[col]]),
      loc_cols
    )
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
