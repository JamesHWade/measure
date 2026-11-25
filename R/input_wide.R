#' Ingest Measurements in Separate Columns
#'
#' `step_measure_input_wide` creates a *specification* of a recipe
#'  step that converts measures organized in multiple columns into an internal
#'  format used by the package.
#'
#' @family input/output steps
#' @inheritParams recipes::step_center
#' @param columns A character string of the selected variable names. This field
#' is a placeholder and will be populated once [prep()] is used.
#' @param location_values A numeric vector of values that specify the location
#' of the measurements (e.g., wavelength etc.) in the same order as the variables
#' selected by `...`. If not specified, a sequence of integers (starting at 1L)
#' is used.
#' @param col_name A single character string specifying the name of the output
#'   column that will contain the measure data. Defaults to `".measures"`. Use
#'   different names when creating multiple measure columns (e.g., `".uv_spectrum"`
#'   and `".ms_spectrum"`).
#' @details
#' This step is designed for data in a format where the analytical measurements
#' are in separate columns.
#'
#' `step_measure_input_wide()` will collect those data and put them into a
#' format used internally by this package. The data structure has a row for
#' each independent experimental unit and a nested tibble with that sample's
#' measure (measurement and location). All columns that are not selected as
#' measurements are retained as row-level metadata.
#'
#' Each input row produces one output row and one nested measurement. A sample
#' identifier is still recommended for downstream joins and diagnostics, but
#' duplicate values in the non-measurement columns do not merge input rows.
#'
#' The selected column names and their axis order are fixed when the recipe is
#' prepared. When baking, those trained columns must be present in `new_data`;
#' extra columns and the order of columns in `new_data` do not change the
#' measurement axis.
#'
#' # Tidying
#'
#' When you [`tidy()`][recipes::tidy.recipe()] this step, a tibble indicating which of
#' the original columns were used to reformat the data.
#'
#'
#' @examplesIf rlang::is_installed("modeldata")
#' data(meats, package = "modeldata")
#'
#' # Outcome data is to the right
#' names(meats) |> tail(10)
#'
#' # ------------------------------------------------------------------------------
#' # Ingest data without adding the location (i.e. wave number) for the spectra
#'
#' rec <-
#'   recipe(water + fat + protein ~ ., data = meats) |>
#'   step_measure_input_wide(starts_with("x_")) |>
#'   prep()
#'
#' summary(rec)
#'
#' # ------------------------------------------------------------------------------
#' # Ingest data without adding the location (i.e. wave number) for the spectra
#'
#' # Make up some locations for the spectra's x-axis
#' index <- seq(1, 2, length.out = 100)
#'
#' rec <-
#'   recipe(water + fat + protein ~ ., data = meats) |>
#'   step_measure_input_wide(starts_with("x_"), location_values = index) |>
#'   prep()
#'
#' summary(rec)
#' @export

step_measure_input_wide <-
  function(
    recipe,
    ...,
    role = "measure",
    trained = FALSE,
    columns = NULL,
    location_values = NULL,
    col_name = ".measures",
    skip = FALSE,
    id = rand_id("measure_input_wide")
  ) {
    add_step(
      recipe,
      step_measure_input_wide_new(
        terms = enquos(...),
        trained = trained,
        role = role,
        columns = columns,
        location_values = location_values,
        col_name = col_name,
        skip = skip,
        id = id
      )
    )
  }

step_measure_input_wide_new <-
  function(terms, role, trained, columns, location_values, col_name, skip, id) {
    step(
      subclass = "measure_input_wide",
      terms = terms,
      role = role,
      trained = trained,
      columns = columns,
      location_values = location_values,
      col_name = col_name,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_measure_input_wide <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  if (length(col_names) == 0) {
    cli::cli_abort("At least one measurement column must be selected.")
  }
  check_wide_measure_columns(training, col_names)

  if (!is.null(x$location_values)) {
    if (!is.numeric(x$location_values) || !is.null(dim(x$location_values))) {
      cli::cli_abort("{.arg location_values} must be a numeric vector.")
    }
    num_inputs <- length(col_names)
    num_loc <- length(x$location_values)
    if (num_inputs != num_loc) {
      msg <- paste0(
        num_inputs,
        " columns were selected as inputs but ",
        "`location_values` has ",
        num_loc,
        " values."
      )
      rlang::abort(msg)
    }
  } else {
    x$location_values <- seq_along(col_names)
  }

  step_measure_input_wide_new(
    terms = x$terms,
    role = "measure",
    trained = TRUE,
    columns = col_names,
    location_values = x$location_values,
    col_name = x$col_name,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_input_wide <- function(object, new_data, ...) {
  columns <- unname(object$columns)
  missing_cols <- setdiff(columns, names(new_data))
  if (length(missing_cols) > 0) {
    cli::cli_abort(c(
      "Required measurement column{?s} not found in {.arg new_data}: {.field {missing_cols}}.",
      "i" = "These columns were selected when the recipe was prepared."
    ))
  }
  check_wide_measure_columns(new_data, columns)

  output_col <- object$col_name
  if (!rlang::is_string(output_col) || !nzchar(output_col)) {
    cli::cli_abort("{.arg col_name} must be a single column name.")
  }
  if (output_col %in% setdiff(names(new_data), columns)) {
    cli::cli_abort(c(
      "Output column {.field {output_col}} already exists in {.arg new_data}.",
      "i" = "Choose a different {.arg col_name} or remove the existing column."
    ))
  }

  values <- as.matrix(new_data[columns])
  measures <- matrix_to_measure(values, object$location_values)
  result <- new_data[, setdiff(names(new_data), columns), drop = FALSE]
  result[[output_col]] <- new_measure_list(measures)

  result
}

#' @export
print.step_measure_input_wide <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Collate wide analytical measurements"
    print_step(x$columns, x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @export
tidy.step_measure_input_wide <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(
      terms = x$columns,
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


check_wide_measure_columns <- function(data, columns) {
  matrix_cols <- columns[vapply(
    data[columns],
    function(x) !is.null(dim(x)),
    logical(1)
  )]
  if (length(matrix_cols) > 0) {
    cli::cli_abort(c(
      "Wide measurement columns must be scalar numeric vectors.",
      "x" = "Array-valued column{?s} selected: {.field {matrix_cols}}."
    ))
  }

  check_type(data[, columns, drop = FALSE], types = c("double", "integer"))
}
