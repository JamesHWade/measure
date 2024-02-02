#' Ingest Measurements from a Single Column
#'
#' `step_measure_input_long` creates a *specification* of a recipe
#'  step that converts measures organized in a column for the analytical results
#'  (and an option column of numeric indices) into an internal format used by
#'  the package.
#' @family input/output steps
#' @inheritParams recipes::step_center
#' @param ... One or more selector functions to choose which _single_ column
#' contains the analytical measurements. The selection should be in the order
#' of the measurement's profile.
#' @param location One or more selector functions to choose which _single_
#' column has the locations of the analytical values.
#' @param columns A character vector of column names determined by the recipe.
#' @details
#' This step is designed for data in a format where there is a column for the
#' analytical measurement (e.g., absorption, etc.) and another with the
#' location of the value (e.g., wave number, etc.).
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
#' # Missing Data
#'
#' Currently, \pkg{measure} assumes that there are equal numbers of values
#' within a sample. If there are missing values in the measurements, you'll
#' need to pad them with missing values (as opposed to an absent row in the
#' long format). If not, an error will occur.
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble indicating which of
#' the original columns were used to reformat the data.
#'
#' @export
step_measure_input_long <-
  function(recipe,
           ...,
           location,
           role = "measure",
           trained = FALSE,
           columns = NULL,
           skip = FALSE,
           id = rand_id("measure_input_long")) {
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
        skip = skip,
        id = id
      )
    )
  }

step_measure_input_long_new <-
  function(terms, role, trained, columns, location, skip, id) {
    step(
      subclass = "measure_input_long",
      terms = terms,
      role = role,
      trained = trained,
      columns = columns,
      location = location,
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
    loc_name <- recipes_eval_select(x$location, training, info)
    check_single_selector(loc_name, "location")
    check_type(training[, loc_name], types = c("double", "integer"))
  } else {
    rlang::abort("'location' is required for long input data")
  }

  step_measure_input_long_new(
    terms = x$terms,
    role = "measure",
    trained = TRUE,
    columns = unname(c(value_name, loc_name)),
    location = x$location,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_input_long <- function(object, new_data, ...) {
  new_data <-
    new_data %>%
    rename_long_cols(object$columns[1], object$columns[2])

  if (!is.na(object$columns[2])) {
    new_data <-
      new_data %>%
      tidyr::nest(.by = c(-value, -location), .key = ".measures")
  } else {
    new_data <-
      new_data %>%
      tidyr::nest(.by = c(-value), .key = ".measures")
  }

  check_measure_dims(new_data)

  new_data
}

#' @export
print.step_measure_input_long <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Collate long analytical measurements"
    print_step(x$columns, x$terms, x$trained, title, width)
    invisible(x)
  }

#' Tidiers for measure steps
#' @param x A recipe step.
#' @rdname tidy.recipe
#' @export
tidy.step_measure_input_long <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = x$columns[!is.na(x$columns)],
                  value = na_dbl)
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names,
                  value = na_dbl)
  }
  res$id <- x$id
  res
}

rename_long_cols <- function(.data, val_chr, loc_chr) {
  if (!is.na(loc_chr)) {
    res <- c(value = val_chr, location = loc_chr)
  } else {
    res <- c(value = unname(val_chr))
  }
  dplyr::rename(.data, dplyr::all_of(res))
}


