#' Ingest Measurements in Separate Columns
#'
#' `step_measure_input_wide` creates a *specification* of a recipe
#'  step that converts measures organized in multiple columns into an internal
#'  format used by the package.
#'
#' @inheritParams recipes::step_center
#' @param columns A character string of the selected variable names. This field
#' is a placeholder and will be populated once [prep()] is used.
#' @param location_values A numeric vector of values that specify the location
#' of the measurements (e.g., wavelength etc.) in the same order as the variables
#' selected by `...`. If not specified, a sequence of integers (starting at 1L)
#' is used.
#' @details
#' This step is designed for data in a format where the analytical measurements
#' are in separate columns.
#'
#' `step_measure_input_wide()` will collect those data and put them into a
#' format used internally by this package. The data structure has a row for
#' each independent experimental unit and a nested tibble with that sample's
#' measure (measurement and location). It assumes that there are unique
#' combinations of the other columns in the data that define individual
#' patterns associated with the pattern. If this is not the case, the special
#' values might be inappropriately restructured.
#'
#' The best advice is to have a column of any type that indicates the unique
#' sample number for each measure. For example, if there are 20 rows in the
#' input data set, the columns that are _not_ analytically measurements show
#' have no duplicate combinations in the 20 rows.
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble indicating which of
#' the original columns were used to reformat the data.
#'
#'
#' @examples
#' data(meats, package = "modeldata")
#'
#' # Outcome data is to the right
#' names(meats) %>% tail(10)
#'
#' # ------------------------------------------------------------------------------
#' # Ingest data without adding the location (i.e. wave number) for the spectra
#'
#' rec <-
#'   recipe(water + fat + protein ~ ., data = meats) %>%
#'   step_measure_input_wide(starts_with("x_")) %>%
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
#'   recipe(water + fat + protein ~ ., data = meats) %>%
#'   step_measure_input_wide(starts_with("x_"), location_values = index) %>%
#'   prep()
#'
#' summary(rec)
#' @export

step_measure_input_wide <-
    function(recipe,
             ...,
             role = "measure",
             trained = FALSE,
             columns = NULL,
             location_values = NULL,
             skip = FALSE,
             id = rand_id("measure_input_wide")) {
      add_step(
        recipe,
        step_measure_input_wide_new(
          terms = enquos(...),
          trained = trained,
          role = role,
          columns = columns,
          location_values = location_values,
          skip = skip,
          id = id
        )
      )
    }

step_measure_input_wide_new <-
  function(terms, role, trained, columns, location_values, na_rm, skip, id) {
    step(
      subclass = "measure_input_wide",
      terms = terms,
      role = role,
      trained = trained,
      columns = columns,
      location_values = location_values,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_measure_input_wide <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  check_type(training[, col_names], types = c("double", "integer"))

  if (!is.null(x$location_values)) {
    num_inputs <- length(col_names)
    num_loc <- length(x$location_values)
    if (num_inputs != num_loc) {
      msg <- paste0(num_inputs, " columns were selected as inputs but ",
                    "`location_values` has ", num_loc, " values.")
      rlang::abort(msg)
    }
    # if
  } else {
    x$location_values <- seq_along(col_names)
  }

  step_measure_input_wide_new(
    terms = x$terms,
    role = "measure",
    trained = TRUE,
    columns = col_names,
    location_values = x$location_values,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_input_wide <- function(object, new_data, ...) {

  # TODO check to make sure that the nested tibble has the same number of rows
  # in case the nesting was bad
  wide_to_list(new_data, object$location_values, object$terms)
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
    res <- tibble(terms = x$columns,
                  value = na_dbl)
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names,
                  value = na_dbl)
  }
  res$id <- x$id
  res
}


wide_to_list <- function(x, ind, selections) {
  x %>%
    dplyr::mutate(..row = seq_len(nrow(x))) %>%
    tidyr::pivot_longer(
      cols = c(!!!selections),
      names_to = "temp",
      values_to = "value"
    ) %>%
    dplyr::select(-temp) %>%
    # TODO convert some of this to use vctrs
    tidyr::nest(.by = c(-value), .key = ".measures") %>%
    dplyr::select(-..row) %>%
    add_location(ind)
}
