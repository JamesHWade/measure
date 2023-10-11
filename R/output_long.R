#' Reorganize Measurements to Two Columns
#'
#' `step_measure_output_long` creates a *specification* of a recipe
#'  step that converts measures to a format with columns for the measurement and
#'  the corresponding location (i.e., "long" format).
#'
#' @inheritParams recipes::step_center
#' @param values_to A single character string for the column containing the
#' analytical maesurement.
#' @param location_to A single character string for the column containing the
#' location of the measurement (e.g. wavenumber or index).
#' @details
#' This step is designed convert analytical measurements from their internal
#' data structure to a two column format.
#' @export

step_measure_output_long <-
  function(recipe,
           values_to = ".measure",
           location_to = ".location",
           role = "predictor",
           trained = FALSE,
           skip = FALSE,
           id = rand_id("measure_output_long")) {
    add_step(
      recipe,
      step_measure_output_long_new(
        values_to = values_to,
        location_to = location_to,
        trained = trained,
        role = role,
        skip = skip,
        id = id
      )
    )
  }

step_measure_output_long_new <-
  function(values_to, location_to, role, trained, skip, id) {
    step(
      subclass = "measure_output_long",
      values_to = values_to,
      location_to = location_to,
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_measure_output_long <- function(x, training, info = NULL, ...) {
  step_measure_output_long_new(
    values_to = x$values_to,
    location_to = x$location_to,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_output_long <- function(object, new_data, ...) {

  rnm <- c(".measures.value", ".measures.location")
  names(rnm) <- c(object$values_to, object$location_to)
  new_data %>%
    tidyr::unnest(cols = c(.measures), names_sep = ".") %>%
    dplyr::rename(!!rnm)
}

#' @export
print.step_measure_output_long <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Restructure analytical measurements to long format"
    print_step(rlang::quos("<internal data>"), rlang::quos("<internal data>"),
               x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @export
tidy.step_measure_output_long <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = na_chr,
                  value = na_dbl)
  } else {
    res <- tibble(terms = na_chr,
                  value = na_dbl)
  }
  res$id <- x$id
  res
}

value_to_tibble <- function(x, prefix = "measure_") {
  x <- matrix(x$value, nrow = 1)
  colnames(x) <- recipes::names0(ncol(x), prefix = prefix)
  dplyr::as_tibble(x)
}


