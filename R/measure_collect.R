#' Nest measure variables for downstream prep and modeling
#'
#' `step_measure_collect` creates a *specification* of a recipe
#'  step that <what it does>
#'
#' @param recipe A recipe object. The step will be added to the
#'  sequence of operations for this recipe.
#' @param ... One or more selector functions to choose which
#'  variables are affected by the step. See [selections()]
#'  for more details. For the `tidy` method, these are not
#'  currently used.
#' @param role Not used by this step since no new variables are
#'  created. <change if role is used>
#' @param trained A logical to indicate if the quantities for
#'  preprocessing have been estimated.
#' @param shape The starting shape of the data, either "long" or "wide" using
#' tidyr-style nomenclature.
#' @param skip A logical. Should the step be skipped when the
#'  recipe is baked by [bake()]? While all operations are baked
#'  when [prep()] is run, some operations may not be able to be
#'  conducted on new data (e.g. processing the outcome variable(s)).
#'  Care should be taken when using `skip = TRUE` as it may affect
#'  the computations for subsequent operations
#' @param id A character string that is unique to this step to identify it.
#' @return <describe return>
#'
#' @export
#' @details To do
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble with columns
#' <describe tidying> is returned.
#'
#' @examples
#' \dontrun{
#' library(modeldata)
#' data(meats)
#' # todo
#' }
step_measure_collect <-
  function(recipe,
           ...,
           role = NA,
           identifiers = NA,
           trained = FALSE,
           shape = c("long", "wide"),
           #<additional args here>
           skip = FALSE,
           id = rand_id("measure_collect")) {
    add_step(
      recipe,
      step_measure_collect_new(
        terms = ellipse_check(...),
        trained = trained,
        role = role,
        shape = shape,
        identifiers = identifiers,
        skip = skip,
        id = id
      )
    )
  }


step_measure_collect_new <-
  function(terms,
           role,
           shape,
           identifiers,
           trained,
           skip,
           id) {
    step(
      subclass = "measure_collect",
      terms = terms,
      role = role,
      trained = trained,
      shape = shape,
      identifiers = identifiers,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_measure_collect <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  check_type(training[, col_names])
  # <prepping action here>
  descriptors <- recipes_eval_select(has_role("descriptor"), training, info)
  conditions <- recipes_eval_select(has_role("condition"), training, info)
  measures  <- recipes_eval_select(x$terms, training, info)
  x$identifiers <- descriptors
  cli::cli_inform("Identifiers: {x$identifiers}")
  cli::cli_inform("Measures: {measures}")

  step_measure_collect_new(
    terms = measures,
    role = x$role,
    trained = TRUE,
    shape = x$shape,
    # <additional args here>
    identifiers = x$identifiers,
    skip = x$skip,
    id = x$id
  )
}

#' Subtract baseline using robust fitting method
#'
#' @param data A dataframe containing the variable for baseline subtraction
#' @param shape The starting shape of the data, either "long" or "wide" using
#' tidyr nomenclature
#' @param measures Measurement columns in data
#' @param identifiers Identifiers in data, a combination of sample identifiers
#' and experimental variables
#'
#' @return A dataframe
#' @export
#'
measure_collect <- function(data, shape, measures, identifiers) {

  if (shape == "wide") {
    data |>
      dplyr::mutate(.index = dplyr::row_number()) |>
      tidyr::pivot_longer(
        cols = {{measures}},
        names_to = "measure",
        values_to = "response"
      ) |>
      dplyr::group_by(dplyr::across({{identifiers}}), .index) |>
      dplyr::group_nest(.key = ".measures") |>
      dplyr::arrange(.index)
  } else {
    data |>
      dplyr::group_by(dplyr::across({{identifiers}})) |>
      dplyr::group_nest(.key = ".measures") |>
      dplyr::mutate(.index = dplyr::row_number()) |>
      dplyr::relocate(.index, .before = .measures)
  }
}
#' @export
bake.step_measure_collect <- function(object, new_data, ...) {
  check_new_data(names(object$object$xnames), object, new_data)
  inform("baking prep")

  cli::cli_inform("Shape: {class(object$shape)}")
  cli::cli_inform("Shape: {object$shape}")
  cli::cli_inform("Identifiers: {class(object$identifiers)}")
  cli::cli_inform("Shape: {object$identifiers}")

  # res <- recipes::check_name(res, new_data, object)

  measure_collect(data        = new_data,
                  shape       = object$shape,
                  measures    = object$terms,
                  identifiers = object$identifiers)
}

#' @export
print.step_measure_collect <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Collect measurements "
    print_step(names(x$means), x$terms, x$trained, title, width)
    invisible(x)
  }

#' @export
tidy.step_measure_collect <- function(x, ...) {
  if (is_trained(x)) {
    # res <-
    # <action here>
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names,
                  value = na_dbl)
  }
  res$id <- x$id
  res
}
