#' Nest measure variables for downstream prep and modeling
#'
#' `step_measure_collect` creates a *specification* of a recipe
#'  step that nests measurement columns into column `.measures`. This is often
#'  the first step in preparing measurement data for downstream modeling.
#'
#' @param recipe A recipe object. The step will be added to the
#'  sequence of operations for this recipe.
#' @param ... One or more selector functions to choose which
#'  variables are affected by the step. See [selections()]
#'  for more details. For the `tidy` method, these are not
#'  currently used.
#' @param role A newly created column `.measure` is given role "measure"
#' @param trained A logical to indicate if the quantities for
#'  preprocessing have been estimated.
#' @param shape The starting shape of the data, either "long" or "wide" using
#' tidyr-style nomenclature.
#' @param identifiers A combination of roles (descriptors + conditions) used
#' for subsequent processing
#' @param columns A character string of variable names that will
#'  be populated (eventually) by the `terms` argument.
#' @param skip A logical. Should the step be skipped when the
#'  recipe is baked by [bake()]? While all operations are baked
#'  when [prep()] is run, some operations may not be able to be
#'  conducted on new data (e.g. processing the outcome variable(s)).
#'  Care should be taken when using `skip = TRUE` as it may affect
#'  the computations for subsequent operations
#' @param id A character string that is unique to this step to identify it.
#' @template step-return
#'
#' @export
#' @details
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble with columns
#' ... fill in later ... is returned.
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
           role = "measure",
           identifiers = NA,
           trained = FALSE,
           shape = c("long", "wide"),
           columns = NULL,
           skip = FALSE,
           id = rand_id("measure_collect")) {
    add_step(
      recipe,
      step_measure_collect_new(
        terms = ellipse_check(...),
        trained = trained,
        role = role,
        shape = shape,
        columns = columns,
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
           columns,
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
      columns = columns,
      identifiers = identifiers,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_measure_collect <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  check_type(training[, col_names])
  x$identifiers <-
    recipes_eval_select(has_role(c("descriptor", "condition")), training, info)
  conditions <- recipes_eval_select(has_role("condition"), training, info)
  x$terms  <- recipes_eval_select(x$terms, training, info)

  step_measure_collect_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    shape = x$shape,
    columns = col_names,
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
        cols = dplyr::all_of(measures),
        names_to = "measure",
        values_to = "response"
      ) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(identifiers)), .index) |>
      dplyr::group_nest(.key = ".measures") |>
      dplyr::arrange(.index)
  } else {
    data |>
      dplyr::group_by(dplyr::across(dplyr::all_of(identifiers))) |>
      dplyr::group_nest(.key = ".measures") |>
      dplyr::mutate(.index = dplyr::row_number()) |>
      dplyr::relocate(.index, .before = .measures)
  }
}

#' @export
bake.step_measure_collect <- function(object, new_data, ...) {
  check_new_data(names(object$object$xnames), object, new_data)
  measure_collect(data        = new_data,
                  shape       = object$shape,
                  measures    = as.character(object$terms),
                  identifiers = object$identifiers)
}

#' @export
print.step_measure_collect <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Collect measurements "
    print_step(x$columns, x$terms, x$trained, title, width)
    invisible(x)
  }

format_measure_collect <- function(x) {
  tibble(
    value = unname(x)
  )
}

#' Tidy method for step_measure_collect
#'
#' @param x A recipe object
#' @param ... Other variables to pass to tidy
#'
#' @rdname tidy.recipe
#' @export
tidy.step_measure_collect <- function(x, ...) {
  cli::cat_print(names(x))
  if (is_trained(x)) {
    # TODO update this to be useful tidy method
    res <- purrr::map_dfr(x$terms, format_measure_collect, .id = "term")
    # columns = sel2char(x$terms)
    # res <- tibble(terms = list(columns),
    #               shape = x$shape,
    #               identifiers = list(x$identifiers))
  } else {
    # TODO update this to be useful tidy method
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names,
                  shape = x$shape,
                  .identifiers = list(x$identifiers))
  }
  res$id <- x$id
  res
}
