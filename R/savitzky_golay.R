#' Savitzky-Golay Pre-Processing
#'
#' `step_measure_savitzky_golay` creates a *specification* of a recipe
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
#' @param degree The polynomial degree to use for smoothing.
#' @param window_size The window size to use for smoothing.
#' @param differentiation_order An integer for the degree of filtering (zero
#' indicates no differentiation).
#' @param skip A logical. Should the step be skipped when the
#'  recipe is baked by [bake()]? While all operations are baked
#'  when [prep()] is run, some operations may not be able to be
#'  conducted on new data (e.g. processing the outcome variable(s)).
#'  Care should be taken when using `skip = TRUE` as it may affect
#'  the computations for subsequent operations
#' @param id A character string that is unique to this step to identify it.
#' @return An updated version of `recipe` with the new step added to the
#' sequence of any existing operations.
#'
#' @export
#' @details
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble with columns
#' <describe tidying> is returned.
#'
# @examples

step_measure_savitzky_golay <-
    function(recipe,
             ...,
             role = NA,
             trained = FALSE,
             degree = 3,
             window_size = 11,
             differentiation_order = 0,
             skip = FALSE,
             id = rand_id("measure_savitzky_golay")) {
      recipes::add_step(
        recipe,
        step_measure_savitzky_golay_new(
          terms = enquos(...),
          trained = trained,
          role = role,
          degree = degree,
          window_size = window_size,
          differentiation_order = differentiation_order,
          skip = FALSE,
          id = id
        )
      )
    }

step_measure_savitzky_golay_new <-
  function(terms, role, trained, degree, window_size, differentiation_order,
           na_rm, skip, id) {
    recipes::step(
      subclass = "measure_savitzky_golay",
      terms = terms,
      role = role,
      trained = trained,
      degree = degree,
      window_size = window_size,
      differentiation_order = differentiation_order,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_measure_savitzky_golay <- function(x, training, info = NULL, ...) {
  col_names <- recipes::recipes_eval_select(x$terms, training, info)
  recipes::check_type(training[, col_names])

  step_measure_savitzky_golay_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    degree = x$degree,
    window_size = x$window_size,
    differentiation_order = x$differentiation_order,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_savitzky_golay <- function(object, new_data, ...) {

  res <-
    .comp_savitzky_golay(
      new_data$.measures,
      diffs = object$differentiation_order,
      degree = object$degree,
      window = object$window_size
    )
  new_data$.measures <- res
  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_savitzky_golay <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Savitzky-Golay preprocessing "
    recipes::print_step(names(x$means), x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname measure-tidy.recipe
#' @export
tidy.step_measure_savitzky_golay <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble::tibble(terms = ".measure",
                          value = na_dbl)
  } else {
    term_names <- recipes::sel2char(x$terms)
    res <- tibble::tibble(terms = term_names,
                          value = na_dbl)
  }
  res$id <- x$id
  res
}

# ------------------------------------------------------------------------------

.comp_savitzky_golay <- function(dat, diffs = 1, degree = 2, window = 3, ...) {
  rlang::check_installed("prospectr")
  loc <- dat[[1]]$location
  dat <- measure_to_matrix(dat)

  cl <-
    rlang::call2(
      "savitzkyGolay",
      .ns = "prospectr",
      X = expr(dat),
      m = diffs,
      p = degree,
      w = window,
      ...
    )
  res <- rlang::eval_tidy(cl)

  if (ncol(res) != ncol(dat)) {
    # Prob can do better
    loc <- 1:ncol(res)
  }

  matrix_to_measure(res, loc)
}

