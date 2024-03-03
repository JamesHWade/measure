#' Savitzky-Golay Pre-Processing
#'
#' `step_measure_savitzky_golay` creates a *specification* of a recipe
#'  step that smooths and filters the measurement sequence.
#'
#' @param recipe A recipe object. The step will be added to the
#'  sequence of operations for this recipe.
#' @param role Not used by this step since no new variables are
#'  created.
#' @param trained A logical to indicate if the quantities for
#'  preprocessing have been estimated.
#' @param degree An integer for the polynomial degree to use for smoothing.
#' @param window_size An odd integer for the window size to use for smoothing.
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
#' @family measure-smoothing
#' @family measure-differencing
#' @details
#' This method can both smooth out random noise and reduce between-predictor
#' correlation. It fits a polynomial to a window of measurements and this results
#' in fewer measurements than the input. Measurements are assumed to be equally
#' spaced.
#'
#' The polynomial degree should be less than the window size. Also, window
#' size must be greater than polynomial degree. If either case is true, the
#' original argument values are increased to satisfy these conditions (with a
#' warning).
#'
#' **No selectors should be supplied to this step function**. The data should be in
#' a special internal format produced by [step_measure_input_wide()] or
#' [step_measure_input_long()].
#'
#' The measurement locations are reset to integer indices starting at one.
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble with columns
#' <describe tidying> is returned.
#'
#' @examples
#' if (rlang::is_installed("prospectr")) {
#'   rec <-
#'     recipe(water + fat + protein ~ ., data = meats_long) %>%
#'     update_role(id, new_role = "id") %>%
#'     step_measure_input_long(transmittance, location = vars(channel)) %>%
#'     step_measure_savitzky_golay(
#'       differentiation_order = 1,
#'       degree = 3,
#'       window_size = 5
#'     ) %>%
#'     prep()
#' }
step_measure_savitzky_golay <-
  function(recipe,
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
  function(role, trained, degree, window_size, differentiation_order,
           na_rm, skip, id) {
    recipes::step(
      subclass = "measure_savitzky_golay",
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
  check_for_measure(training)
  if (!is.numeric(x$degree) | length(x$degree) != 1 | x$degree < 1) {
    cli::cli_abort("{.arg degree} to {.fn  step_measure_savitzky_golay} should
                    be a single integer greater than zero.")
  }
  if (!is.numeric(x$differentiation_order) | length(x$differentiation_order) != 1 |
    x$differentiation_order < 0) {
    cli::cli_abort("The {.arg differentiation_order} argument to
                    {.fn  step_measure_savitzky_golay} should be a single
                    integer greater than -1.")
  }
  if (!is.numeric(x$window_size) | length(x$window_size) != 1 |
    x$window_size < 1 | x$window_size %% 2 != 1) {
    cli::cli_abort("The {.arg window_size} argument to
                    {.fn  step_measure_savitzky_golay} should be a single odd
                    integer greater than 0.")
  }

  # polynomial order p must be geater or equal to differentiation order m
  if (x$degree <= x$differentiation_order) {
    x$degree <- x$differentiation_order + 1
    cli::cli_warn("The {.arg degree} argument to
                   {.fn step_measure_savitzky_golay} should be greater than or
                   equal to {.arg differentiation_order}. The polynomial degree
                   was increased to {x$degree}.")
  }
  # filter length w must be greater than polynomial order p
  if (x$window_size <= x$degree) {
    x$window_size <- x$degree + 1
    if (x$window_size %% 2 == 0) {
      x$window_size <- x$window_size + 1
    }
    cli::cli_warn("The {.arg window_size} argument to
                   {.fn step_measure_savitzky_golay} should be greater than or
                   equal to {.arg degree}. The polynomial degree was increased
                   to {x$window_size}.")
  }

  step_measure_savitzky_golay_new(
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
  # TODO try to approximate the wave numbers that were input.
  new_data$.measures <- res
  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_savitzky_golay <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Savitzky-Golay preprocessing "
    recipes::print_step(
      "<internal measurements>", "<internal measurements>",
      x$trained, title, width
    )
    invisible(x)
  }

#' @rdname tidy.recipe
#' @param x A step object.
#' @param ... Not used.
#' @export
tidy.step_measure_savitzky_golay <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble::tibble(
      terms = ".measure",
      value = na_dbl
    )
  } else {
    term_names <- recipes::sel2char(x$terms)
    res <- tibble::tibble(
      terms = term_names,
      value = na_dbl
    )
  }
  res$id <- x$id
  res
}

#' Set package dependencies
#' @param x A step object.
#' @param ... Not used.
#' @name required_pkgs.recipe
#' @export
required_pkgs.step_isomap <- function(x, ...) {
  c("measure", "prospectr")
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
  res <- try(rlang::eval_tidy(cl), silent = TRUE)

  if (inherits(res, "try-error")) {
    msg <- as.character(res)
    msg <- strsplit(msg, " : ")[[1]][2]
    msg <- gsub("\\n", "", msg)
    cli::cli_abort("Savitzky-Golay computations failed with error: {msg}")
  }

  if (ncol(res) != ncol(dat)) {
    # TODO Prob can do better; can we approximate what the wave numbers should be?
    loc <- 1:ncol(res)
  }

  matrix_to_measure(res, loc)
}
