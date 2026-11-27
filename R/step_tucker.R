# ==============================================================================
# Tucker Decomposition Step
#
# Multi-way decomposition with independent ranks per mode.
# ==============================================================================

#' Tucker Decomposition for Multi-Dimensional Data
#'
#' `step_measure_tucker()` creates a *specification* of a recipe step that
#' applies Tucker decomposition to multi-dimensional measurement data,
#' extracting component scores as features for modeling.
#'
#' @param recipe A recipe object.
#' @param ... One or more selector functions to choose measure columns.
#'   If empty, all nD measure columns are used.
#' @param ranks A vector of ranks for each measurement axis. If a single
#'   integer, the same rank is used for all axes. Default is 3.
#' @param center Logical. Should data be centered before decomposition?
#'   Default is `TRUE`.
#' @param scale Logical. Should data be scaled before decomposition?
#'   Default is `FALSE`.
#' @param max_iter Maximum number of iterations. Default is 500.
#' @param tol Convergence tolerance. Default is 1e-6.
#' @param prefix Prefix for output column names. Default is `"tucker_"`.
#' @param role Not used.
#' @param trained Logical indicating if the step has been trained.
#' @param skip Logical. Should the step be skipped when baking?
#' @param id Unique step identifier.
#'
#' @return An updated recipe with the new step added.
#'
#' @details
#' Tucker decomposition (also known as higher-order SVD or multilinear SVD)
#' decomposes a tensor into a core tensor multiplied by factor matrices along
#' each mode. Unlike PARAFAC, Tucker allows different ranks for each mode,
#' providing more flexibility.
#'
#' ## Requirements
#'
#' - Input must be `measure_nd_list` with two or three measurement axes
#' - All samples must have the same grid (regular, aligned)
#' - The `multiway` package must be installed (in Suggests)
#'
#' ## Output
#'
#' Creates numeric feature columns representing the unfolded core tensor
#' scores for each sample. Scores are ordered in the same column-major order
#' as the core tensor: the first measurement axis varies fastest, followed by
#' the remaining axes.
#'
#' @note
#' This step requires the `multiway` package. Install with:
#' `install.packages("multiway")`
#'
#' @seealso [step_measure_parafac()] for PARAFAC decomposition
#'
#' @family measure-multiway
#' @export
#'
#' @examples
#' \dontrun{
#' library(recipes)
#'
#' # After ingesting 2D data as nD measurements
#' rec <- recipe(concentration ~ ., data = lc_dad_data) |>
#'   step_measure_input_long(
#'     absorbance,
#'     location = vars(time, wavelength)
#'   ) |>
#'   step_measure_tucker(ranks = c(5, 3)) |>
#'   prep()
#'
#' bake(rec, new_data = NULL)
#' }
step_measure_tucker <- function(
  recipe,
  ...,
  ranks = 3L,
  center = TRUE,
  scale = FALSE,
  max_iter = 500L,
  tol = 1e-6,
  prefix = "tucker_",
  role = "predictor",
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_tucker")
) {
  recipes::add_step(
    recipe,
    step_measure_tucker_new(
      terms = rlang::enquos(...),
      ranks = as.integer(ranks),
      center = center,
      scale = scale,
      max_iter = as.integer(max_iter),
      tol = tol,
      prefix = prefix,
      loadings = NULL,
      core = NULL,
      center_values = NULL,
      scale_values = NULL,
      grid_info = NULL,
      measure_cols = NULL,
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_measure_tucker_new <- function(
  terms,
  ranks,
  center,
  scale,
  max_iter,
  tol,
  prefix,
  loadings,
  core,
  center_values,
  scale_values,
  grid_info,
  measure_cols,
  role,
  trained,
  skip,
  id
) {
  recipes::step(
    subclass = "measure_tucker",
    terms = terms,
    ranks = ranks,
    center = center,
    scale = scale,
    max_iter = max_iter,
    tol = tol,
    prefix = prefix,
    loadings = loadings,
    core = core,
    center_values = center_values,
    scale_values = scale_values,
    grid_info = grid_info,
    measure_cols = measure_cols,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_tucker <- function(x, training, info = NULL, ...) {
  # Check for multiway package
  rlang::check_installed(
    "multiway",
    reason = "for Tucker decomposition"
  )

  check_for_measure(training)

  # Get nD measure columns
  if (length(x$terms) == 0) {
    measure_cols <- find_measure_nd_cols(training)
  } else {
    measure_cols <- recipes::recipes_eval_select(x$terms, training, info)
    actual_nd <- find_measure_nd_cols(training)
    invalid <- setdiff(measure_cols, actual_nd)
    if (length(invalid) > 0) {
      cli::cli_abort(
        "Column{?s} {.field {invalid}} {?is/are} not nD measure column{?s}."
      )
    }
  }

  if (length(measure_cols) == 0) {
    cli::cli_abort("No nD measure columns found for Tucker decomposition.")
  }

  # Process each column
  loadings_list <- list()
  core_list <- list()
  center_list <- list()
  scale_list <- list()
  grid_list <- list()

  for (col in measure_cols) {
    result <- .fit_tucker(
      training[[col]],
      ranks = x$ranks,
      center = x$center,
      scale = x$scale,
      max_iter = x$max_iter,
      tol = x$tol
    )
    loadings_list[[col]] <- result$loadings
    core_list[[col]] <- result$core
    center_list[[col]] <- result$center_values
    scale_list[[col]] <- result$scale_values
    grid_list[[col]] <- result$grid_info
  }

  step_measure_tucker_new(
    terms = x$terms,
    ranks = x$ranks,
    center = x$center,
    scale = x$scale,
    max_iter = x$max_iter,
    tol = x$tol,
    prefix = x$prefix,
    loadings = loadings_list,
    core = core_list,
    center_values = center_list,
    scale_values = scale_list,
    grid_info = grid_list,
    measure_cols = measure_cols,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_tucker <- function(object, new_data, ...) {
  recipes::check_new_data(object$measure_cols, object, new_data)
  for (col in object$measure_cols) {
    scores <- .project_tucker(
      new_data[[col]],
      loadings = object$loadings[[col]],
      core = object$core[[col]],
      center_values = object$center_values[[col]],
      scale_values = object$scale_values[[col]],
      grid_info = object$grid_info[[col]]
    )

    # Add score columns
    n_scores <- ncol(scores)
    col_names <- paste0(object$prefix, col, "_", seq_len(n_scores))
    for (i in seq_len(n_scores)) {
      new_data[[col_names[i]]] <- scores[, i]
    }

    # Remove original nD column
    new_data[[col]] <- NULL
  }

  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_tucker <- function(
  x,
  width = max(20, options()$width - 30),
  ...
) {
  ranks_str <- paste(x$ranks, collapse = " x ")
  title <- paste0("Tucker decomposition (ranks = ", ranks_str, ") ")
  if (x$trained) {
    cat(title, "on ", paste(x$measure_cols, collapse = ", "), sep = "")
  } else {
    cat(title, "<pending>", sep = "")
  }
  cat("\n")
  invisible(x)
}

#' @rdname tidy.recipe
#' @export
#' @keywords internal
tidy.step_measure_tucker <- function(x, type = "parameters", ...) {
  if (type == "loadings" && is_trained(x)) {
    result <- tibble::tibble()
    for (col in x$measure_cols) {
      loadings <- x$loadings[[col]]
      for (mode in seq_along(loadings)) {
        mode_loadings <- tibble::as_tibble(
          loadings[[mode]],
          .name_repair = "minimal"
        )
        mode_loadings$column <- col
        mode_loadings$mode <- mode
        mode_loadings$location <- seq_len(nrow(loadings[[mode]]))
        result <- dplyr::bind_rows(result, mode_loadings)
      }
    }
    result$id <- x$id
    return(result)
  }

  if (is_trained(x)) {
    tibble::tibble(
      terms = unname(x$measure_cols),
      ranks = list(x$ranks),
      center = x$center,
      scale = x$scale,
      id = x$id
    )
  } else {
    tibble::tibble(
      terms = rlang::na_chr,
      ranks = list(x$ranks),
      center = x$center,
      scale = x$scale,
      id = x$id
    )
  }
}

#' @export
required_pkgs.step_measure_tucker <- function(x, ...) {
  c("measure", "multiway")
}

# ------------------------------------------------------------------------------
# Internal helpers
# ------------------------------------------------------------------------------

.fit_tucker <- function(measure_nd_list, ranks, center, scale, max_iter, tol) {
  array_result <- .nd_list_to_array(
    measure_nd_list,
    supported_dims = 2:3,
    method = "Tucker"
  )
  preprocessing <- .fit_multiway_preprocessing(
    array_result$array,
    center = center,
    scale = scale
  )
  X <- preprocessing$array
  grid_info <- array_result$grid_info

  n_samples <- dim(X)[1]
  ndim <- length(dim(X)) - 1L

  # Ranks are specified for measurement axes. The decomposition also has a
  # sample mode, whose rank is chosen independently and is not projected at
  # bake time.
  if (length(ranks) == 1) {
    ranks <- rep(ranks, ndim)
  }

  if (length(ranks) != ndim) {
    cli::cli_abort(
      "Length of {.arg ranks} ({length(ranks)}) must match number of dimensions ({ndim})."
    )
  }

  # multiway::tucker expects nfac for every mode, including samples. Keep the
  # historical sample-rank choice for compatibility with existing fits.
  sample_rank <- min(n_samples, max(ranks))
  full_ranks <- c(sample_rank, ranks)

  fit <- multiway::tucker(
    X,
    nfac = full_ranks,
    nstart = 10,
    maxit = max_iter,
    ctol = tol
  )

  # The package exposes factors as A, B, C, and (for a 4-way tensor) D. Keep
  # the sample factor first so that `loadings[-1]` is always the set of
  # measurement-axis factors used by `.project_tucker()`.
  factor_names <- c("A", "B", "C", "D")[seq_along(dim(X))]
  list(
    loadings = lapply(factor_names, function(name) fit[[name]]),
    core = fit$G,
    center_values = preprocessing$center_values,
    scale_values = preprocessing$scale_values,
    grid_info = grid_info
  )
}

.project_tucker <- function(
  measure_nd_list,
  loadings,
  core,
  center_values,
  scale_values,
  grid_info
) {
  # Use the training grid recorded during prep. Rebuilding a grid from the
  # held-out batch would silently change the meaning of each tensor position.
  array_result <- .nd_list_to_array(
    measure_nd_list,
    grid_info = grid_info,
    supported_dims = 2:3,
    method = "Tucker"
  )
  X <- .apply_multiway_preprocessing(
    array_result$array,
    center_values = center_values,
    scale_values = scale_values
  )

  # Contract each content axis with its loadings while keeping the sample
  # axis intact. The result has shape samples x rank_1 x rank_2 [x rank_3].
  content_loadings <- loadings[-1L]
  for (j in seq_along(content_loadings)) {
    mode <- j + 1L
    shape <- dim(X)
    permutation <- c(mode, seq_along(shape)[-mode])
    unfolded <- matrix(aperm(X, permutation), nrow = shape[mode])
    projected <- crossprod(content_loadings[[j]], unfolded)
    dim(projected) <- c(ncol(content_loadings[[j]]), shape[-mode])
    X <- aperm(projected, order(permutation))
  }
  matrix(X, nrow = dim(X)[1L], ncol = prod(dim(X)[-1L]))
}
