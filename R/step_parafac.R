# ==============================================================================
# PARAFAC (Parallel Factor Analysis) Step
#
# Multi-way decomposition for n-dimensional measurement data.
# ==============================================================================

#' PARAFAC Decomposition for Multi-Dimensional Data
#'
#' `step_measure_parafac()` creates a *specification* of a recipe step that
#' applies Parallel Factor Analysis (PARAFAC) to multi-dimensional measurement
#' data, extracting component scores as features for modeling.
#'
#' @param recipe A recipe object.
#' @param ... One or more selector functions to choose measure columns.
#'   If empty, all nD measure columns are used.
#' @param n_components Number of PARAFAC components to extract. Default is 3.
#' @param center Logical. Should data be centered before decomposition?
#'   Default is `TRUE`.
#' @param scale Logical. Should data be scaled before decomposition?
#'   Default is `FALSE`.
#' @param max_iter Maximum number of iterations. Default is 500.
#' @param tol Convergence tolerance. Default is 1e-6.
#' @param prefix Prefix for output column names. Default is `"parafac_"`.
#' @param role Not used.
#' @param trained Logical indicating if the step has been trained.
#' @param skip Logical. Should the step be skipped when baking?
#' @param id Unique step identifier.
#'
#' @return An updated recipe with the new step added.
#'
#' @details
#' PARAFAC (also known as CANDECOMP/PARAFAC or CP decomposition) decomposes a
#' three-way or four-way array into a sum of rank-one tensors. For measurement
#' data like EEM (excitation-emission matrices) or 3D LC-DAD acquisitions,
#' this extracts interpretable components corresponding to underlying chemical
#' species.
#'
#' ## Requirements
#'
#' - Input must be `measure_nd_list` with two or three content dimensions
#' - All samples must have the same grid (regular, aligned)
#' - The `multiway` package must be installed (in Suggests)
#'
#' ## Output
#'
#' With the default prefix, each selected column produces scores named
#' `parafac_<column>_1` through `parafac_<column>_<n_components>`.
#' Baking fits the component scores jointly to the learned loading patterns.
#' This avoids counting shared signal twice when those patterns overlap.
#' For linearly dependent patterns it returns the least-squares solution with
#' the smallest Euclidean norm.
#'
#' Missing cells are omitted from the projection for each sample. More than
#' 10% missing cells across the batch is an error. Within that limit, a
#' sample with no observed values receives missing scores.
#'
#' @seealso [step_measure_tucker()] for Tucker decomposition
#'
#' @family measure-multiway
#' @export
#'
#' @examples
#' \dontrun{
#' library(recipes)
#'
#' # After ingesting EEM data as 2D measurements
#' rec <- recipe(concentration ~ ., data = eem_data) |>
#'   step_measure_input_long(
#'     fluorescence,
#'     location = vars(excitation, emission)
#'   ) |>
#'   step_measure_parafac(n_components = 3) |>
#'   prep()
#'
#' bake(rec, new_data = NULL)
#' }
step_measure_parafac <- function(
  recipe,
  ...,
  n_components = 3L,
  center = TRUE,
  scale = FALSE,
  max_iter = 500L,
  tol = 1e-6,
  prefix = "parafac_",
  role = "predictor",
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_parafac")
) {
  recipes::add_step(
    recipe,
    step_measure_parafac_new(
      terms = rlang::enquos(...),
      n_components = as.integer(n_components),
      center = center,
      scale = scale,
      max_iter = as.integer(max_iter),
      tol = tol,
      prefix = prefix,
      loadings = NULL,
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

step_measure_parafac_new <- function(
  terms,
  n_components,
  center,
  scale,
  max_iter,
  tol,
  prefix,
  loadings,
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
    subclass = "measure_parafac",
    terms = terms,
    n_components = n_components,
    center = center,
    scale = scale,
    max_iter = max_iter,
    tol = tol,
    prefix = prefix,
    loadings = loadings,
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
prep.step_measure_parafac <- function(x, training, info = NULL, ...) {
  # Check for multiway package
  rlang::check_installed(
    "multiway",
    reason = "for PARAFAC decomposition"
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
    cli::cli_abort("No nD measure columns found for PARAFAC.")
  }

  # Process each column
  loadings_list <- list()
  center_list <- list()
  scale_list <- list()
  grid_list <- list()

  for (col in measure_cols) {
    result <- .fit_parafac(
      training[[col]],
      n_components = x$n_components,
      center = x$center,
      scale = x$scale,
      max_iter = x$max_iter,
      tol = x$tol
    )
    loadings_list[[col]] <- result$loadings
    center_list[[col]] <- result$center_values
    scale_list[[col]] <- result$scale_values
    grid_list[[col]] <- result$grid_info
  }

  step_measure_parafac_new(
    terms = x$terms,
    n_components = x$n_components,
    center = x$center,
    scale = x$scale,
    max_iter = x$max_iter,
    tol = x$tol,
    prefix = x$prefix,
    loadings = loadings_list,
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
bake.step_measure_parafac <- function(object, new_data, ...) {
  for (col in object$measure_cols) {
    scores <- .project_parafac(
      new_data[[col]],
      loadings = object$loadings[[col]],
      center_values = object$center_values[[col]],
      scale_values = object$scale_values[[col]],
      grid_info = object$grid_info[[col]]
    )

    # Add score columns
    col_names <- paste0(object$prefix, col, "_", seq_len(object$n_components))
    for (i in seq_len(object$n_components)) {
      new_data[[col_names[i]]] <- scores[, i]
    }

    # Remove original nD column
    new_data[[col]] <- NULL
  }

  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_parafac <- function(
  x,
  width = max(20, options()$width - 30),
  ...
) {
  title <- paste0("PARAFAC decomposition (", x$n_components, " components) ")
  if (x$trained) {
    cat(title, "on ", paste(x$measure_cols, collapse = ", "), sep = "")
  } else {
    cat(title, "<pending>", sep = "")
  }
  cat("\n")
  invisible(x)
}

#' @rdname tidy.recipe
#' @param type For PARAFAC steps, either `"loadings"` or `"parameters"`.
#' @export
#' @keywords internal
tidy.step_measure_parafac <- function(x, type = "parameters", ...) {
  if (type == "loadings" && is_trained(x)) {
    # Return loadings for each mode
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

  # Default: return parameters
  if (is_trained(x)) {
    tibble::tibble(
      terms = unname(x$measure_cols),
      n_components = x$n_components,
      center = x$center,
      scale = x$scale,
      id = x$id
    )
  } else {
    tibble::tibble(
      terms = rlang::na_chr,
      n_components = x$n_components,
      center = x$center,
      scale = x$scale,
      id = x$id
    )
  }
}

# ------------------------------------------------------------------------------
# Internal helpers
# ------------------------------------------------------------------------------

.fit_parafac <- function(
  measure_nd_list,
  n_components,
  center,
  scale,
  max_iter,
  tol
) {
  array_result <- .nd_list_to_array(measure_nd_list, method = "PARAFAC")
  preprocessed <- .fit_multiway_preprocessing(array_result$array, center, scale)

  # Fit PARAFAC using multiway
  fit <- multiway::parafac(
    preprocessed$array,
    nfac = n_components,
    nstart = 10,
    maxit = max_iter,
    ctol = tol
  )

  mode_names <- c("A", "B", "C", "D")[seq_along(dim(preprocessed$array))]
  list(
    loadings = unname(fit[mode_names]),
    center_values = preprocessed$center_values,
    scale_values = preprocessed$scale_values,
    grid_info = array_result$grid_info
  )
}

.project_parafac <- function(
  measure_nd_list,
  loadings,
  center_values,
  scale_values,
  grid_info
) {
  array_result <- .nd_list_to_array(
    measure_nd_list,
    grid_info,
    method = "PARAFAC"
  )
  X <- .apply_multiway_preprocessing(
    array_result$array,
    center_values = center_values,
    scale_values = scale_values
  )

  n_samples <- dim(X)[1]
  n_components <- ncol(loadings[[1]])
  design <- .parafac_projection_design(loadings[-1L])
  # Each row holds one spectrum in column-major coordinate order.
  values <- matrix(X, nrow = n_samples, ncol = nrow(design))
  missing <- is.na(values)
  complete <- rowSums(missing) == 0
  scores <- matrix(NA_real_, nrow = n_samples, ncol = n_components)

  if (any(complete)) {
    operator <- .parafac_joint_ls_operator(design)
    scores[complete, ] <- values[complete, , drop = FALSE] %*% t(operator)
  }

  for (i in which(!complete)) {
    observed <- !missing[i, ]
    if (any(observed)) {
      operator <- .parafac_joint_ls_operator(design[observed, , drop = FALSE])
      scores[i, ] <- operator %*% values[i, observed]
    }
  }

  scores
}

# Each column is one rank-one pattern, with the first coordinate varying fastest.
.parafac_projection_design <- function(content_loadings) {
  n_components <- ncol(content_loadings[[1L]])
  n_cells <- prod(vapply(content_loadings, nrow, integer(1)))
  design <- matrix(0, nrow = n_cells, ncol = n_components)
  for (component in seq_len(n_components)) {
    factors <- lapply(content_loadings, function(mode) mode[, component])
    design[, component] <- Reduce(outer, factors)
  }
  design
}

# A relative singular-value threshold preserves the solution if signal units change.
.parafac_joint_ls_operator <- function(design) {
  decomposition <- svd(design)
  tolerance <- max(dim(design)) * max(decomposition$d) * .Machine$double.eps
  keep <- decomposition$d > tolerance
  if (!any(keep)) {
    return(matrix(0, nrow = ncol(design), ncol = nrow(design)))
  }

  left <- decomposition$u[, keep, drop = FALSE]
  right <- decomposition$v[, keep, drop = FALSE]
  right %*% (t(left) / decomposition$d[keep])
}
