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
#' PARAFAC (also known as CANDECOMP/PARAFAC or CP decomposition) decomposes
#' a three-way or higher array into a sum of rank-one tensors. For measurement
#' data like EEM (excitation-emission matrices) or LC-DAD, this extracts
#' interpretable components corresponding to underlying chemical species.
#'
#' ## Requirements
#'
#' - Input must be `measure_nd_list` with 2+ dimensions
#' - All samples must have the same grid (regular, aligned)
#' - The `multiway` package must be installed (in Suggests)
#'
#' ## Output
#'
#' Creates numeric feature columns: `parafac_1`, `parafac_2`, ..., `parafac_n`
#' representing each sample's scores on the extracted components.
#'
#' @note
#' This step requires the `multiway` package. Install with:
#' `install.packages("multiway")`
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
      terms = x$measure_cols,
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
  # Convert to 3D array (samples x dim1 x dim2)
  array_result <- .nd_list_to_array(measure_nd_list)
  X <- array_result$array
  grid_info <- array_result$grid_info

  n_samples <- dim(X)[1]

  # Center and scale along sample dimension
  center_values <- NULL
  scale_values <- NULL

  if (center) {
    center_values <- apply(X, c(2, 3), mean, na.rm = TRUE)
    for (i in seq_len(n_samples)) {
      X[i, , ] <- X[i, , ] - center_values
    }
  }

  if (scale) {
    scale_values <- apply(X, c(2, 3), stats::sd, na.rm = TRUE)
    scale_values[scale_values == 0] <- 1 # Avoid division by zero
    for (i in seq_len(n_samples)) {
      X[i, , ] <- X[i, , ] / scale_values
    }
  }

  # Fit PARAFAC using multiway
  fit <- multiway::parafac(
    X,
    nfac = n_components,
    nstart = 10,
    maxit = max_iter,
    ctol = tol
  )

  # multiway::parafac returns $A, $B, $C as separate matrices (not $A as a list)
  # $A = sample scores (n_samples x nfac)
  # $B = mode 2 loadings (dim1 x nfac)
  # $C = mode 3 loadings (dim2 x nfac)
  # Store as list for consistent access
  list(
    loadings = list(fit$A, fit$B, fit$C),
    center_values = center_values,
    scale_values = scale_values,
    grid_info = grid_info
  )
}

.project_parafac <- function(
  measure_nd_list,
  loadings,
  center_values,
  scale_values,
  grid_info
) {
  # Convert to array
  array_result <- .nd_list_to_array(measure_nd_list)
  X <- array_result$array

  n_samples <- dim(X)[1]
  n_components <- ncol(loadings[[1]])

  # Apply centering and scaling

  if (!is.null(center_values)) {
    for (i in seq_len(n_samples)) {
      X[i, , ] <- X[i, , ] - center_values
    }
  }

  if (!is.null(scale_values)) {
    for (i in seq_len(n_samples)) {
      X[i, , ] <- X[i, , ] / scale_values
    }
  }

  # Project onto loadings to get scores
  # For PARAFAC, we need to solve for scores given fixed loadings
  scores <- matrix(0, nrow = n_samples, ncol = n_components)

  for (i in seq_len(n_samples)) {
    Xi <- matrix(X[i, , ], nrow = dim(X)[2], ncol = dim(X)[3])
    # Use least squares projection
    # Xi ~= sum_r scores[i,r] * outer(loadings[[2]][,r], loadings[[3]][,r])
    for (r in seq_len(n_components)) {
      # Compute score as inner product
      outer_r <- outer(loadings[[2]][, r], loadings[[3]][, r])
      scores[i, r] <- sum(Xi * outer_r, na.rm = TRUE) /
        sum(outer_r^2, na.rm = TRUE)
    }
  }

  scores
}

.nd_list_to_array <- function(measure_nd_list) {
  n_samples <- length(measure_nd_list)

  # Get grid info from first sample
  first <- measure_nd_list[[1]]
  ndim <- measure_ndim(first)

  if (ndim < 2) {
    cli::cli_abort("PARAFAC requires at least 2D measurement data.")
  }

  # Get unique values for each dimension
  loc_cols <- paste0("location_", seq_len(ndim))
  dim_sizes <- vapply(
    loc_cols,
    function(lc) {
      length(unique(first[[lc]]))
    },
    integer(1)
  )

  grid_info <- lapply(loc_cols, function(lc) {
    sort(unique(first[[lc]]))
  })
  names(grid_info) <- loc_cols

  # Create array: samples x dim1 x dim2 x ...
  arr <- array(NA_real_, dim = c(n_samples, dim_sizes))

  for (i in seq_len(n_samples)) {
    m <- measure_nd_list[[i]]

    # Fill array based on location indices
    for (j in seq_len(nrow(m))) {
      idx <- vector("integer", ndim)
      for (d in seq_len(ndim)) {
        loc_val <- m[[loc_cols[d]]][j]
        idx[d] <- match(loc_val, grid_info[[loc_cols[d]]])
      }
      # Build index expression: arr[i, idx[1], idx[2], ...]
      arr[cbind(i, matrix(idx, nrow = 1))] <- m$value[j]
    }
  }

  list(array = arr, grid_info = grid_info)
}
