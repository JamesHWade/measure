# ==============================================================================
# MCR-ALS (Multivariate Curve Resolution - Alternating Least Squares) Step
#
# Experimental: Extracts pure component spectra and concentration profiles.
# ==============================================================================

#' MCR-ALS Decomposition for Multi-Dimensional Data
#'
#' `step_measure_mcr_als()` creates a *specification* of a recipe step that
#' applies Multivariate Curve Resolution - Alternating Least Squares (MCR-ALS)
#' to multi-dimensional measurement data.
#'
#' @param recipe A recipe object.
#' @param ... One or more selector functions to choose measure columns.
#'   If empty, all nD measure columns are used.
#' @param n_components Number of components to extract. Default is 3.
#' @param max_iter Maximum number of iterations. Default is 500.
#' @param tol Convergence tolerance. Default is 1e-6.
#' @param non_negativity Logical. Should non-negativity constraints be applied?
#'   Default is `TRUE`.
#' @param unimodality Logical. Should unimodality constraints be applied?
#'   Default is `FALSE`.
#' @param prefix Prefix for output column names. Default is `"mcr_"`.
#' @param role Not used.
#' @param trained Logical indicating if the step has been trained.
#' @param skip Logical. Should the step be skipped when baking?
#' @param id Unique step identifier.
#'
#' @return An updated recipe with the new step added.
#'
#' @details
#' MCR-ALS is a powerful technique for resolving mixtures into pure component
#' contributions. It's particularly useful for:
#'
#' - Chromatographic data (time x wavelength)
#' - Spectroscopic mixtures
#' - Process analytical data
#'
#' Unlike PARAFAC, MCR-ALS is a bilinear method that works on 2D data
#' (samples unfolded if 3D). It allows flexible constraints like
#' non-negativity and unimodality.
#'
#' ## Experimental Status
#'
#' This step is experimental and its API may change in future versions.
#'
#' ## Requirements
#'
#' - Input must be `measure_nd_list` with 2 dimensions
#' - All samples must have the same grid (regular, aligned)
#'
#' @note
#' This is an **experimental** feature. The implementation uses a simple
#' ALS algorithm without advanced constraints. For production use, consider
#' using dedicated MCR-ALS packages.
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
#' # After ingesting chromatographic data
#' rec <- recipe(concentration ~ ., data = chrom_data) |>
#'   step_measure_input_long(
#'     absorbance,
#'     location = vars(time, wavelength)
#'   ) |>
#'   step_measure_mcr_als(n_components = 3) |>
#'   prep()
#'
#' bake(rec, new_data = NULL)
#' }
step_measure_mcr_als <- function(
  recipe,
  ...,
  n_components = 3L,
  max_iter = 500L,
  tol = 1e-6,
  non_negativity = TRUE,
  unimodality = FALSE,
  prefix = "mcr_",
  role = "predictor",
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_mcr_als")
) {
  cli::cli_warn(
    c(
      "{.fn step_measure_mcr_als} is experimental.",
      "i" = "The API may change in future versions."
    ),
    .frequency = "once",
    .frequency_id = "mcr_als_experimental"
  )

  recipes::add_step(
    recipe,
    step_measure_mcr_als_new(
      terms = rlang::enquos(...),
      n_components = as.integer(n_components),
      max_iter = as.integer(max_iter),
      tol = tol,
      non_negativity = non_negativity,
      unimodality = unimodality,
      prefix = prefix,
      spectra = NULL,
      concentrations = NULL,
      grid_info = NULL,
      measure_cols = NULL,
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_measure_mcr_als_new <- function(
  terms,
  n_components,
  max_iter,
  tol,
  non_negativity,
  unimodality,
  prefix,
  spectra,
  concentrations,
  grid_info,
  measure_cols,
  role,
  trained,
  skip,
  id
) {
  recipes::step(
    subclass = "measure_mcr_als",
    terms = terms,
    n_components = n_components,
    max_iter = max_iter,
    tol = tol,
    non_negativity = non_negativity,
    unimodality = unimodality,
    prefix = prefix,
    spectra = spectra,
    concentrations = concentrations,
    grid_info = grid_info,
    measure_cols = measure_cols,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_mcr_als <- function(x, training, info = NULL, ...) {
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
    cli::cli_abort("No nD measure columns found for MCR-ALS.")
  }

  # Process each column
  spectra_list <- list()
  conc_list <- list()
  grid_list <- list()

  for (col in measure_cols) {
    result <- .fit_mcr_als(
      training[[col]],
      n_components = x$n_components,
      max_iter = x$max_iter,
      tol = x$tol,
      non_negativity = x$non_negativity,
      unimodality = x$unimodality
    )
    spectra_list[[col]] <- result$spectra
    conc_list[[col]] <- result$concentrations
    grid_list[[col]] <- result$grid_info
  }

  step_measure_mcr_als_new(
    terms = x$terms,
    n_components = x$n_components,
    max_iter = x$max_iter,
    tol = x$tol,
    non_negativity = x$non_negativity,
    unimodality = x$unimodality,
    prefix = x$prefix,
    spectra = spectra_list,
    concentrations = conc_list,
    grid_info = grid_list,
    measure_cols = measure_cols,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_mcr_als <- function(object, new_data, ...) {
  for (col in object$measure_cols) {
    scores <- .project_mcr_als(
      new_data[[col]],
      spectra = object$spectra[[col]],
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
print.step_measure_mcr_als <- function(
  x,
  width = max(20, options()$width - 30),
  ...
) {
  title <- paste0("MCR-ALS decomposition (", x$n_components, " components) ")
  if (x$trained) {
    cat(title, "on ", paste(x$measure_cols, collapse = ", "), sep = "")
  } else {
    cat(title, "<pending>", sep = "")
  }
  if (x$non_negativity) {
    cat(" [non-neg]")
  }
  cat("\n")
  invisible(x)
}

#' @rdname tidy.recipe
#' @export
#' @keywords internal
tidy.step_measure_mcr_als <- function(x, type = "parameters", ...) {
  if (type == "spectra" && is_trained(x)) {
    result <- tibble::tibble()
    for (col in x$measure_cols) {
      spectra <- x$spectra[[col]]
      spec_df <- tibble::as_tibble(spectra, .name_repair = "minimal")
      names(spec_df) <- paste0("component_", seq_len(ncol(spectra)))
      spec_df$column <- col
      spec_df$location <- seq_len(nrow(spectra))
      result <- dplyr::bind_rows(result, spec_df)
    }
    result$id <- x$id
    return(result)
  }

  if (is_trained(x)) {
    tibble::tibble(
      terms = unname(x$measure_cols),
      n_components = x$n_components,
      non_negativity = x$non_negativity,
      unimodality = x$unimodality,
      id = x$id
    )
  } else {
    tibble::tibble(
      terms = rlang::na_chr,
      n_components = x$n_components,
      non_negativity = x$non_negativity,
      unimodality = x$unimodality,
      id = x$id
    )
  }
}

# ------------------------------------------------------------------------------
# Internal helpers
# ------------------------------------------------------------------------------

.fit_mcr_als <- function(
  measure_nd_list,
  n_components,
  max_iter,
  tol,
  non_negativity,
  unimodality
) {
  # Convert to 3D array and unfold to 2D matrix
  array_result <- .nd_list_to_array(measure_nd_list)
  X <- array_result$array
  grid_info <- array_result$grid_info

  n_samples <- dim(X)[1]
  n_rows <- dim(X)[2]
  n_cols <- dim(X)[3]

  # Unfold: stack samples along first mode
  # Result is (n_samples * n_rows) x n_cols
  D <- matrix(0, nrow = n_samples * n_rows, ncol = n_cols)
  for (i in seq_len(n_samples)) {
    start_row <- (i - 1) * n_rows + 1
    end_row <- i * n_rows
    D[start_row:end_row, ] <- X[i, , ]
  }

  # Initialize spectra using SVD
  svd_result <- svd(D, nu = 0, nv = n_components)
  S <- svd_result$v[, seq_len(n_components), drop = FALSE]

  if (non_negativity) {
    S <- pmax(S, 0)
  }

  # ALS iterations
  C <- NULL
  converged <- FALSE
  final_iter <- max_iter

  for (iter in seq_len(max_iter)) {
    # Update concentrations: D = C * S'
    # Use tryCatch for matrix inversion in case regularization is insufficient
    StS <- t(S) %*% S + diag(1e-10, n_components)
    C_new <- tryCatch(
      D %*% S %*% solve(StS),
      error = function(e) {
        cli::cli_abort(
          c(
            "MCR-ALS concentration update failed due to singular matrix.",
            "i" = "Try reducing {.arg n_components} or check data quality."
          )
        )
      }
    )

    if (non_negativity) {
      C_new <- pmax(C_new, 0)
    }

    # Update spectra: D' = S * C'
    CtC <- t(C_new) %*% C_new + diag(1e-10, n_components)
    S_new <- tryCatch(
      t(D) %*% C_new %*% solve(CtC),
      error = function(e) {
        cli::cli_abort(
          c(
            "MCR-ALS spectra update failed due to singular matrix.",
            "i" = "Try reducing {.arg n_components} or check data quality."
          )
        )
      }
    )

    if (non_negativity) {
      S_new <- pmax(S_new, 0)
    }

    # Check convergence
    if (!is.null(C)) {
      diff <- sum((C_new - C)^2) / (sum(C^2) + 1e-10)
      if (diff < tol) {
        converged <- TRUE
        final_iter <- iter
        break
      }
    }

    C <- C_new
    S <- S_new
  }

  # Warn if algorithm did not converge

  if (!converged) {
    cli::cli_warn(
      c(
        "MCR-ALS did not converge after {max_iter} iterations.",
        "i" = "Consider increasing {.arg max_iter} or {.arg tol}."
      )
    )
  }

  # Extract per-sample concentration profiles
  # Reshape C from (n_samples * n_rows) x n_components to n_samples x n_rows x n_components
  concentrations <- array(0, dim = c(n_samples, n_rows, n_components))
  for (i in seq_len(n_samples)) {
    start_row <- (i - 1) * n_rows + 1
    end_row <- i * n_rows
    concentrations[i, , ] <- C[start_row:end_row, ]
  }

  list(
    spectra = S,
    concentrations = concentrations,
    grid_info = grid_info
  )
}

.project_mcr_als <- function(measure_nd_list, spectra, grid_info) {
  # Convert to array
  array_result <- .nd_list_to_array(measure_nd_list)
  X <- array_result$array

  n_samples <- dim(X)[1]
  n_rows <- dim(X)[2]
  n_cols <- dim(X)[3]
  n_components <- ncol(spectra)

  # For each sample, solve D_i = C_i * S' for C_i
  scores <- matrix(0, nrow = n_samples, ncol = n_components)

  # Pre-compute spectra projection matrix
  StS <- t(spectra) %*% spectra + diag(1e-10, n_components)
  StS_inv <- tryCatch(
    solve(StS),
    error = function(e) {
      cli::cli_abort(
        c(
          "MCR-ALS projection failed due to singular spectra matrix.",
          "i" = "The trained model may have degenerate components."
        )
      )
    }
  )

  for (i in seq_len(n_samples)) {
    Di <- matrix(X[i, , ], nrow = n_rows, ncol = n_cols)

    # Solve for concentrations: Di = Ci * S'
    # Ci = Di * S * (S' * S)^-1
    Ci <- Di %*% spectra %*% StS_inv

    # Summarize concentration profiles to single scores (e.g., sum or max)
    scores[i, ] <- colSums(Ci)
  }

  scores
}
