# ==============================================================================
# Batch Effect Correction Steps
#
# This file contains batch correction preprocessing steps:
# - step_measure_batch_reference: Reference-based batch correction
# ==============================================================================

#' Reference-Based Batch Correction
#'
#' `step_measure_batch_reference()` creates a *specification* of a recipe step
#' that corrects for batch effects using reference samples. This is a simpler
#' alternative to ComBat-style correction that doesn't require heavy dependencies.
#'
#' @param recipe A recipe object.
#' @param ... One or more selector functions to choose feature columns.
#' @param batch_col Name of the column containing batch identifiers.
#' @param sample_type_col Name of the column containing sample type.
#' @param reference_type Value(s) in `sample_type_col` that identify reference
#'   samples to use for batch correction. Default is `"reference"`.
#' @param method Correction method:
#'   - `"median_ratio"` (default): Scale by ratio of reference medians
#'   - `"mean_ratio"`: Scale by ratio of reference means
#'   - `"median_center"`: Center batches to common median
#'   - `"mean_center"`: Center batches to common mean
#' @param target_batch Which batch to use as reference. Default is the first
#'   batch (alphabetically). Can also be `"global"` to use global reference
#'   median/mean.
#' @param min_ref Minimum number of reference samples per batch. Default is 2.
#' @param role Not used by this step.
#' @param trained Logical indicating if the step has been trained.
#' @param skip Logical. Should the step be skipped when baking?
#' @param id Unique step identifier.
#'
#' @return An updated recipe with the new step added.
#'
#' @details
#' ## Correction Methods
#'
#' **Median/Mean Ratio**: Multiplies all samples in a batch by:
#' `target_reference / batch_reference`
#'
#' This preserves relative differences within batches while aligning
#' batch centers.
#'
#' **Median/Mean Center**: Subtracts the difference between batch reference
#' and target reference. This is appropriate for log-transformed data.
#'
#' ## Reference Samples
#'
#' Reference samples should be identical samples run in each batch
#' (e.g., pooled QC, reference material). The step will error if any
#' batch lacks sufficient reference samples.
#'
#' @family batch-correction
#' @seealso [step_measure_drift_qc_loess()] for within-batch drift correction.
#'
#' @export
#'
#' @examples
#' library(recipes)
#'
#' # Data with batch effects
#' data <- data.frame(
#'   sample_id = paste0("S", 1:20),
#'   sample_type = rep(c("reference", "unknown", "unknown", "unknown", "reference"), 4),
#'   batch_id = rep(c("B1", "B1", "B2", "B2"), 5),
#'   feature1 = c(rep(100, 10), rep(120, 10)) + rnorm(20, sd = 5),  # Batch effect
#'   feature2 = c(rep(50, 10), rep(45, 10)) + rnorm(20, sd = 2)
#' )
#'
#' rec <- recipe(~ ., data = data) |>
#'   update_role(sample_id, new_role = "id") |>
#'   step_measure_batch_reference(feature1, feature2, batch_col = "batch_id") |>
#'   prep()
#'
#' corrected <- bake(rec, new_data = NULL)
step_measure_batch_reference <- function(
    recipe,
    ...,
    batch_col = "batch_id",
    sample_type_col = "sample_type",
    reference_type = "reference",
    method = c("median_ratio", "mean_ratio", "median_center", "mean_center"),
    target_batch = NULL,
    min_ref = 2,
    role = NA,
    trained = FALSE,
    skip = FALSE,
    id = recipes::rand_id("measure_batch_reference")) {

  method <- match.arg(method)

  recipes::add_step(
    recipe,
    step_measure_batch_reference_new(
      terms = rlang::enquos(...),
      batch_col = batch_col,
      sample_type_col = sample_type_col,
      reference_type = reference_type,
      method = method,
      target_batch = target_batch,
      min_ref = min_ref,
      correction_factors = NULL,
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_measure_batch_reference_new <- function(
    terms,
    batch_col,
    sample_type_col,
    reference_type,
    method,
    target_batch,
    min_ref,
    correction_factors,
    role,
    trained,
    skip,
    id) {

  recipes::step(
    subclass = "measure_batch_reference",
    terms = terms,
    batch_col = batch_col,
    sample_type_col = sample_type_col,
    reference_type = reference_type,
    method = method,
    target_batch = target_batch,
    min_ref = min_ref,
    correction_factors = correction_factors,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_batch_reference <- function(x, training, info = NULL, ...) {

  # Validate required columns
  if (!x$batch_col %in% names(training)) {
    cli::cli_abort("Column {.field {x$batch_col}} not found in data.")
  }
  if (!x$sample_type_col %in% names(training)) {
    cli::cli_abort("Column {.field {x$sample_type_col}} not found in data.")
  }

  # Get feature columns
  col_names <- recipes::recipes_eval_select(x$terms, training, info)

  if (length(col_names) == 0) {
    # Use all numeric columns except batch_col
    numeric_cols <- names(training)[vapply(training, is.numeric, logical(1))]
    col_names <- numeric_cols
  }

  if (length(col_names) == 0) {
    cli::cli_abort("No feature columns found for batch correction.")
  }

  # Get batch and sample type info
  batches <- unique(training[[x$batch_col]])
  sample_types <- training[[x$sample_type_col]]
  is_ref <- sample_types %in% x$reference_type

  # Check for reference samples in each batch
  for (batch in batches) {
    n_ref <- sum(is_ref & training[[x$batch_col]] == batch)
    if (n_ref < x$min_ref) {
      cli::cli_abort(
        "Batch {.val {batch}} has only {n_ref} reference samples (need >= {x$min_ref})."
      )
    }
  }

  # Determine target batch
  target_batch <- x$target_batch
  if (is.null(target_batch)) {
    target_batch <- sort(batches)[1]
  }

  # Calculate correction factors for each feature and batch
  correction_factors <- list()

  for (col in col_names) {
    values <- training[[col]]
    batch_factors <- list()

    # Calculate reference summary for each batch
    ref_summaries <- list()
    for (batch in batches) {
      batch_refs <- values[is_ref & training[[x$batch_col]] == batch]
      ref_summaries[[batch]] <- switch(
        x$method,
        median_ratio = stats::median(batch_refs, na.rm = TRUE),
        mean_ratio = mean(batch_refs, na.rm = TRUE),
        median_center = stats::median(batch_refs, na.rm = TRUE),
        mean_center = mean(batch_refs, na.rm = TRUE)
      )
    }

    # Calculate target reference
    if (target_batch == "global") {
      all_refs <- values[is_ref]
      target_ref <- switch(
        x$method,
        median_ratio = stats::median(all_refs, na.rm = TRUE),
        mean_ratio = mean(all_refs, na.rm = TRUE),
        median_center = stats::median(all_refs, na.rm = TRUE),
        mean_center = mean(all_refs, na.rm = TRUE)
      )
    } else {
      target_ref <- ref_summaries[[target_batch]]
    }

    # Calculate correction factors
    for (batch in batches) {
      if (grepl("ratio", x$method)) {
        batch_factors[[batch]] <- target_ref / ref_summaries[[batch]]
      } else {
        batch_factors[[batch]] <- target_ref - ref_summaries[[batch]]
      }
    }

    correction_factors[[col]] <- list(
      batch_factors = batch_factors,
      target_ref = target_ref,
      ref_summaries = ref_summaries
    )
  }

  step_measure_batch_reference_new(
    terms = x$terms,
    batch_col = x$batch_col,
    sample_type_col = x$sample_type_col,
    reference_type = x$reference_type,
    method = x$method,
    target_batch = target_batch,
    min_ref = x$min_ref,
    correction_factors = correction_factors,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_batch_reference <- function(object, new_data, ...) {

  batches <- new_data[[object$batch_col]]

  for (col in names(object$correction_factors)) {
    factors <- object$correction_factors[[col]]
    values <- new_data[[col]]

    corrected <- values
    for (batch in names(factors$batch_factors)) {
      batch_idx <- batches == batch
      factor <- factors$batch_factors[[batch]]

      if (grepl("ratio", object$method)) {
        corrected[batch_idx] <- values[batch_idx] * factor
      } else {
        corrected[batch_idx] <- values[batch_idx] + factor
      }
    }

    new_data[[col]] <- corrected
  }

  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_batch_reference <- function(x, width = max(20, options()$width - 30), ...) {
  title <- paste0("Reference-based batch correction (", x$method, ")")

  if (x$trained) {
    n_features <- length(x$correction_factors)
    n_batches <- length(x$correction_factors[[1]]$batch_factors)
    cat(title, " [", n_features, " features, ", n_batches, " batches]\n", sep = "")
  } else {
    cat(title, "\n", sep = "")
  }

  invisible(x)
}

#' @rdname tidy.recipe
#' @export
tidy.step_measure_batch_reference <- function(x, ...) {
  if (!x$trained) {
    return(tibble::tibble(
      feature = character(),
      batch = character(),
      correction_factor = double(),
      target_ref = double()
    ))
  }

  results <- lapply(names(x$correction_factors), function(col) {
    factors <- x$correction_factors[[col]]
    tibble::tibble(
      feature = col,
      batch = names(factors$batch_factors),
      correction_factor = unlist(factors$batch_factors),
      reference_value = unlist(factors$ref_summaries),
      target_ref = factors$target_ref
    )
  })

  dplyr::bind_rows(results)
}
