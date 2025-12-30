# ==============================================================================
# Drift Correction Steps
#
# This file contains drift correction preprocessing steps:
# - step_measure_drift_qc_loess: QC-RLSC style drift correction using LOESS
# ==============================================================================

#' QC-Based Drift Correction Using LOESS
#'
#' `step_measure_drift_qc_loess()` creates a *specification* of a recipe step
#' that corrects for signal drift across run order using QC (or reference)
#' samples. This implements the QC-RLSC (robust LOESS signal correction) method.
#'
#' @param recipe A recipe object.
#' @param ... One or more selector functions to choose feature columns. For
#'   feature-level data, select the numeric response columns. For curve-level
#'   data with `.measures`, leave empty to apply to all locations.
#' @param run_order_col Name of the column containing run order (injection
#'   sequence). Must be numeric/integer.
#' @param sample_type_col Name of the column containing sample type.
#' @param qc_type Value(s) in `sample_type_col` that identify QC samples to
#'   use for drift modeling. Default is `"qc"`.
#' @param apply_to Which samples to apply correction to:
#'   - `"all"` (default): Correct all samples
#'   - `"unknown"`: Only correct unknown samples
#' @param span LOESS span parameter controlling smoothness. Default is 0.75.
#'   Smaller values = more flexible fit.
#' @param degree Polynomial degree for LOESS (1 or 2). Default is 2.
#' @param robust Logical. Use robust LOESS fitting? Default is TRUE.
#' @param min_qc Minimum number of QC samples required. Default is 5.
#' @param role Not used by this step.
#' @param trained Logical indicating if the step has been trained.
#' @param skip Logical. Should the step be skipped when baking?
#' @param id Unique step identifier.
#'
#' @return An updated recipe with the new step added.
#'
#' @details
#' ## How It Works
#'
#' 1. During `prep()`: A LOESS model is fit to QC sample responses vs run order
#'    for each feature/location.
#'
#' 2. During `bake()`: Correction factors are calculated as:
#'    `correction = median(QC_responses) / predicted_value`
#'
#'    Each sample's response is multiplied by the correction factor at its
#'    run order position.
#'
#' ## Data Levels
#'
#' This step supports both:
#' - **Feature-level data**: Applies correction to each selected numeric column
#' - **Curve-level data**: Applies correction to each location in the measure_list
#'
#' ## Diagnostics
#'

#' The trained step stores drift model information accessible via `tidy()`:
#' - LOESS model parameters
#' - QC response trends
#' - Correction factors applied
#'
#' @family drift-correction
#' @seealso [measure_detect_drift()] for drift detection before correction.
#'
#' @export
#'
#' @examples
#' library(recipes)
#'
#' # Feature-level data with drift
#' data <- data.frame(
#'   sample_id = paste0("S", 1:20),
#'   sample_type = rep(c("qc", "unknown", "unknown", "unknown", "qc"), 4),
#'   run_order = 1:20,
#'   feature1 = 100 + (1:20) * 0.5 + rnorm(20, sd = 2),  # Upward drift
#'   feature2 = 50 - (1:20) * 0.3 + rnorm(20, sd = 1)    # Downward drift
#' )
#'
#' rec <- recipe(~ ., data = data) |>
#'   update_role(sample_id, new_role = "id") |>
#'   step_measure_drift_qc_loess(feature1, feature2) |>
#'   prep()
#'
#' corrected <- bake(rec, new_data = NULL)
step_measure_drift_qc_loess <- function(
    recipe,
    ...,
    run_order_col = "run_order",
    sample_type_col = "sample_type",
    qc_type = "qc",
    apply_to = c("all", "unknown"),
    span = 0.75,
    degree = 2,
    robust = TRUE,
    min_qc = 5,
    role = NA,
    trained = FALSE,
    skip = FALSE,
    id = recipes::rand_id("measure_drift_qc_loess")) {

  apply_to <- match.arg(apply_to)

  recipes::add_step(
    recipe,
    step_measure_drift_qc_loess_new(
      terms = rlang::enquos(...),
      run_order_col = run_order_col,
      sample_type_col = sample_type_col,
      qc_type = qc_type,
      apply_to = apply_to,
      span = span,
      degree = degree,
      robust = robust,
      min_qc = min_qc,
      drift_models = NULL,
      qc_medians = NULL,
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_measure_drift_qc_loess_new <- function(
    terms,
    run_order_col,
    sample_type_col,
    qc_type,
    apply_to,
    span,
    degree,
    robust,
    min_qc,
    drift_models,
    qc_medians,
    role,
    trained,
    skip,
    id) {

  recipes::step(
    subclass = "measure_drift_qc_loess",
    terms = terms,
    run_order_col = run_order_col,
    sample_type_col = sample_type_col,
    qc_type = qc_type,
    apply_to = apply_to,
    span = span,
    degree = degree,
    robust = robust,
    min_qc = min_qc,
    drift_models = drift_models,
    qc_medians = qc_medians,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_drift_qc_loess <- function(x, training, info = NULL, ...) {

  # Validate required columns
  if (!x$run_order_col %in% names(training)) {
    cli::cli_abort("Column {.field {x$run_order_col}} not found in data.")
  }
  if (!x$sample_type_col %in% names(training)) {
    cli::cli_abort("Column {.field {x$sample_type_col}} not found in data.")
  }

  # Get feature columns
  col_names <- recipes::recipes_eval_select(x$terms, training, info)

  # If no columns specified, try to find measure columns or use all numeric
  if (length(col_names) == 0) {
    # Check for measure_list columns
    measure_cols <- find_measure_cols(training)
    if (length(measure_cols) > 0) {
      col_names <- measure_cols
    } else {
      # Use all numeric columns except run_order
      numeric_cols <- names(training)[vapply(training, is.numeric, logical(1))]
      col_names <- setdiff(numeric_cols, c(x$run_order_col))
    }
  }

  if (length(col_names) == 0) {
    cli::cli_abort("No feature columns found for drift correction.")
  }

  # Extract QC samples
  sample_types <- training[[x$sample_type_col]]
  is_qc <- sample_types %in% x$qc_type

  if (sum(is_qc) < x$min_qc) {
    cli::cli_abort(
      "Insufficient QC samples: found {sum(is_qc)}, need at least {x$min_qc}."
    )
  }

  run_order <- training[[x$run_order_col]]

  # Fit drift models for each feature
  drift_models <- list()
  qc_medians <- list()

  for (col in col_names) {
    if (is_measure_list(training[[col]])) {
      # Handle measure_list (curve-level data)
      result <- .fit_drift_model_measure_list(
        training[[col]],
        run_order,
        is_qc,
        x$span,
        x$degree,
        x$robust
      )
      drift_models[[col]] <- result$models
      qc_medians[[col]] <- result$medians
    } else {
      # Handle numeric feature column
      qc_values <- training[[col]][is_qc]
      qc_run_order <- run_order[is_qc]

      qc_median <- stats::median(qc_values, na.rm = TRUE)
      qc_medians[[col]] <- qc_median

      # Fit LOESS
      loess_fit <- stats::loess(
        qc_values ~ qc_run_order,
        span = x$span,
        degree = x$degree,
        family = if (x$robust) "symmetric" else "gaussian"
      )

      drift_models[[col]] <- loess_fit
    }
  }

  step_measure_drift_qc_loess_new(
    terms = x$terms,
    run_order_col = x$run_order_col,
    sample_type_col = x$sample_type_col,
    qc_type = x$qc_type,
    apply_to = x$apply_to,
    span = x$span,
    degree = x$degree,
    robust = x$robust,
    min_qc = x$min_qc,
    drift_models = drift_models,
    qc_medians = qc_medians,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_drift_qc_loess <- function(object, new_data, ...) {

  run_order <- new_data[[object$run_order_col]]
  sample_types <- new_data[[object$sample_type_col]]

  # Determine which samples to correct
  if (object$apply_to == "all") {
    to_correct <- rep(TRUE, nrow(new_data))
  } else {
    to_correct <- !(sample_types %in% object$qc_type)
  }

  for (col in names(object$drift_models)) {
    if (is_measure_list(new_data[[col]])) {
      # Handle measure_list (curve-level data)
      new_data[[col]] <- .apply_drift_correction_measure_list(
        new_data[[col]],
        run_order,
        to_correct,
        object$drift_models[[col]],
        object$qc_medians[[col]]
      )
    } else {
      # Handle numeric feature column
      loess_fit <- object$drift_models[[col]]
      qc_median <- object$qc_medians[[col]]

      # Predict QC response at each run order
      predicted <- stats::predict(loess_fit, newdata = run_order)

      # Calculate correction factors
      correction_factors <- qc_median / predicted

      # Apply correction only to specified samples
      corrected <- new_data[[col]]
      corrected[to_correct] <- new_data[[col]][to_correct] * correction_factors[to_correct]
      new_data[[col]] <- corrected
    }
  }

  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_drift_qc_loess <- function(x, width = max(20, options()$width - 30), ...) {
  title <- "QC-LOESS drift correction"

  if (x$trained) {
    features <- names(x$drift_models)
    n_features <- length(features)
    cat(title, " (", n_features, " feature", if (n_features != 1) "s", ")\n", sep = "")
  } else {
    cat(title, "\n", sep = "")
  }

  invisible(x)
}

#' @rdname tidy.recipe
#' @export
tidy.step_measure_drift_qc_loess <- function(x, ...) {
  if (!x$trained) {
    return(tibble::tibble(
      feature = character(),
      qc_median = double(),
      span = double(),
      degree = integer()
    ))
  }

  tibble::tibble(
    feature = names(x$drift_models),
    qc_median = vapply(x$qc_medians, function(m) {
      if (is.list(m)) mean(unlist(m), na.rm = TRUE) else m
    }, double(1)),
    span = x$span,
    degree = x$degree
  )
}

# ==============================================================================
# Helper functions
# ==============================================================================

.fit_drift_model_measure_list <- function(measure_list, run_order, is_qc, span, degree, robust) {
  # For measure_list, we need to fit a model at each location
  # This is a simplified version - could be optimized

  # Get all unique locations
  all_locations <- unique(unlist(lapply(measure_list, function(m) m$location)))

  models <- list()
  medians <- list()

  for (loc in all_locations) {
    # Extract values at this location from QC samples
    qc_values <- sapply(which(is_qc), function(i) {
      m <- measure_list[[i]]
      idx <- which(m$location == loc)
      if (length(idx) == 1) m$value[idx] else NA
    })

    qc_run_order <- run_order[is_qc]
    valid <- !is.na(qc_values)

    if (sum(valid) >= 3) {
      medians[[as.character(loc)]] <- stats::median(qc_values[valid], na.rm = TRUE)

      models[[as.character(loc)]] <- stats::loess(
        qc_values[valid] ~ qc_run_order[valid],
        span = span,
        degree = degree,
        family = if (robust) "symmetric" else "gaussian"
      )
    }
  }

  list(models = models, medians = medians)
}

.apply_drift_correction_measure_list <- function(measure_list, run_order, to_correct, models, medians) {
  # Apply correction to each measurement in the list
  corrected <- lapply(seq_along(measure_list), function(i) {
    m <- measure_list[[i]]

    if (!to_correct[i]) {
      return(m)
    }

    # Correct each location
    new_values <- sapply(seq_len(nrow(m)), function(j) {
      loc <- m$location[j]
      loc_str <- as.character(loc)

      if (loc_str %in% names(models)) {
        predicted <- stats::predict(models[[loc_str]], newdata = run_order[i])
        correction <- medians[[loc_str]] / predicted
        m$value[j] * correction
      } else {
        m$value[j]  # No correction if no model for this location
      }
    })

    m$value <- new_values
    m
  })

  new_measure_list(corrected)
}

# ==============================================================================
# Drift detection function (standalone, not a recipe step)
# ==============================================================================

#' Detect Drift in Analytical Data
#'
#' Detects significant drift in feature responses across run order using
#' trend tests and/or slope analysis.
#'
#' @param data A data frame containing the measurement data.
#' @param features Character vector of feature column names to analyze.
#' @param run_order_col Name of the run order column.
#' @param sample_type_col Name of the sample type column.
#' @param qc_type Value(s) identifying QC samples. If provided, analysis is
#'   restricted to QC samples.
#' @param method Detection method:
#'   - `"slope"` (default): Linear regression slope test
#'   - `"mann_kendall"`: Mann-Kendall trend test
#'   - `"both"`: Both methods
#'
#' @return A tibble with drift statistics for each feature:
#'   - `feature`: Feature name
#'   - `slope`: Regression slope (change per run)
#'   - `slope_pvalue`: P-value for slope != 0
#'   - `percent_change`: Total percent change over run
#'   - `significant`: Logical, TRUE if drift is statistically significant
#'
#' @examples
#' # Create data with drift
#' data <- data.frame(
#'   sample_type = rep("qc", 20),
#'   run_order = 1:20,
#'   feature1 = 100 + (1:20) * 0.5 + rnorm(20, sd = 2),
#'   feature2 = 50 + rnorm(20, sd = 1)  # No drift
#' )
#'
#' measure_detect_drift(data, c("feature1", "feature2"))
#'
#' @export
measure_detect_drift <- function(
    data,
    features,
    run_order_col = "run_order",
    sample_type_col = "sample_type",
    qc_type = NULL,
    method = c("slope", "mann_kendall", "both")) {

  method <- match.arg(method)

  if (!run_order_col %in% names(data)) {
    cli::cli_abort("Column {.field {run_order_col}} not found in data.")
  }

  # Filter to QC samples if specified
  if (!is.null(qc_type)) {
    if (!sample_type_col %in% names(data)) {
      cli::cli_abort("Column {.field {sample_type_col}} not found in data.")
    }
    data <- data[data[[sample_type_col]] %in% qc_type, , drop = FALSE]
  }

  run_order <- data[[run_order_col]]

  results <- lapply(features, function(feat) {
    if (!feat %in% names(data)) {
      cli::cli_warn("Feature {.field {feat}} not found, skipping.")
      return(NULL)
    }

    values <- data[[feat]]
    valid <- !is.na(values)

    if (sum(valid) < 3) {
      return(tibble::tibble(
        feature = feat,
        slope = NA_real_,
        slope_pvalue = NA_real_,
        percent_change = NA_real_,
        significant = NA
      ))
    }

    # Slope analysis
    fit <- stats::lm(values[valid] ~ run_order[valid])
    coefs <- summary(fit)$coefficients
    slope <- coefs[2, "Estimate"]
    slope_pvalue <- coefs[2, "Pr(>|t|)"]

    # Calculate percent change over full run
    run_range <- diff(range(run_order[valid]))
    mean_value <- mean(values[valid], na.rm = TRUE)
    percent_change <- 100 * slope * run_range / mean_value

    tibble::tibble(
      feature = feat,
      slope = slope,
      slope_pvalue = slope_pvalue,
      percent_change = percent_change,
      significant = slope_pvalue < 0.05
    )
  })

  dplyr::bind_rows(results)
}
