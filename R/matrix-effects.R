# ==============================================================================
# Matrix Effects Analysis and Standard Addition
#
# This file contains:
# - measure_matrix_effect: Quantify ion suppression/enhancement
# - step_measure_standard_addition: Recipe step for standard addition correction
# ==============================================================================

#' Matrix Effect Analysis
#'
#' Quantifies matrix effects (ion suppression/enhancement) by comparing
#' analyte response in matrix versus neat solution. This is essential for
#' validating LC-MS/MS and other analytical methods where matrix interference
#' is a concern.
#'
#' @param data A data frame containing response data.
#' @param response_col Name of the column containing analyte responses.
#' @param sample_type_col Name of the column indicating sample type
#'   (matrix vs neat/standard).
#' @param matrix_level Value in `sample_type_col` indicating matrix samples.
#' @param neat_level Value in `sample_type_col` indicating neat/standard samples.
#' @param concentration_col Optional column for concentration levels.
#'   If provided, matrix effects are calculated per concentration.
#' @param analyte_col Optional column for analyte names.
#'   If provided, matrix effects are calculated per analyte.
#' @param group_cols Additional grouping columns (e.g., batch, matrix source).
#' @param conf_level Confidence level for intervals. Default is 0.95.
#'
#' @return A `measure_matrix_effect` object containing:
#'   - `results`: Tibble with matrix effect percentages per group
#'   - `statistics`: Overall summary statistics
#'   - `raw_data`: Data used for calculations
#'
#' @details
#' ## Matrix Effect Calculation
#'
#' Matrix effect (ME%) is calculated as:
#' `ME% = (response_in_matrix / response_in_neat) * 100`
#'
#' Or equivalently:
#' `ME% = 100 + ((response_in_matrix - response_in_neat) / response_in_neat) * 100`
#'
#' ## Interpretation
#'
#' - **ME = 100%**: No matrix effect
#' - **ME > 100%**: Ion enhancement
#' - **ME < 100%**: Ion suppression
#'
#' ## Acceptance Criteria (typical)
#'
#' According to ICH M10 and FDA guidance:
#' - ME should be between 80-120% (±20%)
#' - CV of ME should be ≤15%
#'
#' ## Experimental Design
#'
#' To assess matrix effects:
#' 1. Prepare blank matrix (e.g., plasma) from multiple sources
#' 2. Spike analyte post-extraction at known concentration
#' 3. Compare to analyte in neat solvent at same concentration
#'
#' @family calibration
#' @seealso [step_measure_standard_addition()], [measure_accuracy()]
#'
#' @export
#'
#' @examples
#' # Matrix effect study data
#' me_data <- data.frame(
#'   sample_type = rep(c("matrix", "neat"), each = 6),
#'   matrix_lot = rep(c("Lot1", "Lot2", "Lot3", "Lot1", "Lot2", "Lot3"), 2),
#'   concentration = rep(c("low", "high"), each = 3, times = 2),
#'   response = c(
#'     # Matrix samples (some suppression)
#'     9500, 9800, 9200, 48000, 49500, 47000,
#'     # Neat samples
#'     10000, 10000, 10000, 50000, 50000, 50000
#'   )
#' )
#'
#' me <- measure_matrix_effect(
#'   me_data,
#'   response_col = "response",
#'   sample_type_col = "sample_type",
#'   matrix_level = "matrix",
#'   neat_level = "neat",
#'   concentration_col = "concentration"
#' )
#'
#' print(me)
#' tidy(me)
measure_matrix_effect <- function(
    data,
    response_col,
    sample_type_col,
    matrix_level,
    neat_level,
    concentration_col = NULL,
    analyte_col = NULL,
    group_cols = NULL,
    conf_level = 0.95) {

  # Validate inputs
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame.")
  }

  # Required columns
  for (col in c(response_col, sample_type_col)) {
    if (!col %in% names(data)) {
      cli::cli_abort("Column {.field {col}} not found in data.")
    }
  }

  if (!is.numeric(data[[response_col]])) {
    cli::cli_abort("Response column {.field {response_col}} must be numeric.")
  }

  # Validate sample type levels
  sample_types <- unique(data[[sample_type_col]])
  if (!matrix_level %in% sample_types) {
    cli::cli_abort(
      "Matrix level {.val {matrix_level}} not found in {.field {sample_type_col}}."
    )
  }
  if (!neat_level %in% sample_types) {
    cli::cli_abort(
      "Neat level {.val {neat_level}} not found in {.field {sample_type_col}}."
    )
  }

  # Optional columns
  optional_cols <- c(concentration_col, analyte_col, group_cols)
  for (col in optional_cols) {
    if (!is.null(col) && !col %in% names(data)) {
      cli::cli_abort("Column {.field {col}} not found in data.")
    }
  }

  # Build grouping structure
  grouping_cols <- c(concentration_col, analyte_col, group_cols)
  grouping_cols <- grouping_cols[!is.null(grouping_cols)]

  # Separate matrix and neat samples
  matrix_data <- data[data[[sample_type_col]] == matrix_level, ]
  neat_data <- data[data[[sample_type_col]] == neat_level, ]

  if (nrow(matrix_data) == 0) {
    cli::cli_abort("No matrix samples found with level {.val {matrix_level}}.")
  }
  if (nrow(neat_data) == 0) {
    cli::cli_abort("No neat samples found with level {.val {neat_level}}.")
  }

  # Calculate matrix effects per group
  if (length(grouping_cols) > 0) {
    results <- .calculate_me_grouped(
      matrix_data, neat_data, response_col, grouping_cols, conf_level
    )
  } else {
    results <- .calculate_me_simple(
      matrix_data, neat_data, response_col, conf_level
    )
  }

  # Overall statistics
  all_me <- results$matrix_effect_pct
  mean_me_val <- mean(all_me, na.rm = TRUE)
  sd_me_val <- sd(all_me, na.rm = TRUE)

  # Safe CV calculation (avoid division by zero)
  cv_me_val <- if (!is.na(mean_me_val) && mean_me_val != 0) {
    100 * sd_me_val / mean_me_val
  } else {
    NA_real_
  }

  statistics <- list(
    n_groups = nrow(results),
    mean_me = mean_me_val,
    sd_me = sd_me_val,
    cv_me = cv_me_val,
    min_me = min(all_me, na.rm = TRUE),
    max_me = max(all_me, na.rm = TRUE),
    n_suppression = sum(all_me < 100, na.rm = TRUE),
    n_enhancement = sum(all_me > 100, na.rm = TRUE),
    n_acceptable = sum(all_me >= 80 & all_me <= 120, na.rm = TRUE)
  )

  structure(
    list(
      results = results,
      statistics = statistics,
      raw_data = list(
        matrix = matrix_data,
        neat = neat_data
      ),
      call = match.call()
    ),
    class = "measure_matrix_effect",
    response_col = response_col,
    sample_type_col = sample_type_col,
    matrix_level = matrix_level,
    neat_level = neat_level,
    conf_level = conf_level
  )
}

#' Calculate matrix effect for simple (ungrouped) case
#' @noRd
.calculate_me_simple <- function(matrix_data, neat_data, response_col, conf_level) {
  matrix_responses <- matrix_data[[response_col]]
  neat_responses <- neat_data[[response_col]]

  # Remove NAs
  matrix_responses <- matrix_responses[!is.na(matrix_responses)]
  neat_responses <- neat_responses[!is.na(neat_responses)]

  # Check for empty vectors after NA removal

  if (length(matrix_responses) == 0) {
    cli::cli_abort("All matrix responses are NA; cannot calculate matrix effect.")
  }
  if (length(neat_responses) == 0) {
    cli::cli_abort("All neat responses are NA; cannot calculate matrix effect.")
  }

  mean_matrix <- mean(matrix_responses)
  mean_neat <- mean(neat_responses)

  # Check for division by zero
  if (mean_neat == 0) {
    cli::cli_abort(
      "Mean response of neat samples is zero; cannot calculate matrix effect percentage."
    )
  }

  me_pct <- (mean_matrix / mean_neat) * 100

  # CI for ratio using delta method approximation
  n_matrix <- length(matrix_responses)
  n_neat <- length(neat_responses)

  # Handle edge cases with insufficient data for CI
  min_n <- min(n_matrix, n_neat)
  if (min_n < 2) {
    # Cannot calculate CI with single observation
    se_ratio <- NA_real_
    t_crit <- NA_real_
  } else if (mean_matrix == 0) {
    # Cannot calculate SE ratio when mean_matrix is zero
    se_ratio <- NA_real_
    t_crit <- NA_real_
  } else {
    se_matrix <- sd(matrix_responses) / sqrt(n_matrix)
    se_neat <- sd(neat_responses) / sqrt(n_neat)

    # Approximation for ratio SE
    se_ratio <- me_pct * sqrt((se_matrix/mean_matrix)^2 + (se_neat/mean_neat)^2)
    t_crit <- stats::qt((1 + conf_level) / 2, df = min_n - 1)
  }

  tibble::tibble(
    n_matrix = n_matrix,
    n_neat = n_neat,
    mean_matrix_response = mean_matrix,
    mean_neat_response = mean_neat,
    matrix_effect_pct = me_pct,
    me_ci_lower = me_pct - t_crit * se_ratio,
    me_ci_upper = me_pct + t_crit * se_ratio,
    interpretation = dplyr::case_when(
      me_pct < 80 ~ "strong_suppression",
      me_pct < 100 ~ "suppression",
      me_pct > 120 ~ "strong_enhancement",
      me_pct > 100 ~ "enhancement",
      TRUE ~ "none"
    )
  )
}

#' Calculate matrix effect by groups
#' @noRd
.calculate_me_grouped <- function(matrix_data, neat_data, response_col,
                                   grouping_cols, conf_level) {

  # Get unique groups from both datasets
  matrix_groups <- unique(matrix_data[, grouping_cols, drop = FALSE])
  neat_groups <- unique(neat_data[, grouping_cols, drop = FALSE])

  # Find common groups
  # Use interaction to create unique keys
  matrix_key <- do.call(paste, c(matrix_groups, sep = "_"))
  neat_key <- do.call(paste, c(neat_groups, sep = "_"))

  common_keys <- intersect(matrix_key, neat_key)

  if (length(common_keys) == 0) {
    cli::cli_abort(
      "No matching groups found between matrix and neat samples."
    )
  }

  results_list <- list()

  for (key in common_keys) {
    # Find rows matching this key
    matrix_mask <- do.call(paste, c(matrix_data[, grouping_cols, drop = FALSE], sep = "_")) == key
    neat_mask <- do.call(paste, c(neat_data[, grouping_cols, drop = FALSE], sep = "_")) == key

    group_matrix <- matrix_data[matrix_mask, ]
    group_neat <- neat_data[neat_mask, ]

    # Calculate ME for this group
    group_result <- .calculate_me_simple(group_matrix, group_neat, response_col, conf_level)

    # Add grouping information
    group_info <- group_matrix[1, grouping_cols, drop = FALSE]
    results_list[[key]] <- cbind(group_info, group_result)
  }

  dplyr::bind_rows(results_list)
}

#' @export
print.measure_matrix_effect <- function(x, ...) {
  cat("measure_matrix_effect\n")
  cat(cli::rule(), "\n\n")

  stats <- x$statistics

  cat("Overall Matrix Effect Summary:\n")
  cat("  Groups evaluated:", stats$n_groups, "\n")
  cat("  Mean ME:", format(stats$mean_me, digits = 1), "%\n")
  cat("  SD ME:", format(stats$sd_me, digits = 1), "%\n")
  cat("  CV ME:", format(stats$cv_me, digits = 1), "%\n")
  cat("  Range:", format(stats$min_me, digits = 1), "-",
      format(stats$max_me, digits = 1), "%\n\n")

  cat("Effect Classification:\n")
  cat("  Ion suppression (ME < 100%):", stats$n_suppression, "\n")
  cat("  Ion enhancement (ME > 100%):", stats$n_enhancement, "\n")
  cat("  Acceptable (80-120%):", stats$n_acceptable, "/", stats$n_groups, "\n")

  invisible(x)
}

#' @rdname tidy.recipe
#' @export
tidy.measure_matrix_effect <- function(x, ...) {
  x$results
}

#' @rdname tidy.recipe
#' @export
glance.measure_matrix_effect <- function(x, ...) {
  stats <- x$statistics

  tibble::tibble(
    n_groups = stats$n_groups,
    mean_me_pct = stats$mean_me,
    cv_me_pct = stats$cv_me,
    min_me_pct = stats$min_me,
    max_me_pct = stats$max_me,
    pct_acceptable = 100 * stats$n_acceptable / stats$n_groups,
    all_acceptable = stats$n_acceptable == stats$n_groups
  )
}

#' Plot Matrix Effects
#'
#' Creates a visualization of matrix effects showing suppression/enhancement.
#'
#' @param object A `measure_matrix_effect` object.
#' @param type Plot type: "bar", "point", or "forest". Default "bar".
#' @param show_limits Show acceptable limits (80-120%)? Default TRUE.
#' @param ... Additional arguments (unused).
#'
#' @return A ggplot object.
#'
#' @importFrom ggplot2 autoplot
#' @export
autoplot.measure_matrix_effect <- function(object, type = c("bar", "point", "forest"),
                                            show_limits = TRUE, ...) {
  type <- match.arg(type)
  results <- object$results

  # Create row index if needed
  if (!"group" %in% names(results)) {
    results$group <- seq_len(nrow(results))
  }

  # Color based on interpretation
  interp_colors <- c(
    "strong_suppression" = "#D73027",
    "suppression" = "#FC8D59",
    "none" = "#1A9850",
    "enhancement" = "#91CF60",
    "strong_enhancement" = "#D73027"
  )

  if (type == "bar") {
    p <- ggplot2::ggplot(results, ggplot2::aes(
      x = factor(.data$group),
      y = .data$matrix_effect_pct,
      fill = .data$interpretation
    )) +
      ggplot2::geom_col(width = 0.7) +
      ggplot2::scale_fill_manual(values = interp_colors, name = "Effect")

  } else if (type == "point") {
    p <- ggplot2::ggplot(results, ggplot2::aes(
      x = factor(.data$group),
      y = .data$matrix_effect_pct,
      color = .data$interpretation
    )) +
      ggplot2::geom_point(size = 3) +
      ggplot2::scale_color_manual(values = interp_colors, name = "Effect")

  } else {  # forest
    p <- ggplot2::ggplot(results, ggplot2::aes(
      x = .data$matrix_effect_pct,
      y = factor(.data$group),
      color = .data$interpretation
    )) +
      ggplot2::geom_point(size = 3) +
      ggplot2::geom_errorbar(
        ggplot2::aes(xmin = .data$me_ci_lower, xmax = .data$me_ci_upper),
        width = 0.2,
        orientation = "y"
      ) +
      ggplot2::scale_color_manual(values = interp_colors, name = "Effect") +
      ggplot2::geom_vline(xintercept = 100, linetype = "solid", color = "gray50")
  }

  if (show_limits) {
    if (type == "forest") {
      p <- p +
        ggplot2::geom_vline(xintercept = c(80, 120), linetype = "dashed",
                            color = "#FEE08B", linewidth = 0.8)
    } else {
      p <- p +
        ggplot2::geom_hline(yintercept = c(80, 100, 120),
                            linetype = c("dashed", "solid", "dashed"),
                            color = c("#FEE08B", "gray50", "#FEE08B"),
                            linewidth = 0.8)
    }
  }

  p +
    ggplot2::labs(
      title = "Matrix Effect Analysis",
      x = if (type == "forest") "Matrix Effect (%)" else "Group",
      y = if (type == "forest") "Group" else "Matrix Effect (%)"
    ) +
    ggplot2::theme_minimal()
}

# ==============================================================================
# Standard Addition Step
# ==============================================================================

#' Standard Addition Correction
#'
#' `step_measure_standard_addition()` creates a *specification* of a recipe step
#' that performs standard addition correction to compensate for matrix effects.
#' This method creates a sample-specific calibration for each unknown to
#' accurately quantify in the presence of matrix interference.
#'
#' @param recipe A recipe object.
#' @param ... One or more selector functions to choose response columns
#'   to correct using standard addition.
#' @param addition_col Name of the column containing the amount of standard
#'   added (spike amount). Default is `"addition"`.
#' @param sample_id_col Name of the column identifying unique samples.
#'   Each sample gets its own standard addition curve.
#' @param min_points Minimum number of addition points required per sample.
#'   Default is 3.
#' @param output_suffix Suffix for output concentration columns.
#'   Default is `"_corrected"`.
#' @param diagnostics Include diagnostic information (R², slope, intercept)?
#'   Default is TRUE.
#' @param role Recipe role for new columns. Default is `"outcome"`.
#' @param trained Logical indicating if the step has been trained.
#' @param skip Logical. Should the step be skipped when baking?
#' @param id Unique step identifier.
#'
#' @return An updated recipe with the new step added.
#'
#' @details
#' ## Standard Addition Method
#'
#' Standard addition works by:
#' 1. Splitting each unknown sample into multiple aliquots
#' 2. Adding increasing known amounts of analyte to each aliquot
#' 3. Measuring response for all aliquots
#' 4. Fitting regression: `response = intercept + slope * addition`
#' 5. Calculating original concentration from the x-intercept
#'
#' The x-intercept (where response = 0) is at `-intercept / slope`.
#' Since intercept is positive (response from original sample) and slope
#' is positive (response increases with addition), the original concentration
#' is: `concentration = intercept / slope`
#'
#' ## Data Format
#'
#' The input data should have:
#' - A sample identifier column (each unique sample)
#' - An addition amount column (0 for unspiked, then increasing amounts)
#' - Response column(s) to be corrected
#'
#' ## When to Use
#'
#' Use standard addition when:
#' - Significant matrix effects are present
#' - Matrix-matched calibrators are not available
#' - Sample-to-sample matrix variation is expected
#'
#' ## Limitations
#'
#' - Requires multiple measurements per sample
#' - Assumes linear response over the addition range
#' - Does not correct for non-specific interferences
#'
#' @family calibration
#' @seealso [measure_matrix_effect()], [measure_calibration()]
#'
#' @export
#'
#' @examples
#' library(recipes)
#'
#' # Standard addition data for two samples
#' sa_data <- data.frame(
#'   sample_id = rep(c("Sample1", "Sample2"), each = 4),
#'   addition = rep(c(0, 10, 20, 30), 2),
#'   response = c(
#'     # Sample 1: original conc ~15
#'     150, 250, 350, 450,
#'     # Sample 2: original conc ~25
#'     250, 350, 450, 550
#'   )
#' )
#'
#' rec <- recipe(~ ., data = sa_data) |>
#'   step_measure_standard_addition(
#'     response,
#'     addition_col = "addition",
#'     sample_id_col = "sample_id"
#'   ) |>
#'   prep()
#'
#' bake(rec, new_data = NULL)
step_measure_standard_addition <- function(
    recipe,
    ...,
    addition_col = "addition",
    sample_id_col,
    min_points = 3,
    output_suffix = "_corrected",
    diagnostics = TRUE,
    role = "outcome",
    trained = FALSE,
    skip = FALSE,
    id = recipes::rand_id("measure_standard_addition")) {

  if (missing(sample_id_col)) {
    cli::cli_abort(
      "{.arg sample_id_col} must be provided to identify unique samples."
    )
  }

  if (min_points < 2) {
    cli::cli_abort(
      "{.arg min_points} must be at least 2 for regression."
    )
  }

  recipes::add_step(
    recipe,
    step_measure_standard_addition_new(
      terms = rlang::enquos(...),
      addition_col = addition_col,
      sample_id_col = sample_id_col,
      min_points = min_points,
      output_suffix = output_suffix,
      diagnostics = diagnostics,
      role = role,
      trained = trained,
      col_names = NULL,
      calibrations = NULL,
      skip = skip,
      id = id
    )
  )
}

#' Internal constructor
#' @noRd
step_measure_standard_addition_new <- function(
    terms,
    addition_col,
    sample_id_col,
    min_points,
    output_suffix,
    diagnostics,
    role,
    trained,
    col_names,
    calibrations,
    skip,
    id) {

  recipes::step(
    subclass = "measure_standard_addition",
    terms = terms,
    addition_col = addition_col,
    sample_id_col = sample_id_col,
    min_points = min_points,
    output_suffix = output_suffix,
    diagnostics = diagnostics,
    role = role,
    trained = trained,
    col_names = col_names,
    calibrations = calibrations,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_standard_addition <- function(x, training, info = NULL, ...) {

  # Validate required columns
  if (!x$addition_col %in% names(training)) {
    cli::cli_abort(
      "Addition column {.field {x$addition_col}} not found in training data."
    )
  }
  if (!x$sample_id_col %in% names(training)) {
    cli::cli_abort(
      "Sample ID column {.field {x$sample_id_col}} not found in training data."
    )
  }

  if (!is.numeric(training[[x$addition_col]])) {
    cli::cli_abort(
      "Addition column {.field {x$addition_col}} must be numeric."
    )
  }

  # Get response columns
  col_names <- recipes::recipes_eval_select(x$terms, training, info)

  if (length(col_names) == 0) {
    cli::cli_abort(
      "No response columns selected. Use column selectors in {.code ...}."
    )
  }

  # Validate all selected columns are numeric
  for (col in col_names) {
    if (!is.numeric(training[[col]])) {
      cli::cli_abort(
        "Response column {.field {col}} must be numeric."
      )
    }
  }

  # Fit standard addition calibrations per sample and per response
  calibrations <- .fit_standard_additions(
    training,
    col_names,
    x$addition_col,
    x$sample_id_col,
    x$min_points
  )

  step_measure_standard_addition_new(
    terms = x$terms,
    addition_col = x$addition_col,
    sample_id_col = x$sample_id_col,
    min_points = x$min_points,
    output_suffix = x$output_suffix,
    diagnostics = x$diagnostics,
    role = x$role,
    trained = TRUE,
    col_names = col_names,
    calibrations = calibrations,
    skip = x$skip,
    id = x$id
  )
}

#' Fit standard addition calibrations
#' @noRd
.fit_standard_additions <- function(data, response_cols, addition_col,
                                     sample_id_col, min_points) {

  samples <- unique(data[[sample_id_col]])
  calibrations <- list()

  for (sample in samples) {
    sample_data <- data[data[[sample_id_col]] == sample, ]

    if (nrow(sample_data) < min_points) {
      cli::cli_warn(
        "Sample {.val {sample}} has only {nrow(sample_data)} points; requires {min_points}."
      )
      next
    }

    additions <- sample_data[[addition_col]]

    for (resp_col in response_cols) {
      responses <- sample_data[[resp_col]]

      # Fit linear regression: response ~ addition
      if (sum(!is.na(responses)) < min_points) {
        cli::cli_warn(
          "Sample {.val {sample}} has insufficient non-NA values for {.field {resp_col}}."
        )
        next
      }

      fit <- tryCatch({
        stats::lm(responses ~ additions)
      }, error = function(e) {
        cli::cli_warn(
          c(
            "Failed to fit standard addition for sample {.val {sample}}, {.field {resp_col}}.",
            "i" = "Error: {e$message}"
          )
        )
        NULL
      })

      if (is.null(fit)) {
        next
      }

      coefs <- stats::coef(fit)
      intercept <- coefs[1]
      slope <- coefs[2]

      # Calculate original concentration from intercept
      # Standard addition: response = slope * (addition + original_conc)
      # At addition = 0: intercept = slope * original_conc
      # Therefore: original_conc = intercept / slope
      if (slope <= 0) {
        cli::cli_warn(
          "Non-positive slope for sample {.val {sample}}, {.field {resp_col}}. Cannot determine concentration."
        )
        original_conc <- NA_real_
      } else {
        original_conc <- intercept / slope
      }

      # R-squared
      r_squared <- summary(fit)$r.squared

      key <- paste(sample, resp_col, sep = "___")
      calibrations[[key]] <- list(
        sample = sample,
        response = resp_col,
        intercept = intercept,
        slope = slope,
        original_concentration = original_conc,
        r_squared = r_squared,
        n_points = sum(!is.na(responses))
      )
    }
  }

  calibrations
}

#' @export
bake.step_measure_standard_addition <- function(object, new_data, ...) {

  # For standard addition, we typically work with training data only

# (the calibration IS the result)

  # Get unique samples from new_data
  samples <- unique(new_data[[object$sample_id_col]])

  # Create output columns
  for (resp_col in object$col_names) {
    output_col <- paste0(resp_col, object$output_suffix)
    new_data[[output_col]] <- NA_real_

    if (object$diagnostics) {
      new_data[[paste0(resp_col, "_sa_slope")]] <- NA_real_
      new_data[[paste0(resp_col, "_sa_intercept")]] <- NA_real_
      new_data[[paste0(resp_col, "_sa_rsquared")]] <- NA_real_
    }

    for (sample in samples) {
      key <- paste(sample, resp_col, sep = "___")
      cal <- object$calibrations[[key]]

      if (is.null(cal)) {
        cli::cli_warn(
          "No calibration found for sample {.val {sample}}, {.field {resp_col}}."
        )
        next
      }

      sample_mask <- new_data[[object$sample_id_col]] == sample

      # Set the original concentration (same for all rows of this sample)
      new_data[[output_col]][sample_mask] <- cal$original_concentration

      if (object$diagnostics) {
        new_data[[paste0(resp_col, "_sa_slope")]][sample_mask] <- cal$slope
        new_data[[paste0(resp_col, "_sa_intercept")]][sample_mask] <- cal$intercept
        new_data[[paste0(resp_col, "_sa_rsquared")]][sample_mask] <- cal$r_squared
      }
    }
  }

  new_data
}

#' @export
print.step_measure_standard_addition <- function(
    x,
    width = max(20, options()$width - 30),
    ...) {

  title <- "Standard addition correction"

  if (x$trained) {
    n_samples <- length(unique(vapply(x$calibrations, `[[`, character(1), "sample")))
    n_cols <- length(x$col_names)
    desc <- glue::glue(
      "{n_cols} response{?s} for {n_samples} sample{?s}"
    )
  } else {
    desc <- "(not trained)"
  }

  cat(title, ": ", as.character(desc), "\n", sep = "")
  invisible(x)
}

#' @rdname tidy.recipe
#' @export
tidy.step_measure_standard_addition <- function(x, ...) {
  if (x$trained) {
    calibrations_df <- do.call(rbind, lapply(x$calibrations, function(cal) {
      tibble::tibble(
        sample = cal$sample,
        response = cal$response,
        original_concentration = cal$original_concentration,
        slope = cal$slope,
        intercept = cal$intercept,
        r_squared = cal$r_squared,
        n_points = cal$n_points
      )
    }))
    calibrations_df$id <- x$id
    calibrations_df
  } else {
    tibble::tibble(
      sample = character(),
      response = character(),
      original_concentration = numeric(),
      slope = numeric(),
      intercept = numeric(),
      r_squared = numeric(),
      n_points = integer(),
      id = character()
    )
  }
}
