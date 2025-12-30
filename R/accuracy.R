# ==============================================================================
# Accuracy, Linearity, and Related Method Validation Functions
#
# This file contains:
# - measure_accuracy: Bias and recovery calculations
# - measure_linearity: Linearity assessment
# - measure_carryover: Carryover evaluation
# ==============================================================================

#' Accuracy Assessment
#'
#' Calculates accuracy metrics including bias, recovery, and confidence intervals
#' for method validation.
#'
#' @param data A data frame containing measured and reference values.
#' @param measured_col Name of the column containing measured values.
#' @param reference_col Name of the column containing reference/nominal values.
#' @param group_col Optional grouping column (e.g., concentration level).
#' @param conf_level Confidence level for intervals. Default is 0.95.
#'
#' @return A `measure_accuracy` object containing:
#'   - `n`: Number of observations
#'   - `mean_measured`: Mean of measured values
#'   - `mean_reference`: Mean of reference values
#'   - `bias`: Absolute bias (measured - reference)
#'   - `bias_pct`: Relative bias as percentage
#'   - `recovery`: Recovery percentage (measured/reference * 100)
#'   - `recovery_ci_lower`, `recovery_ci_upper`: Confidence interval for recovery
#'
#' @details
#' Accuracy expresses the closeness of agreement between a measured value
#' and a reference value. It is typically assessed using:
#' - **Bias**: Systematic difference from the reference value
#' - **Recovery**: Percentage of the reference value that is measured
#'
#' ## ICH Q2 Requirements
#' Accuracy should be assessed at a minimum of 3 concentration levels
#' covering the specified range (typically 80-120% of the target).
#'
#' @family accuracy
#' @seealso [measure_linearity()], [measure_carryover()]
#'
#' @export
#'
#' @examples
#' # Accuracy at multiple levels
#' set.seed(123)
#' data <- data.frame(
#'   level = rep(c("low", "mid", "high"), each = 5),
#'   nominal = rep(c(10, 50, 100), each = 5),
#'   measured = c(
#'     rnorm(5, 10.2, 0.3),
#'     rnorm(5, 49.5, 1.5),
#'     rnorm(5, 101, 3)
#'   )
#' )
#'
#' result <- measure_accuracy(data, "measured", "nominal", group_col = "level")
#' print(result)
measure_accuracy <- function(
    data,
    measured_col,
    reference_col,
    group_col = NULL,
    conf_level = 0.95) {

  # Validate inputs
  for (col in c(measured_col, reference_col)) {
    if (!col %in% names(data)) {
      cli::cli_abort("Column {.field {col}} not found in data.")
    }
  }

  if (!is.null(group_col) && !group_col %in% names(data)) {
    cli::cli_abort("Column {.field {group_col}} not found in data.")
  }

  # Calculate accuracy stats
  if (is.null(group_col)) {
    result <- calculate_accuracy_stats(
      data[[measured_col]],
      data[[reference_col]],
      conf_level = conf_level,
      group = "overall"
    )
    result <- tibble::as_tibble(result)
  } else {
    groups <- unique(data[[group_col]])
    results <- lapply(groups, function(g) {
      idx <- data[[group_col]] == g
      calculate_accuracy_stats(
        data[[measured_col]][idx],
        data[[reference_col]][idx],
        conf_level = conf_level,
        group = g
      )
    })
    result <- dplyr::bind_rows(results)
  }

  structure(
    result,
    class = c("measure_accuracy", "tbl_df", "tbl", "data.frame"),
    measured_col = measured_col,
    reference_col = reference_col,
    conf_level = conf_level
  )
}

#' Linearity Assessment
#'
#' Assesses linearity of a method by evaluating the relationship between
#' response and concentration across the specified range.
#'
#' @param data A data frame containing concentration and response data.
#' @param conc_col Name of the column containing concentrations.
#' @param response_col Name of the column containing responses.
#' @param method Linearity assessment method:
#'   - `"regression"` (default): Linear regression with diagnostics
#'   - `"residual"`: Residual analysis and lack-of-fit test
#' @param conf_level Confidence level for intervals. Default is 0.95.
#'
#' @return A `measure_linearity` object containing:
#'   - `r_squared`: Coefficient of determination
#'   - `adj_r_squared`: Adjusted R-squared
#'   - `slope`: Regression slope with CI
#'   - `intercept`: Regression intercept with CI
#'   - `residual_sd`: Residual standard deviation
#'   - `lack_of_fit`: Lack-of-fit test results (if replicates exist)
#'   - `range`: Concentration range evaluated
#'
#' @details
#' Linearity demonstrates that the method produces results that are
#' directly proportional to analyte concentration within a given range.
#'
#' ## Assessment Criteria
#' - R-squared >= 0.99 (typical for many applications)
#' - Residuals randomly distributed around zero
#' - No systematic pattern in residual plots
#' - Lack-of-fit test not significant (p > 0.05)
#'
#' ## ICH Q2 Requirements
#' Linearity should be evaluated across the range with at least 5
#' concentration levels. Report the regression equation, correlation
#' coefficient, and visual inspection of residual plots.
#'
#' @family accuracy
#' @seealso [measure_accuracy()], [measure_calibration_fit()]
#'
#' @export
#'
#' @examples
#' # Linearity assessment
#' set.seed(123)
#' data <- data.frame(
#'   concentration = rep(c(10, 25, 50, 75, 100), each = 3),
#'   response = rep(c(10, 25, 50, 75, 100), each = 3) * 1.5 + rnorm(15, 0, 2)
#' )
#'
#' result <- measure_linearity(data, "concentration", "response")
#' print(result)
measure_linearity <- function(
    data,
    conc_col,
    response_col,
    method = c("regression", "residual"),
    conf_level = 0.95) {

  method <- match.arg(method)

  # Validate inputs
  for (col in c(conc_col, response_col)) {
    if (!col %in% names(data)) {
      cli::cli_abort("Column {.field {col}} not found in data.")
    }
  }

  conc <- data[[conc_col]]
  response <- data[[response_col]]

  # Remove NA
  complete <- complete.cases(conc, response)
  conc <- conc[complete]
  response <- response[complete]
  n <- length(conc)

  if (n < 3) {
    cli::cli_abort("At least 3 data points required for linearity assessment.")
  }

  # Fit linear model
  fit <- stats::lm(response ~ conc)
  fit_summary <- summary(fit)

  # Extract coefficients
  coefs <- stats::coef(fit)
  conf_int <- stats::confint(fit, level = conf_level)

  # Regression statistics
  r_squared <- fit_summary$r.squared
  adj_r_squared <- fit_summary$adj.r.squared
  residual_sd <- fit_summary$sigma

  # Residual analysis
  residuals <- stats::residuals(fit)
  fitted_values <- stats::fitted(fit)

  # Check for lack of fit if there are replicates
  conc_levels <- unique(conc)
  has_replicates <- length(conc_levels) < n

  lof_result <- NULL
  if (has_replicates) {
    lof_result <- lack_of_fit_test(conc, response, fit)
  }

  # Build result
  result <- list(
    n = n,
    n_levels = length(conc_levels),
    range_min = min(conc),
    range_max = max(conc),
    slope = coefs[2],
    slope_ci_lower = conf_int[2, 1],
    slope_ci_upper = conf_int[2, 2],
    intercept = coefs[1],
    intercept_ci_lower = conf_int[1, 1],
    intercept_ci_upper = conf_int[1, 2],
    r_squared = r_squared,
    adj_r_squared = adj_r_squared,
    residual_sd = residual_sd,
    residual_cv = 100 * residual_sd / mean(response),
    lack_of_fit = lof_result,
    model = fit
  )

  structure(
    result,
    class = "measure_linearity",
    conc_col = conc_col,
    response_col = response_col,
    method = method,
    conf_level = conf_level
  )
}

#' Carryover Assessment
#'
#' Evaluates carryover by analyzing blank samples run after high-concentration
#' samples.
#'
#' @param data A data frame containing the run sequence with blanks after highs.
#' @param response_col Name of the column containing response values.
#' @param sample_type_col Name of the column identifying sample types.
#' @param run_order_col Name of the column containing run order.
#' @param blank_type Value identifying blank samples. Default is `"blank"`.
#' @param high_type Value identifying high-concentration samples. Default is `"high"`.
#' @param threshold Carryover threshold as percentage of LLOQ or high response.
#'   Default is 20 (meaning 20% of LLOQ).
#' @param lloq Optional LLOQ value for threshold calculation.
#'
#' @return A `measure_carryover` object containing:
#'   - `blank_responses`: Response values in blanks after high samples
#'   - `mean_blank`: Mean blank response
#'   - `max_blank`: Maximum blank response
#'   - `high_responses`: High sample responses
#'   - `carryover_pct`: Carryover as percentage of high or LLOQ
#'   - `pass`: Whether carryover is within acceptable limits
#'
#' @details
#' Carryover is the appearance of analyte in a blank sample due to
#' contamination from a previous high-concentration sample. It is typically
#' assessed by analyzing blank samples immediately after the highest
#' calibration standard or QC sample.
#'
#' ## Acceptance Criteria (ICH M10)
#' Carryover in the blank sample following the high concentration should
#' not exceed:
#' - 20% of the LLOQ (for the analyte)
#' - 5% of the internal standard response
#'
#' @family accuracy
#' @seealso [measure_accuracy()], [measure_system_suitability()]
#'
#' @export
#'
#' @examples
#' # Carryover assessment
#' data <- data.frame(
#'   run_order = 1:10,
#'   sample_type = c("std", "std", "std", "high", "blank",
#'                   "qc", "qc", "high", "blank", "std"),
#'   response = c(100, 500, 1000, 5000, 5, 500, 510, 4900, 8, 100)
#' )
#'
#' result <- measure_carryover(
#'   data,
#'   response_col = "response",
#'   sample_type_col = "sample_type",
#'   run_order_col = "run_order",
#'   lloq = 50
#' )
#' print(result)
measure_carryover <- function(
    data,
    response_col,
    sample_type_col,
    run_order_col,
    blank_type = "blank",
    high_type = "high",
    threshold = 20,
    lloq = NULL) {

  # Validate inputs
  for (col in c(response_col, sample_type_col, run_order_col)) {
    if (!col %in% names(data)) {
      cli::cli_abort("Column {.field {col}} not found in data.")
    }
  }

  # Sort by run order

  data <- data[order(data[[run_order_col]]), ]

  # Find blanks that follow high samples
  n <- nrow(data)
  sample_types <- data[[sample_type_col]]
  responses <- data[[response_col]]

  blanks_after_high <- logical(n)
  high_before_blank <- numeric(n)

  for (i in 2:n) {
    if (sample_types[i] == blank_type && sample_types[i-1] == high_type) {
      blanks_after_high[i] <- TRUE
      high_before_blank[i] <- responses[i-1]
    }
  }

  if (sum(blanks_after_high) == 0) {
    cli::cli_abort("No blank samples found immediately after high samples.")
  }

  # Extract relevant responses
  blank_responses <- responses[blanks_after_high]
  high_responses <- high_before_blank[blanks_after_high]

  # Calculate carryover
  mean_blank <- mean(blank_responses, na.rm = TRUE)
  max_blank <- max(blank_responses, na.rm = TRUE)
  mean_high <- mean(high_responses, na.rm = TRUE)

  # Calculate carryover percentage
  if (!is.null(lloq)) {
    carryover_pct <- 100 * max_blank / lloq
    reference_value <- lloq
    reference_type <- "LLOQ"
  } else {
    carryover_pct <- 100 * max_blank / mean_high
    reference_value <- mean_high
    reference_type <- "high"
  }

  # Assess pass/fail
  pass <- carryover_pct <= threshold

  result <- list(
    n_pairs = sum(blanks_after_high),
    blank_responses = blank_responses,
    high_responses = high_responses,
    mean_blank = mean_blank,
    max_blank = max_blank,
    mean_high = mean_high,
    carryover_pct = carryover_pct,
    threshold = threshold,
    reference_value = reference_value,
    reference_type = reference_type,
    pass = pass
  )

  structure(
    result,
    class = "measure_carryover",
    lloq = lloq,
    blank_type = blank_type,
    high_type = high_type
  )
}

# ==============================================================================
# Helper functions
# ==============================================================================

#' Calculate accuracy statistics
#' @noRd
calculate_accuracy_stats <- function(measured, reference, conf_level, group) {
  complete <- complete.cases(measured, reference)
  measured <- measured[complete]
  reference <- reference[complete]
  n <- length(measured)

  if (n < 2) {
    cli::cli_abort("At least 2 observations required to calculate accuracy.")
  }

  # Calculate recovery for each pair
  recovery <- 100 * measured / reference

  # Summary statistics
  mean_measured <- mean(measured)
  mean_reference <- mean(reference)
  mean_recovery <- mean(recovery)
  sd_recovery <- sd(recovery)

  # Bias
  bias <- mean_measured - mean_reference
  bias_pct <- 100 * bias / mean_reference

  # Confidence interval for recovery
  alpha <- 1 - conf_level
  t_crit <- stats::qt(1 - alpha / 2, df = n - 1)
  se_recovery <- sd_recovery / sqrt(n)
  recovery_ci_lower <- mean_recovery - t_crit * se_recovery
  recovery_ci_upper <- mean_recovery + t_crit * se_recovery

  tibble::tibble(
    group = group,
    n = n,
    mean_measured = mean_measured,
    mean_reference = mean_reference,
    bias = bias,
    bias_pct = bias_pct,
    mean_recovery = mean_recovery,
    sd_recovery = sd_recovery,
    cv_recovery = 100 * sd_recovery / mean_recovery,
    recovery_ci_lower = recovery_ci_lower,
    recovery_ci_upper = recovery_ci_upper
  )
}

#' Lack of fit test for linearity
#' @noRd
lack_of_fit_test <- function(conc, response, fit) {
  # Group by concentration level
  conc_factor <- as.factor(conc)
  levels_vec <- levels(conc_factor)

  # Calculate pure error SS
  ss_pure_error <- 0
  df_pure_error <- 0

  for (lev in levels_vec) {
    group_response <- response[conc_factor == lev]
    if (length(group_response) > 1) {
      ss_pure_error <- ss_pure_error + sum((group_response - mean(group_response))^2)
      df_pure_error <- df_pure_error + length(group_response) - 1
    }
  }

  if (df_pure_error == 0) {
    return(list(
      f_statistic = NA,
      p_value = NA,
      significant = NA,
      note = "No replicates for pure error estimation"
    ))
  }

  # Calculate lack of fit SS
  ss_residual <- sum(stats::residuals(fit)^2)
  ss_lack_of_fit <- ss_residual - ss_pure_error
  df_lack_of_fit <- length(levels_vec) - 2  # number of levels - 2 (intercept + slope)

  if (df_lack_of_fit <= 0) {
    return(list(
      f_statistic = NA,
      p_value = NA,
      significant = NA,
      note = "Insufficient levels for lack-of-fit test"
    ))
  }

  # F-test
  ms_lack_of_fit <- ss_lack_of_fit / df_lack_of_fit
  ms_pure_error <- ss_pure_error / df_pure_error
  f_stat <- ms_lack_of_fit / ms_pure_error
  p_value <- stats::pf(f_stat, df_lack_of_fit, df_pure_error, lower.tail = FALSE)

  list(
    f_statistic = f_stat,
    df_lof = df_lack_of_fit,
    df_pe = df_pure_error,
    p_value = p_value,
    significant = p_value < 0.05
  )
}

# ==============================================================================
# Print methods
# ==============================================================================

#' @export
print.measure_accuracy <- function(x, ...) {
  cat("measure_accuracy\n")
  cat(cli::rule(), "\n\n")

  for (i in seq_len(nrow(x))) {
    if (x$group[i] != "overall") {
      cat("Group:", x$group[i], "\n")
    }
    cat("  n =", x$n[i], "\n")
    cat("  Mean measured =", format(x$mean_measured[i], digits = 4), "\n")
    cat("  Mean reference =", format(x$mean_reference[i], digits = 4), "\n")
    cat("  Bias =", format(x$bias[i], digits = 4),
        "(", format(x$bias_pct[i], digits = 2), "%)\n")
    cat("  Recovery =", format(x$mean_recovery[i], digits = 2), "%\n")
    cat("  Recovery 95% CI: [",
        format(x$recovery_ci_lower[i], digits = 2), "%, ",
        format(x$recovery_ci_upper[i], digits = 2), "%]\n", sep = "")
    cat("\n")
  }

  invisible(x)
}

#' @export
print.measure_linearity <- function(x, ...) {
  cat("measure_linearity\n")
  cat(cli::rule(), "\n\n")

  cat("Data:\n")
  cat("  n =", x$n, "(", x$n_levels, "levels )\n")
  cat("  Range:", x$range_min, "-", x$range_max, "\n\n")

  cat("Regression:\n")
  cat("  Slope =", format(x$slope, digits = 4), "\n")
  cat("    95% CI: [", format(x$slope_ci_lower, digits = 4), ", ",
      format(x$slope_ci_upper, digits = 4), "]\n", sep = "")
  cat("  Intercept =", format(x$intercept, digits = 4), "\n")
  cat("    95% CI: [", format(x$intercept_ci_lower, digits = 4), ", ",
      format(x$intercept_ci_upper, digits = 4), "]\n\n", sep = "")


  cat("Fit Quality:\n")
  cat("  R-squared =", format(x$r_squared, digits = 5), "\n")
  cat("  Adj. R-squared =", format(x$adj_r_squared, digits = 5), "\n")
  cat("  Residual SD =", format(x$residual_sd, digits = 4), "\n")
  cat("  Residual CV =", format(x$residual_cv, digits = 2), "%\n\n")

  if (!is.null(x$lack_of_fit)) {
    lof <- x$lack_of_fit
    if (!is.na(lof$f_statistic)) {
      cat("Lack-of-Fit Test:\n")
      cat("  F =", format(lof$f_statistic, digits = 3), "\n")
      cat("  p-value =", format(lof$p_value, digits = 4), "\n")
      if (lof$significant) {
        cat("  Result: SIGNIFICANT (potential non-linearity)\n")
      } else {
        cat("  Result: Not significant (linearity acceptable)\n")
      }
    } else if (!is.null(lof$note)) {
      cat("Lack-of-Fit Test:", lof$note, "\n")
    }
  }

  invisible(x)
}

#' @export
print.measure_carryover <- function(x, ...) {
  cat("measure_carryover\n")
  cat(cli::rule(), "\n\n")

  cat("Evaluation:\n")
  cat("  High-blank pairs:", x$n_pairs, "\n")
  cat("  Mean high response:", format(x$mean_high, digits = 4), "\n")
  cat("  Mean blank response:", format(x$mean_blank, digits = 4), "\n")
  cat("  Max blank response:", format(x$max_blank, digits = 4), "\n\n")

  cat("Carryover:\n")
  cat("  Reference (", x$reference_type, "):", format(x$reference_value, digits = 4), "\n", sep = "")
  cat("  Carryover:", format(x$carryover_pct, digits = 2), "% of", x$reference_type, "\n")
  cat("  Threshold:", x$threshold, "%\n\n")

  if (x$pass) {
    cat("Result: PASS\n")
  } else {
    cat("Result: FAIL\n")
  }

  invisible(x)
}

# ==============================================================================
# Tidy methods
# ==============================================================================

#' @rdname tidy.recipe
#' @export
tidy.measure_accuracy <- function(x, ...) {
  tibble::as_tibble(x)
}

#' @rdname tidy.recipe
#' @export
tidy.measure_linearity <- function(x, ...) {
  tibble::tibble(
    n = x$n,
    n_levels = x$n_levels,
    range_min = x$range_min,
    range_max = x$range_max,
    slope = x$slope,
    slope_ci_lower = x$slope_ci_lower,
    slope_ci_upper = x$slope_ci_upper,
    intercept = x$intercept,
    intercept_ci_lower = x$intercept_ci_lower,
    intercept_ci_upper = x$intercept_ci_upper,
    r_squared = x$r_squared,
    adj_r_squared = x$adj_r_squared,
    residual_sd = x$residual_sd,
    residual_cv = x$residual_cv,
    lof_f = if (!is.null(x$lack_of_fit)) x$lack_of_fit$f_statistic else NA_real_,
    lof_p = if (!is.null(x$lack_of_fit)) x$lack_of_fit$p_value else NA_real_,
    lof_significant = if (!is.null(x$lack_of_fit)) x$lack_of_fit$significant else NA
  )
}

#' @rdname tidy.recipe
#' @export
tidy.measure_carryover <- function(x, ...) {
  tibble::tibble(
    n_pairs = x$n_pairs,
    mean_blank = x$mean_blank,
    max_blank = x$max_blank,
    mean_high = x$mean_high,
    carryover_pct = x$carryover_pct,
    threshold = x$threshold,
    reference_type = x$reference_type,
    reference_value = x$reference_value,
    pass = x$pass
  )
}

# ==============================================================================
# Autoplot methods
# ==============================================================================

#' @rdname autoplot.measure_uncertainty_budget
#' @export
autoplot.measure_linearity <- function(object, type = c("fit", "residuals"), ...) {
  type <- match.arg(type)
  fit <- object$model

  if (type == "fit") {
    # Predicted vs actual
    data <- data.frame(
      concentration = fit$model[[2]],
      response = fit$model[[1]],
      fitted = stats::fitted(fit)
    )

    p <- ggplot2::ggplot(data, ggplot2::aes(x = .data$concentration)) +
      ggplot2::geom_point(ggplot2::aes(y = .data$response), size = 2) +
      ggplot2::geom_line(ggplot2::aes(y = .data$fitted), color = "steelblue") +
      ggplot2::labs(
        title = "Linearity Assessment",
        subtitle = paste0("R-squared = ", format(object$r_squared, digits = 4)),
        x = attr(object, "conc_col"),
        y = attr(object, "response_col")
      ) +
      ggplot2::theme_minimal()
  } else {
    # Residual plot
    data <- data.frame(
      fitted = stats::fitted(fit),
      residuals = stats::residuals(fit)
    )

    p <- ggplot2::ggplot(data, ggplot2::aes(x = .data$fitted, y = .data$residuals)) +
      ggplot2::geom_point(size = 2) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      ggplot2::labs(
        title = "Residual Plot",
        x = "Fitted Values",
        y = "Residuals"
      ) +
      ggplot2::theme_minimal()
  }

  p
}
