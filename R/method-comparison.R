# ==============================================================================
# Method Comparison Functions
#
# This file contains functions for comparing analytical methods:
# - measure_bland_altman: Bland-Altman analysis with limits of agreement
# - measure_deming_regression: Deming regression for method comparison
# - measure_passing_bablok: Passing-Bablok regression (requires mcr)
# - measure_proficiency_score: z-scores, En scores, zeta scores
# ==============================================================================

#' Bland-Altman Method Comparison
#'
#' Performs Bland-Altman analysis to compare two measurement methods. This
#' calculates the mean bias, limits of agreement, and optionally tests for
#' proportional bias.
#'
#' @param data A data frame containing paired measurements from both methods.
#' @param method1_col Name of the column containing method 1 (reference) values.
#' @param method2_col Name of the column containing method 2 (test) values.
#' @param id_col Optional name of a column identifying paired observations.
#' @param conf_level Confidence level for intervals. Default is 0.95.
#' @param regression Test for proportional bias:
#'   - `"none"` (default): No regression test
#'   - `"linear"`: Test for linear trend in bias
#'   - `"quadratic"`: Test for quadratic trend
#'
#' @return A `measure_bland_altman` object containing:
#'   - `data`: Tibble with mean, difference, and LOA for each observation
#'   - `statistics`: List of summary statistics (bias, SD, LOA, CIs)
#'   - `regression`: Regression results if requested (model, p-value)
#'
#' @details
#' ## Interpretation
#'
#' The Bland-Altman plot shows the difference between methods against their
#' mean. Key features:
#' - **Mean bias**: Average difference (systematic error)
#' - **Limits of agreement (LOA)**: Range containing 95% of differences
#' - **Proportional bias**: Trend in differences with concentration
#'
#' ## Acceptance Criteria
#'
#' Methods are typically considered interchangeable if:
#' - Mean bias is clinically/analytically insignificant
#' - LOA width is acceptable for the intended use
#' - No significant proportional bias
#'
#' @family method-comparison
#' @seealso [measure_deming_regression()], [measure_passing_bablok()]
#'
#' @export
#'
#' @examples
#' # Compare two blood glucose meters
#' set.seed(123)
#' data <- data.frame(
#'   patient_id = 1:30,
#'   meter_A = rnorm(30, mean = 100, sd = 15),
#'   meter_B = rnorm(30, mean = 102, sd = 16)
#' )
#'
#' ba <- measure_bland_altman(
#'   data,
#'   method1_col = "meter_A",
#'   method2_col = "meter_B",
#'   regression = "linear"
#' )
#'
#' print(ba)
#' tidy(ba)
#'
#' # Visualize
#' ggplot2::autoplot(ba)
measure_bland_altman <- function(
    data,
    method1_col,
    method2_col,
    id_col = NULL,
    conf_level = 0.95,
    regression = c("none", "linear", "quadratic")) {

  regression <- match.arg(regression)

  # Validate inputs
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame.")
  }

  for (col in c(method1_col, method2_col)) {
    if (!col %in% names(data)) {
      cli::cli_abort("Column {.field {col}} not found in data.")
    }
    if (!is.numeric(data[[col]])) {
      cli::cli_abort("Column {.field {col}} must be numeric.")
    }
  }

  if (!is.null(id_col) && !id_col %in% names(data)) {
    cli::cli_abort("ID column {.field {id_col}} not found in data.")
  }

  # Extract values
  method1 <- data[[method1_col]]
  method2 <- data[[method2_col]]

  # Remove incomplete pairs
  complete <- complete.cases(method1, method2)
  if (sum(!complete) > 0) {
    cli::cli_warn(
      "Removed {sum(!complete)} observation{?s} with missing values."
    )
  }
  method1 <- method1[complete]
  method2 <- method2[complete]
  n <- length(method1)

  if (n < 3) {
    cli::cli_abort("At least 3 paired observations required.")
  }

  # Calculate mean and difference
  mean_vals <- (method1 + method2) / 2
  diff_vals <- method2 - method1

  # Calculate Bland-Altman statistics
  ba_stats <- .calculate_ba_stats(mean_vals, diff_vals, conf_level)

  # Create data tibble
  result_data <- tibble::tibble(
    obs = seq_len(n),
    method1 = method1,
    method2 = method2,
    mean = mean_vals,
    difference = diff_vals,
    lower_loa = ba_stats$lower_loa,
    upper_loa = ba_stats$upper_loa
  )

  if (!is.null(id_col)) {
    result_data$id <- data[[id_col]][complete]
  }

  # Test for proportional bias if requested
  regression_result <- NULL
  if (regression != "none") {
    regression_result <- .test_proportional_bias(mean_vals, diff_vals, regression)
  }

  structure(
    list(
      data = result_data,
      statistics = ba_stats,
      regression = regression_result,
      call = match.call()
    ),
    class = "measure_bland_altman",
    method1_col = method1_col,
    method2_col = method2_col,
    conf_level = conf_level,
    regression_type = regression
  )
}

#' Deming Regression for Method Comparison
#'
#' Performs Deming regression to compare two measurement methods when both
#' have measurement error. This is preferred over ordinary least squares
#' when both methods have non-negligible error.
#'
#' @param data A data frame containing paired measurements.
#' @param method1_col Name of column for method 1 (typically reference/comparator).
#' @param method2_col Name of column for method 2 (typically test method).
#' @param error_ratio Ratio of error variances (var_method2 / var_method1).
#'   Default is 1 (equal variances). Can be estimated from replicate data.
#' @param method1_sd Optional known SD of method 1. Used to calculate error_ratio.
#' @param method2_sd Optional known SD of method 2. Used to calculate error_ratio.
#' @param bootstrap Use bootstrap for confidence intervals? Default is FALSE.
#' @param bootstrap_n Number of bootstrap samples. Default is 1000.
#' @param conf_level Confidence level for intervals. Default is 0.95.
#'
#' @return A `measure_deming_regression` object containing:
#'   - `coefficients`: Tibble with intercept and slope estimates and CIs
#'   - `statistics`: List of diagnostic statistics (RMSE, R-squared)
#'   - `data_summary`: Summary of input data
#'   - `bootstrap`: Bootstrap results if requested
#'
#' @details
#' ## Error Ratio
#'
#' The error ratio (lambda) represents the ratio of error variances:
#' `lambda = var(method2) / var(method1)`
#'
#' Common approaches:

#' - `lambda = 1`: Assume equal error variances
#' - Estimate from replicates: Use SDs from replicate measurements
#' - Estimate from calibration: Use known method precision data
#'
#' ## Interpretation
#'
#' For equivalent methods:
#' - Slope should be close to 1 (proportional agreement)
#' - Intercept should be close to 0 (no constant bias)
#'
#' If 95% CI for slope includes 1 and CI for intercept includes 0,
#' methods are considered equivalent.
#'
#' ## Implementation
#'
#' If the `mcr` package is available, it is used for fitting. Otherwise,
#' a manual implementation is used with optional bootstrap CIs.
#'
#' @family method-comparison
#' @seealso [measure_bland_altman()], [measure_passing_bablok()]
#'
#' @export
#'
#' @examples
#' # Method comparison data
#' data <- data.frame(
#'   reference = c(5.2, 10.5, 15.8, 25.3, 50.1, 75.4, 100.2),
#'   new_method = c(5.1, 10.8, 16.2, 25.9, 49.8, 76.1, 101.3)
#' )
#'
#' # Deming regression with bootstrap CIs
#' result <- measure_deming_regression(
#'   data,
#'   method1_col = "reference",
#'   method2_col = "new_method",
#'   bootstrap = TRUE,
#'   bootstrap_n = 500
#' )
#'
#' print(result)
#' tidy(result)
measure_deming_regression <- function(
    data,
    method1_col,
    method2_col,
    error_ratio = NULL,
    method1_sd = NULL,
    method2_sd = NULL,
    bootstrap = FALSE,
    bootstrap_n = 1000,
    conf_level = 0.95) {

  # Validate inputs
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame.")
  }

  for (col in c(method1_col, method2_col)) {
    if (!col %in% names(data)) {
      cli::cli_abort("Column {.field {col}} not found in data.")
    }
    if (!is.numeric(data[[col]])) {
      cli::cli_abort("Column {.field {col}} must be numeric.")
    }
  }

  # Extract values
  x <- data[[method1_col]]
  y <- data[[method2_col]]

  # Remove incomplete pairs
  complete <- complete.cases(x, y)
  if (sum(!complete) > 0) {
    cli::cli_warn(
      "Removed {sum(!complete)} observation{?s} with missing values."
    )
  }
  x <- x[complete]
  y <- y[complete]
  n <- length(x)

  if (n < 3) {
    cli::cli_abort("At least 3 paired observations required.")
  }

  # Determine error ratio
  if (!is.null(error_ratio)) {
    lambda <- error_ratio
  } else if (!is.null(method1_sd) && !is.null(method2_sd)) {
    lambda <- method2_sd^2 / method1_sd^2
  } else {
    lambda <- 1
    cli::cli_inform(
      "Using default error ratio of 1. Provide {.arg error_ratio} or SDs for more accurate results."
    )
  }

  # Try mcr package first
  use_mcr <- requireNamespace("mcr", quietly = TRUE)

  if (use_mcr) {
    mcr_fit <- mcr::mcreg(x, y, method.reg = "Deming", error.ratio = lambda)
    intercept <- mcr_fit@para["Intercept"]
    slope <- mcr_fit@para["Slope"]
    intercept_ci <- mcr_fit@para.ci["Intercept", ]
    slope_ci <- mcr_fit@para.ci["Slope", ]
    residuals <- y - (intercept + slope * x)
    rmse <- sqrt(mean(residuals^2))
    r_squared <- stats::cor(x, y)^2

    coef_tibble <- tibble::tibble(
      term = c("intercept", "slope"),
      estimate = c(intercept, slope),
      ci_lower = c(intercept_ci["CI.low"], slope_ci["CI.low"]),
      ci_upper = c(intercept_ci["CI.up"], slope_ci["CI.up"])
    )

    bootstrap_result <- NULL
  } else {
    # Use manual implementation
    cli::cli_inform(
      "Using manual Deming implementation. Install {.pkg mcr} for additional features."
    )

    fit <- .deming_regression_manual(x, y, lambda)

    if (bootstrap) {
      boot_result <- .deming_bootstrap(x, y, lambda, bootstrap_n, conf_level)

      coef_tibble <- tibble::tibble(
        term = c("intercept", "slope"),
        estimate = c(fit$intercept, fit$slope),
        ci_lower = c(boot_result$intercept_ci[1], boot_result$slope_ci[1]),
        ci_upper = c(boot_result$intercept_ci[2], boot_result$slope_ci[2])
      )

      bootstrap_result <- boot_result
    } else {
      coef_tibble <- tibble::tibble(
        term = c("intercept", "slope"),
        estimate = c(fit$intercept, fit$slope),
        ci_lower = NA_real_,
        ci_upper = NA_real_
      )

      bootstrap_result <- NULL
    }

    rmse <- fit$rmse
    r_squared <- fit$r_squared
    residuals <- fit$residuals
  }

  # Data summary
  data_summary <- tibble::tibble(
    statistic = c("n", "method1_mean", "method1_sd", "method2_mean", "method2_sd", "correlation"),
    value = c(n, mean(x), sd(x), mean(y), sd(y), stats::cor(x, y))
  )

  structure(
    list(
      coefficients = coef_tibble,
      statistics = list(
        n = n,
        error_ratio = lambda,
        rmse = rmse,
        r_squared = r_squared
      ),
      data_summary = data_summary,
      bootstrap = bootstrap_result,
      fitted = coef_tibble$estimate[1] + coef_tibble$estimate[2] * x,
      residuals = residuals,
      call = match.call()
    ),
    class = "measure_deming_regression",
    method1_col = method1_col,
    method2_col = method2_col,
    conf_level = conf_level,
    used_mcr = use_mcr
  )
}

#' Passing-Bablok Regression for Method Comparison
#'
#' Performs Passing-Bablok regression, a non-parametric method for comparing
#' two analytical methods. This is robust to outliers and does not require
#' normal distribution of residuals.
#'
#' @param data A data frame containing paired measurements.
#' @param method1_col Name of column for method 1 (reference/comparator).
#' @param method2_col Name of column for method 2 (test method).
#' @param conf_level Confidence level for intervals. Default is 0.95.
#' @param alpha Significance level for CUSUM linearity test. Default is 0.05.
#'
#' @return A `measure_passing_bablok` object containing:
#'   - `coefficients`: Tibble with intercept and slope estimates and CIs
#'   - `linearity`: CUSUM test results for linearity assumption
#'   - `statistics`: Summary statistics
#'
#' @details
#' ## Method
#'
#' Passing-Bablok regression:
#' 1. Calculates slopes between all pairs of points
#' 2. Uses median slope as the estimate (robust to outliers)
#' 3. Calculates intercept from median slope
#' 4. Uses non-parametric confidence intervals
#'
#' ## CUSUM Linearity Test
#'
#' Tests the assumption of linear relationship. If significant (p < alpha),
#' the linear model may not be appropriate.
#'
#' ## Interpretation
#'
#' For equivalent methods:
#' - 95% CI for slope includes 1
#' - 95% CI for intercept includes 0
#'
#' ## Requirements
#'
#' This function requires the `mcr` package. Install with:
#' `install.packages("mcr")`
#'
#' @family method-comparison
#' @seealso [measure_bland_altman()], [measure_deming_regression()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Requires mcr package
#' data <- data.frame(
#'   reference = c(5.2, 10.5, 15.8, 25.3, 50.1, 75.4, 100.2),
#'   new_method = c(5.1, 10.8, 16.2, 25.9, 49.8, 76.1, 101.3)
#' )
#'
#' result <- measure_passing_bablok(
#'   data,
#'   method1_col = "reference",
#'   method2_col = "new_method"
#' )
#'
#' print(result)
#' }
measure_passing_bablok <- function(
    data,
    method1_col,
    method2_col,
    conf_level = 0.95,
    alpha = 0.05) {

  # Check for mcr package - required for Passing-Bablok
  if (!requireNamespace("mcr", quietly = TRUE)) {
    cli::cli_abort(
      c(
        "{.fn measure_passing_bablok} requires the {.pkg mcr} package.",
        "i" = "Install with: {.code install.packages('mcr')}"
      )
    )
  }

  # Validate inputs
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame.")
  }

  for (col in c(method1_col, method2_col)) {
    if (!col %in% names(data)) {
      cli::cli_abort("Column {.field {col}} not found in data.")
    }
    if (!is.numeric(data[[col]])) {
      cli::cli_abort("Column {.field {col}} must be numeric.")
    }
  }

  # Extract values
  x <- data[[method1_col]]
  y <- data[[method2_col]]

  # Remove incomplete pairs
  complete <- complete.cases(x, y)
  if (sum(!complete) > 0) {
    cli::cli_warn(
      "Removed {sum(!complete)} observation{?s} with missing values."
    )
  }
  x <- x[complete]
  y <- y[complete]
  n <- length(x)

  if (n < 10) {
    cli::cli_warn(
      "Passing-Bablok regression typically requires at least 10 observations for reliable results."
    )
  }

  # Fit Passing-Bablok regression using mcr
  pb_fit <- mcr::mcreg(x, y, method.reg = "PaBa", alpha = 1 - conf_level)

  # Extract coefficients
  intercept <- pb_fit@para["Intercept"]
  slope <- pb_fit@para["Slope"]
  intercept_ci <- pb_fit@para.ci["Intercept", ]
  slope_ci <- pb_fit@para.ci["Slope", ]

  coef_tibble <- tibble::tibble(
    term = c("intercept", "slope"),
    estimate = c(intercept, slope),
    ci_lower = c(intercept_ci["CI.low"], slope_ci["CI.low"]),
    ci_upper = c(intercept_ci["CI.up"], slope_ci["CI.up"])
  )

  # CUSUM linearity test
  # mcr provides this in the object
  cusum_test <- tryCatch({
    list(
      p_value = pb_fit@cinfo[4],  # CUSUM p-value
      linear = pb_fit@cinfo[4] > alpha
    )
  }, error = function(e) {
    list(p_value = NA, linear = NA)
  })

  # Calculate residuals and RMSE
  fitted <- intercept + slope * x
  residuals <- y - fitted
  rmse <- sqrt(mean(residuals^2))

  structure(
    list(
      coefficients = coef_tibble,
      linearity = cusum_test,
      statistics = list(
        n = n,
        rmse = rmse,
        correlation = stats::cor(x, y)
      ),
      fitted = fitted,
      residuals = residuals,
      call = match.call()
    ),
    class = "measure_passing_bablok",
    method1_col = method1_col,
    method2_col = method2_col,
    conf_level = conf_level
  )
}

#' Proficiency Testing Scores
#'
#' Calculates proficiency testing scores (z-scores, En scores, or zeta scores)
#' for evaluating laboratory performance in interlaboratory comparisons.
#'
#' @param data A data frame containing measurement data.
#' @param measured_col Name of column with measured/reported values.
#' @param reference_col Name of column with reference/assigned values.
#' @param uncertainty_col Name of column with measurement uncertainties.
#'   Required for En and zeta scores.
#' @param reference_uncertainty_col Name of column with reference value
#'   uncertainties. Optional for En/zeta scores.
#' @param score_type Type of score to calculate:
#'   - `"z_score"` (default): (measured - reference) / sigma
#'   - `"en_score"`: (measured - reference) / sqrt(U_meas^2 + U_ref^2)
#'   - `"zeta_score"`: Similar to En, for correlated uncertainties
#' @param sigma Standard deviation for z-score calculation. If NULL,
#'   estimated from the data.
#' @param group_col Optional grouping column for separate assessments.
#'
#' @return A `measure_proficiency_score` object containing:
#'   - `scores`: Tibble with individual scores and flags
#'   - `statistics`: Summary statistics and counts
#'
#' @details
#' ## Score Interpretation
#'
#' | |Score| | Status | Action |
#' |---------|---------------|--------|
#' | <= 2    | Satisfactory  | None   |
#' | 2-3     | Questionable  | Review |
#' | > 3     | Unsatisfactory| Investigate |
#'
#' ## Score Types
#'
#' **z-score**: Uses a fixed standard deviation (sigma), typically derived
#' from historical data or consensus of participants.
#'
#' **En score**: Uses expanded uncertainties of both the lab result and
#' reference value. Appropriate when uncertainties are well-characterized.
#'
#' **zeta score**: Similar to En, but accounts for potential correlation
#' between lab and reference uncertainties.
#'
#' @family method-comparison
#' @seealso [measure_accuracy()], [criteria_proficiency_testing()]
#'
#' @export
#'
#' @examples
#' # Proficiency testing results from multiple labs
#' pt_data <- data.frame(
#'   lab_id = paste0("Lab_", 1:10),
#'   measured = c(99.2, 100.5, 98.8, 101.2, 97.5, 100.1, 99.8, 102.3, 100.6, 94.0),
#'   assigned = rep(100, 10),
#'   uncertainty = c(1.5, 2.0, 1.8, 1.6, 2.2, 1.9, 1.7, 2.1, 1.5, 2.0)
#' )
#'
#' # z-scores with known sigma
#' z_result <- measure_proficiency_score(
#'   pt_data,
#'   measured_col = "measured",
#'   reference_col = "assigned",
#'   score_type = "z_score",
#'   sigma = 2.5
#' )
#'
#' print(z_result)
#'
#' # En scores using uncertainties
#' en_result <- measure_proficiency_score(
#'   pt_data,
#'   measured_col = "measured",
#'   reference_col = "assigned",
#'   uncertainty_col = "uncertainty",
#'   score_type = "en_score"
#' )
#'
#' print(en_result)
measure_proficiency_score <- function(
    data,
    measured_col,
    reference_col,
    uncertainty_col = NULL,
    reference_uncertainty_col = NULL,
    score_type = c("z_score", "en_score", "zeta_score"),
    sigma = NULL,
    group_col = NULL) {

  score_type <- match.arg(score_type)

  # Validate inputs
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame.")
  }

  for (col in c(measured_col, reference_col)) {
    if (!col %in% names(data)) {
      cli::cli_abort("Column {.field {col}} not found in data.")
    }
    if (!is.numeric(data[[col]])) {
      cli::cli_abort("Column {.field {col}} must be numeric.")
    }
  }

  # Validate uncertainty columns for En/zeta scores
  if (score_type %in% c("en_score", "zeta_score")) {
    if (is.null(uncertainty_col)) {
      cli::cli_abort(
        "{.arg uncertainty_col} is required for {score_type} calculation."
      )
    }
    if (!uncertainty_col %in% names(data)) {
      cli::cli_abort("Uncertainty column {.field {uncertainty_col}} not found in data.")
    }
  }

  # Extract values
  measured <- data[[measured_col]]
  reference <- data[[reference_col]]

  # Determine sigma for z-scores
  if (score_type == "z_score") {
    if (is.null(sigma)) {
      # Estimate from robust IQR
      differences <- measured - reference
      sigma <- stats::IQR(differences, na.rm = TRUE) / 1.349  # Robust estimate
      cli::cli_inform(
        "Estimated sigma = {format(sigma, digits = 3)} from IQR of differences."
      )
    }
  }

  # Calculate scores
  n <- nrow(data)
  scores <- numeric(n)

  if (score_type == "z_score") {
    scores <- .calculate_z_score(measured, reference, sigma)
  } else if (score_type %in% c("en_score", "zeta_score")) {
    u_measured <- data[[uncertainty_col]]

    # Reference uncertainty (default to 0 if not provided)
    if (!is.null(reference_uncertainty_col) && reference_uncertainty_col %in% names(data)) {
      u_reference <- data[[reference_uncertainty_col]]
    } else {
      u_reference <- 0
    }

    scores <- .calculate_en_score(measured, reference, u_measured, u_reference)
  }

  # Flag scores
  flags <- .flag_proficiency_scores(scores)

  # Build result tibble
  scores_tibble <- tibble::tibble(
    row = seq_len(n),
    measured = measured,
    reference = reference,
    score = scores,
    abs_score = abs(scores),
    flag = flags
  )

  if (!is.null(uncertainty_col)) {
    scores_tibble$uncertainty <- data[[uncertainty_col]]
  }

  if (!is.null(group_col) && group_col %in% names(data)) {
    scores_tibble$group <- data[[group_col]]
  }

  # Calculate summary statistics
  n_satisfactory <- sum(flags == "satisfactory", na.rm = TRUE)
  n_questionable <- sum(flags == "questionable", na.rm = TRUE)
  n_unsatisfactory <- sum(flags == "unsatisfactory", na.rm = TRUE)
  n_valid <- sum(!is.na(scores))

  statistics <- list(
    score_type = score_type,
    n = n,
    n_valid = n_valid,
    n_satisfactory = n_satisfactory,
    n_questionable = n_questionable,
    n_unsatisfactory = n_unsatisfactory,
    pct_satisfactory = 100 * n_satisfactory / n_valid,
    mean_score = mean(scores, na.rm = TRUE),
    sd_score = sd(scores, na.rm = TRUE),
    max_abs_score = max(abs(scores), na.rm = TRUE),
    sigma = sigma  # NULL for En/zeta scores
  )

  structure(
    list(
      scores = scores_tibble,
      statistics = statistics,
      call = match.call()
    ),
    class = "measure_proficiency_score",
    measured_col = measured_col,
    reference_col = reference_col,
    score_type = score_type
  )
}

# ==============================================================================
# Print methods
# ==============================================================================

#' @export
print.measure_bland_altman <- function(x, ...) {
  cat("measure_bland_altman\n")
  cat(cli::rule(), "\n\n")

  stats <- x$statistics

  cat("Bias Statistics:\n")
  cat("  n =", stats$n, "\n")
  cat("  Mean bias =", format(stats$mean_bias, digits = 4), "\n")
  cat("  SD of differences =", format(stats$sd_diff, digits = 4), "\n")
  cat("  95% CI for bias: [", format(stats$bias_ci_lower, digits = 4),
      ", ", format(stats$bias_ci_upper, digits = 4), "]\n\n", sep = "")

  cat("Limits of Agreement:\n")
  cat("  Lower LOA =", format(stats$lower_loa, digits = 4),
      "(95% CI: [", format(stats$loa_lower_ci[1], digits = 4), ", ",
      format(stats$loa_lower_ci[2], digits = 4), "])\n", sep = " ")
  cat("  Upper LOA =", format(stats$upper_loa, digits = 4),
      "(95% CI: [", format(stats$loa_upper_ci[1], digits = 4), ", ",
      format(stats$loa_upper_ci[2], digits = 4), "])\n", sep = " ")
  cat("  LOA Width =", format(stats$loa_width, digits = 4), "\n\n")

  if (!is.null(x$regression)) {
    reg <- x$regression
    cat("Proportional Bias Test:\n")
    cat("  Slope =", format(reg$slope, digits = 4), "\n")
    cat("  p-value =", format(reg$slope_p_value, digits = 4), "\n")
    if (reg$significant) {
      cat("  Result: SIGNIFICANT proportional bias detected\n")
    } else {
      cat("  Result: No significant proportional bias\n")
    }
  }

  invisible(x)
}

#' @export
print.measure_deming_regression <- function(x, ...) {
  cat("measure_deming_regression\n")
  cat(cli::rule(), "\n\n")

  cat("Coefficients:\n")
  print(x$coefficients, n = 2)
  cat("\n")

  cat("Statistics:\n")
  cat("  n =", x$statistics$n, "\n")
  cat("  Error ratio =", format(x$statistics$error_ratio, digits = 3), "\n")
  cat("  RMSE =", format(x$statistics$rmse, digits = 4), "\n")
  cat("  R\u00b2 =", format(x$statistics$r_squared, digits = 4), "\n")

  if (attr(x, "used_mcr")) {
    cat("\n(Fitted using mcr package)\n")
  } else if (!is.null(x$bootstrap)) {
    cat("\n(CIs from", x$bootstrap$n_successful, "bootstrap samples)\n")
  }

  invisible(x)
}

#' @export
print.measure_passing_bablok <- function(x, ...) {
  cat("measure_passing_bablok\n")
  cat(cli::rule(), "\n\n")

  cat("Coefficients:\n")
  print(x$coefficients, n = 2)
  cat("\n")

  cat("Statistics:\n")
  cat("  n =", x$statistics$n, "\n")
  cat("  RMSE =", format(x$statistics$rmse, digits = 4), "\n")
  cat("  Correlation =", format(x$statistics$correlation, digits = 4), "\n\n")

  cat("Linearity Test (CUSUM):\n")
  if (!is.na(x$linearity$p_value)) {
    cat("  p-value =", format(x$linearity$p_value, digits = 4), "\n")
    if (x$linearity$linear) {
      cat("  Result: Linear relationship acceptable\n")
    } else {
      cat("  Result: SIGNIFICANT deviation from linearity\n")
    }
  } else {
    cat("  Not available\n")
  }

  invisible(x)
}

#' @export
print.measure_proficiency_score <- function(x, ...) {
  cat("measure_proficiency_score\n")
  cat(cli::rule(), "\n\n")

  stats <- x$statistics

  cat("Score Type:", stats$score_type, "\n")
  if (!is.null(stats$sigma)) {
    cat("Sigma:", format(stats$sigma, digits = 3), "\n")
  }
  cat("\n")

  cat("Results (n =", stats$n_valid, "):\n")
  cat("  Satisfactory (|z| \u2264 2):", stats$n_satisfactory,
      "(", format(stats$pct_satisfactory, digits = 1), "%)\n")
  cat("  Questionable (2 < |z| \u2264 3):", stats$n_questionable, "\n")
  cat("  Unsatisfactory (|z| > 3):", stats$n_unsatisfactory, "\n\n")

  cat("Score Statistics:\n")
  cat("  Mean score:", format(stats$mean_score, digits = 3), "\n")
  cat("  SD score:", format(stats$sd_score, digits = 3), "\n")
  cat("  Max |score|:", format(stats$max_abs_score, digits = 3), "\n")

  invisible(x)
}

# ==============================================================================
# Tidy methods
# ==============================================================================

#' @rdname tidy.recipe
#' @export
tidy.measure_bland_altman <- function(x, ...) {
  stats <- x$statistics

  tibble::tibble(
    statistic = c("n", "mean_bias", "sd_diff", "lower_loa", "upper_loa",
                  "loa_width", "bias_ci_lower", "bias_ci_upper"),
    value = c(stats$n, stats$mean_bias, stats$sd_diff, stats$lower_loa,
              stats$upper_loa, stats$loa_width, stats$bias_ci_lower,
              stats$bias_ci_upper)
  )
}

#' @rdname tidy.recipe
#' @export
tidy.measure_deming_regression <- function(x, ...) {
  x$coefficients
}

#' @rdname tidy.recipe
#' @export
tidy.measure_passing_bablok <- function(x, ...) {
  x$coefficients
}

#' @rdname tidy.recipe
#' @export
tidy.measure_proficiency_score <- function(x, type = c("scores", "summary"), ...) {
  type <- match.arg(type)

  if (type == "scores") {
    x$scores
  } else {
    stats <- x$statistics
    tibble::tibble(
      statistic = c("n", "n_satisfactory", "n_questionable", "n_unsatisfactory",
                    "pct_satisfactory", "mean_score", "sd_score", "max_abs_score"),
      value = c(stats$n, stats$n_satisfactory, stats$n_questionable,
                stats$n_unsatisfactory, stats$pct_satisfactory,
                stats$mean_score, stats$sd_score, stats$max_abs_score)
    )
  }
}

# ==============================================================================
# Glance methods
# ==============================================================================

#' @rdname tidy.recipe
#' @export
glance.measure_bland_altman <- function(x, ...) {
  stats <- x$statistics
  reg <- x$regression

  tibble::tibble(
    n = stats$n,
    mean_bias = stats$mean_bias,
    loa_width = stats$loa_width,
    proportional_bias_p = if (!is.null(reg)) reg$slope_p_value else NA_real_,
    proportional_bias = if (!is.null(reg)) reg$significant else NA
  )
}

#' @rdname tidy.recipe
#' @export
glance.measure_deming_regression <- function(x, ...) {
  coefs <- x$coefficients

  tibble::tibble(
    intercept = coefs$estimate[1],
    slope = coefs$estimate[2],
    intercept_ci_includes_0 = coefs$ci_lower[1] <= 0 & coefs$ci_upper[1] >= 0,
    slope_ci_includes_1 = coefs$ci_lower[2] <= 1 & coefs$ci_upper[2] >= 1,
    r_squared = x$statistics$r_squared,
    rmse = x$statistics$rmse
  )
}

#' @rdname tidy.recipe
#' @export
glance.measure_passing_bablok <- function(x, ...) {
  coefs <- x$coefficients

  tibble::tibble(
    intercept = coefs$estimate[1],
    slope = coefs$estimate[2],
    intercept_ci_includes_0 = coefs$ci_lower[1] <= 0 & coefs$ci_upper[1] >= 0,
    slope_ci_includes_1 = coefs$ci_lower[2] <= 1 & coefs$ci_upper[2] >= 1,
    linearity_p = x$linearity$p_value,
    linear = x$linearity$linear
  )
}

#' @rdname tidy.recipe
#' @export
glance.measure_proficiency_score <- function(x, ...) {
  stats <- x$statistics

  tibble::tibble(
    score_type = stats$score_type,
    n = stats$n,
    pct_satisfactory = stats$pct_satisfactory,
    max_abs_score = stats$max_abs_score,
    all_satisfactory = stats$n_unsatisfactory == 0 && stats$n_questionable == 0
  )
}

# ==============================================================================
# Autoplot methods
# ==============================================================================

#' Plot Bland-Altman Analysis
#'
#' Creates a Bland-Altman plot showing differences vs means with limits of agreement.
#'
#' @param object A `measure_bland_altman` object.
#' @param show_loa Show limits of agreement? Default TRUE.
#' @param show_ci Show confidence intervals for LOA? Default FALSE.
#' @param ... Additional arguments (unused).
#'
#' @return A ggplot object.
#'
#' @importFrom ggplot2 autoplot
#' @export
autoplot.measure_bland_altman <- function(object, show_loa = TRUE, show_ci = FALSE, ...) {
  data <- object$data
  stats <- object$statistics

  p <- ggplot2::ggplot(data, ggplot2::aes(x = .data$mean, y = .data$difference)) +
    ggplot2::geom_point(alpha = 0.7, size = 2) +
    ggplot2::geom_hline(yintercept = stats$mean_bias, color = "#2166AC", linewidth = 0.8) +
    ggplot2::labs(
      title = "Bland-Altman Plot",
      x = "Mean of Methods",
      y = "Difference (Method 2 - Method 1)"
    ) +
    ggplot2::theme_minimal()

  if (show_loa) {
    p <- p +
      ggplot2::geom_hline(yintercept = stats$lower_loa, color = "#B2182B",
                          linetype = "dashed", linewidth = 0.8) +
      ggplot2::geom_hline(yintercept = stats$upper_loa, color = "#B2182B",
                          linetype = "dashed", linewidth = 0.8)
  }

  if (show_ci) {
    p <- p +
      ggplot2::geom_rect(
        ggplot2::aes(xmin = -Inf, xmax = Inf,
                     ymin = stats$loa_lower_ci[1], ymax = stats$loa_lower_ci[2]),
        fill = "#B2182B", alpha = 0.1, inherit.aes = FALSE
      ) +
      ggplot2::geom_rect(
        ggplot2::aes(xmin = -Inf, xmax = Inf,
                     ymin = stats$loa_upper_ci[1], ymax = stats$loa_upper_ci[2]),
        fill = "#B2182B", alpha = 0.1, inherit.aes = FALSE
      )
  }

  # Add regression line if proportional bias was tested
  if (!is.null(object$regression)) {
    p <- p +
      ggplot2::geom_smooth(method = "lm", se = FALSE, color = "#762A83",
                           linetype = "dotted", linewidth = 0.8)
  }

  p
}

#' Plot Method Comparison Regression
#'
#' Creates a scatter plot with regression line for method comparison.
#'
#' @param object A `measure_deming_regression` or `measure_passing_bablok` object.
#' @param show_identity Show y = x identity line? Default TRUE.
#' @param ... Additional arguments (unused).
#'
#' @return A ggplot object.
#'
#' @importFrom ggplot2 autoplot
#' @export
autoplot.measure_deming_regression <- function(object, show_identity = TRUE, ...) {
  coefs <- object$coefficients
  intercept <- coefs$estimate[1]
  slope <- coefs$estimate[2]

  # Reconstruct original data from summary
  # This is a limitation - we don't store raw data
  data_summary <- object$data_summary
  n <- data_summary$value[data_summary$statistic == "n"]

  # Create a simple range for the line
  x_range <- c(0, data_summary$value[data_summary$statistic == "method1_mean"] * 2)

  p <- ggplot2::ggplot() +
    ggplot2::geom_abline(intercept = intercept, slope = slope,
                         color = "#2166AC", linewidth = 1) +
    ggplot2::labs(
      title = "Deming Regression",
      subtitle = sprintf("y = %.3f + %.3fx", intercept, slope),
      x = attr(object, "method1_col"),
      y = attr(object, "method2_col")
    ) +
    ggplot2::theme_minimal()

  if (show_identity) {
    p <- p +
      ggplot2::geom_abline(intercept = 0, slope = 1,
                           color = "gray50", linetype = "dashed", linewidth = 0.8)
  }

  p
}

#' @rdname autoplot.measure_deming_regression
#' @export
autoplot.measure_passing_bablok <- function(object, show_identity = TRUE, ...) {
  coefs <- object$coefficients
  intercept <- coefs$estimate[1]
  slope <- coefs$estimate[2]

  p <- ggplot2::ggplot() +
    ggplot2::geom_abline(intercept = intercept, slope = slope,
                         color = "#2166AC", linewidth = 1) +
    ggplot2::labs(
      title = "Passing-Bablok Regression",
      subtitle = sprintf("y = %.3f + %.3fx", intercept, slope),
      x = attr(object, "method1_col"),
      y = attr(object, "method2_col")
    ) +
    ggplot2::theme_minimal()

  if (show_identity) {
    p <- p +
      ggplot2::geom_abline(intercept = 0, slope = 1,
                           color = "gray50", linetype = "dashed", linewidth = 0.8)
  }

  p
}

#' Plot Proficiency Test Scores
#'
#' Creates a bar chart or dot plot of proficiency scores with threshold lines.
#'
#' @param object A `measure_proficiency_score` object.
#' @param type Plot type: "bar" or "point". Default "bar".
#' @param ... Additional arguments (unused).
#'
#' @return A ggplot object.
#'
#' @importFrom ggplot2 autoplot
#' @export
autoplot.measure_proficiency_score <- function(object, type = c("bar", "point"), ...) {
  type <- match.arg(type)
  scores <- object$scores

  # Color mapping for flags
  flag_colors <- c(
    "satisfactory" = "#1A9850",
    "questionable" = "#FEE08B",
    "unsatisfactory" = "#D73027"
  )

  if (type == "bar") {
    p <- ggplot2::ggplot(scores, ggplot2::aes(x = .data$row, y = .data$score, fill = .data$flag)) +
      ggplot2::geom_col(width = 0.7) +
      ggplot2::scale_fill_manual(values = flag_colors, name = "Status")
  } else {
    p <- ggplot2::ggplot(scores, ggplot2::aes(x = .data$row, y = .data$score, color = .data$flag)) +
      ggplot2::geom_point(size = 3) +
      ggplot2::scale_color_manual(values = flag_colors, name = "Status")
  }

  p +
    ggplot2::geom_hline(yintercept = c(-3, -2, 0, 2, 3),
                        linetype = c("dashed", "dotted", "solid", "dotted", "dashed"),
                        color = c("#D73027", "#FEE08B", "gray50", "#FEE08B", "#D73027"),
                        linewidth = 0.5) +
    ggplot2::labs(
      title = "Proficiency Test Scores",
      subtitle = object$statistics$score_type,
      x = "Observation",
      y = "Score"
    ) +
    ggplot2::theme_minimal()
}
