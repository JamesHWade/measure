# ==============================================================================
# Method Comparison Helper Functions
#
# Internal helper functions for method comparison analyses:
# - Bland-Altman calculations
# - Deming regression (manual implementation)
# - Proficiency score calculations
# ==============================================================================

#' Calculate Bland-Altman statistics
#' @param mean_vals Numeric vector of mean values (method1 + method2) / 2
#' @param diff_vals Numeric vector of differences (method2 - method1)
#' @param conf_level Confidence level for intervals
#' @return List with bias, LOA, and confidence intervals
#' @noRd
.calculate_ba_stats <- function(mean_vals, diff_vals, conf_level) {
  n <- length(diff_vals)
  mean_bias <- mean(diff_vals, na.rm = TRUE)
  sd_diff <- sd(diff_vals, na.rm = TRUE)
  se_bias <- sd_diff / sqrt(n)

  # Limits of agreement (using 1.96 for 95% reference)
  z <- stats::qnorm((1 + 0.95) / 2)  # Always use 95% for LOA definition
  lower_loa <- mean_bias - z * sd_diff
  upper_loa <- mean_bias + z * sd_diff
  loa_width <- upper_loa - lower_loa

  # Confidence intervals for LOA
  # SE of LOA approximation: sqrt(3 * sd^2 / n)
  se_loa <- sqrt(3 * sd_diff^2 / n)
  t_crit <- stats::qt((1 + conf_level) / 2, df = n - 1)

  # CI for mean bias
  bias_ci_lower <- mean_bias - t_crit * se_bias
  bias_ci_upper <- mean_bias + t_crit * se_bias

  # CI for LOA
  loa_lower_ci_lower <- lower_loa - t_crit * se_loa
  loa_lower_ci_upper <- lower_loa + t_crit * se_loa
  loa_upper_ci_lower <- upper_loa - t_crit * se_loa
  loa_upper_ci_upper <- upper_loa + t_crit * se_loa

  list(
    n = n,
    mean_bias = mean_bias,
    se_bias = se_bias,
    bias_ci_lower = bias_ci_lower,
    bias_ci_upper = bias_ci_upper,
    sd_diff = sd_diff,
    lower_loa = lower_loa,
    upper_loa = upper_loa,
    loa_width = loa_width,
    loa_lower_ci = c(lower = loa_lower_ci_lower, upper = loa_lower_ci_upper),
    loa_upper_ci = c(lower = loa_upper_ci_lower, upper = loa_upper_ci_upper)
  )
}

#' Test for proportional bias in Bland-Altman
#' @param mean_vals Mean of methods
#' @param diff_vals Difference between methods
#' @param type "linear" or "quadratic"
#' @return List with model, p-value, and significance
#' @noRd
.test_proportional_bias <- function(mean_vals, diff_vals, type = c("linear", "quadratic")) {
  type <- match.arg(type)

  if (type == "linear") {
    fit <- stats::lm(diff_vals ~ mean_vals)
  } else {
    fit <- stats::lm(diff_vals ~ mean_vals + I(mean_vals^2))
  }

  summary_fit <- summary(fit)

  # F-test for overall model significance
  f_stat <- summary_fit$fstatistic
  if (is.null(f_stat) || length(f_stat) < 3) {
    p_value <- NA
  } else {
    p_value <- stats::pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE)
  }

  # Slope coefficient and p-value for linear term
  coefs <- summary_fit$coefficients
  slope <- if (nrow(coefs) >= 2) coefs[2, "Estimate"] else NA
  slope_p <- if (nrow(coefs) >= 2) coefs[2, "Pr(>|t|)"] else NA

  list(
    model = fit,
    slope = slope,
    slope_p_value = slope_p,
    model_p_value = p_value,
    significant = !is.na(p_value) && p_value < 0.05
  )
}

#' Manual Deming regression implementation
#'
#' Implements classical Deming regression for method comparison when both
#' methods have measurement error.
#'
#' @param x Numeric vector of method 1 values
#' @param y Numeric vector of method 2 values
#' @param error_ratio Ratio of error variances (var_y / var_x). Default is 1.
#' @return List with intercept, slope, residuals, and diagnostics
#' @noRd
.deming_regression_manual <- function(x, y, error_ratio = 1) {
  n <- length(x)
  mean_x <- mean(x, na.rm = TRUE)
  mean_y <- mean(y, na.rm = TRUE)

  # Centered values
  x_c <- x - mean_x
  y_c <- y - mean_y

  # Calculate sums of squares and cross-products
  sxx <- sum(x_c^2, na.rm = TRUE) / (n - 1)
  syy <- sum(y_c^2, na.rm = TRUE) / (n - 1)
  sxy <- sum(x_c * y_c, na.rm = TRUE) / (n - 1)

  # Deming slope formula
  # beta = (syy - lambda*sxx + sqrt((syy - lambda*sxx)^2 + 4*lambda*sxy^2)) / (2*sxy)
  lambda <- error_ratio
  u <- syy - lambda * sxx
  discriminant <- u^2 + 4 * lambda * sxy^2

  if (discriminant < 0) {
    cli::cli_abort("Cannot compute Deming slope: negative discriminant.")
  }

  # Choose sign based on correlation direction
  if (sxy >= 0) {
    slope <- (u + sqrt(discriminant)) / (2 * sxy)
  } else {
    slope <- (u - sqrt(discriminant)) / (2 * sxy)
  }

  # Intercept
  intercept <- mean_y - slope * mean_x

  # Residuals (perpendicular distance in Deming)
  # Simplified: using y-residuals for practical purposes
  fitted <- intercept + slope * x
  residuals <- y - fitted
  rmse <- sqrt(mean(residuals^2, na.rm = TRUE))

  # R-squared approximation
  ss_total <- sum((y - mean_y)^2, na.rm = TRUE)
  ss_residual <- sum(residuals^2, na.rm = TRUE)
  r_squared <- 1 - ss_residual / ss_total

  # Warn if R-squared is negative (model fits worse than horizontal line)
  if (r_squared < 0) {
    cli::cli_warn(
      c(
        "Negative R-squared ({format(r_squared, digits = 3)}) indicates model fits worse than a horizontal line.",
        "i" = "The Deming regression may not be appropriate for this data."
      )
    )
    r_squared <- 0
  }

  list(
    intercept = intercept,
    slope = slope,
    fitted = fitted,
    residuals = residuals,
    rmse = rmse,
    r_squared = r_squared,
    error_ratio = error_ratio,
    n = n,
    mean_x = mean_x,
    mean_y = mean_y
  )
}

#' Bootstrap confidence intervals for Deming regression
#' @param x Method 1 values
#' @param y Method 2 values
#' @param error_ratio Error variance ratio
#' @param n_boot Number of bootstrap samples
#' @param conf_level Confidence level
#' @return List with bootstrap samples and confidence intervals
#' @noRd
.deming_bootstrap <- function(x, y, error_ratio, n_boot = 1000, conf_level = 0.95) {
  n <- length(x)
  boot_intercept <- numeric(n_boot)
  boot_slope <- numeric(n_boot)
  error_messages <- character(0)

  for (i in seq_len(n_boot)) {
    idx <- sample(n, n, replace = TRUE)
    tryCatch({
      boot_fit <- .deming_regression_manual(x[idx], y[idx], error_ratio)
      boot_intercept[i] <- boot_fit$intercept
      boot_slope[i] <- boot_fit$slope
    }, error = function(e) {
      boot_intercept[i] <<- NA
      boot_slope[i] <<- NA
      error_messages <<- c(error_messages, e$message)
    })
  }

  # Remove failed bootstraps
  valid <- !is.na(boot_intercept) & !is.na(boot_slope)
  boot_intercept <- boot_intercept[valid]
  boot_slope <- boot_slope[valid]
  n_failed <- sum(!valid)

  if (n_failed > 0) {
    unique_errors <- unique(error_messages)
    cli::cli_warn(
      c(
        "{n_failed} bootstrap sample{?s} failed.",
        "i" = "Common error{?s}: {head(unique_errors, 3)}"
      )
    )
  }

  if (length(boot_slope) < 100) {
    cli::cli_warn(
      "Only {length(boot_slope)} successful bootstrap samples; CIs may be unreliable."
    )
  }

  alpha <- 1 - conf_level
  ci_intercept <- stats::quantile(boot_intercept, c(alpha/2, 1 - alpha/2), na.rm = TRUE)
  ci_slope <- stats::quantile(boot_slope, c(alpha/2, 1 - alpha/2), na.rm = TRUE)

  list(
    n_successful = length(boot_slope),
    intercept_ci = ci_intercept,
    slope_ci = ci_slope,
    intercept_samples = boot_intercept,
    slope_samples = boot_slope
  )
}

#' Calculate z-score for proficiency testing
#' @param measured Measured value
#' @param reference Reference/assigned value
#' @param sigma Standard deviation for scoring
#' @return z-score
#' @noRd
.calculate_z_score <- function(measured, reference, sigma) {
  # Handle zero or negative sigma
 if (any(sigma <= 0, na.rm = TRUE)) {
    cli::cli_abort("Sigma must be positive for z-score calculation.")
  }
  (measured - reference) / sigma
}

#' Calculate En score for proficiency testing
#' @param measured Measured value
#' @param reference Reference/assigned value
#' @param u_measured Standard uncertainty of measured value
#' @param u_reference Standard uncertainty of reference value
#' @return En score
#' @noRd
.calculate_en_score <- function(measured, reference, u_measured, u_reference) {
  combined_u <- sqrt(u_measured^2 + u_reference^2)

  # Check for zero combined uncertainty
  if (any(combined_u == 0, na.rm = TRUE)) {
    cli::cli_abort(
      "Combined uncertainty is zero for some observations; En-score cannot be calculated."
    )
  }

  (measured - reference) / combined_u
}

#' Calculate zeta score for proficiency testing
#' @param measured Measured value
#' @param reference Reference/assigned value
#' @param u_measured Standard uncertainty of measured value
#' @param u_reference Standard uncertainty of reference value (may include shared component)
#' @return zeta score (same as En when no shared uncertainty)
#' @noRd
.calculate_zeta_score <- function(measured, reference, u_measured, u_reference) {
  # zeta is similar to En but used when uncertainties may be correlated
  # For independent uncertainties, it's the same as En
  .calculate_en_score(measured, reference, u_measured, u_reference)
}

#' Flag proficiency scores
#' @param scores Numeric vector of absolute scores
#' @return Character vector of flags
#' @noRd
.flag_proficiency_scores <- function(scores) {
  abs_scores <- abs(scores)
  dplyr::case_when(
    abs_scores <= 2 ~ "satisfactory",
    abs_scores <= 3 ~ "questionable",
    TRUE ~ "unsatisfactory"
  )
}
