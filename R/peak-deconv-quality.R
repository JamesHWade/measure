# ==============================================================================
# Quality Assessment for Peak Deconvolution
#
# Comprehensive quality metrics to evaluate deconvolution results and provide
# guidance on fit quality.
# ==============================================================================

#' Assess Deconvolution Quality
#'
#' Calculates comprehensive quality metrics for a peak deconvolution fit,
#' including goodness-of-fit statistics, information criteria, per-peak
#' quality, and residual diagnostics.
#'
#' @param x Numeric vector of x-axis values.
#' @param y Numeric vector of observed y-axis values.
#' @param result Deconvolution result list from `optimize_deconvolution()`.
#' @param models List of `peak_model` objects used in deconvolution.
#'
#' @return A list of class `deconv_quality` containing:
#'   - `goodness_of_fit`: R-squared, RMSE, MAE, chi-squared
#'   - `information_criteria`: AIC, BIC, AICc
#'   - `peak_quality`: Per-peak purity, overlap, area
#'   - `residual_analysis`: Autocorrelation, heteroscedasticity, normality tests
#'   - `overall_grade`: Letter grade (A/B/C/D/F)
#'   - `convergence_info`: Optimization convergence details
#'
#' @examples
#' # Create synthetic data and fit
#' x <- seq(0, 20, by = 0.1)
#' true_y <- 1.5 * exp(-0.5 * ((x - 8) / 1)^2) +
#'   0.8 * exp(-0.5 * ((x - 12) / 1.5)^2)
#' y <- true_y + rnorm(length(x), sd = 0.05)
#'
#' models <- list(gaussian_peak_model(), gaussian_peak_model())
#' init_params <- list(
#'   list(height = 1.2, center = 7.5, width = 1.2),
#'   list(height = 0.6, center = 12.5, width = 1.8)
#' )
#'
#' result <- optimize_deconvolution(x, y, models, init_params)
#' quality <- assess_deconv_quality(x, y, result, models)
#' print(quality)
#'
#' @family peak-deconvolution
#' @export
assess_deconv_quality <- function(x, y, result, models) {
  y_fit <- result$fitted_values
  residuals <- result$residuals

  n_params <- sum(vapply(models, function(m) m$n_params, integer(1)))

  quality_list <- list(
    goodness_of_fit = .calc_goodness_of_fit(y, y_fit, residuals),
    information_criteria = .calc_information_criteria(
      y,
      y_fit,
      residuals,
      n_params
    ),
    peak_quality = .calc_peak_quality(x, result$parameters, models),
    residual_analysis = .analyze_residuals(x, residuals),
    overall_grade = .assign_quality_grade(
      r_squared = .calc_r_squared(y, y_fit),
      residual_autocorr = .test_residual_autocorrelation(residuals)
    ),
    convergence_info = list(
      converged = result$convergence,
      n_iterations = result$n_iterations,
      final_value = result$final_value,
      optimizer = result$optimizer %||% "unknown",
      elapsed_time = result$elapsed_time
    )
  )

  class(quality_list) <- c("deconv_quality", "list")
  quality_list
}


#' Check if Fit Passes Quality Gates
#'
#' Evaluates a deconvolution quality assessment against configurable thresholds
#' to determine if the fit is acceptable.
#'
#' @param quality A `deconv_quality` object from `assess_deconv_quality()`.
#' @param reject_threshold Minimum R-squared to accept (default 0.85).
#' @param warn_threshold R-squared threshold for warning (default 0.95).
#'
#' @return A list with:
#'   - `status`: `"pass"`, `"warn"`, or `"reject"`
#'   - `pass`, `warn`, `reject`: Logical flags
#'   - `messages`: Character vector of issues found
#'   - `grade`: Overall quality grade
#'
#' @family peak-deconvolution
#' @export
check_quality_gates <- function(
  quality,
  reject_threshold = 0.85,
  warn_threshold = 0.95
) {
  r2 <- quality$goodness_of_fit$r_squared
  autocorr <- quality$residual_analysis$autocorrelation

  messages <- character(0)
  status <- "pass"

  # Check R-squared
  if (is.na(r2) || r2 < reject_threshold) {
    status <- "reject"
    messages <- c(
      messages,
      sprintf(
        "R-squared (%.3f) below rejection threshold (%.2f)",
        r2 %||% NA,
        reject_threshold
      )
    )
  } else if (r2 < warn_threshold) {
    if (status == "pass") {
      status <- "warn"
    }
    messages <- c(
      messages,
      sprintf(
        "R-squared (%.3f) below warning threshold (%.2f)",
        r2,
        warn_threshold
      )
    )
  }

  # Check autocorrelation
  if (!is.null(autocorr) && isTRUE(autocorr$significant)) {
    if (status == "pass") {
      status <- "warn"
    }
    messages <- c(
      messages,
      sprintf(
        "Significant residual autocorrelation detected (r=%.3f)",
        autocorr$lag1_corr %||% NA
      )
    )
  }

  # Check peak overlap
  if (!is.null(quality$peak_quality) && nrow(quality$peak_quality) > 0) {
    max_overlap <- max(quality$peak_quality$overlap_degree, na.rm = TRUE)
    if (!is.na(max_overlap) && max_overlap > 0.8) {
      if (status == "pass") {
        status <- "warn"
      }
      messages <- c(
        messages,
        sprintf("High peak overlap detected (%.1f%% max)", max_overlap * 100)
      )
    }
  }

  # Check convergence
  if (
    !is.null(quality$convergence_info) && !quality$convergence_info$converged
  ) {
    if (status == "pass") {
      status <- "warn"
    }
    messages <- c(messages, "Optimization did not converge")
  }

  list(
    status = status,
    pass = status == "pass",
    warn = status == "warn",
    reject = status == "reject",
    messages = messages,
    grade = quality$overall_grade
  )
}


#' @export
print.deconv_quality <- function(x, ...) {
  cat("Deconvolution Quality Assessment\n")
  cat("================================\n\n")

  # Overall grade
  grade <- x$overall_grade
  grade_desc <- switch(
    grade,
    "A" = "excellent",
    "B" = "good",
    "C" = "acceptable",
    "D" = "poor",
    "F" = "failed",
    "unknown"
  )
  cat(sprintf("Overall Grade: %s (%s)\n\n", grade, grade_desc))

  # Goodness of fit
  gof <- x$goodness_of_fit
  cat("Goodness of Fit:\n")
  cat(sprintf("  R-squared: %.4f\n", gof$r_squared %||% NA))
  cat(sprintf("  RMSE:      %.4f\n", gof$rmse %||% NA))
  cat(sprintf("  MAE:       %.4f\n", gof$mae %||% NA))
  cat("\n")

  # Convergence
  if (!is.null(x$convergence_info)) {
    ci <- x$convergence_info
    cat("Optimization:\n")
    cat(sprintf("  Converged:   %s\n", if (ci$converged) "Yes" else "No"))
    cat(sprintf("  Iterations:  %d\n", ci$n_iterations %||% NA))
    cat(sprintf("  Optimizer:   %s\n", ci$optimizer %||% "unknown"))
    cat(sprintf("  Final SSE:   %.4f\n", ci$final_value %||% NA))
  }

  invisible(x)
}


#' @export
summary.deconv_quality <- function(object, ...) {
  gates <- check_quality_gates(object)

  cat("Deconvolution Quality Summary\n")
  cat("=============================\n\n")

  cat(sprintf("Status: %s\n", toupper(gates$status)))
  cat(sprintf("Grade:  %s\n\n", object$overall_grade))

  if (length(gates$messages) > 0) {
    cat("Issues:\n")
    for (msg in gates$messages) {
      cat(sprintf("  - %s\n", msg))
    }
    cat("\n")
  }

  # Per-peak summary
  if (!is.null(object$peak_quality) && nrow(object$peak_quality) > 0) {
    cat("Peak Summary:\n")
    pq <- object$peak_quality
    for (i in seq_len(nrow(pq))) {
      cat(sprintf(
        "  Peak %d: Area=%.2f (%.1f%%), Purity=%.1f%%\n",
        pq$peak_id[i],
        pq$area[i],
        pq$area_percent[i],
        pq$purity[i] * 100
      ))
    }
  }

  invisible(object)
}


# ==============================================================================
# Internal Quality Calculation Functions
# ==============================================================================

#' Calculate Goodness of Fit Metrics
#' @noRd
.calc_goodness_of_fit <- function(y_obs, y_fit, residuals) {
  list(
    r_squared = .calc_r_squared(y_obs, y_fit),
    rmse = .calc_rmse(residuals),
    mae = .calc_mae(residuals),
    chi_squared = .calc_chi_squared(y_obs, residuals),
    normalized_rmse = .calc_rmse(residuals) / diff(range(y_obs))
  )
}


#' Calculate R-squared
#' @noRd
.calc_r_squared <- function(y_obs, y_fit) {
  ss_res <- sum((y_obs - y_fit)^2)
  ss_tot <- sum((y_obs - mean(y_obs))^2)

  if (ss_tot == 0) {
    return(NA_real_)
  }

  1 - (ss_res / ss_tot)
}


#' Calculate Root Mean Squared Error
#' @noRd
.calc_rmse <- function(residuals) {
  sqrt(mean(residuals^2))
}


#' Calculate Mean Absolute Error
#' @noRd
.calc_mae <- function(residuals) {
  mean(abs(residuals))
}


#' Calculate Chi-Squared Statistic
#' @noRd
.calc_chi_squared <- function(y_obs, residuals) {
  weights <- pmax(abs(y_obs), 1)
  sum((residuals^2) / weights)
}


#' Calculate Information Criteria (AIC, BIC)
#' @noRd
.calc_information_criteria <- function(y_obs, y_fit, residuals, n_params) {
  n <- length(y_obs)
  sse <- sum(residuals^2)

  # Log-likelihood (assuming Gaussian errors)
  sigma2 <- sse / n
  log_lik <- -n / 2 * (log(2 * pi) + log(sigma2) + 1)

  # AIC = -2 * log_lik + 2 * k
  aic <- -2 * log_lik + 2 * n_params

  # BIC = -2 * log_lik + k * log(n)
  bic <- -2 * log_lik + n_params * log(n)

  # AICc (corrected AIC for small samples)
  aicc <- if (n - n_params - 1 > 0) {
    aic + (2 * n_params * (n_params + 1)) / (n - n_params - 1)
  } else {
    NA_real_
  }

  list(
    aic = aic,
    bic = bic,
    aicc = aicc,
    log_likelihood = log_lik
  )
}


#' Calculate Per-Peak Quality Metrics
#' @noRd
.calc_peak_quality <- function(x, parameters, models) {
  n_peaks <- length(parameters)

  if (n_peaks == 0) {
    return(tibble::tibble(
      peak_id = integer(0),
      purity = numeric(0),
      overlap_degree = numeric(0),
      area = numeric(0),
      height = numeric(0),
      area_percent = numeric(0)
    ))
  }

  # Calculate individual peak contributions
  peak_data <- vector("list", n_peaks)
  peak_areas <- numeric(n_peaks)
  peak_heights <- numeric(n_peaks)

  for (i in seq_len(n_peaks)) {
    y_peak <- peak_model_value(models[[i]], x, parameters[[i]])
    peak_data[[i]] <- y_peak
    peak_areas[i] <- pracma::trapz(x, y_peak)
    peak_heights[i] <- max(y_peak)
  }

  # Calculate purity and overlap for each peak
  purity <- numeric(n_peaks)
  overlap <- numeric(n_peaks)

  for (i in seq_len(n_peaks)) {
    y_this_peak <- peak_data[[i]]
    peak_idx <- which.max(y_this_peak)

    # Purity: how much of the signal at peak maximum comes from this peak
    total_at_peak <- sum(vapply(peak_data, function(y) y[peak_idx], numeric(1)))
    if (total_at_peak == 0) {
      purity[i] <- NA_real_
    } else {
      purity[i] <- y_this_peak[peak_idx] / total_at_peak
    }

    # Overlap: average contribution from other peaks
    if (n_peaks > 1) {
      other_peaks <- setdiff(seq_len(n_peaks), i)
      other_contribution <- Reduce(`+`, peak_data[other_peaks])
      overlap[i] <- mean(
        other_contribution / (y_this_peak + other_contribution + 1e-10)
      )
    } else {
      overlap[i] <- 0
    }
  }

  tibble::tibble(
    peak_id = seq_len(n_peaks),
    purity = purity,
    overlap_degree = overlap,
    area = peak_areas,
    height = peak_heights,
    area_percent = if (sum(peak_areas) == 0) {
      rep(NA_real_, n_peaks)
    } else {
      peak_areas / sum(peak_areas) * 100
    }
  )
}


#' Analyze Fit Residuals
#' @noRd
.analyze_residuals <- function(x, residuals) {
  list(
    mean_residual = mean(residuals),
    std_residual = stats::sd(residuals),
    max_abs_residual = max(abs(residuals)),
    autocorrelation = .test_residual_autocorrelation(residuals),
    heteroscedasticity = .test_heteroscedasticity(x, residuals),
    normality = .test_residual_normality(residuals),
    runs_test = .runs_test(residuals)
  )
}


#' Test for Residual Autocorrelation
#' @noRd
.test_residual_autocorrelation <- function(residuals) {
  if (length(residuals) < 3) {
    return(list(lag1_corr = NA_real_, significant = FALSE))
  }

  # Lag-1 autocorrelation
  acf_result <- stats::acf(residuals, lag.max = 1, plot = FALSE)
  lag1_corr <- acf_result$acf[2]

  # Approximate significance test
  se <- 1 / sqrt(length(residuals))
  significant <- abs(lag1_corr) > 2 * se

  list(
    lag1_corr = lag1_corr,
    significant = significant,
    threshold = 2 * se
  )
}


#' Test for Heteroscedasticity
#' @noRd
.test_heteroscedasticity <- function(x, residuals) {
  if (length(residuals) < 10) {
    return(list(correlation = NA_real_, significant = FALSE))
  }

  cor_test <- stats::cor.test(x, abs(residuals), method = "spearman")

  list(
    correlation = as.numeric(cor_test$estimate),
    p_value = cor_test$p.value,
    significant = cor_test$p.value < 0.05
  )
}


#' Test Residual Normality (Shapiro-Wilk)
#' @noRd
.test_residual_normality <- function(residuals) {
  if (length(residuals) < 3 || length(residuals) > 5000) {
    return(list(statistic = NA_real_, p_value = NA_real_, normal = NA))
  }

  shapiro_result <- stats::shapiro.test(residuals)

  list(
    statistic = as.numeric(shapiro_result$statistic),
    p_value = shapiro_result$p.value,
    normal = shapiro_result$p.value > 0.05
  )
}


#' Runs Test for Randomness
#' @noRd
.runs_test <- function(residuals) {
  if (length(residuals) < 10) {
    return(list(n_runs = NA_integer_, random = NA))
  }

  # Count runs (sequences of same sign)
  signs <- sign(residuals)
  signs <- signs[signs != 0]

  if (length(signs) < 10) {
    return(list(n_runs = NA_integer_, random = NA))
  }

  runs <- rle(signs)
  n_runs <- length(runs$lengths)

  # Expected number of runs under randomness
  n_pos <- sum(signs > 0)
  n_neg <- sum(signs < 0)
  n <- length(signs)

  if (n_pos == 0 || n_neg == 0 || n < 2) {
    return(list(
      n_runs = NA_integer_,
      expected_runs = NA_real_,
      z_score = NA_real_,
      p_value = NA_real_,
      random = NA
    ))
  }

  expected_runs <- (2 * n_pos * n_neg) / n + 1
  var_runs <- (2 * n_pos * n_neg * (2 * n_pos * n_neg - n)) / (n^2 * (n - 1))

  if (!is.finite(var_runs) || var_runs <= 0) {
    z_score <- NA_real_
    p_value <- NA_real_
  } else {
    z_score <- (n_runs - expected_runs) / sqrt(var_runs)
    p_value <- 2 * stats::pnorm(-abs(z_score))
  }

  list(
    n_runs = n_runs,
    expected_runs = expected_runs,
    z_score = z_score,
    p_value = p_value,
    random = p_value > 0.05
  )
}


#' Assign Overall Quality Grade
#' @noRd
.assign_quality_grade <- function(r_squared, residual_autocorr) {
  if (is.na(r_squared)) {
    return("F")
  }

  # Start with R-squared based grade
  if (r_squared >= 0.99) {
    grade <- "A"
  } else if (r_squared >= 0.95) {
    grade <- "B"
  } else if (r_squared >= 0.90) {
    grade <- "C"
  } else if (r_squared >= 0.80) {
    grade <- "D"
  } else {
    grade <- "F"
  }

  # Downgrade if residuals show systematic problems
  if (!is.null(residual_autocorr) && isTRUE(residual_autocorr$significant)) {
    grades <- c("A", "B", "C", "D", "F")
    idx <- match(grade, grades)
    if (!is.na(idx) && idx < length(grades)) {
      grade <- grades[idx + 1]
    }
  }

  grade
}
