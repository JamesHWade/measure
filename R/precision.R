# ==============================================================================
# Precision Studies
#
# This file contains precision analysis functions for method validation:
# - measure_repeatability: Within-run precision
# - measure_intermediate_precision: Between-run precision
# - measure_reproducibility: Between-lab precision
# - measure_gage_rr: Gage R&R / Measurement System Analysis
# ==============================================================================

#' Repeatability (Within-Run Precision)
#'
#' Calculates repeatability statistics for replicate measurements performed
#' under identical conditions (same operator, instrument, short time interval).
#'
#' @param data A data frame containing replicate measurements.
#' @param response_col Name of the column containing the response values.
#' @param group_col Optional name of a grouping column (e.g., concentration level).
#'   If provided, repeatability is calculated within each group.
#' @param conf_level Confidence level for intervals. Default is 0.95.
#'
#' @return A `measure_precision` object containing:
#'   - `mean`: Mean of the replicates
#'   - `sd`: Standard deviation
#'   - `cv`: Coefficient of variation (%)
#'   - `n`: Number of replicates
#'   - `se`: Standard error
#'   - `ci_lower`, `ci_upper`: Confidence interval for the mean
#'
#' @details
#' Repeatability represents the precision of a method under constant conditions
#' over a short time interval. It is typically assessed using at least 6

#' replicates of a sample at each concentration level of interest.
#'
#' The coefficient of variation (CV) is reported as a percentage:
#' `CV = 100 * SD / mean`
#'
#' @family precision
#' @seealso [measure_intermediate_precision()], [measure_reproducibility()]
#'
#' @export
#'
#' @examples
#' # Simple repeatability from replicate measurements
#' data <- data.frame(
#'   sample_id = rep("QC1", 10),
#'   concentration = rnorm(10, mean = 100, sd = 2)
#' )
#' measure_repeatability(data, "concentration")
#'
#' # Repeatability at multiple concentration levels
#' data <- data.frame(
#'   level = rep(c("low", "mid", "high"), each = 6),
#'   concentration = c(
#'     rnorm(6, 10, 0.5),
#'     rnorm(6, 50, 2),
#'     rnorm(6, 100, 4)
#'   )
#' )
#' measure_repeatability(data, "concentration", group_col = "level")
measure_repeatability <- function(
  data,
  response_col,
  group_col = NULL,
  conf_level = 0.95
) {
  # Validate inputs
  if (!response_col %in% names(data)) {
    cli::cli_abort("Column {.field {response_col}} not found in data.")
  }

  if (!is.null(group_col) && !group_col %in% names(data)) {
    cli::cli_abort("Column {.field {group_col}} not found in data.")
  }

  # Calculate precision stats
  if (is.null(group_col)) {
    result <- calculate_precision_stats(
      data[[response_col]],
      conf_level = conf_level,
      group = "overall"
    )
    result <- tibble::as_tibble(result)
  } else {
    groups <- unique(data[[group_col]])
    results <- lapply(groups, function(g) {
      values <- data[[response_col]][data[[group_col]] == g]
      calculate_precision_stats(values, conf_level = conf_level, group = g)
    })
    result <- dplyr::bind_rows(results)
  }

  structure(
    result,
    class = c("measure_precision", "tbl_df", "tbl", "data.frame"),
    precision_type = "repeatability",
    response_col = response_col,
    conf_level = conf_level
  )
}

#' Intermediate Precision (Between-Run Precision)
#'
#' Calculates intermediate precision statistics for measurements performed
#' under varying conditions (different days, analysts, or instruments).
#'
#' @param data A data frame containing measurements with factor columns.
#' @param response_col Name of the column containing the response values.
#' @param factors Character vector of factor column names (e.g., `c("day", "analyst")`).
#' @param group_col Optional grouping column (e.g., concentration level).
#' @param conf_level Confidence level for intervals. Default is 0.95.
#'
#' @return A `measure_precision` object containing variance components and
#'   precision estimates:
#'   - `component`: Name of the variance component
#'   - `variance`: Estimated variance
#'   - `percent_variance`: Percentage of total variance
#'   - `sd`: Standard deviation (square root of variance)
#'   - `cv`: Coefficient of variation (%) for that component
#'
#' @details
#' Intermediate precision quantifies the variability due to different
#' conditions within the same laboratory. This typically includes:
#' - Different days
#' - Different analysts
#' - Different equipment (of the same type)
#'
#' The function uses a one-way or nested ANOVA approach to estimate
#' variance components. For more complex designs, consider using mixed
#' effects models with the `lme4` package.
#'
#' @family precision
#' @seealso [measure_repeatability()], [measure_reproducibility()]
#'
#' @export
#'
#' @examples
#' # Intermediate precision across days
#' set.seed(123)
#' data <- data.frame(
#'   day = rep(1:5, each = 6),
#'   concentration = rnorm(30, mean = 100, sd = 3) +
#'     rep(rnorm(5, 0, 2), each = 6)  # Day effect
#' )
#' measure_intermediate_precision(data, "concentration", factors = "day")
measure_intermediate_precision <- function(
  data,
  response_col,
  factors,
  group_col = NULL,
  conf_level = 0.95
) {
  # Validate inputs
  if (!response_col %in% names(data)) {
    cli::cli_abort("Column {.field {response_col}} not found in data.")
  }

  for (f in factors) {
    if (!f %in% names(data)) {
      cli::cli_abort("Factor column {.field {f}} not found in data.")
    }
  }

  if (!is.null(group_col) && !group_col %in% names(data)) {
    cli::cli_abort("Column {.field {group_col}} not found in data.")
  }

  # Calculate variance components
  if (is.null(group_col)) {
    result <- calculate_variance_components(
      data,
      response_col,
      factors,
      conf_level
    )
    result$group <- "overall"
  } else {
    groups <- unique(data[[group_col]])
    results <- lapply(groups, function(g) {
      sub_data <- data[data[[group_col]] == g, ]
      res <- calculate_variance_components(
        sub_data,
        response_col,
        factors,
        conf_level
      )
      res$group <- g
      res
    })
    result <- dplyr::bind_rows(results)
  }

  structure(
    result,
    class = c("measure_precision", "tbl_df", "tbl", "data.frame"),
    precision_type = "intermediate",
    response_col = response_col,
    factors = factors,
    conf_level = conf_level
  )
}

#' Reproducibility (Between-Lab Precision)
#'
#' Calculates reproducibility statistics for measurements performed
#' at different laboratories.
#'
#' @param data A data frame containing measurements from multiple labs.
#' @param response_col Name of the column containing the response values.
#' @param lab_col Name of the column identifying the laboratory.
#' @param group_col Optional grouping column (e.g., concentration level).
#' @param conf_level Confidence level for intervals. Default is 0.95.
#'
#' @return A `measure_precision` object containing:
#'   - Within-lab variance (repeatability)
#'   - Between-lab variance
#'   - Total reproducibility variance
#'   - Corresponding CV estimates
#'
#' @details
#' Reproducibility represents the precision of a method when performed
#' at different laboratories. It includes both within-lab (repeatability)
#' and between-lab variance components.
#'
#' @family precision
#' @seealso [measure_repeatability()], [measure_intermediate_precision()]
#'
#' @export
#'
#' @examples
#' # Reproducibility across laboratories
#' set.seed(123)
#' data <- data.frame(
#'   lab_id = rep(c("Lab_A", "Lab_B", "Lab_C"), each = 10),
#'   concentration = rnorm(30, mean = 100, sd = 2) +
#'     rep(c(0, 3, -2), each = 10)  # Lab bias
#' )
#' measure_reproducibility(data, "concentration", lab_col = "lab_id")
measure_reproducibility <- function(
  data,
  response_col,
  lab_col,
  group_col = NULL,
  conf_level = 0.95
) {
  # Validate inputs
  if (!response_col %in% names(data)) {
    cli::cli_abort("Column {.field {response_col}} not found in data.")
  }

  if (!lab_col %in% names(data)) {
    cli::cli_abort("Column {.field {lab_col}} not found in data.")
  }

  # Use intermediate precision with lab as the factor
  result <- calculate_variance_components(
    data,
    response_col,
    lab_col,
    conf_level
  )

  # Add reproducibility-specific summary
  total_var <- sum(result$variance)
  result$reproducibility_sd <- sqrt(total_var)
  grand_mean <- mean(data[[response_col]], na.rm = TRUE)
  result$reproducibility_cv <- 100 * sqrt(total_var) / grand_mean

  structure(
    result,
    class = c("measure_precision", "tbl_df", "tbl", "data.frame"),
    precision_type = "reproducibility",
    response_col = response_col,
    lab_col = lab_col,
    conf_level = conf_level
  )
}

#' Gage R&R (Measurement System Analysis)
#'
#' Performs a Gage Repeatability and Reproducibility study to assess
#' measurement system variation.
#'
#' @param data A data frame containing Gage R&R study data.
#' @param response_col Name of the column containing the measurements.
#' @param part_col Name of the column identifying parts/samples.
#' @param operator_col Name of the column identifying operators/analysts.
#' @param tolerance Optional specification tolerance for calculating %Study
#'   variation and %Tolerance.
#' @param conf_level Confidence level. Default is 0.95.
#' @param k Multiplier for study variation calculation. Default is 5.15 (99%).
#'
#' @return A `measure_gage_rr` object containing:
#'   - Variance components (Repeatability, Reproducibility, Part-to-Part)
#'   - %Contribution of each component
#'   - %Study Variation (using k * sigma)
#'   - %Tolerance (if tolerance provided)
#'   - Number of distinct categories (ndc)
#'
#' @details
#' Gage R&R decomposes total measurement variation into:
#' - **Repeatability (EV)**: Equipment variation - variability from repeated
#'   measurements by the same operator on the same part
#' - **Reproducibility (AV)**: Appraiser variation - variability between
#'   operators measuring the same parts
#' - **Part-to-Part (PV)**: True variation between parts
#'
#' ## Acceptance Criteria (typical guidelines)
#' - %R&R < 10%: Measurement system acceptable
#' - %R&R 10-30%: Measurement system may be acceptable depending on application
#' - %R&R > 30%: Measurement system needs improvement
#'
#' The number of distinct categories (ndc) should be >= 5 for a capable
#' measurement system.
#'
#' @family precision
#' @seealso [measure_repeatability()], [measure_intermediate_precision()]
#'
#' @export
#'
#' @examples
#' # Gage R&R study with 10 parts, 3 operators, 2 replicates each
#' set.seed(123)
#' data <- expand.grid(
#'   part = 1:10,
#'   operator = c("A", "B", "C"),
#'   replicate = 1:2
#' )
#' data$measurement <- 50 +
#'   (data$part - 5) * 2 +  # Part-to-part variation
#'   ifelse(data$operator == "A", 0.5,
#'          ifelse(data$operator == "B", -0.3, 0)) +  # Operator effect
#'   rnorm(nrow(data), 0, 0.5)  # Repeatability
#'
#' result <- measure_gage_rr(
#'   data,
#'   response_col = "measurement",
#'   part_col = "part",
#'   operator_col = "operator",
#'   tolerance = 20
#' )
#' print(result)
measure_gage_rr <- function(
  data,
  response_col,
  part_col,
  operator_col,
  tolerance = NULL,
  conf_level = 0.95,
  k = 5.15
) {
  # Validate inputs
  for (col in c(response_col, part_col, operator_col)) {
    if (!col %in% names(data)) {
      cli::cli_abort("Column {.field {col}} not found in data.")
    }
  }

  # Convert to factors for proper ANOVA
  data[[part_col]] <- as.factor(data[[part_col]])
  data[[operator_col]] <- as.factor(data[[operator_col]])

  # Get counts
  parts <- levels(data[[part_col]])
  operators <- levels(data[[operator_col]])
  n_parts <- length(parts)
  n_operators <- length(operators)

  # Calculate replicate count per part-operator combination
  n_replicates <- nrow(data) / (n_parts * n_operators)

  if (n_replicates != floor(n_replicates)) {
    cli::cli_warn("Unbalanced design detected. Results may be approximate.")
    n_replicates <- ceiling(n_replicates)
  }

  # Fit two-way ANOVA model
  formula <- stats::as.formula(
    paste(response_col, "~", part_col, "*", operator_col)
  )
  fit <- stats::aov(formula, data = data)
  anova_table <- summary(fit)[[1]]

  # Clean up row names (they may have trailing spaces)
  rownames(anova_table) <- trimws(rownames(anova_table))

  # Extract mean squares using grep to handle name variations
  part_row <- grep(
    paste0("^", part_col, "$"),
    rownames(anova_table),
    value = TRUE
  )
  operator_row <- grep(
    paste0("^", operator_col, "$"),
    rownames(anova_table),
    value = TRUE
  )
  interaction_row <- grep(
    ":",
    rownames(anova_table),
    value = TRUE,
    fixed = TRUE
  )

  ms_part <- anova_table[part_row, "Mean Sq"]
  ms_operator <- anova_table[operator_row, "Mean Sq"]
  ms_interaction <- anova_table[interaction_row, "Mean Sq"]
  ms_error <- anova_table["Residuals", "Mean Sq"]

  # Calculate variance components (EMS method)
  # Negative estimates are set to zero (standard practice for ANOVA-based

  # variance components), but we warn since this can indicate poor model fit
  var_repeatability <- ms_error
  raw_interaction <- (ms_interaction - ms_error) / n_replicates
  raw_operator <- (ms_operator - ms_interaction) / (n_parts * n_replicates)
  raw_part <- (ms_part - ms_interaction) / (n_operators * n_replicates)

  clipped <- c(
    if (raw_interaction < 0) "interaction",
    if (raw_operator < 0) "operator",
    if (raw_part < 0) "part"
  )
  if (length(clipped) > 0) {
    cli::cli_warn(
      "Negative variance component{?s} set to zero for {.val {clipped}}. This may indicate insufficient variation or poor model fit."
    )
  }

  var_interaction <- max(0, raw_interaction)
  var_operator <- max(0, raw_operator)
  var_part <- max(0, raw_part)

  # Combine components
  var_reproducibility <- var_operator + var_interaction
  var_rr <- var_repeatability + var_reproducibility
  var_total <- var_rr + var_part

  # Calculate standard deviations
  sd_repeatability <- sqrt(var_repeatability)
  sd_reproducibility <- sqrt(var_reproducibility)
  sd_rr <- sqrt(var_rr)
  sd_part <- sqrt(var_part)
  sd_total <- sqrt(var_total)

  # Calculate study variation (k * sigma)
  sv_repeatability <- k * sd_repeatability
  sv_reproducibility <- k * sd_reproducibility
  sv_rr <- k * sd_rr
  sv_part <- k * sd_part
  sv_total <- k * sd_total

  # Calculate percentages
  pct_contribution <- 100 *
    c(
      var_repeatability,
      var_reproducibility,
      var_rr,
      var_part
    ) /
    var_total

  pct_study_var <- 100 *
    c(
      sd_repeatability,
      sd_reproducibility,
      sd_rr,
      sd_part
    ) /
    sd_total

  # Calculate %Tolerance if provided
  if (!is.null(tolerance)) {
    pct_tolerance <- 100 *
      c(
        sv_repeatability,
        sv_reproducibility,
        sv_rr,
        sv_part
      ) /
      tolerance
  } else {
    pct_tolerance <- rep(NA_real_, 4)
  }

  # Number of distinct categories
  ndc <- floor(1.41 * (sd_part / sd_rr))

  # Build result table
  result <- tibble::tibble(
    source = c("Repeatability", "Reproducibility", "Total R&R", "Part-to-Part"),
    variance = c(var_repeatability, var_reproducibility, var_rr, var_part),
    std_dev = c(sd_repeatability, sd_reproducibility, sd_rr, sd_part),
    study_var = c(sv_repeatability, sv_reproducibility, sv_rr, sv_part),
    pct_contribution = pct_contribution,
    pct_study_var = pct_study_var,
    pct_tolerance = pct_tolerance
  )

  structure(
    result,
    class = c("measure_gage_rr", "tbl_df", "tbl", "data.frame"),
    n_parts = n_parts,
    n_operators = n_operators,
    n_replicates = n_replicates,
    ndc = ndc,
    tolerance = tolerance,
    k = k,
    total_variance = var_total,
    conf_level = conf_level
  )
}

# ==============================================================================
# Helper functions
# ==============================================================================

#' Calculate precision statistics for a vector
#' @noRd
calculate_precision_stats <- function(x, conf_level = 0.95, group = NULL) {
  x <- x[!is.na(x)]
  n <- length(x)

  if (n < 2) {
    cli::cli_abort("At least 2 observations required to calculate precision.")
  }

  m <- mean(x)
  s <- stats::sd(x)
  se <- s / sqrt(n)
  cv <- 100 * s / m

  # Confidence interval
  alpha <- 1 - conf_level
  t_crit <- stats::qt(1 - alpha / 2, df = n - 1)
  ci_lower <- m - t_crit * se
  ci_upper <- m + t_crit * se

  tibble::tibble(
    group = group,
    n = n,
    mean = m,
    sd = s,
    cv = cv,
    se = se,
    ci_lower = ci_lower,
    ci_upper = ci_upper
  )
}

#' Calculate variance components from ANOVA
#' @noRd
calculate_variance_components <- function(
  data,
  response_col,
  factors,
  conf_level
) {
  # Build formula
  formula_str <- paste(response_col, "~", paste(factors, collapse = " + "))
  formula <- stats::as.formula(formula_str)

  # Fit ANOVA
  fit <- stats::aov(formula, data = data)
  anova_table <- summary(fit)[[1]]

  # Extract variance components
  grand_mean <- mean(data[[response_col]], na.rm = TRUE)
  total_n <- nrow(data)

  # Residual variance (within-group, repeatability)
  ms_residual <- anova_table["Residuals", "Mean Sq"]
  df_residual <- anova_table["Residuals", "Df"]

  components <- list()

  for (f in factors) {
    ms_factor <- anova_table[f, "Mean Sq"]
    df_factor <- anova_table[f, "Df"]

    # Number of observations per level
    n_per_level <- total_n / (df_factor + 1)

    # Variance component (using expected mean squares)
    var_factor <- max(0, (ms_factor - ms_residual) / n_per_level)

    components[[f]] <- tibble::tibble(
      component = f,
      variance = var_factor,
      sd = sqrt(var_factor),
      df = df_factor
    )
  }

  # Add residual (repeatability)
  components[["Residual"]] <- tibble::tibble(
    component = "Residual",
    variance = ms_residual,
    sd = sqrt(ms_residual),
    df = df_residual
  )

  result <- dplyr::bind_rows(components)

  # Calculate percentage contributions
  total_var <- sum(result$variance)
  result$percent_variance <- 100 * result$variance / total_var
  result$cv <- 100 * result$sd / grand_mean

  result
}

# ==============================================================================
# Print methods
# ==============================================================================

#' @export
print.measure_precision <- function(x, ...) {
  precision_type <- attr(x, "precision_type")

  cat("measure_precision:", precision_type, "\n")
  cat(cli::rule(), "\n")

  if (precision_type == "repeatability") {
    for (i in seq_len(nrow(x))) {
      if (!is.null(x$group) && x$group[i] != "overall") {
        cat("\nGroup:", x$group[i], "\n")
      }
      cat("  n =", x$n[i], "\n")
      cat("  Mean =", format(x$mean[i], digits = 4), "\n")
      cat("  SD =", format(x$sd[i], digits = 4), "\n")
      cat("  CV =", format(x$cv[i], digits = 2), "%\n")
      cat(
        "  95% CI: [",
        format(x$ci_lower[i], digits = 4),
        ", ",
        format(x$ci_upper[i], digits = 4),
        "]\n",
        sep = ""
      )
    }
  } else {
    # Variance components display
    cat("\nVariance Components:\n")
    for (i in seq_len(nrow(x))) {
      cat(
        "  ",
        x$component[i],
        ": ",
        format(x$variance[i], digits = 4),
        " (",
        format(x$percent_variance[i], digits = 1),
        "%)\n",
        sep = ""
      )
    }
    cat("\nCV by component:\n")
    for (i in seq_len(nrow(x))) {
      cat(
        "  ",
        x$component[i],
        ": ",
        format(x$cv[i], digits = 2),
        "%\n",
        sep = ""
      )
    }
  }

  invisible(x)
}

#' @export
print.measure_gage_rr <- function(x, ...) {
  cat("measure_gage_rr: Measurement System Analysis\n")
  cat(cli::rule(), "\n\n")

  cat("Study design:\n")
  cat("  Parts:", attr(x, "n_parts"), "\n")
  cat("  Operators:", attr(x, "n_operators"), "\n")
  cat("  Replicates:", attr(x, "n_replicates"), "\n\n")

  cat("Variance Components:\n")
  for (i in seq_len(nrow(x))) {
    cat(
      "  ",
      x$source[i],
      ": ",
      format(x$variance[i], digits = 4),
      " (",
      format(x$pct_contribution[i], digits = 1),
      "% contribution)\n",
      sep = ""
    )
  }

  cat("\n% Study Variation:\n")
  for (i in seq_len(nrow(x))) {
    cat(
      "  ",
      x$source[i],
      ": ",
      format(x$pct_study_var[i], digits = 1),
      "%\n",
      sep = ""
    )
  }

  if (!is.null(attr(x, "tolerance"))) {
    cat("\n% Tolerance:\n")
    for (i in seq_len(nrow(x))) {
      cat(
        "  ",
        x$source[i],
        ": ",
        format(x$pct_tolerance[i], digits = 1),
        "%\n",
        sep = ""
      )
    }
  }

  cat("\nNumber of Distinct Categories (ndc):", attr(x, "ndc"), "\n")

  # Assessment
  pct_rr <- x$pct_study_var[x$source == "Total R&R"]
  cat("\nAssessment:\n")
  if (pct_rr < 10) {
    cat("  Measurement system is ACCEPTABLE (%R&R < 10%)\n")
  } else if (pct_rr < 30) {
    cat("  Measurement system MAY BE ACCEPTABLE (%R&R 10-30%)\n")
  } else {
    cat(
      "
  Measurement system NEEDS IMPROVEMENT (%R&R > 30%)\n"
    )
  }

  invisible(x)
}

# ==============================================================================
# Tidy methods
# ==============================================================================

#' @rdname tidy.recipe
#' @export
tidy.measure_precision <- function(x, ...) {
  # Already a tibble, just ensure clean output
  tibble::as_tibble(x)
}

#' @rdname tidy.recipe
#' @export
tidy.measure_gage_rr <- function(x, ...) {
  tibble::as_tibble(x)
}
