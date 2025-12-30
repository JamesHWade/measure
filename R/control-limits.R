# ==============================================================================
# Control Charts and Statistical Process Control
#
# This file contains control chart functions for QC monitoring:
# - measure_control_limits: Calculate control limits
# - measure_control_chart: Generate control charts with multi-rule support
# - measure_system_suitability: System suitability checks
# ==============================================================================

#' Calculate Control Limits
#'
#' Calculates control limits for quality control monitoring using Shewhart
#' rules and optionally EWMA or CUSUM statistics.
#'
#' @param data A data frame containing QC measurements.
#' @param value_col Name of the column containing QC values.
#' @param group_col Optional grouping column (e.g., for different QC levels).
#' @param type Type of control chart: `"shewhart"` (default), `"ewma"`, or `"cusum"`.
#' @param n_sigma Number of standard deviations for control limits. Default is 3.
#' @param target Optional target value. If NULL, calculated from data mean.
#' @param lambda EWMA smoothing parameter (0 < lambda <= 1). Default is 0.2.
#' @param k CUSUM slack parameter. Default is 0.5 (in sigma units).
#' @param h CUSUM decision interval. Default is 5 (in sigma units).
#'
#' @return A `measure_control_limits` object containing:
#'   - `center`: Center line (target or mean)
#'   - `lcl`: Lower control limit
#'   - `ucl`: Upper control limit
#'   - `lwl`: Lower warning limit (2 sigma)
#'   - `uwl`: Upper warning limit (2 sigma)
#'   - `sigma`: Estimated standard deviation
#'   - Additional statistics depending on chart type
#'
#' @details
#' ## Shewhart Charts
#' Classic control charts with limits at mean +/- n*sigma:
#' - UCL/LCL: Action limits (typically 3 sigma)
#' - UWL/LWL: Warning limits (typically 2 sigma)
#'
#' ## EWMA Charts
#' Exponentially weighted moving average, more sensitive to small shifts:
#' - Control limits narrow as more data is collected
#' - Lambda parameter controls weight of recent observations
#'
#' ## CUSUM Charts
#' Cumulative sum chart for detecting persistent shifts:
#' - Upper and lower CUSUM statistics track cumulative deviations
#' - Decision interval h determines sensitivity
#'
#' @family control-charts
#' @seealso [measure_control_chart()], [measure_system_suitability()]
#'
#' @export
#'
#' @examples
#' # Calculate Shewhart control limits
#' set.seed(123)
#' qc_data <- data.frame(
#'   run_order = 1:30,
#'   qc_value = rnorm(30, mean = 100, sd = 2)
#' )
#' limits <- measure_control_limits(qc_data, "qc_value")
#' print(limits)
#'
#' # EWMA control limits
#' limits_ewma <- measure_control_limits(qc_data, "qc_value", type = "ewma")
measure_control_limits <- function(
    data,
    value_col,
    group_col = NULL,
    type = c("shewhart", "ewma", "cusum"),
    n_sigma = 3,
    target = NULL,
    lambda = 0.2,
    k = 0.5,
    h = 5) {

  type <- match.arg(type)

  # Validate inputs
  if (!value_col %in% names(data)) {
    cli::cli_abort("Column {.field {value_col}} not found in data.")
  }

  if (!is.null(group_col) && !group_col %in% names(data)) {
    cli::cli_abort("Column {.field {group_col}} not found in data.")
  }

  # Calculate limits
  if (is.null(group_col)) {
    result <- calculate_limits_single(
      data[[value_col]], type, n_sigma, target, lambda, k, h
    )
    result$group <- "all"
    result <- tibble::as_tibble(result)
  } else {
    groups <- unique(data[[group_col]])
    results <- lapply(groups, function(g) {
      values <- data[[value_col]][data[[group_col]] == g]
      res <- calculate_limits_single(values, type, n_sigma, target, lambda, k, h)
      res$group <- g
      res
    })
    result <- dplyr::bind_rows(results)
  }

  structure(
    result,
    class = c("measure_control_limits", "tbl_df", "tbl", "data.frame"),
    type = type,
    n_sigma = n_sigma,
    lambda = lambda,
    k = k,
    h = h
  )
}

#' Generate Control Chart
#'
#' Creates a control chart with optional multi-rule (Westgard) violation detection.
#'
#' @param data A data frame containing QC measurements.
#' @param value_col Name of the column containing QC values.
#' @param order_col Name of the column containing run order/sequence.
#' @param limits Optional `measure_control_limits` object. If NULL, calculated
#'   from the data.
#' @param rules Character vector of Westgard rules to apply. Default is
#'   `c("1_3s", "2_2s", "R_4s", "4_1s", "10x")`.
#' @param group_col Optional grouping column.
#'
#' @return A `measure_control_chart` object containing:
#'   - `data`: The input data with added violation flags
#'   - `limits`: The control limits used
#'   - `violations`: Summary of rule violations
#'   - `rules_applied`: Which rules were checked
#'
#' @details
#' ## Westgard Rules
#' The function supports common Westgard multi-rules:
#' - **1:3s**: One point beyond 3 sigma (action required)
#' - **2:2s**: Two consecutive points beyond 2 sigma (warning)
#' - **R:4s**: Range of two consecutive points > 4 sigma
#' - **4:1s**: Four consecutive points beyond 1 sigma (same side)
#' - **10x**: Ten consecutive points on same side of mean
#'
#' ## Interpretation
#' - Violations are flagged with the specific rule that was triggered

#' - Multiple rules can be triggered by the same point
#' - A run is considered "in control" if no violations are detected
#'
#' @family control-charts
#' @seealso [measure_control_limits()], [autoplot.measure_control_chart()]
#'
#' @export
#'
#' @examples
#' # Generate control chart with Westgard rules
#' set.seed(123)
#' qc_data <- data.frame(
#'   run_order = 1:50,
#'   qc_value = c(rnorm(45, 100, 2), rnorm(5, 106, 2))  # Last 5 shifted
#' )
#' chart <- measure_control_chart(qc_data, "qc_value", "run_order")
#' print(chart)
measure_control_chart <- function(
    data,
    value_col,
    order_col,
    limits = NULL,
    rules = c("1_3s", "2_2s", "R_4s", "4_1s", "10x"),
    group_col = NULL) {

  # Validate inputs
  for (col in c(value_col, order_col)) {
    if (!col %in% names(data)) {
      cli::cli_abort("Column {.field {col}} not found in data.")
    }
  }

  # Sort by order
  data <- data[order(data[[order_col]]), ]

  # Calculate limits if not provided
  if (is.null(limits)) {
    limits <- measure_control_limits(data, value_col, group_col)
  }

  # Apply rules and detect violations
  if (is.null(group_col)) {
    result <- apply_westgard_rules(
      data, value_col, limits, rules
    )
  } else {
    groups <- unique(data[[group_col]])
    results <- lapply(groups, function(g) {
      sub_data <- data[data[[group_col]] == g, ]
      group_limits <- limits[limits$group == g, ]
      apply_westgard_rules(sub_data, value_col, group_limits, rules)
    })
    result <- list(
      data = dplyr::bind_rows(lapply(results, `[[`, "data")),
      violations = dplyr::bind_rows(lapply(results, `[[`, "violations"))
    )
  }

  structure(
    list(
      data = result$data,
      limits = limits,
      violations = result$violations,
      rules_applied = rules,
      in_control = nrow(result$violations) == 0
    ),
    class = "measure_control_chart"
  )
}

#' System Suitability Check
#'
#' Performs system suitability tests on QC or reference samples to verify
#' instrument performance meets requirements.
#'
#' @param data A data frame containing system suitability data.
#' @param metrics Named list of columns and their acceptance criteria.
#'   Each element should be a list with `col`, `min`, and/or `max`.
#' @param sample_type_col Optional column identifying sample types.
#' @param sst_type Value in sample_type_col that identifies SST samples.
#'
#' @return A `measure_sst` object containing:
#'   - `results`: Pass/fail status for each metric
#'   - `summary`: Overall pass/fail and summary statistics
#'   - `details`: Individual sample results
#'
#' @details
#' System suitability testing (SST) verifies that the analytical system
#' is performing adequately before, during, or after a run. Common metrics
#' include:
#' - Peak resolution
#' - Retention time reproducibility
#' - Peak symmetry/tailing factor
#' - Signal-to-noise ratio
#' - Plate count
#'
#' @family control-charts
#'
#' @export
#'
#' @examples
#' # System suitability check
#' sst_data <- data.frame(
#'   sample_id = paste0("SST_", 1:5),
#'   resolution = c(2.1, 2.3, 2.2, 2.0, 2.1),
#'   tailing = c(1.1, 1.0, 1.2, 1.1, 1.0),
#'   plates = c(5200, 5100, 5300, 5000, 5150)
#' )
#'
#' result <- measure_system_suitability(
#'   sst_data,
#'   metrics = list(
#'     resolution = list(col = "resolution", min = 2.0),
#'     tailing = list(col = "tailing", max = 1.5),
#'     plates = list(col = "plates", min = 5000)
#'   )
#' )
#' print(result)
measure_system_suitability <- function(
    data,
    metrics,
    sample_type_col = NULL,
    sst_type = "sst") {

  # Filter to SST samples if specified
  if (!is.null(sample_type_col)) {
    if (!sample_type_col %in% names(data)) {
      cli::cli_abort("Column {.field {sample_type_col}} not found in data.")
    }
    data <- data[data[[sample_type_col]] == sst_type, ]
  }

  if (nrow(data) == 0) {
    cli::cli_abort("No system suitability samples found.")
  }

  # Evaluate each metric
  results <- lapply(names(metrics), function(metric_name) {
    spec <- metrics[[metric_name]]
    col <- spec$col

    if (!col %in% names(data)) {
      cli::cli_warn("Column {.field {col}} not found. Skipping metric {.field {metric_name}}.")
      return(NULL)
    }

    values <- data[[col]]
    mean_val <- mean(values, na.rm = TRUE)
    sd_val <- sd(values, na.rm = TRUE)
    cv_val <- 100 * sd_val / mean_val

    # Check against limits
    pass_min <- if (!is.null(spec$min)) all(values >= spec$min, na.rm = TRUE) else TRUE
    pass_max <- if (!is.null(spec$max)) all(values <= spec$max, na.rm = TRUE) else TRUE
    pass <- pass_min && pass_max

    tibble::tibble(
      metric = metric_name,
      column = col,
      n = sum(!is.na(values)),
      mean = mean_val,
      sd = sd_val,
      cv = cv_val,
      min_spec = spec$min %||% NA_real_,
      max_spec = spec$max %||% NA_real_,
      observed_min = min(values, na.rm = TRUE),
      observed_max = max(values, na.rm = TRUE),
      pass = pass
    )
  })

  results <- dplyr::bind_rows(results)

  structure(
    list(
      results = results,
      overall_pass = all(results$pass),
      n_samples = nrow(data),
      n_metrics = nrow(results),
      n_pass = sum(results$pass),
      n_fail = sum(!results$pass)
    ),
    class = "measure_sst"
  )
}

# ==============================================================================
# Helper functions
# ==============================================================================

#' Calculate control limits for a single group
#' @noRd
calculate_limits_single <- function(
    values,
    type,
    n_sigma,
    target,
    lambda,
    k,
    h) {

  values <- values[!is.na(values)]
  n <- length(values)

  if (n < 2) {
    cli::cli_abort("At least 2 observations required to calculate control limits.")
  }

  # Center and spread
  center <- if (is.null(target)) mean(values) else target
  sigma <- sd(values)

  if (type == "shewhart") {
    # Standard Shewhart limits
    list(
      n = n,
      center = center,
      sigma = sigma,
      lcl = center - n_sigma * sigma,
      ucl = center + n_sigma * sigma,
      lwl = center - 2 * sigma,
      uwl = center + 2 * sigma,
      l1s = center - sigma,
      u1s = center + sigma
    )
  } else if (type == "ewma") {
    # EWMA control limits
    # Asymptotic limits
    ewma_sigma <- sigma * sqrt(lambda / (2 - lambda))
    list(
      n = n,
      center = center,
      sigma = sigma,
      ewma_sigma = ewma_sigma,
      lcl = center - n_sigma * ewma_sigma,
      ucl = center + n_sigma * ewma_sigma,
      lambda = lambda
    )
  } else if (type == "cusum") {
    # CUSUM parameters
    list(
      n = n,
      center = center,
      sigma = sigma,
      k = k * sigma,  # Slack in original units
      h = h * sigma,  # Decision interval in original units
      k_sigma = k,
      h_sigma = h
    )
  }
}

#' Apply Westgard rules to detect violations
#' @noRd
apply_westgard_rules <- function(data, value_col, limits, rules) {
  values <- data[[value_col]]
  n <- length(values)

  center <- limits$center[1]
  sigma <- limits$sigma[1]
  ucl <- limits$ucl[1]
  lcl <- limits$lcl[1]
  uwl <- limits$uwl[1]
  lwl <- limits$lwl[1]
  u1s <- limits$u1s[1]
  l1s <- limits$l1s[1]

  # Initialize violation tracking
  violations <- character(n)

  # Standardized values
  z <- (values - center) / sigma

  # Rule 1:3s - One point beyond 3 sigma
  if ("1_3s" %in% rules) {
    out_3s <- abs(z) > 3
    violations[out_3s] <- paste0(violations[out_3s], "1:3s ")
  }

  # Rule 2:2s - Two consecutive points beyond 2 sigma (same side)
  if ("2_2s" %in% rules && n >= 2) {
    for (i in 2:n) {
      if ((z[i] > 2 && z[i-1] > 2) || (z[i] < -2 && z[i-1] < -2)) {
        violations[i] <- paste0(violations[i], "2:2s ")
        violations[i-1] <- paste0(violations[i-1], "2:2s ")
      }
    }
  }

  # Rule R:4s - Range of two consecutive points > 4 sigma
  if ("R_4s" %in% rules && n >= 2) {
    for (i in 2:n) {
      if (abs(z[i] - z[i-1]) > 4) {
        violations[i] <- paste0(violations[i], "R:4s ")
        violations[i-1] <- paste0(violations[i-1], "R:4s ")
      }
    }
  }

  # Rule 4:1s - Four consecutive points beyond 1 sigma (same side)
  if ("4_1s" %in% rules && n >= 4) {
    for (i in 4:n) {
      if (all(z[(i-3):i] > 1) || all(z[(i-3):i] < -1)) {
        for (j in (i-3):i) {
          violations[j] <- paste0(violations[j], "4:1s ")
        }
      }
    }
  }

  # Rule 10x - Ten consecutive points on same side of mean
  if ("10x" %in% rules && n >= 10) {
    for (i in 10:n) {
      if (all(z[(i-9):i] > 0) || all(z[(i-9):i] < 0)) {
        for (j in (i-9):i) {
          violations[j] <- paste0(violations[j], "10x ")
        }
      }
    }
  }

  # Clean up violations
  violations <- trimws(violations)
  data$violation <- violations
  data$has_violation <- nchar(violations) > 0

  # Summary of violations
  violation_summary <- data[data$has_violation, c(names(data)[1], value_col, "violation")]

  list(
    data = data,
    violations = violation_summary
  )
}

# ==============================================================================
# Print methods
# ==============================================================================

#' @export
print.measure_control_limits <- function(x, ...) {
  type <- attr(x, "type")
  cat("measure_control_limits:", type, "chart\n")
  cat(cli::rule(), "\n\n")

  for (i in seq_len(nrow(x))) {
    if (x$group[i] != "all") {
      cat("Group:", x$group[i], "\n")
    }
    cat("  n =", x$n[i], "\n")
    cat("  Center =", format(x$center[i], digits = 4), "\n")
    cat("  Sigma =", format(x$sigma[i], digits = 4), "\n")

    if (type == "shewhart") {
      cat("  UCL (+3s) =", format(x$ucl[i], digits = 4), "\n")
      cat("  UWL (+2s) =", format(x$uwl[i], digits = 4), "\n")
      cat("  LWL (-2s) =", format(x$lwl[i], digits = 4), "\n")
      cat("  LCL (-3s) =", format(x$lcl[i], digits = 4), "\n")
    } else if (type == "ewma") {
      cat("  Lambda =", attr(x, "lambda"), "\n")
      cat("  UCL =", format(x$ucl[i], digits = 4), "\n")
      cat("  LCL =", format(x$lcl[i], digits = 4), "\n")
    } else if (type == "cusum") {
      cat("  k =", format(x$k[i], digits = 4), "\n")
      cat("  h =", format(x$h[i], digits = 4), "\n")
    }
    cat("\n")
  }

  invisible(x)
}

#' @export
print.measure_control_chart <- function(x, ...) {
  cat("measure_control_chart\n")
  cat(cli::rule(), "\n\n")

  cat("Observations:", nrow(x$data), "\n")
  cat("Rules applied:", paste(x$rules_applied, collapse = ", "), "\n")
  cat("Violations detected:", nrow(x$violations), "\n")

  if (x$in_control) {
    cat("\nStatus: IN CONTROL\n")
  } else {
    cat("\nStatus: OUT OF CONTROL\n")
    cat("\nViolation summary:\n")
    print(tibble::as_tibble(x$violations), n = 10)
  }

  invisible(x)
}

#' @export
print.measure_sst <- function(x, ...) {
  cat("measure_system_suitability\n")
  cat(cli::rule(), "\n\n")

  cat("Samples evaluated:", x$n_samples, "\n")
  cat("Metrics checked:", x$n_metrics, "\n")
  cat("Passed:", x$n_pass, "/", x$n_metrics, "\n\n")

  if (x$overall_pass) {
    cat("Overall Status: PASS\n\n")
  } else {
    cat("Overall Status: FAIL\n\n")
  }

  cat("Results:\n")
  print(x$results)

  invisible(x)
}

# ==============================================================================
# Tidy methods
# ==============================================================================

#' @rdname tidy.recipe
#' @export
tidy.measure_control_limits <- function(x, ...) {

  tibble::as_tibble(x)
}

#' @rdname tidy.recipe
#' @export
tidy.measure_control_chart <- function(x, type = c("data", "violations", "limits"), ...) {
  type <- match.arg(type)
  switch(type,
    data = tibble::as_tibble(x$data),
    violations = tibble::as_tibble(x$violations),
    limits = tidy(x$limits)
  )
}

#' @rdname tidy.recipe
#' @export
tidy.measure_sst <- function(x, ...) {
  x$results
}

# ==============================================================================
# Autoplot methods
# ==============================================================================

#' @rdname autoplot.measure_uncertainty_budget
#' @export
autoplot.measure_control_chart <- function(object, ...) {
  data <- object$data
  limits <- object$limits

  # Find the order column (first non-value column before violation)
  cols <- names(data)
  order_col <- cols[1]
  value_col <- cols[2]

  p <- ggplot2::ggplot(data, ggplot2::aes(
    x = .data[[order_col]],
    y = .data[[value_col]]
  )) +
    # Control limits
    ggplot2::geom_hline(
      yintercept = limits$center[1],
      linetype = "solid",
      color = "darkgreen"
    ) +
    ggplot2::geom_hline(
      yintercept = c(limits$ucl[1], limits$lcl[1]),
      linetype = "dashed",
      color = "red"
    ) +
    ggplot2::geom_hline(
      yintercept = c(limits$uwl[1], limits$lwl[1]),
      linetype = "dotted",
      color = "orange"
    ) +
    # Points
    ggplot2::geom_line(color = "gray50") +
    ggplot2::geom_point(
      ggplot2::aes(color = .data$has_violation),
      size = 2
    ) +
    ggplot2::scale_color_manual(
      values = c("FALSE" = "steelblue", "TRUE" = "red"),
      labels = c("In Control", "Violation"),
      name = "Status"
    ) +
    ggplot2::labs(
      title = "Control Chart",
      subtitle = paste("Rules:", paste(object$rules_applied, collapse = ", ")),
      x = "Observation",
      y = "Value"
    ) +
    ggplot2::theme_minimal()

  p
}
