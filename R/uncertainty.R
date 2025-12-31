# ==============================================================================
# Uncertainty Budget Functions (ISO GUM Style)
#
# This file provides functions for calculating measurement uncertainty
# following ISO GUM (Guide to the Expression of Uncertainty in Measurement).
# ==============================================================================

#' Create an Uncertainty Component
#'
#' Defines a single uncertainty component for use in an uncertainty budget.
#' This follows ISO GUM terminology with Type A (statistical) and Type B
#' (other means) uncertainty evaluation.
#'
#' @param name Name/description of the uncertainty source.
#' @param value Standard uncertainty value (u).
#' @param type Type of evaluation:
#'   - `"A"`: Statistical evaluation (from repeated measurements)
#'   - `"B"`: Evaluated by other means (from specifications, certificates, etc.)
#' @param sensitivity Sensitivity coefficient (c). Default is 1.
#'   The contribution to combined uncertainty is `|c| * u`.
#' @param df Degrees of freedom for this component. Default is `Inf`
#'   (for Type B with no DOF information).
#' @param distribution Distribution assumed for Type B:
#'   - `"normal"`: Normal distribution (default for Type A)
#'   - `"rectangular"`: Uniform distribution (common for Type B)
#'   - `"triangular"`: Triangular distribution
#'   - `"u-shaped"`: U-shaped distribution
#' @param coverage_factor Coverage factor (k) used to derive this value from
#'   an expanded uncertainty. Default is 1 (value is already standard uncertainty).
#'
#' @return An `uncertainty_component` object.
#'
#' @details
#' ## Type A Evaluation
#' For Type A components, the standard uncertainty is typically the standard
#' error of the mean: `u = s / sqrt(n)`, with `df = n - 1`.
#'
#' ## Type B Evaluation
#' For Type B components from expanded uncertainties with coverage k:
#' `u = U / k`. For rectangular distributions: `u = a / sqrt(3)`.
#'
#' @seealso [measure_uncertainty_budget()] for combining components,
#'   [measure_uncertainty()] for quick uncertainty calculation.
#'
#' @examples
#' # Type A: Repeatability from 10 measurements
#' u_repeat <- uncertainty_component(
#'   name = "Repeatability",
#'   value = 0.05,  # Standard error of mean
#'   type = "A",
#'   df = 9
#' )
#'
#' # Type B: Calibrator uncertainty from certificate (k=2)
#' u_cal <- uncertainty_component(
#'   name = "Calibrator",
#'   value = 0.02 / 2,  # Divide expanded uncertainty by k
#'   type = "B",
#'   df = 50
#' )
#'
#' # Type B: Temperature effect (rectangular distribution)
#' u_temp <- uncertainty_component(
#'   name = "Temperature",
#'   value = 0.1 / sqrt(3),  # Half-width / sqrt(3) for rectangular
#'   type = "B",
#'   distribution = "rectangular"
#' )
#'
#' @export
uncertainty_component <- function(
  name,
  value,
  type = c("A", "B"),
  sensitivity = 1,
  df = Inf,
  distribution = c("normal", "rectangular", "triangular", "u-shaped"),
  coverage_factor = 1
) {
  type <- match.arg(type)
  distribution <- match.arg(distribution)

  if (!is.character(name) || length(name) != 1) {
    cli::cli_abort("{.arg name} must be a single character string.")
  }
  if (!is.numeric(value) || length(value) != 1 || value < 0) {
    cli::cli_abort("{.arg value} must be a non-negative number.")
  }
  if (!is.numeric(sensitivity) || length(sensitivity) != 1) {
    cli::cli_abort("{.arg sensitivity} must be a single number.")
  }
  if (!is.numeric(df) || length(df) != 1 || df <= 0) {
    cli::cli_abort("{.arg df} must be a positive number.")
  }

  # Apply coverage factor if value was given as expanded uncertainty
  standard_uncertainty <- value / coverage_factor

  structure(
    list(
      name = name,
      value = standard_uncertainty,
      type = type,
      sensitivity = sensitivity,
      df = df,
      distribution = distribution,
      contribution = abs(sensitivity) * standard_uncertainty
    ),
    class = "uncertainty_component"
  )
}

#' @export
print.uncertainty_component <- function(x, ...) {
  cat("<uncertainty_component>\n")
  cat("  Name: ", x$name, "\n", sep = "")
  cat("  Type: ", x$type, "\n", sep = "")
  cat("  u: ", format(x$value, digits = 4), "\n", sep = "")
  cat("  c: ", format(x$sensitivity, digits = 4), "\n", sep = "")
  cat("  |c|*u: ", format(x$contribution, digits = 4), "\n", sep = "")
  cat("  df: ", if (is.infinite(x$df)) "Inf" else round(x$df), "\n", sep = "")
  invisible(x)
}

#' Create an Uncertainty Budget
#'
#' Combines multiple uncertainty components into a complete uncertainty
#' budget following ISO GUM methodology. Calculates combined standard
#' uncertainty, effective degrees of freedom (Welch-Satterthwaite), and
#' expanded uncertainty.
#'
#' @param ... [uncertainty_component()] objects to include in the budget.
#' @param .list Optional list of uncertainty components.
#' @param k Coverage factor for expanded uncertainty. Default is 2
#'   (approximately 95% coverage for normal distribution).
#' @param result_value Optional. The measurement result value, used for
#'   calculating relative uncertainty.
#'
#' @return A `measure_uncertainty_budget` object containing:
#'   - `components`: List of input uncertainty components
#'   - `combined_u`: Combined standard uncertainty
#'   - `effective_df`: Effective degrees of freedom (Welch-Satterthwaite)
#'   - `coverage_factor`: The k value used
#'   - `expanded_U`: Expanded uncertainty (k * combined_u)
#'   - `result_value`: The measurement result (if provided)
#'   - `relative_u`: Relative standard uncertainty (if result provided)
#'
#' @details
#' ## Combined Standard Uncertainty
#' Calculated as the root sum of squares of contributions:
#' \deqn{u_c = \sqrt{\sum_i (c_i \cdot u_i)^2}}
#'
#' ## Welch-Satterthwaite Effective Degrees of Freedom
#' \deqn{\nu_{eff} = \frac{u_c^4}{\sum_i \frac{(c_i \cdot u_i)^4}{\nu_i}}}
#'
#' This is used to determine the appropriate coverage factor for a
#' given confidence level.
#'
#' ## Expanded Uncertainty
#' \deqn{U = k \cdot u_c}
#'
#' With k=2, this provides approximately 95% coverage.
#'
#' @seealso [uncertainty_component()] for creating components,
#'   [tidy.measure_uncertainty_budget()] for extracting results,
#'   [autoplot.measure_uncertainty_budget()] for visualization.
#'
#' @examples
#' # Create components
#' u_repeat <- uncertainty_component("Repeatability", 0.05, type = "A", df = 9)
#' u_cal <- uncertainty_component("Calibrator", 0.02, type = "B", df = 50)
#' u_temp <- uncertainty_component("Temperature", 0.03, type = "B")
#'
#' # Create budget
#' budget <- measure_uncertainty_budget(u_repeat, u_cal, u_temp, k = 2)
#' print(budget)
#'
#' # With result value for relative uncertainty
#' budget <- measure_uncertainty_budget(
#'   u_repeat, u_cal, u_temp,
#'   result_value = 10.5
#' )
#'
#' @export
measure_uncertainty_budget <- function(
  ...,
  .list = NULL,
  k = 2,
  result_value = NULL
) {
  components <- list(...)

  # Add from .list
  if (!is.null(.list)) {
    components <- c(components, .list)
  }

  # Validate all components
  for (i in seq_along(components)) {
    if (!inherits(components[[i]], "uncertainty_component")) {
      cli::cli_abort(
        "Argument {i} must be an {.cls uncertainty_component} object."
      )
    }
  }

  if (length(components) == 0) {
    cli::cli_abort("At least one uncertainty component is required.")
  }

  # Calculate combined standard uncertainty
  contributions <- vapply(components, function(c) c$contribution, double(1))
  combined_u <- sqrt(sum(contributions^2))

  # Calculate effective degrees of freedom (Welch-Satterthwaite)
  dfs <- vapply(components, function(c) c$df, double(1))
  u4_over_df <- contributions^4 / dfs
  effective_df <- combined_u^4 / sum(u4_over_df)

  # Calculate expanded uncertainty
  expanded_U <- k * combined_u

  # Calculate relative uncertainty if result provided
  relative_u <- NULL
  relative_U <- NULL
  if (!is.null(result_value)) {
    if (result_value != 0) {
      relative_u <- combined_u / abs(result_value)
      relative_U <- expanded_U / abs(result_value)
    }
  }

  structure(
    list(
      components = components,
      combined_u = combined_u,
      effective_df = effective_df,
      coverage_factor = k,
      expanded_U = expanded_U,
      result_value = result_value,
      relative_u = relative_u,
      relative_U = relative_U
    ),
    class = "measure_uncertainty_budget"
  )
}

#' Quick Uncertainty Calculation
#'
#' A convenience function that returns just the key uncertainty values
#' without the full budget object.
#'
#' @inheritParams measure_uncertainty_budget
#'
#' @return A named list with:
#'   - `combined_u`: Combined standard uncertainty
#'   - `expanded_U`: Expanded uncertainty
#'   - `effective_df`: Effective degrees of freedom
#'   - `coverage_factor`: Coverage factor used
#'
#' @examples
#' u1 <- uncertainty_component("A", 0.05, type = "A", df = 9)
#' u2 <- uncertainty_component("B", 0.03, type = "B")
#'
#' measure_uncertainty(u1, u2)
#'
#' @export
measure_uncertainty <- function(..., .list = NULL, k = 2) {
  budget <- measure_uncertainty_budget(..., .list = .list, k = k)

  list(
    combined_u = budget$combined_u,
    expanded_U = budget$expanded_U,
    effective_df = budget$effective_df,
    coverage_factor = budget$coverage_factor
  )
}

#' @export
print.measure_uncertainty_budget <- function(x, ...) {
  n <- length(x$components)
  n_type_a <- sum(vapply(x$components, function(c) c$type == "A", logical(1)))
  n_type_b <- n - n_type_a

  cat("<measure_uncertainty_budget>\n")
  cat(
    "  Components: ",
    n,
    " (",
    n_type_a,
    " Type A, ",
    n_type_b,
    " Type B)\n",
    sep = ""
  )
  cat("  Combined u: ", format(x$combined_u, digits = 4), "\n", sep = "")
  cat("  Effective df: ", format(x$effective_df, digits = 1), "\n", sep = "")
  cat("  Coverage k: ", x$coverage_factor, "\n", sep = "")
  cat("  Expanded U: ", format(x$expanded_U, digits = 4), "\n", sep = "")

  if (!is.null(x$result_value)) {
    cat("\n  Result: ", format(x$result_value, digits = 4), "\n", sep = "")
    if (!is.null(x$relative_u)) {
      cat(
        "  Relative u: ",
        format(100 * x$relative_u, digits = 2),
        "%\n",
        sep = ""
      )
      cat(
        "  Relative U: ",
        format(100 * x$relative_U, digits = 2),
        "%\n",
        sep = ""
      )
    }
  }

  invisible(x)
}

#' Tidy an Uncertainty Budget
#'
#' Extract uncertainty budget information in tidy format.
#'
#' @param x A [measure_uncertainty_budget] object.
#' @param type What to return:
#'   - `"components"` (default): Table of individual components
#'   - `"summary"`: Single row summary of the budget
#' @param ... Additional arguments (unused).
#'
#' @return A tibble with budget information.
#'
#' @examples
#' u1 <- uncertainty_component("Repeatability", 0.05, type = "A", df = 9)
#' u2 <- uncertainty_component("Calibrator", 0.02, type = "B")
#' budget <- measure_uncertainty_budget(u1, u2)
#'
#' tidy(budget)
#' tidy(budget, type = "summary")
#'
#' @importFrom generics tidy
#' @export
tidy.measure_uncertainty_budget <- function(
  x,
  type = c("components", "summary"),
  ...
) {
  type <- match.arg(type)

  if (type == "components") {
    components <- x$components
    contributions <- vapply(components, function(c) c$contribution, double(1))
    total_variance <- sum(contributions^2)

    tibble::tibble(
      name = vapply(components, function(c) c$name, character(1)),
      type = vapply(components, function(c) c$type, character(1)),
      u = vapply(components, function(c) c$value, double(1)),
      sensitivity = vapply(components, function(c) c$sensitivity, double(1)),
      contribution = contributions,
      variance_contribution = contributions^2,
      percent_contribution = 100 * contributions^2 / total_variance,
      df = vapply(components, function(c) c$df, double(1))
    )
  } else {
    tibble::tibble(
      combined_u = x$combined_u,
      effective_df = x$effective_df,
      coverage_factor = x$coverage_factor,
      expanded_U = x$expanded_U,
      result_value = x$result_value %||% NA_real_,
      relative_u = x$relative_u %||% NA_real_,
      relative_U = x$relative_U %||% NA_real_
    )
  }
}

#' Plot Uncertainty Budget
#'
#' Creates a Pareto chart showing the relative contribution of each
#' uncertainty component to the combined uncertainty.
#'
#' @param object A [measure_uncertainty_budget] object.
#' @param ... Additional arguments (unused).
#'
#' @return A ggplot object showing the Pareto chart.
#'
#' @examples
#' library(ggplot2)
#' u1 <- uncertainty_component("Repeatability", 0.05, type = "A", df = 9)
#' u2 <- uncertainty_component("Calibrator", 0.02, type = "B")
#' u3 <- uncertainty_component("Temperature", 0.03, type = "B")
#' budget <- measure_uncertainty_budget(u1, u2, u3)
#'
#' autoplot(budget)
#'
#' @importFrom ggplot2 autoplot
#' @export
autoplot.measure_uncertainty_budget <- function(object, ...) {
  tidy_data <- tidy(object, type = "components")

  # Order by contribution
  tidy_data <- tidy_data[order(-tidy_data$percent_contribution), ]
  tidy_data$name <- factor(tidy_data$name, levels = tidy_data$name)

  # Calculate cumulative percentage for Pareto line
  tidy_data$cumulative <- cumsum(tidy_data$percent_contribution)

  ggplot2::ggplot(
    tidy_data,
    ggplot2::aes(x = .data$name, y = .data$percent_contribution)
  ) +
    ggplot2::geom_col(ggplot2::aes(fill = .data$type), width = 0.7) +
    ggplot2::geom_line(
      ggplot2::aes(y = .data$cumulative, group = 1),
      linewidth = 0.8,
      color = "gray30"
    ) +
    ggplot2::geom_point(
      ggplot2::aes(y = .data$cumulative),
      size = 2,
      color = "gray30"
    ) +
    ggplot2::scale_fill_manual(
      values = c("A" = "#2166AC", "B" = "#B2182B"),
      labels = c("A" = "Type A", "B" = "Type B"),
      name = "Evaluation"
    ) +
    ggplot2::scale_y_continuous(
      name = "Contribution to Variance (%)",
      sec.axis = ggplot2::sec_axis(~., name = "Cumulative (%)")
    ) +
    ggplot2::labs(
      title = "Uncertainty Budget - Pareto Chart",
      subtitle = sprintf(
        "Combined u = %s, Expanded U (k=%s) = %s",
        format(object$combined_u, digits = 4),
        object$coverage_factor,
        format(object$expanded_U, digits = 4)
      ),
      x = "Uncertainty Source"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
}

# ==============================================================================
# Helper functions for common uncertainty scenarios
# ==============================================================================

#' Create Type A Uncertainty from Repeated Measurements
#'
#' Helper function to calculate Type A uncertainty from a vector of
#' repeated measurements.
#'
#' @param x Numeric vector of repeated measurements.
#' @param name Name for this uncertainty component.
#' @param sensitivity Sensitivity coefficient (default 1).
#'
#' @return An [uncertainty_component] object.
#'
#' @examples
#' measurements <- c(10.1, 10.3, 9.9, 10.2, 10.0)
#' u_repeat <- uncertainty_type_a(measurements, "Repeatability")
#'
#' @export
uncertainty_type_a <- function(x, name = "Type A", sensitivity = 1) {
  x <- x[!is.na(x)]
  n <- length(x)

  if (n < 2) {
    cli::cli_abort(
      "At least 2 measurements are required for Type A evaluation."
    )
  }

  se <- stats::sd(x) / sqrt(n)
  df <- n - 1

  uncertainty_component(
    name = name,
    value = se,
    type = "A",
    sensitivity = sensitivity,
    df = df,
    distribution = "normal"
  )
}

#' Create Type B Uncertainty from Expanded Uncertainty
#'
#' Helper function to create a Type B uncertainty component from an
#' expanded uncertainty value (e.g., from a certificate).
#'
#' @param expanded_U The expanded uncertainty value.
#' @param k Coverage factor used for the expanded uncertainty.
#' @param name Name for this uncertainty component.
#' @param df Degrees of freedom (default Inf).
#' @param sensitivity Sensitivity coefficient (default 1).
#'
#' @return An [uncertainty_component] object.
#'
#' @examples
#' # From a calibrator certificate: U = 0.05, k = 2
#' u_cal <- uncertainty_type_b_expanded(0.05, k = 2, name = "Calibrator")
#'
#' @export
uncertainty_type_b_expanded <- function(
  expanded_U,
  k = 2,
  name = "Type B",
  df = Inf,
  sensitivity = 1
) {
  uncertainty_component(
    name = name,
    value = expanded_U,
    type = "B",
    sensitivity = sensitivity,
    df = df,
    distribution = "normal",
    coverage_factor = k
  )
}

#' Create Type B Uncertainty from Rectangular Distribution
#'
#' Helper function to create a Type B uncertainty component from a
#' rectangular (uniform) distribution, common for specifications or tolerances.
#'
#' @param half_width The half-width of the rectangular distribution (a).
#'   Standard uncertainty will be `a / sqrt(3)`.
#' @param name Name for this uncertainty component.
#' @param sensitivity Sensitivity coefficient (default 1).
#'
#' @return An [uncertainty_component] object.
#'
#' @examples
#' # Temperature stability +/- 0.5 degrees
#' u_temp <- uncertainty_type_b_rectangular(0.5, name = "Temperature")
#'
#' @export
uncertainty_type_b_rectangular <- function(
  half_width,
  name = "Type B",
  sensitivity = 1
) {
  u <- half_width / sqrt(3)

  uncertainty_component(
    name = name,
    value = u,
    type = "B",
    sensitivity = sensitivity,
    df = Inf,
    distribution = "rectangular"
  )
}
