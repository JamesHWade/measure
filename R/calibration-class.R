# ==============================================================================
# Calibration Class Infrastructure
#
# This file defines the S3 class for calibration curve objects.
# The fitting and prediction logic is in calibration-fit.R
# ==============================================================================

#' Calibration Curve Object
#'
#' A calibration curve object stores the fitted model, diagnostics, and
#' metadata for quantitation workflows. Created by [measure_calibration_fit()].
#'
#' @section Structure:
#' A `measure_calibration` object is a list containing:
#' \describe{
#'   \item{model}{The underlying fitted model (lm object)}
#'   \item{model_type}{Character: "linear" or "quadratic"}
#'   \item{weights_type}{Character: weighting scheme used}
#'   \item{formula}{The model formula}
#'   \item{data}{The calibration data used for fitting}
#'   \item{diagnostics}{List of diagnostic statistics}
#'   \item{outliers}{Data frame of flagged outliers (if any)}
#'   \item{call}{The original function call}
#' }
#'
#' @seealso [measure_calibration_fit()] for creating calibration objects,
#'   [measure_calibration_predict()] for prediction,
#'   [tidy.measure_calibration()] for extracting coefficients,
#'   [autoplot.measure_calibration()] for diagnostic plots.
#'
#' @name measure_calibration
#' @aliases measure_calibration-class
NULL

#' Create a New Calibration Object
#'
#' Low-level constructor for calibration objects. Users should use
#' [measure_calibration_fit()] instead.
#'
#' @param model Fitted lm object.
#' @param model_type Character: "linear" or "quadratic".
#' @param weights_type Character: weighting scheme used.
#' @param formula The model formula.
#' @param data The calibration data.
#' @param response_col Name of response column.
#' @param conc_col Name of concentration column.
#' @param diagnostics List of diagnostic statistics.
#' @param outliers Data frame of flagged outliers.
#' @param call The original function call.
#'
#' @return A `measure_calibration` object.
#'
#' @keywords internal
#' @noRd
new_measure_calibration <- function(
  model,
  model_type,
  weights_type,
  formula,
  data,
  response_col,
  conc_col,
  diagnostics = list(),
  outliers = NULL,
  call = NULL
) {
  structure(
    list(
      model = model,
      model_type = model_type,
      weights_type = weights_type,
      formula = formula,
      data = data,
      response_col = response_col,
      conc_col = conc_col,
      diagnostics = diagnostics,
      outliers = outliers,
      call = call
    ),
    class = "measure_calibration"
  )
}

#' Test if Object is a Calibration Curve
#'
#' @param x Object to test.
#'
#' @return Logical: TRUE if `x` is a `measure_calibration` object.
#'
#' @examples
#' # After fitting a calibration curve
#' data <- data.frame(
#'   nominal_conc = c(0, 10, 25, 50, 100),
#'   response = c(0.5, 15.2, 35.8, 72.1, 148.3)
#' )
#' cal <- measure_calibration_fit(data, response ~ nominal_conc)
#' is_measure_calibration(cal)
#'
#' @export
is_measure_calibration <- function(x) {
  inherits(x, "measure_calibration")
}

#' @export
print.measure_calibration <- function(x, ...) {
  cat("<measure_calibration>\n")
  cat("  Model: ", x$model_type, "\n", sep = "")
  cat("  Weighting: ", x$weights_type, "\n", sep = "")
  cat("  Formula: ", deparse(x$formula), "\n", sep = "")
  cat("  N points: ", nrow(x$data), "\n", sep = "")

  if (!is.null(x$diagnostics$r_squared)) {
    cat(
      "  R\u00b2: ",
      format(x$diagnostics$r_squared, digits = 5),
      "\n",
      sep = ""
    )
  }

  if (!is.null(x$outliers) && nrow(x$outliers) > 0) {
    cat("  Outliers flagged: ", nrow(x$outliers), "\n", sep = "")
  }

  invisible(x)
}

#' Tidy a Calibration Curve
#'
#' Extract coefficients and statistics from a calibration curve in tidy format.
#'
#' @param x A [measure_calibration] object.
#' @param ... Additional arguments (unused).
#'
#' @return A tibble with columns:
#'   - `term`: Coefficient name (intercept, slope, quadratic)
#'   - `estimate`: Coefficient estimate
#'   - `std_error`: Standard error
#'   - `statistic`: t-statistic
#'   - `p_value`: p-value
#'
#' @examples
#' data <- data.frame(
#'   nominal_conc = c(0, 10, 25, 50, 100),
#'   response = c(0.5, 15.2, 35.8, 72.1, 148.3)
#' )
#' cal <- measure_calibration_fit(data, response ~ nominal_conc)
#' tidy(cal)
#'
#' @importFrom generics tidy
#' @export
tidy.measure_calibration <- function(x, ...) {
  coefs <- summary(x$model)$coefficients

  tibble::tibble(
    term = rownames(coefs),
    estimate = coefs[, "Estimate"],
    std_error = coefs[, "Std. Error"],
    statistic = coefs[, "t value"],
    p_value = coefs[, "Pr(>|t|)"]
  )
}

#' Glance at Calibration Curve Summary
#'
#' Extract one-row summary statistics from a calibration curve.
#'
#' @param x A [measure_calibration] object.
#' @param ... Additional arguments (unused).
#'
#' @return A tibble with columns:
#'   - `r_squared`: Coefficient of determination
#'   - `adj_r_squared`: Adjusted R-squared
#'   - `sigma`: Residual standard error
#'   - `df`: Degrees of freedom
#'   - `model_type`: Model type (linear/quadratic)
#'   - `weights_type`: Weighting scheme
#'   - `n_points`: Number of calibration points
#'   - `n_outliers`: Number of flagged outliers
#'
#' @examples
#' data <- data.frame(
#'   nominal_conc = c(0, 10, 25, 50, 100),
#'   response = c(0.5, 15.2, 35.8, 72.1, 148.3)
#' )
#' cal <- measure_calibration_fit(data, response ~ nominal_conc)
#' glance(cal)
#'
#' @importFrom generics glance
#' @export
glance.measure_calibration <- function(x, ...) {
  mod_summary <- summary(x$model)

  tibble::tibble(
    r_squared = mod_summary$r.squared,
    adj_r_squared = mod_summary$adj.r.squared,
    sigma = mod_summary$sigma,
    df = mod_summary$df[2],
    model_type = x$model_type,
    weights_type = x$weights_type,
    n_points = nrow(x$data),
    n_outliers = if (is.null(x$outliers)) 0L else nrow(x$outliers)
  )
}

#' Plot Calibration Curve Diagnostics
#'
#' Creates diagnostic plots for a calibration curve using ggplot2.
#'
#' @param object A [measure_calibration] object.
#' @param type Type of plot:
#'   - `"curve"` (default): Calibration curve with data points
#'   - `"residuals"`: Residuals vs concentration
#'   - `"qq"`: Normal Q-Q plot of residuals
#'   - `"all"`: All diagnostic plots combined
#' @param ... Additional arguments passed to ggplot2 functions.
#'
#' @return A ggplot object.
#'
#' @examples
#' library(ggplot2)
#' data <- data.frame(
#'   nominal_conc = c(0, 10, 25, 50, 100),
#'   response = c(0.5, 15.2, 35.8, 72.1, 148.3)
#' )
#' cal <- measure_calibration_fit(data, response ~ nominal_conc)
#' autoplot(cal)
#' autoplot(cal, type = "residuals")
#'
#' @importFrom ggplot2 autoplot
#' @export
autoplot.measure_calibration <- function(
  object,
  type = c("curve", "residuals", "qq", "all"),
  ...
) {
  type <- match.arg(type)

  if (type == "all") {
    p1 <- .calibration_plot_curve(object)
    p2 <- .calibration_plot_residuals(object)
    p3 <- .calibration_plot_qq(object)

    # Return list of plots if patchwork not available
    if (requireNamespace("patchwork", quietly = TRUE)) {
      return(patchwork::wrap_plots(p1, p2, p3, ncol = 2))
    } else {
      return(list(curve = p1, residuals = p2, qq = p3))
    }
  }

  switch(
    type,
    curve = .calibration_plot_curve(object),
    residuals = .calibration_plot_residuals(object),
    qq = .calibration_plot_qq(object)
  )
}

# Internal plot helpers
.calibration_plot_curve <- function(object) {
  data <- object$data
  conc_col <- object$conc_col
  response_col <- object$response_col

  # Create prediction line
  conc_range <- range(data[[conc_col]], na.rm = TRUE)
  pred_data <- data.frame(
    x = seq(conc_range[1], conc_range[2], length.out = 100)
  )
  names(pred_data) <- conc_col
  pred_data$predicted <- stats::predict(object$model, newdata = pred_data)

  p <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = .data[[conc_col]], y = .data[[response_col]])
  ) +
    ggplot2::geom_point(size = 2.5, alpha = 0.7) +
    ggplot2::geom_line(
      data = pred_data,
      ggplot2::aes(x = .data[[conc_col]], y = .data$predicted),
      linewidth = 0.8,
      color = "#2166AC"
    ) +
    ggplot2::labs(
      title = "Calibration Curve",
      subtitle = sprintf(
        "%s (R\u00b2 = %s)",
        object$model_type,
        format(object$diagnostics$r_squared, digits = 4)
      ),
      x = "Concentration",
      y = "Response"
    ) +
    ggplot2::theme_minimal()

  # Mark outliers if present
  if (!is.null(object$outliers) && nrow(object$outliers) > 0) {
    outlier_data <- object$outliers
    p <- p +
      ggplot2::geom_point(
        data = outlier_data,
        ggplot2::aes(x = .data[[conc_col]], y = .data[[response_col]]),
        color = "red",
        shape = 1,
        size = 4,
        stroke = 1.2
      )
  }

  p
}

.calibration_plot_residuals <- function(object) {
  data <- object$data
  conc_col <- object$conc_col
  data$.resid <- stats::residuals(object$model)

  ggplot2::ggplot(data, ggplot2::aes(x = .data[[conc_col]], y = .data$.resid)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    ggplot2::geom_point(size = 2.5, alpha = 0.7) +
    ggplot2::labs(
      title = "Residuals vs Concentration",
      x = "Concentration",
      y = "Residual"
    ) +
    ggplot2::theme_minimal()
}

.calibration_plot_qq <- function(object) {
  resid_std <- stats::rstandard(object$model)

  qq_data <- tibble::tibble(
    theoretical = stats::qqnorm(resid_std, plot.it = FALSE)$x,
    sample = stats::qqnorm(resid_std, plot.it = FALSE)$y
  )

  ggplot2::ggplot(
    qq_data,
    ggplot2::aes(x = .data$theoretical, y = .data$sample)
  ) +
    ggplot2::geom_abline(
      slope = 1,
      intercept = 0,
      linetype = "dashed",
      color = "gray50"
    ) +
    ggplot2::geom_point(size = 2.5, alpha = 0.7) +
    ggplot2::labs(
      title = "Normal Q-Q Plot",
      x = "Theoretical Quantiles",
      y = "Standardized Residuals"
    ) +
    ggplot2::theme_minimal()
}

#' Extract Calibration Curve Data
#'
#' S3 method to extract the underlying data from a calibration object
#' in a format suitable for ggplot2.
#'
#' @param model A [measure_calibration] object.
#' @param data Ignored.
#' @param ... Additional arguments (unused).
#'
#' @return A data frame with the calibration data and fitted values/residuals.
#'
#' @importFrom ggplot2 fortify
#' @export
fortify.measure_calibration <- function(model, data = NULL, ...) {
  cal_data <- model$data
  cal_data$.fitted <- stats::fitted(model$model)
  cal_data$.resid <- stats::residuals(model$model)
  cal_data$.std_resid <- stats::rstandard(model$model)

  # Add outlier flag
  if (!is.null(model$outliers) && nrow(model$outliers) > 0) {
    cal_data$.outlier <- FALSE
    # Mark outliers (would need row matching logic)
  }

  cal_data
}

#' Augment Calibration Data
#'
#' Add fitted values and residuals to calibration data.
#'
#' @param x A [measure_calibration] object.
#' @param ... Additional arguments (unused).
#'
#' @return A tibble with the original calibration data plus:
#'   - `.fitted`: Fitted values
#'   - `.resid`: Residuals
#'   - `.std_resid`: Standardized residuals
#'   - `.hat`: Leverage values
#'   - `.cooksd`: Cook's distance
#'
#' @importFrom generics augment
#' @export
augment.measure_calibration <- function(x, ...) {
  data <- tibble::as_tibble(x$data)
  data$.fitted <- stats::fitted(x$model)
  data$.resid <- stats::residuals(x$model)
  data$.std_resid <- stats::rstandard(x$model)
  data$.hat <- stats::hatvalues(x$model)
  data$.cooksd <- stats::cooks.distance(x$model)

  data
}
