# ==============================================================================
# Calibration Curve Fitting and Prediction
#
# This file provides functions for fitting calibration curves and
# predicting concentrations from responses.
# ==============================================================================

#' Fit a Calibration Curve
#'
#' Fits a weighted or unweighted calibration curve for quantitation.
#' Supports linear and quadratic models with various weighting schemes.
#'
#' @param data A data frame containing calibration data.
#' @param formula A formula specifying the model. The left-hand side should
#'   be the response variable, and the right-hand side should be the
#'   concentration variable (e.g., `response ~ nominal_conc`).
#' @param model Model type: `"linear"` (default) or `"quadratic"`.
#' @param weights Weighting scheme:
#'   - `"none"` (default): Unweighted regression
#'   - `"1/x"`: Weight by 1/concentration
#'   - `"1/x2"`: Weight by 1/concentration^2
#'   - `"1/y"`: Weight by 1/response
#'   - `"1/y2"`: Weight by 1/response^2
#'   - A numeric vector of custom weights (must match data rows)
#' @param origin Logical. If TRUE, force the curve through the origin (zero
#'   intercept). Default is FALSE.
#' @param outlier_method Method for flagging outliers:
#'   - `"none"` (default): No outlier detection
#'   - `"studentized"`: Flag points with |studentized residual| > `outlier_threshold`
#'   - `"cook"`: Flag points with Cook's distance > `outlier_threshold`
#' @param outlier_threshold Threshold for outlier detection. Default is 2.5
#'   for studentized residuals or 1 for Cook's distance.
#' @param outlier_action What to do with outliers:
#'   - `"flag"` (default): Flag but include in fit
#'   - `"remove"`: Remove from fit (with audit trail)
#' @param sample_type_col Optional column name for sample type. If provided,
#'   only rows with `sample_type == "standard"` are used for fitting.
#' @param revision Optional non-empty character string identifying the
#'   calibration revision for batch provenance.
#' @param conc_unit Optional character string declaring the concentration unit
#'   (e.g., `"mg/L"`).
#'
#' @return A [measure_calibration] object containing the fitted model,
#'   diagnostics, and metadata.
#'
#' @details
#' ## Weighting
#' Weighting is essential when response variance changes with concentration
#' (heteroscedasticity). Common patterns:
#' - Constant CV: Use `"1/x2"` or `"1/y2"`
#' - Constant absolute error: Use `"none"`
#' - Proportional error: Use `"1/x"` or `"1/y"`
#'
#' ## Outlier Handling
#' By default, outliers are flagged but NOT removed. This follows the
#' principle of "flag, don't drop" for analytical data. If removal is
#' enabled, the removed points are stored in the result for audit purposes.
#'
#' @seealso [measure_calibration_predict()] for prediction,
#'   [autoplot.measure_calibration()] for diagnostic plots,
#'   [tidy.measure_calibration()] for extracting coefficients.
#'
#' @examples
#' # Simple linear calibration
#' data <- data.frame(
#'   nominal_conc = c(0, 10, 25, 50, 100, 200),
#'   response = c(0.5, 15.2, 35.8, 72.1, 148.3, 295.7)
#' )
#' cal <- measure_calibration_fit(data, response ~ nominal_conc)
#' print(cal)
#'
#' # Weighted calibration (1/x^2)
#' cal_weighted <- measure_calibration_fit(
#'   data,
#'   response ~ nominal_conc,
#'   weights = "1/x2"
#' )
#'
#' # Quadratic model
#' cal_quad <- measure_calibration_fit(
#'   data,
#'   response ~ nominal_conc,
#'   model = "quadratic"
#' )
#'
#' @export
measure_calibration_fit <- function(
  data,
  formula,
  model = c("linear", "quadratic"),
  weights = c("none", "1/x", "1/x2", "1/y", "1/y2"),
  origin = FALSE,
  outlier_method = c("none", "studentized", "cook"),
  outlier_threshold = NULL,
  outlier_action = c("flag", "remove"),
  sample_type_col = NULL,
  revision = NULL,
  conc_unit = NULL
) {
  call <- match.call()
  model <- rlang::arg_match(model)
  outlier_method <- rlang::arg_match(outlier_method)
  outlier_action <- rlang::arg_match(outlier_action)

  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame.")
  }

  if (
    !is.null(revision) &&
      (!is.character(revision) ||
        length(revision) != 1L ||
        is.na(revision) ||
        !nzchar(trimws(revision)))
  ) {
    cli::cli_abort("{.arg revision} must be a single non-empty string or NULL.")
  }

  if (
    !is.null(conc_unit) &&
      (!is.character(conc_unit) ||
        length(conc_unit) != 1L ||
        is.na(conc_unit) ||
        !nzchar(trimws(conc_unit)))
  ) {
    cli::cli_abort(
      "{.arg conc_unit} must be a single non-empty string or NULL."
    )
  }

  # Handle weights argument
  if (is.numeric(weights)) {
    if (length(weights) != nrow(data)) {
      cli::cli_abort(
        "Custom weights must have length equal to number of data rows ({nrow(data)})."
      )
    }
    weight_values <- weights
    weights_type <- "custom"
  } else {
    weights <- rlang::arg_match(weights)
    weights_type <- weights
    weight_values <- NULL
  }

  # Parse formula
  formula_terms <- all.vars(formula)
  if (length(formula_terms) != 2) {
    cli::cli_abort(
      "Formula must have exactly two variables (response ~ concentration)."
    )
  }
  response_col <- formula_terms[1]
  conc_col <- formula_terms[2]

  # Validate columns exist
  if (!response_col %in% names(data)) {
    cli::cli_abort("Response column {.field {response_col}} not found in data.")
  }
  if (!conc_col %in% names(data)) {
    cli::cli_abort(
      "Concentration column {.field {conc_col}} not found in data."
    )
  }

  # Track active rows so custom weights stay aligned through filtering
  keep_rows <- rep(TRUE, nrow(data))

  # Filter to standards if sample_type_col provided
  if (!is.null(sample_type_col)) {
    if (!sample_type_col %in% names(data)) {
      cli::cli_abort(
        "Sample type column {.field {sample_type_col}} not found in data."
      )
    }
    sample_types <- data[[sample_type_col]]
    keep_rows <- !is.na(sample_types) & sample_types == "standard"
    if (!any(keep_rows)) {
      cli::cli_abort("No standard samples found in data.")
    }
  }

  # Remove rows with NA in response or concentration
  complete_idx <- stats::complete.cases(data[, c(response_col, conc_col)])
  keep_rows <- keep_rows & complete_idx
  fit_data <- data[keep_rows, , drop = FALSE]

  if (nrow(fit_data) < 3) {
    cli::cli_abort("At least 3 non-NA calibration points are required.")
  }

  # Calculate or subset weights
  if (is.null(weight_values)) {
    weight_values <- .calculate_weights(
      fit_data[[conc_col]],
      fit_data[[response_col]],
      weights_type
    )
  } else {
    weight_values <- weight_values[keep_rows]
  }

  # Build formula based on model type and origin
  fit_formula <- .build_calibration_formula(
    response_col,
    conc_col,
    model,
    origin
  )

  # Fit the model
  if (weights_type == "none") {
    fit <- stats::lm(fit_formula, data = fit_data)
  } else {
    # Add weights to data frame for lm() to find them
    fit_data$.weights <- weight_values
    fit <- stats::lm(fit_formula, data = fit_data, weights = .weights)
  }

  # Calculate diagnostics
  diagnostics <- .calculate_calibration_diagnostics(
    fit,
    fit_data,
    conc_col,
    response_col
  )

  # Outlier detection
  outliers <- NULL
  if (outlier_method != "none") {
    outliers <- .detect_calibration_outliers(
      fit,
      fit_data,
      conc_col,
      response_col,
      outlier_method,
      outlier_threshold
    )

    # Refit without outliers if requested
    if (
      outlier_action == "remove" && !is.null(outliers) && nrow(outliers) > 0
    ) {
      outlier_rows <- outliers$.row
      fit_data_clean <- fit_data[-outlier_rows, , drop = FALSE]

      if (nrow(fit_data_clean) < 3) {
        cli::cli_warn(
          "Too few points remain after outlier removal. Keeping all points."
        )
      } else {
        if (weights_type == "none") {
          fit <- stats::lm(fit_formula, data = fit_data_clean)
        } else {
          # Weights are already in the .weights column from earlier
          fit <- stats::lm(
            fit_formula,
            data = fit_data_clean,
            weights = .weights
          )
        }
        fit_data <- fit_data_clean
        diagnostics <- .calculate_calibration_diagnostics(
          fit,
          fit_data,
          conc_col,
          response_col
        )
      }
    }
  }

  new_measure_calibration(
    model = fit,
    model_type = model,
    weights_type = weights_type,
    formula = formula,
    data = fit_data,
    response_col = response_col,
    conc_col = conc_col,
    diagnostics = diagnostics,
    outliers = outliers,
    call = call,
    revision = revision,
    conc_unit = conc_unit,
    conc_range = range(fit_data[[conc_col]], na.rm = TRUE)
  )
}

#' Predict Concentrations from Calibration Curve
#'
#' Uses a fitted calibration curve to predict concentrations from responses.
#'
#' @param object A [measure_calibration] object from [measure_calibration_fit()].
#' @param newdata A data frame containing the response values to predict from.
#'   Must contain a column with the same name as the response variable in the
#'   calibration formula.
#' @param interval Type of interval to calculate:
#'   - `"none"` (default): Point estimates only
#'   - `"confidence"`: Confidence intervals
#'   - `"prediction"`: Prediction intervals
#' @param level Confidence level for intervals (default 0.95).
#' @param flag_range Logical. If `TRUE`, adds a `.range_status` column
#'   classifying each predicted concentration as `"in_range"`,
#'   `"below_range"`, `"above_range"`, or `"missing"` relative to `[lloq, uloq]`.
#'   Default is `FALSE`.
#' @param lloq Optional lower limit of quantitation for range flagging.
#'   Defaults to the minimum calibration standard concentration.
#' @param uloq Optional upper limit of quantitation for range flagging.
#'   Defaults to the maximum calibration standard concentration.
#' @param ... Additional arguments (unused).
#'
#' @return A tibble with columns:
#'   - `.pred_conc`: Predicted concentration
#'   - `.pred_lower`: Lower bound (if intervals requested)
#'   - `.pred_upper`: Upper bound (if intervals requested)
#'   - `.range_status`: Range status (if `flag_range = TRUE`)
#'
#' @details
#' For inverse prediction (response -> concentration), the function uses
#' root-finding when the model is quadratic. For linear models, direct
#' algebraic inversion is used.
#'
#' ## Interval Calculation
#' Intervals are calculated using the delta method for the inverse prediction.
#' For quadratic models, intervals are approximate.
#'
#' @seealso [measure_calibration_fit()] for fitting calibration curves.
#'
#' @examples
#' # Fit calibration curve
#' cal_data <- data.frame(
#'   nominal_conc = c(0, 10, 25, 50, 100),
#'   response = c(0.5, 15.2, 35.8, 72.1, 148.3)
#' )
#' cal <- measure_calibration_fit(cal_data, response ~ nominal_conc)
#'
#' # Predict concentrations from new responses
#' unknowns <- data.frame(response = c(45, 85, 120))
#' measure_calibration_predict(cal, unknowns)
#'
#' # With prediction intervals
#' measure_calibration_predict(cal, unknowns, interval = "prediction")
#'
#' @export
measure_calibration_predict <- function(
  object,
  newdata,
  interval = c("none", "confidence", "prediction"),
  level = 0.95,
  flag_range = FALSE,
  lloq = NULL,
  uloq = NULL,
  ...
) {
  if (!is_measure_calibration(object)) {
    cli::cli_abort("{.arg object} must be a {.cls measure_calibration} object.")
  }

  interval <- rlang::arg_match(interval)

  response_col <- object$response_col
  conc_col <- object$conc_col

  if (!response_col %in% names(newdata)) {
    cli::cli_abort(
      "Response column {.field {response_col}} not found in newdata."
    )
  }

  responses <- newdata[[response_col]]

  # Get model coefficients
  coefs <- stats::coef(object$model)

  # Predict concentrations (inverse prediction)
  if (object$model_type == "linear") {
    pred_conc <- .inverse_predict_linear(responses, coefs, object)
  } else {
    pred_conc <- .inverse_predict_quadratic(responses, coefs, object)
  }

  result <- tibble::tibble(.pred_conc = as.numeric(pred_conc))

  # Calculate intervals if requested
  if (interval != "none") {
    interval_result <- .calculate_prediction_intervals(
      object,
      responses,
      pred_conc,
      interval,
      level
    )
    result$.pred_lower <- as.numeric(interval_result$lower)
    result$.pred_upper <- as.numeric(interval_result$upper)
  }

  if (isTRUE(flag_range)) {
    cal_range <- object$conc_range %||%
      range(object$data[[conc_col]], na.rm = TRUE)
    lower_bound <- lloq %||% cal_range[1]
    upper_bound <- uloq %||% cal_range[2]
    range_status <- rep("in_range", length(pred_conc))
    missing_idx <- is.na(pred_conc) | !is.finite(pred_conc)
    range_status[missing_idx] <- "missing"
    range_status[!missing_idx & pred_conc < lower_bound] <- "below_range"
    range_status[!missing_idx & pred_conc > upper_bound] <- "above_range"
    result$.range_status <- range_status
  }

  result
}

# ==============================================================================
# Internal helper functions
# ==============================================================================

.calculate_weights <- function(x, y, weights_type) {
  switch(
    weights_type,
    "none" = NULL,
    "1/x" = {
      w <- 1 / x
      w[!is.finite(w)] <- max(w[is.finite(w)], na.rm = TRUE) * 10
      w
    },
    "1/x2" = {
      w <- 1 / (x^2)
      w[!is.finite(w)] <- max(w[is.finite(w)], na.rm = TRUE) * 10
      w
    },
    "1/y" = {
      w <- 1 / abs(y)
      w[!is.finite(w)] <- max(w[is.finite(w)], na.rm = TRUE) * 10
      w
    },
    "1/y2" = {
      w <- 1 / (y^2)
      w[!is.finite(w)] <- max(w[is.finite(w)], na.rm = TRUE) * 10
      w
    },
    NULL
  )
}

.build_calibration_formula <- function(response_col, conc_col, model, origin) {
  if (model == "linear") {
    if (origin) {
      stats::as.formula(paste(response_col, "~ 0 +", conc_col))
    } else {
      stats::as.formula(paste(response_col, "~", conc_col))
    }
  } else {
    # Quadratic
    if (origin) {
      stats::as.formula(paste(
        response_col,
        "~ 0 +",
        conc_col,
        "+ I(",
        conc_col,
        "^2)"
      ))
    } else {
      stats::as.formula(paste(
        response_col,
        "~",
        conc_col,
        "+ I(",
        conc_col,
        "^2)"
      ))
    }
  }
}

.calculate_calibration_diagnostics <- function(
  fit,
  data,
  conc_col,
  response_col
) {
  mod_summary <- summary(fit)

  # Basic statistics
  r_squared <- mod_summary$r.squared
  adj_r_squared <- mod_summary$adj.r.squared
  sigma <- mod_summary$sigma

  # Back-calculated accuracy
  fitted_resp <- stats::fitted(fit)
  observed_resp <- data[[response_col]]
  accuracy <- 100 * (fitted_resp - observed_resp) / observed_resp
  accuracy[!is.finite(accuracy)] <- NA

  # Calculate %RE at each level
  residual_error <- 100 * abs(fitted_resp - observed_resp) / observed_resp
  residual_error[!is.finite(residual_error)] <- NA

  finite_re <- residual_error[is.finite(residual_error)]
  finite_acc <- accuracy[is.finite(accuracy)]

  list(
    r_squared = r_squared,
    adj_r_squared = adj_r_squared,
    sigma = sigma,
    mean_accuracy = if (length(finite_acc) > 0L) mean(finite_acc) else NA_real_,
    max_re = if (length(finite_re) > 0L) max(finite_re) else NA_real_,
    mean_re = if (length(finite_re) > 0L) mean(finite_re) else NA_real_
  )
}

.detect_calibration_outliers <- function(
  fit,
  data,
  conc_col,
  response_col,
  method,
  threshold
) {
  if (method == "studentized") {
    threshold <- threshold %||% 2.5
    resid_std <- stats::rstudent(fit)
    outlier_idx <- which(abs(resid_std) > threshold)
    outlier_metric <- resid_std
    metric_name <- "studentized_resid"
  } else if (method == "cook") {
    threshold <- threshold %||% 1
    cooksd <- stats::cooks.distance(fit)
    outlier_idx <- which(cooksd > threshold)
    outlier_metric <- cooksd
    metric_name <- "cooks_distance"
  } else {
    return(NULL)
  }

  if (length(outlier_idx) == 0) {
    return(NULL)
  }

  outlier_data <- data[outlier_idx, c(conc_col, response_col), drop = FALSE]
  outlier_data$.row <- outlier_idx
  outlier_data[[metric_name]] <- outlier_metric[outlier_idx]
  outlier_data$.threshold <- threshold
  outlier_data$.method <- method

  tibble::as_tibble(outlier_data)
}

.inverse_predict_linear <- function(responses, coefs, object) {
  # y = a + bx  =>  x = (y - a) / b
  intercept <- if ("(Intercept)" %in% names(coefs)) coefs["(Intercept)"] else 0
  slope <- coefs[object$conc_col]

  if (is.na(slope) || !is.finite(slope) || abs(slope) < .Machine$double.eps) {
    cli::cli_warn("Slope is essentially zero; cannot calculate concentrations.")
    return(rep(NA_real_, length(responses)))
  }

  unname((responses - intercept) / slope)
}

.inverse_predict_quadratic <- function(responses, coefs, object) {
  # y = a + bx + cx^2  =>  solve for x

  intercept <- if ("(Intercept)" %in% names(coefs)) coefs["(Intercept)"] else 0
  linear_term <- coefs[object$conc_col]
  quad_name <- paste0("I(", object$conc_col, "^2)")
  quad_term <- coefs[quad_name]

  if (
    is.na(linear_term) ||
      !is.finite(linear_term) ||
      is.na(quad_term) ||
      !is.finite(quad_term)
  ) {
    return(rep(NA_real_, length(responses)))
  }

  if (abs(quad_term) < .Machine$double.eps) {
    return(.inverse_predict_linear(responses, coefs, object))
  }

  # For each response, solve: cx^2 + bx + (a - y) = 0
  vapply(
    responses,
    function(y) {
      if (is.na(y) || !is.finite(y)) {
        return(NA_real_)
      }
      a_coef <- quad_term
      b_coef <- linear_term
      c_coef <- intercept - y

      discriminant <- b_coef^2 - 4 * a_coef * c_coef

      if (is.na(discriminant) || discriminant < 0) {
        return(NA_real_)
      }

      # Take positive root (or root closest to calibration range)
      roots <- (-b_coef + c(-1, 1) * sqrt(discriminant)) / (2 * a_coef)
      roots <- roots[is.finite(roots) & roots >= 0]

      if (length(roots) == 0L) {
        return(NA_real_)
      }

      min(roots)
    },
    numeric(1)
  )
}

.calculate_prediction_intervals <- function(
  object,
  responses,
  pred_conc,
  interval_type,
  level
) {
  fit <- object$model
  sigma <- summary(fit)$sigma
  n <- nrow(object$data)
  df <- summary(fit)$df[2]
  t_crit <- stats::qt((1 + level) / 2, df)

  if (object$model_type == "linear") {
    # Classical inverse prediction intervals (ISO 11843 / Draper & Smith)
    # Confidence: SE = (sigma/|b|) * sqrt(1/n + (x0 - xbar)^2 / Sxx)
    # Prediction: SE = (sigma/|b|) * sqrt(1 + 1/n + (x0 - xbar)^2 / Sxx)
    coefs <- stats::coef(fit)
    slope <- coefs[object$conc_col]
    cal_conc <- object$data[[object$conc_col]]
    x_bar <- mean(cal_conc)
    sxx <- sum((cal_conc - x_bar)^2)

    extra <- if (interval_type == "prediction") 1 else 0
    se_pred <- (sigma / abs(slope)) *
      sqrt(extra + 1 / n + (pred_conc - x_bar)^2 / sxx)
  } else {
    # Quadratic models: use delta method approximation
    coefs <- stats::coef(fit)
    slope <- coefs[object$conc_col]
    se_pred <- sigma / abs(slope)
    if (interval_type == "prediction") {
      se_pred <- se_pred * sqrt(1 + 1 / n)
    }
  }

  list(
    lower = pred_conc - t_crit * se_pred,
    upper = pred_conc + t_crit * se_pred
  )
}

# ==============================================================================
# Calibration Verification
# ==============================================================================

#' Verify Calibration Curve Performance
#'
#' Evaluates the performance of a calibration curve using verification samples
#' (continuing calibration verification - CCV, or independent QC samples).
#' This function assesses whether the calibration remains valid during or
#' between analytical runs.
#'
#' @param calibration A [measure_calibration] object from
#'   [measure_calibration_fit()].
#' @param verification_data A data frame containing verification samples with
#'   known concentrations.
#' @param nominal_col Name of the column containing nominal (known)
#'   concentrations. Default is `"nominal_conc"`.
#' @param acceptance_pct Acceptance criterion as percent deviation from nominal.
#'   Default is 15 (i.e., ±15%).
#' @param acceptance_pct_lloq Acceptance criterion for samples at the lower
#'   limit of quantitation (LLOQ). Default is 20 (i.e., ±20%).
#' @param lloq Lower limit of quantitation. Samples at or near this level use
#'   `acceptance_pct_lloq`. Default is NULL (use same criterion for all).
#' @param sample_type_col Optional column indicating sample types. Only samples
#'   with type containing "qc" or "ccv" will be used if specified.
#' @param criteria Optional [measure_criteria] object for custom acceptance
#'   criteria. If provided, overrides `acceptance_pct` settings.
#'
#' @return A `measure_calibration_verify` object (a tibble) containing:
#'   - Predicted concentrations
#'   - Accuracy (%nominal)
#'   - Deviation from nominal (%)
#'   - Pass/fail status for each sample
#'   - Overall verification status
#'
#' @details
#' ## Verification Workflow
#'
#' Calibration verification is typically performed:
#' 1. At the beginning and end of analytical batches
#' 2. After every N unknown samples (e.g., every 10)
#' 3. When instrument performance is in question
#'
#' ## Acceptance Criteria
#'
#' Default criteria are based on bioanalytical guidelines:
#' - Standard samples: ±15% of nominal
#' - LLOQ samples: ±20% of nominal
#'
#' For more stringent applications (e.g., clinical chemistry), consider
#' using ±10% or providing custom criteria.
#'
#' @seealso [measure_calibration_fit()] for fitting calibration curves,
#'   [measure_calibration_predict()] for prediction,
#'   [measure_criteria()] for custom acceptance criteria.
#'
#' @examples
#' # Fit calibration
#' cal_data <- data.frame(
#'   nominal_conc = c(1, 5, 10, 50, 100, 500),
#'   response = c(1.2, 5.8, 11.3, 52.1, 105.2, 498.7)
#' )
#' cal <- measure_calibration_fit(cal_data, response ~ nominal_conc)
#'
#' # Verify with QC samples
#' qc_data <- data.frame(
#'   sample_id = c("QC_Low", "QC_Mid", "QC_High"),
#'   nominal_conc = c(3, 75, 400),
#'   response = c(3.3, 77.2, 385.1)
#' )
#'
#' verify_result <- measure_calibration_verify(cal, qc_data)
#' print(verify_result)
#'
#' @export
measure_calibration_verify <- function(
  calibration,
  verification_data,
  nominal_col = "nominal_conc",
  acceptance_pct = 15,
  acceptance_pct_lloq = 20,
  lloq = NULL,
  sample_type_col = NULL,
  criteria = NULL
) {
  if (!is_measure_calibration(calibration)) {
    cli::cli_abort(
      "{.arg calibration} must be a {.cls measure_calibration} object."
    )
  }

  if (!is.data.frame(verification_data)) {
    cli::cli_abort("{.arg verification_data} must be a data frame.")
  }

  # Check response column exists
  response_col <- calibration$response_col
  if (!response_col %in% names(verification_data)) {
    cli::cli_abort(
      "Response column {.field {response_col}} not found in verification_data."
    )
  }

  # Check nominal column exists
  if (!nominal_col %in% names(verification_data)) {
    cli::cli_abort(
      "Nominal concentration column {.field {nominal_col}} not found in verification_data."
    )
  }

  # Filter to verification samples if sample_type_col provided
  verify_data <- verification_data
  if (!is.null(sample_type_col)) {
    if (!sample_type_col %in% names(verification_data)) {
      cli::cli_abort(
        "Sample type column {.field {sample_type_col}} not found in data."
      )
    }
    sample_types <- tolower(verification_data[[sample_type_col]])
    is_verify <- grepl("qc|ccv|verification|standard", sample_types)
    verify_data <- verification_data[is_verify, , drop = FALSE]

    if (nrow(verify_data) == 0) {
      cli::cli_abort("No verification samples found in data.")
    }
  }

  # Get nominal and observed values
  nominal_values <- verify_data[[nominal_col]]
  response_values <- verify_data[[response_col]]

  # Predict concentrations using calibration
  predictions <- measure_calibration_predict(
    calibration,
    newdata = verify_data
  )

  predicted_conc <- predictions$.pred_conc

  # Calculate accuracy metrics
  accuracy_pct <- (predicted_conc / nominal_values) * 100
  deviation_pct <- ((predicted_conc - nominal_values) / nominal_values) * 100

  # Determine acceptance limits for each sample
  if (!is.null(lloq)) {
    # Use wider limits for samples near LLOQ
    is_lloq <- nominal_values <= lloq * 1.5 # 50% margin for LLOQ region
    acceptance_limits <- ifelse(is_lloq, acceptance_pct_lloq, acceptance_pct)
  } else {
    acceptance_limits <- rep(acceptance_pct, length(nominal_values))
  }

  # Apply criteria
  if (!is.null(criteria)) {
    # Use custom criteria if provided
    pass <- sapply(seq_along(accuracy_pct), function(i) {
      acc <- accuracy_pct[i]
      # Assume criteria has accuracy bounds
      acc >= (100 - acceptance_limits[i]) & acc <= (100 + acceptance_limits[i])
    })
  } else {
    # Use default percentage-based criteria
    pass <- abs(deviation_pct) <= acceptance_limits
  }

  # Build result
  result <- tibble::tibble(
    nominal_conc = nominal_values,
    response = response_values,
    predicted_conc = predicted_conc,
    accuracy_pct = accuracy_pct,
    deviation_pct = deviation_pct,
    acceptance_limit = acceptance_limits,
    pass = pass
  )

  # Add sample ID if available
  if ("sample_id" %in% names(verify_data)) {
    result <- tibble::add_column(
      result,
      sample_id = verify_data$sample_id,
      .before = 1
    )
  }

  # Calculate summary statistics: missing/NA verification samples must fail
  # overall verification rather than silently passing.
  n_total <- nrow(result)
  n_pass <- sum(pass %in% TRUE)
  n_fail <- n_total - n_pass
  overall_pass <- n_total > 0L && n_fail == 0L

  # Add attributes
  attr(result, "n_total") <- n_total

  attr(result, "n_pass") <- n_pass
  attr(result, "n_fail") <- n_fail
  attr(result, "overall_pass") <- overall_pass
  attr(result, "acceptance_pct") <- acceptance_pct
  attr(result, "acceptance_pct_lloq") <- acceptance_pct_lloq
  attr(result, "lloq") <- lloq
  attr(result, "calibration") <- calibration

  class(result) <- c("measure_calibration_verify", class(result))
  result
}

#' @export
print.measure_calibration_verify <- function(x, ...) {
  n_total <- attr(x, "n_total")
  n_pass <- attr(x, "n_pass")
  n_fail <- attr(x, "n_fail")
  overall_pass <- attr(x, "overall_pass")
  acceptance_pct <- attr(x, "acceptance_pct")

  cli::cli_h1("Calibration Verification")

  if (overall_pass) {
    cli::cli_alert_success(
      "Overall: PASS ({n_pass}/{n_total} samples within {acceptance_pct}%)"
    )
  } else {
    cli::cli_alert_danger(
      "Overall: FAIL ({n_fail}/{n_total} samples out of specification)"
    )
  }

  cli::cli_h2("Sample Results")
  print(tibble::as_tibble(x), n = 20)

  invisible(x)
}

#' @rdname tidy.measure_calibration
#' @export
tidy.measure_calibration_verify <- function(x, ...) {
  finite_acc <- x$accuracy_pct[is.finite(x$accuracy_pct)]
  finite_dev <- abs(x$deviation_pct[is.finite(x$deviation_pct)])
  tibble::tibble(
    n_samples = attr(x, "n_total"),
    n_pass = attr(x, "n_pass"),
    n_fail = attr(x, "n_fail"),
    pass_rate = if (attr(x, "n_total") > 0L) {
      attr(x, "n_pass") / attr(x, "n_total") * 100
    } else {
      NA_real_
    },
    overall_pass = attr(x, "overall_pass"),
    mean_accuracy_pct = if (length(finite_acc) > 0L) {
      mean(finite_acc)
    } else {
      NA_real_
    },
    mean_abs_deviation = if (length(finite_dev) > 0L) {
      mean(finite_dev)
    } else {
      NA_real_
    },
    max_abs_deviation = if (length(finite_dev) > 0L) {
      max(finite_dev)
    } else {
      NA_real_
    }
  )
}
