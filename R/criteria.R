# ==============================================================================
# Criteria and Assessment System
#
# This file provides a unified mechanism for encoding acceptance criteria
# and producing pass/fail assessments across all analytical functions.
# ==============================================================================

# ------------------------------------------------------------------------------
# Criterion class: Single acceptance rule
# ------------------------------------------------------------------------------

#' Create an Acceptance Criterion
#'
#' Defines a single acceptance criterion for analytical validation. Criteria
#' are used with [measure_assess()] to produce pass/fail decisions.
#'
#' @param name Character string naming this criterion (e.g., "cv_qc", "r_squared").
#' @param operator Comparison operator: `"<"`, `"<="`, `">"`, `">="`, `"=="`,
#'   `"!="`, `"between"`, or `"outside"`.
#' @param threshold Numeric threshold value. For `"between"` and `"outside"`,
#'   provide a length-2 vector `c(lower, upper)`.
#' @param description Optional human-readable description of the criterion.
#' @param priority Optional priority level: `"critical"`, `"major"`, `"minor"`.
#'   Affects how failures are reported.
#'
#' @return A `measure_criterion` object.
#'
#' @seealso [measure_criteria()] for combining multiple criteria,
#'   [measure_assess()] for evaluating criteria.
#'
#' @examples
#' # QC coefficient of variation must be < 15%
#' criterion("cv_qc", "<", 15, description = "QC CV < 15%")
#'
#' # R-squared must be >= 0.99
#' criterion("r_squared", ">=", 0.99)
#'
#' # Recovery must be between 80% and 120%
#' criterion("recovery", "between", c(80, 120), priority = "critical")
#'
#' @export
criterion <- function(
    name,
    operator = c("<", "<=", ">", ">=", "==", "!=", "between", "outside"),
    threshold,
    description = NULL,
    priority = c("major", "critical", "minor")) {

  operator <- match.arg(operator)
  priority <- match.arg(priority)

  if (!is.character(name) || length(name) != 1) {
    cli::cli_abort("{.arg name} must be a single character string.")
  }

  if (!is.numeric(threshold)) {
    cli::cli_abort("{.arg threshold} must be numeric.")
  }

  if (operator %in% c("between", "outside")) {
    if (length(threshold) != 2) {
      cli::cli_abort(
        "Operator {.val {operator}} requires a length-2 threshold vector c(lower, upper)."
      )
    }
    if (threshold[1] >= threshold[2]) {
      cli::cli_abort(
        "For {.val {operator}}, threshold[1] must be less than threshold[2]."
      )
    }
  } else {
    if (length(threshold) != 1) {
      cli::cli_abort(
        "Operator {.val {operator}} requires a single threshold value."
      )
    }
  }

  structure(
    list(
      name = name,
      operator = operator,
      threshold = threshold,
      description = description %||% .generate_description(name, operator, threshold),
      priority = priority
    ),
    class = "measure_criterion"
  )
}

#' @export
print.measure_criterion <- function(x, ...) {
  cat("<measure_criterion>\n")
  cat("  ", x$name, " ", x$operator, " ", format(x$threshold), sep = "")
  if (x$operator == "between") {
    cat(" [", x$threshold[1], ", ", x$threshold[2], "]", sep = "")
  } else if (x$operator == "outside") {
    cat(" (outside [", x$threshold[1], ", ", x$threshold[2], "])", sep = "")
  }
  cat("\n")
  cat("  Priority: ", x$priority, "\n", sep = "")
  if (!is.null(x$description)) {
    cat("  Description: ", x$description, "\n", sep = "")
  }
  invisible(x)
}

# Generate default description from operator and threshold
.generate_description <- function(name, operator, threshold) {
  if (operator == "between") {
    sprintf("%s in [%s, %s]", name, threshold[1], threshold[2])
  } else if (operator == "outside") {
    sprintf("%s outside [%s, %s]", name, threshold[1], threshold[2])
  } else {
    sprintf("%s %s %s", name, operator, threshold)
  }
}

# ------------------------------------------------------------------------------
# Criteria set: Collection of criteria
# ------------------------------------------------------------------------------

#' Create a Set of Acceptance Criteria
#'
#' Combines multiple [criterion()] objects into a criteria set for use
#' with [measure_assess()].
#'
#' @param ... [criterion()] objects or named arguments that will be
#'   converted to criteria. Named arguments use the format
#'   `name = list(operator, threshold)` or `name = threshold` (assumes `"<="`).
#' @param .list Optional list of criterion objects.
#'
#' @return A `measure_criteria` object (list of `measure_criterion` objects).
#'
#' @seealso [criterion()] for creating individual criteria,
#'   [measure_assess()] for evaluating criteria.
#'
#' @examples
#' # Using criterion() objects
#' measure_criteria(
#'   criterion("cv_qc", "<", 15),
#'   criterion("r_squared", ">=", 0.99),
#'   criterion("recovery", "between", c(80, 120))
#' )
#'
#' # Using shorthand notation
#' measure_criteria(
#'   cv_qc = list("<", 15),
#'   r_squared = list(">=", 0.99),
#'   bias = list("between", c(-10, 10))
#' )
#'
#' # Simple threshold (assumes "<=")
#' measure_criteria(
#'   cv = 15,       # cv <= 15
#'   rsd = 20       # rsd <= 20
#' )
#'
#' @export
measure_criteria <- function(..., .list = NULL) {
  dots <- list(...)

  # Process each argument
  criteria <- lapply(seq_along(dots), function(i) {
    x <- dots[[i]]
    nm <- names(dots)[i]

    # Already a criterion object

if (inherits(x, "measure_criterion")) {
      return(x)
    }

    # Named argument with shorthand
    if (!is.null(nm) && nzchar(nm)) {
      if (is.numeric(x) && length(x) == 1) {
        # Simple threshold: assume "<="
        return(criterion(nm, "<=", x))
      } else if (is.list(x) && length(x) >= 2) {
        # List format: list(operator, threshold, ...)
        return(criterion(
          name = nm,
          operator = x[[1]],
          threshold = x[[2]],
          description = x$description,
          priority = x$priority %||% "major"
        ))
      }
    }

    cli::cli_abort(
      "Argument {i} must be a {.cls measure_criterion} object or a named shorthand."
    )
  })

  # Add from .list
  if (!is.null(.list)) {
    for (x in .list) {
      if (!inherits(x, "measure_criterion")) {
        cli::cli_abort("All elements of {.arg .list} must be criterion objects.")
      }
      criteria <- c(criteria, list(x))
    }
  }

  # Extract names
  names(criteria) <- vapply(criteria, function(c) c$name, character(1))

  structure(criteria, class = "measure_criteria")
}

#' @export
print.measure_criteria <- function(x, ...) {
  n <- length(x)
  cat("<measure_criteria> with", n, if (n == 1) "criterion\n" else "criteria\n")

  if (n > 0) {
    for (i in seq_along(x)) {
      crit <- x[[i]]
      symbol <- switch(
        crit$priority,
        critical = cli::col_red("\u2022"),
        major = cli::col_yellow("\u2022"),
        minor = cli::col_blue("\u2022")
      )
      cat("  ", symbol, " ", crit$description, "\n", sep = "")
    }
  }

  invisible(x)
}

#' @export
`[.measure_criteria` <- function(x, i, ...) {
  out <- NextMethod()
  structure(out, class = "measure_criteria")
}

#' @export
c.measure_criteria <- function(...) {
  all_criteria <- unlist(lapply(list(...), unclass), recursive = FALSE)
  structure(all_criteria, class = "measure_criteria")
}

# ------------------------------------------------------------------------------
# Assessment: Evaluate criteria against data
# ------------------------------------------------------------------------------

#' Assess Data Against Acceptance Criteria
#'
#' Evaluates a set of values against acceptance criteria and returns a
#' detailed assessment table with pass/fail status.
#'
#' @param data A named list or data frame containing the values to assess.
#'   Names must match criterion names.
#' @param criteria A [measure_criteria()] object defining the acceptance criteria.
#' @param action What to do on failure: `"return"` (default) returns the
#'   assessment table, `"warn"` issues a warning for failures,
#'   `"error"` stops on any critical failures.
#'
#' @return A tibble with class `measure_assessment` containing:
#'   - `criterion`: Name of the criterion
#'   - `value`: The observed value
#'   - `threshold`: The threshold value(s)
#'   - `operator`: The comparison operator
#'   - `pass`: Logical indicating pass/fail
#'   - `priority`: Priority level of the criterion
#'   - `description`: Human-readable description
#'
#' @seealso [measure_criteria()] for creating criteria,
#'   [criterion()] for individual criteria.
#'
#' @examples
#' # Define criteria
#' crit <- measure_criteria(
#'   cv_qc = list("<", 15),
#'   r_squared = list(">=", 0.99),
#'   recovery = list("between", c(80, 120))
#' )
#'
#' # Assess results
#' results <- list(cv_qc = 12.5, r_squared = 0.995, recovery = 98.2)
#' measure_assess(results, crit)
#'
#' # Assess with some failures
#' results_bad <- list(cv_qc = 18.3, r_squared = 0.985, recovery = 75)
#' measure_assess(results_bad, crit)
#'
#' @export
measure_assess <- function(
    data,
    criteria,
    action = c("return", "warn", "error")) {

  action <- match.arg(action)

  if (!inherits(criteria, "measure_criteria")) {
    cli::cli_abort("{.arg criteria} must be a {.cls measure_criteria} object.")
  }

  # Convert data frame to list if needed
  if (is.data.frame(data)) {
    data <- as.list(data)
  }

  if (!is.list(data)) {
    cli::cli_abort("{.arg data} must be a named list or data frame.")
  }

  # Evaluate each criterion
  results <- lapply(criteria, function(crit) {
    name <- crit$name

    # Check if value exists
    if (!name %in% names(data)) {
      return(tibble::tibble(
        criterion = name,
        value = NA_real_,
        threshold = list(crit$threshold),
        operator = crit$operator,
        pass = NA,
        priority = crit$priority,
        description = crit$description,
        message = "Value not found in data"
      ))
    }

    value <- data[[name]]

    # Handle NA values
    if (is.na(value)) {
      return(tibble::tibble(
        criterion = name,
        value = NA_real_,
        threshold = list(crit$threshold),
        operator = crit$operator,
        pass = NA,
        priority = crit$priority,
        description = crit$description,
        message = "Value is NA"
      ))
    }

    # Evaluate the criterion
    pass <- .evaluate_criterion(value, crit$operator, crit$threshold)

    tibble::tibble(
      criterion = name,
      value = value,
      threshold = list(crit$threshold),
      operator = crit$operator,
      pass = pass,
      priority = crit$priority,
      description = crit$description,
      message = if (pass) "PASS" else "FAIL"
    )
  })

  assessment <- dplyr::bind_rows(results)
  class(assessment) <- c("measure_assessment", class(assessment))

  # Handle failures based on action
  failures <- assessment[!is.na(assessment$pass) & !assessment$pass, ]

  if (nrow(failures) > 0) {
    failure_msgs <- sprintf(
      "%s: %s = %s (expected %s %s)",
      failures$criterion,
      failures$criterion,
      format(failures$value),
      failures$operator,
      vapply(failures$threshold, function(t) {
        if (length(t) == 2) paste0("[", t[1], ", ", t[2], "]") else as.character(t)
      }, character(1))
    )

    if (action == "error") {
      critical_failures <- failures[failures$priority == "critical", ]
      if (nrow(critical_failures) > 0) {
        cli::cli_abort(c(
          "Critical acceptance criteria failed:",
          stats::setNames(failure_msgs[failures$priority == "critical"], rep("x", sum(failures$priority == "critical")))
        ))
      }
    }

    if (action == "warn") {
      cli::cli_warn(c(
        "Acceptance criteria failures:",
        stats::setNames(failure_msgs, rep("!", length(failure_msgs)))
      ))
    }
  }

  assessment
}

# Evaluate a single criterion
.evaluate_criterion <- function(value, operator, threshold) {
  switch(
    operator,
    "<" = value < threshold,
    "<=" = value <= threshold,
    ">" = value > threshold,
    ">=" = value >= threshold,
    "==" = value == threshold,
    "!=" = value != threshold,
    "between" = value >= threshold[1] && value <= threshold[2],
    "outside" = value < threshold[1] || value > threshold[2],
    NA
  )
}

#' @export
print.measure_assessment <- function(x, ...) {
  n_pass <- sum(x$pass, na.rm = TRUE)
  n_fail <- sum(!x$pass, na.rm = TRUE)
  n_na <- sum(is.na(x$pass))

  overall <- if (n_fail == 0 && n_na == 0) {
    cli::col_green("PASS")
  } else if (n_fail > 0) {
    cli::col_red("FAIL")
  } else {
    cli::col_yellow("INCOMPLETE")
  }

  cat("<measure_assessment> [", overall, "]\n", sep = "")
  cat("  ", n_pass, " passed, ", n_fail, " failed", sep = "")
  if (n_na > 0) cat(", ", n_na, " not evaluated", sep = "")
  cat("\n\n")

  # Print details
  for (i in seq_len(nrow(x))) {
    row <- x[i, ]
    status_symbol <- if (is.na(row$pass)) {
      cli::col_yellow("?")
    } else if (row$pass) {
      cli::col_green("\u2713")
    } else {
      cli::col_red("\u2717")
    }

    thresh_str <- if (length(row$threshold[[1]]) == 2) {
      paste0("[", row$threshold[[1]][1], ", ", row$threshold[[1]][2], "]")
    } else {
      as.character(row$threshold[[1]])
    }

    cat(
      "  ", status_symbol, " ", row$criterion,
      ": ", format(row$value, digits = 4),
      " (", row$operator, " ", thresh_str, ")\n",
      sep = ""
    )
  }

  invisible(x)
}

#' Check if All Criteria Pass
#'
#' A convenience function to check if all criteria in an assessment passed.
#'
#' @param assessment A `measure_assessment` object from [measure_assess()].
#' @param na_pass Logical. Should NA results count as pass? Default is FALSE.
#'
#' @return Logical: TRUE if all criteria passed, FALSE otherwise.
#'
#' @examples
#' crit <- measure_criteria(cv = 15, rsd = 20)
#' results <- list(cv = 10, rsd = 15)
#' assessment <- measure_assess(results, crit)
#' all_pass(assessment)
#'
#' @export
all_pass <- function(assessment, na_pass = FALSE) {

  if (!inherits(assessment, "measure_assessment")) {
    cli::cli_abort("{.arg assessment} must be a {.cls measure_assessment} object.")
  }

  if (na_pass) {
    all(assessment$pass | is.na(assessment$pass))
  } else {
    # NA counts as failure - replace NA with FALSE before checking
    pass_values <- assessment$pass
    pass_values[is.na(pass_values)] <- FALSE
    all(pass_values)
  }
}

#' Extract Failed Criteria
#'
#' Returns only the criteria that failed assessment.
#'
#' @param assessment A `measure_assessment` object from [measure_assess()].
#'
#' @return A filtered `measure_assessment` tibble containing only failures.
#'
#' @examples
#' crit <- measure_criteria(cv = 15, rsd = 20)
#' results <- list(cv = 18, rsd = 25)  # Both fail
#' assessment <- measure_assess(results, crit)
#' get_failures(assessment)
#'
#' @export
get_failures <- function(assessment) {
  if (!inherits(assessment, "measure_assessment")) {
    cli::cli_abort("{.arg assessment} must be a {.cls measure_assessment} object.")
  }

  out <- assessment[!is.na(assessment$pass) & !assessment$pass, ]
  class(out) <- class(assessment)
  out
}

# ------------------------------------------------------------------------------
# Preset criteria for common use cases
# ------------------------------------------------------------------------------

#' Preset Acceptance Criteria
#'
#' Factory functions that return commonly-used criteria sets for analytical
#' validation workflows.
#'
#' @param cv_qc Maximum allowable CV for QC samples (default 15%, bioanalytical).
#' @param cv_calibration Maximum allowable CV for calibration replicates (default 20%).
#' @param cv_repeatability Maximum allowable CV for repeatability (default 2%, ICH Q2).
#' @param cv_intermediate Maximum allowable CV for intermediate precision (default 5%, ICH Q2).
#' @param r_squared Minimum R-squared for calibration curve.
#' @param recovery_range Acceptable recovery range as c(lower, upper).
#' @param accuracy_bias Maximum allowable bias (default 15%).
#'
#' @return A [measure_criteria] object.
#'
#' @examples
#' # Default bioanalytical criteria
#' criteria_bioanalytical()
#'
#' # Custom thresholds
#' criteria_bioanalytical(cv_qc = 20, r_squared = 0.98)
#'
#' @name criteria_presets
NULL

#' @rdname criteria_presets
#' @export
criteria_bioanalytical <- function(
    cv_qc = 15,
    cv_calibration = 20,
    r_squared = 0.99,
    recovery_range = c(80, 120),
    accuracy_bias = 15) {

  measure_criteria(
    criterion("cv_qc", "<=", cv_qc,
      description = sprintf("QC CV <= %s%%", cv_qc),
      priority = "critical"
    ),
    criterion("cv_calibration", "<=", cv_calibration,
      description = sprintf("Calibration CV <= %s%%", cv_calibration),
      priority = "major"
    ),
    criterion("r_squared", ">=", r_squared,
      description = sprintf("R\u00b2 >= %s", r_squared),
      priority = "critical"
    ),
    criterion("recovery", "between", recovery_range,
      description = sprintf("Recovery %s-%s%%", recovery_range[1], recovery_range[2]),
      priority = "critical"
    ),
    criterion("accuracy_bias", "between", c(-accuracy_bias, accuracy_bias),
      description = sprintf("Bias within +/-%s%%", accuracy_bias),
      priority = "major"
    )
  )
}

#' @rdname criteria_presets
#' @export
criteria_ich_q2 <- function(
    cv_repeatability = 2,
    cv_intermediate = 5,
    recovery_range = c(98, 102),
    r_squared = 0.999) {

  measure_criteria(
    criterion("cv_repeatability", "<=", cv_repeatability,
      description = sprintf("Repeatability RSD <= %s%%", cv_repeatability),
      priority = "critical"
    ),
    criterion("cv_intermediate", "<=", cv_intermediate,
      description = sprintf("Intermediate precision RSD <= %s%%", cv_intermediate),
      priority = "major"
    ),
    criterion("recovery", "between", recovery_range,
      description = sprintf("Recovery %s-%s%%", recovery_range[1], recovery_range[2]),
      priority = "critical"
    ),
    criterion("r_squared", ">=", r_squared,
      description = sprintf("R\u00b2 >= %s", r_squared),
      priority = "critical"
    )
  )
}

#' @rdname criteria_presets
#' @param loa_width Maximum acceptable limits of agreement width.
#' @param bias_max Maximum acceptable mean bias.
#' @param proportional_bias_p Significance level for proportional bias test.
#' @export
criteria_bland_altman <- function(
    loa_width = NULL,
    bias_max = NULL,
    proportional_bias_p = 0.05) {

  criteria_list <- list()

  if (!is.null(loa_width)) {
    criteria_list <- c(criteria_list, list(
      criterion("loa_width", "<=", loa_width,
        description = sprintf("LOA width <= %s", loa_width),
        priority = "major"
      )
    ))
  }

  if (!is.null(bias_max)) {
    criteria_list <- c(criteria_list, list(
      criterion("mean_bias", "between", c(-bias_max, bias_max),
        description = sprintf("Mean bias within +/-%s", bias_max),
        priority = "critical"
      )
    ))
  }

  criteria_list <- c(criteria_list, list(
    criterion("proportional_bias_p", ">=", proportional_bias_p,
      description = sprintf("Proportional bias p >= %s (not significant)", proportional_bias_p),
      priority = "major"
    )
  ))

  measure_criteria(.list = criteria_list)
}

#' @rdname criteria_presets
#' @param slope_range Acceptable range for regression slope (default c(0.9, 1.1)).
#' @param intercept_range Acceptable range for regression intercept.
#' @export
criteria_method_comparison <- function(
    slope_range = c(0.9, 1.1),
    intercept_range = NULL,
    r_squared = 0.95) {

  criteria_list <- list(
    criterion("slope", "between", slope_range,
      description = sprintf("Slope in [%s, %s]", slope_range[1], slope_range[2]),
      priority = "critical"
    ),
    criterion("r_squared", ">=", r_squared,
      description = sprintf("R\u00b2 >= %s", r_squared),
      priority = "major"
    )
  )

  if (!is.null(intercept_range)) {
    criteria_list <- c(criteria_list, list(
      criterion("intercept", "between", intercept_range,
        description = sprintf("Intercept in [%s, %s]", intercept_range[1], intercept_range[2]),
        priority = "major"
      )
    ))
  }

  measure_criteria(.list = criteria_list)
}

#' @rdname criteria_presets
#' @param max_z_score Maximum acceptable absolute z-score.
#' @param pct_satisfactory Minimum percentage of satisfactory results.
#' @export
criteria_proficiency_testing <- function(
    max_z_score = 2,
    pct_satisfactory = 100) {

  measure_criteria(
    criterion("max_abs_score", "<=", max_z_score,
      description = sprintf("Max |z-score| <= %s", max_z_score),
      priority = "critical"
    ),
    criterion("pct_satisfactory", ">=", pct_satisfactory,
      description = sprintf("Satisfactory results >= %s%%", pct_satisfactory),
      priority = "major"
    )
  )
}

#' @rdname criteria_presets
#' @param me_range Acceptable matrix effect range (default c(80, 120)).
#' @param me_cv Maximum acceptable CV of matrix effects.
#' @export
criteria_matrix_effects <- function(
    me_range = c(80, 120),
    me_cv = 15) {

  measure_criteria(
    criterion("mean_me_pct", "between", me_range,
      description = sprintf("Mean ME in [%s%%, %s%%]", me_range[1], me_range[2]),
      priority = "critical"
    ),
    criterion("cv_me_pct", "<=", me_cv,
      description = sprintf("CV of ME <= %s%%", me_cv),
      priority = "major"
    ),
    criterion("min_me_pct", ">=", me_range[1],
      description = sprintf("Min ME >= %s%%", me_range[1]),
      priority = "major"
    ),
    criterion("max_me_pct", "<=", me_range[2],
      description = sprintf("Max ME <= %s%%", me_range[2]),
      priority = "major"
    )
  )
}

#' @rdname criteria_presets
#' @param surrogate_recovery Acceptable surrogate recovery range.
#' @export
criteria_surrogate_recovery <- function(
    surrogate_recovery = c(70, 130)) {

  measure_criteria(
    criterion("min_recovery", ">=", surrogate_recovery[1],
      description = sprintf("Min recovery >= %s%%", surrogate_recovery[1]),
      priority = "major"
    ),
    criterion("max_recovery", "<=", surrogate_recovery[2],
      description = sprintf("Max recovery <= %s%%", surrogate_recovery[2]),
      priority = "major"
    )
  )
}
