# A constrained R worker for the separations MCP prototype. JSON is exchanged
# over stdin/stdout; neither file paths nor R expressions come from the caller.
suppressPackageStartupMessages(library(jsonlite))

input <- fromJSON(
  paste(readLines(file("stdin"), warn = FALSE), collapse = "\n"),
  simplifyVector = FALSE
)
warning_state <- new.env(parent = emptyenv())
warning_state$seen <- character()

package_versions <- function() {
  names <- c("measure", "measure.sec", "recipes")
  stats::setNames(
    lapply(names, function(name) {
      if (requireNamespace(name, quietly = TRUE)) {
        as.character(utils::packageVersion(name))
      } else {
        NULL
      }
    }),
    names
  )
}

# A package version alone does not distinguish an unreleased development
# checkout from another build with the same DESCRIPTION version.
package_code_fingerprints <- function() {
  names <- c("measure", "measure.sec")
  stats::setNames(
    lapply(names, function(name) {
      if (!requireNamespace(name, quietly = TRUE)) {
        return(NULL)
      }
      code_file <- file.path(
        system.file(package = name),
        "R",
        paste0(name, ".rdb")
      )
      if (!file.exists(code_file)) {
        return(NULL)
      }
      unname(as.character(tools::md5sum(code_file)))
    }),
    names
  )
}

as_num <- function(x) as.numeric(unlist(x, use.names = FALSE))

analyze <- function(input) {
  if (identical(input$operation, "capabilities")) {
    return(list(
      r_version = as.character(getRversion()),
      packages = package_versions(),
      installed_r_code_md5 = package_code_fingerprints(),
      sec_calibrated_curve_support = requireNamespace(
        "measure.sec",
        quietly = TRUE
      ) &&
        all(
          c("mw_col", "mw_scale") %in%
            names(formals(measure.sec::step_sec_mw_averages))
        ) &&
        all(
          c("mw_col", "mw_scale", "integration_range") %in%
            names(formals(measure.sec::step_sec_mw_distribution))
        )
    ))
  }
  if (
    !requireNamespace("measure", quietly = TRUE) ||
      !requireNamespace("recipes", quietly = TRUE)
  ) {
    stop("The measure and recipes R packages are required.")
  }

  if (identical(input$operation, "quantitation")) {
    method <- input$method
    standards <- data.frame(
      concentration = vapply(
        method$standards,
        function(s) as.numeric(s$concentration),
        numeric(1)
      ),
      response = vapply(
        method$standards,
        function(s) as.numeric(s$response),
        numeric(1)
      )
    )
    calibration <- measure::measure_calibration_fit(
      standards,
      response ~ concentration,
      model = "linear",
      weights = method$weighting,
      origin = method$force_origin
    )
    coefficients <- stats::coef(calibration$model)
    slope <- unname(coefficients["concentration"])
    if (!is.finite(slope) || slope <= 0) {
      stop("The fitted calibration slope must be positive and finite.")
    }
    predicted <- measure::measure_calibration_predict(
      calibration,
      data.frame(response = as.numeric(input$response))
    )$.pred_conc[[1]]
    if (!is.finite(predicted)) {
      stop("The calibration could not produce a finite concentration.")
    }
    limits <- range(standards$concentration)
    outside <- predicted < limits[1] || predicted > limits[2]
    if (outside) {
      warning(
        "Predicted concentration lies outside the supplied standard concentration range."
      )
    }
    intercept <- if ("(Intercept)" %in% names(coefficients)) {
      unname(coefficients["(Intercept)"])
    } else {
      0
    }
    return(list(
      kind = "peak_quantitation",
      summary = list(
        analyte = method$analyte,
        concentration = predicted,
        concentration_unit = method$concentration_unit,
        response = as.numeric(input$response),
        response_unit = method$response_unit,
        outside_standard_range = outside
      ),
      diagnostics = list(
        slope = slope,
        intercept = intercept,
        r_squared = calibration$diagnostics$r_squared,
        residual_standard_error = calibration$diagnostics$sigma,
        residual_degrees_of_freedom = calibration$model$df.residual,
        standard_range = limits,
        standard_results = data.frame(
          standards,
          fitted_response = as.numeric(stats::fitted(calibration$model))
        )
      ),
      notes = c(
        "The caller assigns the peak identity and declares that the standard and unknown response bases match.",
        "No internal-standard correction, dilution factor, detection limit, or uncertainty interval is applied."
      ),
      packages = package_versions(),
      installed_r_code_md5 = package_code_fingerprints(),
      r_version = as.character(getRversion())
    ))
  }

  x <- as_num(input$trace$axis)
  y <- as_num(input$trace$signal)
  dat <- tibble::tibble(trace = input$trace$trace_id)
  dat$signal <- measure::new_measure_list(list(measure::new_measure_tbl(x, y)))
  rec <- recipes::recipe(dat)

  if (identical(input$operation, "chromatogram")) {
    method <- input$method
    if (identical(method$baseline, "als")) {
      rec <- measure::step_measure_baseline_als(
        rec,
        measures = "signal",
        lambda = method$baseline_lambda,
        p = method$baseline_p
      )
    }
    if (identical(method$smoothing, "gaussian")) {
      rec <- measure::step_measure_smooth_gaussian(
        rec,
        measures = "signal",
        sigma = method$smoothing_sigma
      )
    }
    rec <- measure::step_measure_peaks_detect(
      rec,
      measures = "signal",
      algorithm = "prominence",
      min_height = method$min_height,
      min_prominence = method$min_prominence,
      min_distance = method$min_distance
    )
    rec <- measure::step_measure_peaks_integrate(
      rec,
      measures = "signal",
      method = "trapezoid",
      baseline = method$integration_baseline
    )
    out <- recipes::bake(recipes::prep(rec), new_data = NULL)
    peaks <- as.data.frame(out$.peaks[[1]])
    return(list(
      kind = "chromatogram",
      peak_count = nrow(peaks),
      peaks = peaks,
      processed_trace = list(
        axis = out$signal[[1]]$location,
        signal = out$signal[[1]]$value
      ),
      notes = c(
        "Peak areas use the specified baseline and trapezoidal integration; negative integrand values are clipped to zero.",
        "Peak area is detector response, not concentration or compound identity."
      ),
      packages = package_versions(),
      installed_r_code_md5 = package_code_fingerprints(),
      r_version = as.character(getRversion())
    ))
  }

  if (identical(input$operation, "sec")) {
    if (!requireNamespace("measure.sec", quietly = TRUE)) {
      stop("The measure.sec R package is required for SEC analysis.")
    }
    if (
      !all(
        c("mw_col", "mw_scale") %in%
          names(formals(measure.sec::step_sec_mw_averages))
      ) ||
        !all(
          c("mw_col", "mw_scale", "integration_range") %in%
            names(formals(measure.sec::step_sec_mw_distribution))
        )
    ) {
      stop(
        "The installed measure.sec lacks calibrated-curve support; install the corrected development revision."
      )
    }
    method <- input$method
    standards <- data.frame(
      location = vapply(
        method$standards,
        function(s) as.numeric(s$location),
        numeric(1)
      ),
      mw = vapply(
        method$standards,
        function(s) as.numeric(s$molecular_weight_g_mol),
        numeric(1)
      )
    )
    rec <- measure.sec::step_sec_conventional_cal(
      rec,
      measures = "signal",
      standards = standards,
      fit_type = method$fit_type,
      extrapolation = method$extrapolation,
      output_col = "calibrated_mw"
    )
    limits <- if (is.null(method$integration_range)) {
      NULL
    } else {
      as_num(method$integration_range)
    }
    selected <- if (is.null(limits)) {
      rep(TRUE, length(x))
    } else {
      x >= limits[1] & x <= limits[2]
    }
    if (sum(selected & y > 0) < 2) {
      stop(
        "The integration range must contain at least two positive signal points."
      )
    }
    rec <- measure.sec::step_sec_mw_averages(
      rec,
      measures = "signal",
      integration_range = limits
    )
    rec <- measure.sec::step_sec_mw_distribution(
      rec,
      measures = "signal",
      type = "both",
      n_points = 101L,
      integration_range = limits
    )
    trained <- recipes::prep(rec)
    out <- recipes::bake(trained, new_data = NULL)
    diag <- trained$steps[[1]]$calibration_diagnostics
    if (diag$degrees_of_freedom <= 0) {
      warning(
        "Calibration has no residual degrees of freedom; fit uncertainty cannot be estimated from these standards."
      )
    }
    differential <- out$signal_differential[[1]]
    cumulative <- out$signal_cumulative[[1]]
    return(list(
      kind = "sec",
      summary = list(
        mn_g_mol = out$mw_mn[[1]],
        mw_g_mol = out$mw_mw[[1]],
        mz_g_mol = out$mw_mz[[1]],
        mp_g_mol = out$mw_mp[[1]],
        dispersity = out$mw_dispersity[[1]]
      ),
      diagnostics = list(
        calibration_range = trained$steps[[1]]$calibration_range,
        r_squared = diag$r_squared,
        rmse_log_mw = diag$rmse_log_mw,
        max_abs_pct_deviation = diag$max_abs_pct_deviation,
        residual_degrees_of_freedom = diag$degrees_of_freedom,
        standard_results = as.data.frame(diag$standard_results)
      ),
      distribution = list(
        log10_mw = differential$location,
        differential = differential$value,
        cumulative = cumulative$value
      ),
      notes = c(
        "Relative SEC calibration depends on the standards and their polymer/solvent applicability.",
        "The supplied signal is assumed proportional to weight concentration and already baseline-corrected."
      ),
      packages = package_versions(),
      installed_r_code_md5 = package_code_fingerprints(),
      r_version = as.character(getRversion())
    ))
  }
  stop("Unsupported analysis operation.")
}

result <- tryCatch(
  withCallingHandlers(
    list(ok = TRUE, result = analyze(input)),
    warning = function(w) {
      warning_state$seen <- c(warning_state$seen, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  ),
  error = function(e) list(ok = FALSE, error = conditionMessage(e))
)
result$warnings <- unique(warning_state$seen)
cat(
  toJSON(result, auto_unbox = TRUE, null = "null", na = "null", digits = 16),
  "\n"
)
