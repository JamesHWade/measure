# ==============================================================================
# Scalar Record Curve Extraction & External-Calibration Batch Quantitation
#
# Bridges validated scalar measurement records (`measure_record`) to
# peak-integrated external calibration workflows with explicit provenance,
# QC verification, and distinct outcomes for missing, interfered, and
# out-of-range injections.
# ==============================================================================

#' Extract a provenance-linked curve table from a measurement record
#'
#' `measure_record_curves()` joins the relational tables of a [measure_record()]
#' into a recipe-ready tibble while retaining sample, injection, channel, curve,
#' source-digest, and parsed source-row provenance. Curves are returned in their
#' original order without sorting, resampling, or unit conversion.
#'
#' @param record A [measure_record()] object.
#' @param channel_id Optional single character string restricting output to one
#'   declared channel in `record$channels`.
#'
#' @return A tibble with one row per curve and columns `sample_id`,
#'   `injection_id`, `curve_id`, `channel_id`, `channel_name`, `source_id`,
#'   `source_name`, `source_sha256`, `axis_name`, `axis_unit`, `value_name`,
#'   `value_unit`, `n_points`, `source_row_start`, `source_row_end`,
#'   `.measures`, and `.source_rows`.
#'
#' @seealso [measure_record()], [read_measure_record_delim()],
#'   [measure_quantify_batch()]
#'
#' @examples
#' curve <- new_measure_tbl(c(1, 2, 3), c(0.1, 0.5, 0.2))
#' rec <- measure_record(
#'   samples = tibble::tibble(sample_id = "S1"),
#'   injections = tibble::tibble(injection_id = "INJ-1", sample_id = "S1"),
#'   channels = tibble::tibble(channel_id = "uv254", channel_name = "UV 254 nm"),
#'   curves = tibble::tibble(
#'     curve_id = "C1",
#'     injection_id = "INJ-1",
#'     channel_id = "uv254",
#'     .measures = list(curve)
#'   )
#' )
#' measure_record_curves(rec)
#'
#' @export
measure_record_curves <- function(record, channel_id = NULL) {
  validate_measure_record(record)

  if (!is.null(channel_id)) {
    if (
      !is.character(channel_id) ||
        length(channel_id) != 1L ||
        is.na(channel_id) ||
        !nzchar(trimws(channel_id))
    ) {
      cli::cli_abort(
        "{.arg channel_id} must be a single non-empty character string or NULL."
      )
    }
    if (!channel_id %in% record$channels$channel_id) {
      cli::cli_abort(
        "Channel {.val {channel_id}} was not found in {.field record$channels}."
      )
    }
  }

  curves <- record$curves
  if (!is.null(channel_id)) {
    curves <- curves[curves$channel_id == channel_id, , drop = FALSE]
  }

  inj_idx <- match(curves$injection_id, record$injections$injection_id)
  ch_idx <- match(curves$channel_id, record$channels$channel_id)
  src_idx <- match(curves$source_id, record$sources$source_id)

  n_points <- vapply(curves$.measures, nrow, integer(1))
  row_bounds <- lapply(curves$.source_rows, function(rows) {
    finite_rows <- rows[!is.na(rows)]
    if (length(finite_rows) == 0L) {
      c(NA_integer_, NA_integer_)
    } else {
      c(min(finite_rows), max(finite_rows))
    }
  })
  source_row_start <- vapply(row_bounds, `[[`, integer(1), 1L)
  source_row_end <- vapply(row_bounds, `[[`, integer(1), 2L)

  tibble::tibble(
    sample_id = record$injections$sample_id[inj_idx],
    injection_id = curves$injection_id,
    curve_id = curves$curve_id,
    channel_id = curves$channel_id,
    channel_name = record$channels$channel_name[ch_idx],
    source_id = curves$source_id,
    source_name = ifelse(
      is.na(src_idx),
      NA_character_,
      record$sources$source_name[src_idx]
    ),
    source_sha256 = ifelse(
      is.na(src_idx),
      NA_character_,
      record$sources$source_sha256[src_idx]
    ),
    axis_name = curves$axis_name,
    axis_unit = curves$axis_unit,
    value_name = curves$value_name,
    value_unit = curves$value_unit,
    n_points = n_points,
    source_row_start = source_row_start,
    source_row_end = source_row_end,
    .measures = curves$.measures,
    .source_rows = curves$.source_rows
  )
}

#' Quantify a scalar chromatography batch with external calibration
#'
#' `measure_quantify_batch()` runs a complete single-analyte, external
#' calibration workflow over a scalar [measure_record()]: peak detection and
#' integration via [step_measure_peaks_detect()] and
#' [step_measure_peaks_integrate()], calibration fitting via
#' [measure_calibration_fit()], QC verification via
#' [measure_calibration_verify()], dilution back-calculation, and explicit
#' per-injection outcome flagging.
#'
#' Every reported row links back to its `sample_id`, `injection_id`,
#' `curve_id`, `source_id`, `source_sha256`, parsed source-row bounds, selected
#' peak bounds, area, `calibration_revision`, estimated and reported
#' concentrations, and QC status. Missing peaks, multiple coeluting/interfering
#' peaks in the analyte window, blank interferences, and out-of-range unknowns
#' (`"below_lloq"` / `"above_uloq"`) are reported as distinct outcomes rather
#' than silently coerced to zero or extrapolated as valid.
#'
#' @param record A [measure_record()] containing one scalar chromatogram per
#'   injection on the target channel.
#' @param manifest A data frame describing the batch injections with required
#'   columns:
#'   - `injection_id`: Unique character IDs matching `record$injections`.
#'   - `injection_type`: One of `"standard"`, `"blank"`, `"qc"`, or `"unknown"`.
#'   - `nominal_conc`: Numeric nominal concentration (finite for `"standard"`
#'     and `"qc"`, `NA` or `0` for `"blank"`, `NA` for `"unknown"`).
#'   Optional columns:
#'   - `dilution_factor`: Positive finite numeric dilution factor (default `1`).
#'     Applied to back-calculate original sample concentration after checking
#'     in-vial concentration against `[lloq, uloq]`.
#'   - `batch_order`: Integer run order (default `seq_len(nrow(manifest))`).
#' @param window Length-2 finite numeric vector `c(min, max)` defining the
#'   target analyte retention-time window (`window[1] < window[2]`).
#' @param channel_id Optional channel ID when `record` has multiple channels.
#'   Required if `record$channels` has more than one row.
#' @param analyte Single non-empty string naming the target analyte.
#' @param conc_unit Optional single non-empty string declaring concentration
#'   units (e.g., `"mg/L"`).
#' @param calibration_revision Single non-empty string identifying the
#'   calibration revision recorded on each quantified injection.
#' @param peak_algorithm Peak detection algorithm passed to
#'   [step_measure_peaks_detect()]. Default `"prominence"`.
#' @param min_height Minimum peak height passed to [step_measure_peaks_detect()].
#' @param min_prominence Minimum peak prominence passed to
#'   [step_measure_peaks_detect()].
#' @param min_distance Minimum peak distance passed to
#'   [step_measure_peaks_detect()].
#' @param algorithm_params Optional list of algorithm-specific parameters passed
#'   to [step_measure_peaks_detect()].
#' @param integration_method Integration rule passed to
#'   [step_measure_peaks_integrate()]: `"trapezoid"` (default) or `"simpson"`.
#' @param baseline Peak baseline convention passed to
#'   [step_measure_peaks_integrate()]: `"local"` (default), `"none"`, or
#'   `"global"`.
#' @param model Calibration model passed to [measure_calibration_fit()]:
#'   `"linear"` (default) or `"quadratic"`.
#' @param weights Calibration weighting scheme passed to
#'   [measure_calibration_fit()].
#' @param origin Logical; force calibration curve through the origin? Default
#'   `FALSE`.
#' @param lloq Optional lower limit of quantitation (in-vial concentration).
#'   Defaults to the minimum positive fitted standard concentration.
#' @param uloq Optional upper limit of quantitation (in-vial concentration).
#'   Defaults to the maximum fitted standard concentration.
#' @param qc_acceptance_pct Maximum allowed absolute percent deviation from
#'   nominal for standards and QC samples. Default `15`.
#' @param blank_max_area Optional absolute peak area ceiling for blank
#'   injections. When `NULL` (default), blanks are checked against
#'   `blank_max_pct_lloq` percent of the fitted response at `lloq`.
#' @param blank_max_pct_lloq Percent of LLOQ response tolerated in a blank when
#'   `blank_max_area` is `NULL`. Default `20`.
#'
#' @return A `measure_batch_results` object (inheriting from
#'   [measure_results()]) with components:
#'   - `results`: Per-injection tibble linking provenance, peaks, areas,
#'     calibration revision, estimated/reported concentrations, and outcomes.
#'   - `calibration`: Fitted [measure_calibration] object.
#'   - `verification`: [measure_calibration_verify] object for QC injections (or
#'     `NULL` if no QC injections are present).
#'   - `method`: Named list of declared batch parameters and source digests.
#'   - `peaks`: All detected peaks across injections for diagnostics.
#'   - `data`, `measures`, `id_cols`: Inherited [measure_results()] fields for
#'     plotting and report rendering.
#'
#' @seealso [measure_record_curves()], [measure_calibration_fit()],
#'   [measure_calibration_verify()], [measure_results()],
#'   [render_measure_report()]
#'
#' @examples
#' path <- system.file(
#'   "extdata",
#'   "lc_batch_control.csv",
#'   package = "measure",
#'   mustWork = TRUE
#' )
#' manifest_path <- system.file(
#'   "extdata",
#'   "lc_batch_manifest.csv",
#'   package = "measure",
#'   mustWork = TRUE
#' )
#' cols <- c(
#'   sample_id = "sample_id",
#'   injection_id = "injection_id",
#'   channel_id = "channel_id",
#'   channel_name = "channel_name",
#'   curve_id = "curve_id",
#'   axis_name = "axis_name",
#'   axis_unit = "axis_unit",
#'   value_name = "value_name",
#'   value_unit = "value_unit",
#'   location = "location",
#'   value = "value"
#' )
#' rec <- read_measure_record_delim(path, cols, source_id = "lc-batch-control")
#' manifest <- utils::read.csv(manifest_path, stringsAsFactors = FALSE)
#' batch <- measure_quantify_batch(
#'   rec,
#'   manifest,
#'   window = c(4.8, 5.6),
#'   analyte = "caffeine_control",
#'   conc_unit = "mg/L",
#'   calibration_revision = "cal-control-v1",
#'   min_prominence = 1.0
#' )
#' summary(batch)
#'
#' @export
measure_quantify_batch <- function(
  record,
  manifest,
  window,
  channel_id = NULL,
  analyte = "analyte",
  conc_unit = NULL,
  calibration_revision = "cal-v1",
  peak_algorithm = "prominence",
  min_height = 0,
  min_prominence = 0,
  min_distance = 0,
  algorithm_params = list(),
  integration_method = c("trapezoid", "simpson"),
  baseline = c("local", "none", "global"),
  model = c("linear", "quadratic"),
  weights = c("none", "1/x", "1/x2", "1/y", "1/y2"),
  origin = FALSE,
  lloq = NULL,
  uloq = NULL,
  qc_acceptance_pct = 15,
  blank_max_area = NULL,
  blank_max_pct_lloq = 20
) {
  validate_measure_record(record)
  integration_method <- match.arg(integration_method)
  baseline <- match.arg(baseline)
  model <- match.arg(model)
  if (!is.numeric(weights)) {
    weights <- match.arg(weights)
  }

  .validate_batch_arguments(
    window = window,
    analyte = analyte,
    conc_unit = conc_unit,
    calibration_revision = calibration_revision,
    qc_acceptance_pct = qc_acceptance_pct,
    blank_max_area = blank_max_area,
    blank_max_pct_lloq = blank_max_pct_lloq,
    lloq = lloq,
    uloq = uloq
  )

  if (is.null(channel_id)) {
    if (nrow(record$channels) == 1L) {
      channel_id <- record$channels$channel_id[[1L]]
    } else {
      cli::cli_abort(
        "{.arg channel_id} must be supplied when {.arg record} contains {nrow(record$channels)} channels."
      )
    }
  }

  curves_tbl <- measure_record_curves(record, channel_id = channel_id)
  if (anyDuplicated(curves_tbl$injection_id)) {
    dup <- curves_tbl$injection_id[duplicated(curves_tbl$injection_id)][[1L]]
    cli::cli_abort(
      "Channel {.val {channel_id}} has multiple curves for injection {.val {dup}}."
    )
  }

  manifest_tbl <- .validate_batch_manifest(manifest, curves_tbl$injection_id)

  # Align curves to manifest run order so output rows mirror the batch sequence
  curve_match <- match(manifest_tbl$injection_id, curves_tbl$injection_id)
  batch_curves <- curves_tbl[curve_match, , drop = FALSE]
  batch_curves$batch_order <- manifest_tbl$batch_order
  batch_curves$injection_type <- manifest_tbl$injection_type
  batch_curves$nominal_conc <- manifest_tbl$nominal_conc
  batch_curves$dilution_factor <- manifest_tbl$dilution_factor

  # Run core peak detection + integration recipe on the scalar curves
  id_cols <- setdiff(names(batch_curves), ".measures")
  peak_input <- batch_curves[c(id_cols, ".measures")]
  peak_rec <- recipes::recipe(~., data = peak_input) |>
    recipes::update_role(dplyr::all_of(id_cols), new_role = "id") |>
    step_measure_peaks_detect(
      algorithm = peak_algorithm,
      min_height = min_height,
      min_distance = min_distance,
      min_prominence = min_prominence,
      algorithm_params = algorithm_params,
      measures = ".measures"
    ) |>
    step_measure_peaks_integrate(
      method = integration_method,
      baseline = baseline,
      measures = ".measures"
    ) |>
    recipes::prep(training = peak_input)

  baked <- recipes::bake(peak_rec, new_data = NULL)

  # Select target-window peak(s) per injection and map peak bounds to source rows
  n_inj <- nrow(baked)
  peak_count <- integer(n_inj)
  peak_status <- character(n_inj)
  peak_location <- rep(NA_real_, n_inj)
  peak_height <- rep(NA_real_, n_inj)
  left_base <- rep(NA_real_, n_inj)
  right_base <- rep(NA_real_, n_inj)
  area <- rep(NA_real_, n_inj)
  peak_source_row_start <- rep(NA_integer_, n_inj)
  peak_source_row_end <- rep(NA_integer_, n_inj)
  all_peaks_list <- vector("list", n_inj)

  for (i in seq_len(n_inj)) {
    pks <- baked$.peaks[[i]]
    if (nrow(pks) > 0L) {
      all_peaks_list[[i]] <- tibble::add_column(
        tibble::as_tibble(pks),
        injection_id = baked$injection_id[[i]],
        sample_id = baked$sample_id[[i]],
        .before = 1L
      )
    } else {
      all_peaks_list[[i]] <- tibble::tibble(
        injection_id = character(),
        sample_id = character(),
        peak_id = integer(),
        location = double(),
        height = double(),
        left_base = double(),
        right_base = double(),
        area = double()
      )
    }

    in_win <- pks[
      pks$location >= window[[1L]] & pks$location <= window[[2L]],
      ,
      drop = FALSE
    ]
    n_found <- nrow(in_win)
    peak_count[[i]] <- n_found

    if (n_found == 0L) {
      peak_status[[i]] <- "missing"
    } else if (n_found == 1L) {
      peak_status[[i]] <- "unique"
      peak_location[[i]] <- in_win$location[[1L]]
      peak_height[[i]] <- in_win$height[[1L]]
      left_base[[i]] <- in_win$left_base[[1L]]
      right_base[[i]] <- in_win$right_base[[1L]]
      area[[i]] <- in_win$area[[1L]]

      curve_tbl <- baked$.measures[[i]]
      src_rows <- baked$.source_rows[[i]]
      in_peak <- curve_tbl$location >= left_base[[i]] &
        curve_tbl$location <= right_base[[i]]
      peak_rows <- src_rows[in_peak]
      peak_rows <- peak_rows[!is.na(peak_rows)]
      if (length(peak_rows) > 0L) {
        peak_source_row_start[[i]] <- min(peak_rows)
        peak_source_row_end[[i]] <- max(peak_rows)
      }
    } else {
      peak_status[[i]] <- "interfered"
    }
  }

  # Fit calibration curve on standards with uniquely detected peaks
  std_fit_idx <- baked$injection_type == "standard" &
    peak_status == "unique" &
    is.finite(area) &
    is.finite(baked$nominal_conc)

  if (sum(std_fit_idx) < 3L) {
    cli::cli_abort(
      "At least 3 standard injections with uniquely integrated peaks are required to fit calibration (found {sum(std_fit_idx)})."
    )
  }

  cal_input <- tibble::tibble(
    injection_id = baked$injection_id[std_fit_idx],
    nominal_conc = baked$nominal_conc[std_fit_idx],
    area = area[std_fit_idx]
  )
  calibration <- measure_calibration_fit(
    cal_input,
    area ~ nominal_conc,
    model = model,
    weights = weights,
    origin = origin,
    revision = calibration_revision,
    conc_unit = conc_unit
  )

  pos_std <- cal_input$nominal_conc[cal_input$nominal_conc > 0]
  resolved_lloq <- lloq %||%
    if (length(pos_std) > 0L) min(pos_std) else min(cal_input$nominal_conc)
  resolved_uloq <- uloq %||% max(cal_input$nominal_conc)

  if (resolved_lloq >= resolved_uloq) {
    cli::cli_abort(
      "{.arg lloq} ({resolved_lloq}) must be strictly less than {.arg uloq} ({resolved_uloq})."
    )
  }

  # Determine blank area threshold
  if (!is.null(blank_max_area)) {
    resolved_blank_max_area <- blank_max_area
  } else {
    lloq_frame <- data.frame(nominal_conc = resolved_lloq)
    lloq_response <- as.numeric(stats::predict(
      calibration$model,
      newdata = lloq_frame
    ))
    resolved_blank_max_area <- (blank_max_pct_lloq / 100) * abs(lloq_response)
  }

  # Predict in-vial concentrations for all uniquely detected peaks
  pred_tbl <- measure_calibration_predict(
    calibration,
    newdata = tibble::tibble(area = area),
    flag_range = TRUE,
    lloq = resolved_lloq,
    uloq = resolved_uloq
  )
  raw_est_conc <- pred_tbl$.pred_conc

  estimated_conc <- rep(NA_real_, n_inj)
  reported_conc <- rep(NA_real_, n_inj)
  accuracy_pct <- rep(NA_real_, n_inj)
  deviation_pct <- rep(NA_real_, n_inj)
  outcome <- character(n_inj)
  qc_status <- character(n_inj)

  for (i in seq_len(n_inj)) {
    inj_type <- baked$injection_type[[i]]
    p_stat <- peak_status[[i]]
    dil <- baked$dilution_factor[[i]]
    nom <- baked$nominal_conc[[i]]

    if (inj_type == "blank") {
      if (p_stat == "missing") {
        outcome[[i]] <- "blank_ok"
        qc_status[[i]] <- "pass"
      } else if (
        p_stat == "unique" &&
          is.finite(area[[i]]) &&
          area[[i]] <= resolved_blank_max_area
      ) {
        outcome[[i]] <- "blank_ok"
        qc_status[[i]] <- "pass"
      } else {
        outcome[[i]] <- "blank_interference"
        qc_status[[i]] <- "fail"
      }
    } else if (inj_type == "standard") {
      if (p_stat == "missing") {
        outcome[[i]] <- "missing_peak"
        qc_status[[i]] <- "fail"
      } else if (p_stat == "interfered") {
        outcome[[i]] <- "interfered_peak"
        qc_status[[i]] <- "fail"
      } else {
        estimated_conc[[i]] <- raw_est_conc[[i]]
        reported_conc[[i]] <- estimated_conc[[i]] * dil
        if (is.finite(nom) && nom > 0) {
          accuracy_pct[[i]] <- 100 * reported_conc[[i]] / nom
          deviation_pct[[i]] <- 100 * (reported_conc[[i]] - nom) / nom
          qc_status[[i]] <- if (abs(deviation_pct[[i]]) <= qc_acceptance_pct) {
            "pass"
          } else {
            "fail"
          }
        } else {
          qc_status[[i]] <- "pass"
        }
        outcome[[i]] <- "calibrated"
      }
    } else if (inj_type == "qc") {
      if (p_stat == "missing") {
        outcome[[i]] <- "missing_peak"
        qc_status[[i]] <- "fail"
      } else if (p_stat == "interfered") {
        outcome[[i]] <- "interfered_peak"
        qc_status[[i]] <- "fail"
      } else {
        estimated_conc[[i]] <- raw_est_conc[[i]]
        reported_conc[[i]] <- estimated_conc[[i]] * dil
        accuracy_pct[[i]] <- 100 * reported_conc[[i]] / nom
        deviation_pct[[i]] <- 100 * (reported_conc[[i]] - nom) / nom
        if (
          is.finite(deviation_pct[[i]]) &&
            abs(deviation_pct[[i]]) <= qc_acceptance_pct
        ) {
          outcome[[i]] <- "qc_pass"
          qc_status[[i]] <- "pass"
        } else {
          outcome[[i]] <- "qc_fail"
          qc_status[[i]] <- "fail"
        }
      }
    } else {
      # unknown injection
      if (p_stat == "missing") {
        outcome[[i]] <- "missing_peak"
        qc_status[[i]] <- "not_quantified"
      } else if (p_stat == "interfered") {
        outcome[[i]] <- "interfered_peak"
        qc_status[[i]] <- "not_quantified"
      } else {
        estimated_conc[[i]] <- raw_est_conc[[i]]
        r_stat <- pred_tbl$.range_status[[i]]
        if (r_stat == "below_range") {
          outcome[[i]] <- "below_lloq"
          qc_status[[i]] <- "out_of_range"
        } else if (r_stat == "above_range") {
          outcome[[i]] <- "above_uloq"
          qc_status[[i]] <- "out_of_range"
        } else if (r_stat == "missing") {
          outcome[[i]] <- "missing_peak"
          qc_status[[i]] <- "not_quantified"
        } else {
          reported_conc[[i]] <- estimated_conc[[i]] * dil
          outcome[[i]] <- "ok"
          qc_status[[i]] <- "reported"
        }
      }
    }
  }

  results_tbl <- tibble::tibble(
    batch_order = baked$batch_order,
    sample_id = baked$sample_id,
    injection_id = baked$injection_id,
    curve_id = baked$curve_id,
    channel_id = baked$channel_id,
    injection_type = baked$injection_type,
    analyte = rep(analyte, n_inj),
    nominal_conc = baked$nominal_conc,
    dilution_factor = baked$dilution_factor,
    conc_unit = rep(conc_unit %||% NA_character_, n_inj),
    peak_count = peak_count,
    peak_status = peak_status,
    peak_location = peak_location,
    peak_height = peak_height,
    left_base = left_base,
    right_base = right_base,
    area = area,
    calibration_revision = rep(calibration_revision, n_inj),
    estimated_conc = estimated_conc,
    reported_conc = reported_conc,
    accuracy_pct = accuracy_pct,
    deviation_pct = deviation_pct,
    outcome = outcome,
    qc_status = qc_status,
    source_id = baked$source_id,
    source_name = baked$source_name,
    source_sha256 = baked$source_sha256,
    source_row_start = baked$source_row_start,
    source_row_end = baked$source_row_end,
    peak_source_row_start = peak_source_row_start,
    peak_source_row_end = peak_source_row_end
  )

  # Build measure_calibration_verify on QC injections (using in-vial nominal)
  qc_idx <- baked$injection_type == "qc"
  verification <- NULL
  if (any(qc_idx)) {
    qc_frame <- tibble::tibble(
      sample_id = baked$sample_id[qc_idx],
      injection_id = baked$injection_id[qc_idx],
      nominal_conc = baked$nominal_conc[qc_idx] / baked$dilution_factor[qc_idx],
      area = area[qc_idx]
    )
    verification <- measure_calibration_verify(
      calibration,
      qc_frame,
      nominal_col = "nominal_conc",
      acceptance_pct = qc_acceptance_pct
    )
  }

  method_info <- list(
    analyte = analyte,
    channel_id = channel_id,
    window = as.numeric(window),
    conc_unit = conc_unit,
    axis_unit = unique(baked$axis_unit[!is.na(baked$axis_unit)]),
    value_unit = unique(baked$value_unit[!is.na(baked$value_unit)]),
    peak_algorithm = peak_algorithm,
    min_height = min_height,
    min_prominence = min_prominence,
    min_distance = min_distance,
    integration_method = integration_method,
    baseline = baseline,
    model = model,
    weights = if (is.numeric(weights)) "custom" else weights,
    origin = origin,
    calibration_revision = calibration_revision,
    lloq = resolved_lloq,
    uloq = resolved_uloq,
    qc_acceptance_pct = qc_acceptance_pct,
    blank_max_area = resolved_blank_max_area,
    source_sha256 = unique(baked$source_sha256[!is.na(baked$source_sha256)])
  )

  # Build measure_results base so render_measure_report() works out of the box
  base_data <- baked[c("sample_id", "injection_id", ".measures")]
  base_results <- measure_results(
    base_data,
    measures = ".measures",
    id_cols = c("sample_id", "injection_id")
  )
  base_results$results <- results_tbl
  base_results$calibration <- calibration
  base_results$verification <- verification
  base_results$method <- method_info
  base_results$peaks <- do.call(rbind, all_peaks_list)
  class(base_results) <- c("measure_batch_results", class(base_results))
  base_results
}

#' Test whether an object is a batch quantitation result
#'
#' @param x Object to test.
#'
#' @return `TRUE` if `x` inherits from `measure_batch_results`; otherwise
#'   `FALSE`.
#'
#' @export
is_measure_batch_results <- function(x) {
  inherits(x, "measure_batch_results")
}

#' @rdname measure_quantify_batch
#' @param x A `measure_batch_results` object.
#' @param ... Additional arguments (unused).
#' @export
print.measure_batch_results <- function(x, ...) {
  res <- x$results
  m <- x$method
  unit_str <- if (!is.null(m$conc_unit)) paste0(" ", m$conc_unit) else ""
  cat("<measure_batch_results>\n")
  cat("  Analyte: ", m$analyte, " (channel: ", m$channel_id, ")\n", sep = "")
  cat(
    "  Window: [",
    format(m$window[1], digits = 5),
    ", ",
    format(m$window[2], digits = 5),
    "]\n",
    sep = ""
  )
  cat(
    "  Calibration: ",
    m$model,
    " (weights = ",
    m$weights,
    ", revision = ",
    m$calibration_revision,
    ", R\u00b2 = ",
    format(x$calibration$diagnostics$r_squared, digits = 6),
    ")\n",
    sep = ""
  )
  cat(
    "  Range: ",
    format(m$lloq, digits = 5),
    " to ",
    format(m$uloq, digits = 5),
    unit_str,
    "\n",
    sep = ""
  )
  cat(
    "  Injections: ",
    nrow(res),
    " (standards = ",
    sum(res$injection_type == "standard"),
    ", blanks = ",
    sum(res$injection_type == "blank"),
    ", qc = ",
    sum(res$injection_type == "qc"),
    ", unknowns = ",
    sum(res$injection_type == "unknown"),
    ")\n",
    sep = ""
  )
  outcome_counts <- table(res$outcome)
  outcome_summary <- paste(
    paste0(names(outcome_counts), "=", as.integer(outcome_counts)),
    collapse = ", "
  )
  cat("  Outcomes: ", outcome_summary, "\n", sep = "")
  invisible(x)
}

#' @rdname measure_quantify_batch
#' @param object A `measure_batch_results` object.
#' @param ... Additional arguments (unused).
#' @export
summary.measure_batch_results <- function(object, ...) {
  object$results
}

#' @rdname measure_quantify_batch
#' @importFrom generics tidy
#' @export
tidy.measure_batch_results <- function(x, ...) {
  x$results
}

#' Plot a quantified chromatography batch
#'
#' @param object A `measure_batch_results` object from
#'   [measure_quantify_batch()].
#' @param type Plot type: `"chromatograms"` (default) shows overlaid batch
#'   traces with the target retention window shaded; `"calibration"` shows the
#'   fitted calibration curve with QC and unknown injections overlaid.
#' @param ... Additional arguments (unused).
#'
#' @return A `ggplot2` object.
#'
#' @importFrom ggplot2 autoplot
#' @export
autoplot.measure_batch_results <- function(
  object,
  type = c("chromatograms", "calibration"),
  ...
) {
  type <- match.arg(type)
  m <- object$method

  if (type == "calibration") {
    p <- autoplot.measure_calibration(object$calibration, type = "curve")
    overlay <- object$results[
      object$results$injection_type %in%
        c("qc", "unknown") &
        is.finite(object$results$area) &
        is.finite(object$results$estimated_conc),
      ,
      drop = FALSE
    ]
    if (nrow(overlay) > 0L) {
      p <- p +
        ggplot2::geom_point(
          data = overlay,
          ggplot2::aes(
            x = .data$estimated_conc,
            y = .data$area,
            shape = .data$outcome,
            colour = .data$outcome
          ),
          size = 3,
          inherit.aes = FALSE
        )
    }
    x_label <- if (!is.null(m$conc_unit)) {
      paste0("In-vial concentration (", m$conc_unit, ")")
    } else {
      "In-vial concentration"
    }
    return(
      p +
        ggplot2::labs(
          title = paste0(
            "External calibration: ",
            m$analyte,
            " (",
            m$calibration_revision,
            ")"
          ),
          x = x_label,
          y = "Integrated peak area"
        )
    )
  }

  # Chromatograms plot
  trace_list <- lapply(seq_len(nrow(object$data)), function(i) {
    sig <- object$data$.measures[[i]]
    tibble::tibble(
      injection_id = object$results$injection_id[[i]],
      injection_type = object$results$injection_type[[i]],
      outcome = object$results$outcome[[i]],
      location = sig$location,
      value = sig$value
    )
  })
  traces <- do.call(rbind, trace_list)

  x_label <- if (length(m$axis_unit) == 1L) {
    paste0("Retention time (", m$axis_unit, ")")
  } else {
    "Location"
  }
  y_label <- if (length(m$value_unit) == 1L) {
    paste0("Signal (", m$value_unit, ")")
  } else {
    "Signal"
  }

  ggplot2::ggplot(
    traces,
    ggplot2::aes(
      x = .data$location,
      y = .data$value,
      group = .data$injection_id,
      colour = .data$injection_type
    )
  ) +
    ggplot2::annotate(
      "rect",
      xmin = m$window[1],
      xmax = m$window[2],
      ymin = -Inf,
      ymax = Inf,
      alpha = 0.14,
      fill = "#4A90A4"
    ) +
    ggplot2::geom_line(linewidth = 0.65, alpha = 0.85) +
    ggplot2::facet_wrap(~injection_type, scales = "free_y") +
    ggplot2::labs(
      title = paste0("Batch chromatograms: ", m$analyte),
      subtitle = sprintf(
        "Shaded band marks the declared analyte window [%g, %g]",
        m$window[1],
        m$window[2]
      ),
      x = x_label,
      y = y_label,
      colour = "Injection type"
    ) +
    ggplot2::theme_minimal(base_size = 11)
}

#' @rdname measure_report_sections
#' @export
measure_report_sections.measure_batch_results <- function(x, ...) {
  m <- x$method
  unit_suffix <- if (!is.null(m$conc_unit)) paste0(" ", m$conc_unit) else ""
  method_tbl <- tibble::tribble(
    ~Field,
    ~Value,
    "Analyte",
    m$analyte,
    "Channel",
    m$channel_id,
    "Retention window",
    sprintf("%g to %g", m$window[1], m$window[2]),
    "Peak detection",
    sprintf(
      "%s (min_height=%g, min_prominence=%g, min_distance=%g)",
      m$peak_algorithm,
      m$min_height,
      m$min_prominence,
      m$min_distance
    ),
    "Integration",
    sprintf("%s above %s baseline", m$integration_method, m$baseline),
    "Calibration",
    sprintf(
      "%s (weights=%s, origin=%s, revision=%s, R\u00b2=%.6f)",
      m$model,
      m$weights,
      m$origin,
      m$calibration_revision,
      x$calibration$diagnostics$r_squared
    ),
    "Quantitation range",
    sprintf("%g to %g%s", m$lloq, m$uloq, unit_suffix),
    "QC acceptance",
    sprintf("+/-%g%% deviation from nominal", m$qc_acceptance_pct),
    "Blank area ceiling",
    sprintf("%.6g", m$blank_max_area),
    "Source SHA-256",
    if (length(m$source_sha256) > 0L) {
      paste(m$source_sha256, collapse = ", ")
    } else {
      NA_character_
    }
  )

  report_cols <- c(
    "injection_id",
    "sample_id",
    "injection_type",
    "nominal_conc",
    "dilution_factor",
    "peak_count",
    "peak_location",
    "area",
    "calibration_revision",
    "estimated_conc",
    "reported_conc",
    "deviation_pct",
    "outcome",
    "qc_status"
  )

  list(
    `Method & Provenance` = method_tbl,
    `Batch Quantitation` = x$results[report_cols],
    `Calibration Curve` = autoplot.measure_batch_results(
      x,
      type = "calibration"
    ),
    Chromatograms = autoplot.measure_batch_results(x, type = "chromatograms")
  )
}

# ==============================================================================
# Internal validation helpers
# ==============================================================================

.validate_batch_arguments <- function(
  window,
  analyte,
  conc_unit,
  calibration_revision,
  qc_acceptance_pct,
  blank_max_area,
  blank_max_pct_lloq,
  lloq,
  uloq
) {
  if (
    !is.numeric(window) ||
      length(window) != 2L ||
      !all(is.finite(window)) ||
      window[[1L]] >= window[[2L]]
  ) {
    cli::cli_abort(
      "{.arg window} must be a length-2 finite numeric vector with {.code window[1] < window[2]}."
    )
  }

  for (pair in list(
    list(name = "analyte", val = analyte),
    list(name = "calibration_revision", val = calibration_revision)
  )) {
    if (
      !is.character(pair$val) ||
        length(pair$val) != 1L ||
        is.na(pair$val) ||
        !nzchar(trimws(pair$val))
    ) {
      cli::cli_abort(
        "{.arg {pair$name}} must be a single non-empty character string."
      )
    }
  }

  if (
    !is.null(conc_unit) &&
      (!is.character(conc_unit) ||
        length(conc_unit) != 1L ||
        is.na(conc_unit) ||
        !nzchar(trimws(conc_unit)))
  ) {
    cli::cli_abort(
      "{.arg conc_unit} must be a single non-empty character string or NULL."
    )
  }

  if (
    !is.numeric(qc_acceptance_pct) ||
      length(qc_acceptance_pct) != 1L ||
      !is.finite(qc_acceptance_pct) ||
      qc_acceptance_pct <= 0
  ) {
    cli::cli_abort("{.arg qc_acceptance_pct} must be a positive finite number.")
  }

  if (
    !is.null(blank_max_area) &&
      (!is.numeric(blank_max_area) ||
        length(blank_max_area) != 1L ||
        !is.finite(blank_max_area) ||
        blank_max_area < 0)
  ) {
    cli::cli_abort(
      "{.arg blank_max_area} must be a non-negative finite number or NULL."
    )
  }

  if (
    !is.numeric(blank_max_pct_lloq) ||
      length(blank_max_pct_lloq) != 1L ||
      !is.finite(blank_max_pct_lloq) ||
      blank_max_pct_lloq < 0
  ) {
    cli::cli_abort(
      "{.arg blank_max_pct_lloq} must be a non-negative finite number."
    )
  }

  if (
    !is.null(lloq) &&
      (!is.numeric(lloq) || length(lloq) != 1L || !is.finite(lloq) || lloq < 0)
  ) {
    cli::cli_abort("{.arg lloq} must be a non-negative finite number or NULL.")
  }

  if (
    !is.null(uloq) &&
      (!is.numeric(uloq) || length(uloq) != 1L || !is.finite(uloq) || uloq <= 0)
  ) {
    cli::cli_abort("{.arg uloq} must be a positive finite number or NULL.")
  }
}

.validate_batch_manifest <- function(manifest, available_injections) {
  if (!is.data.frame(manifest)) {
    cli::cli_abort("{.arg manifest} must be a data frame.")
  }

  required_cols <- c("injection_id", "injection_type", "nominal_conc")
  missing_cols <- setdiff(required_cols, names(manifest))
  if (length(missing_cols) > 0L) {
    cli::cli_abort(
      "{.arg manifest} is missing required column{?s}: {.field {missing_cols}}."
    )
  }

  if (nrow(manifest) == 0L) {
    cli::cli_abort("{.arg manifest} must contain at least one injection.")
  }

  inj_id <- manifest$injection_id
  if (!is.character(inj_id) || anyNA(inj_id) || !all(nzchar(trimws(inj_id)))) {
    cli::cli_abort(
      "{.field manifest$injection_id} must be non-missing, non-empty character strings."
    )
  }
  if (anyDuplicated(inj_id)) {
    dup <- inj_id[duplicated(inj_id)][[1L]]
    cli::cli_abort(
      "{.field manifest$injection_id} contains duplicate ID {.val {dup}}."
    )
  }

  unknown_inj <- setdiff(inj_id, available_injections)
  if (length(unknown_inj) > 0L) {
    cli::cli_abort(
      "Manifest injection{?s} not found in the selected record channel: {.val {unknown_inj}}."
    )
  }

  valid_types <- c("standard", "blank", "qc", "unknown")
  inj_type <- manifest$injection_type
  if (!is.character(inj_type) || anyNA(inj_type)) {
    cli::cli_abort(
      "{.field manifest$injection_type} must be a non-missing character vector."
    )
  }
  inj_type <- tolower(trimws(inj_type))
  bad_types <- setdiff(unique(inj_type), valid_types)
  if (length(bad_types) > 0L) {
    cli::cli_abort(
      c(
        "Unsupported {.field injection_type} value{?s}: {.val {bad_types}}.",
        "i" = "Allowed values are {.val {valid_types}}."
      )
    )
  }

  nom <- manifest$nominal_conc
  if (!is.numeric(nom)) {
    cli::cli_abort("{.field manifest$nominal_conc} must be numeric.")
  }
  nom <- as.numeric(nom)

  needs_nom <- inj_type %in% c("standard", "qc")
  if (!all(is.finite(nom[needs_nom]))) {
    bad_ids <- inj_id[needs_nom & !is.finite(nom)]
    cli::cli_abort(
      "Standard and QC injections require finite {.field nominal_conc}: {.val {bad_ids}}."
    )
  }
  if (any(nom[inj_type == "qc"] <= 0)) {
    bad_ids <- inj_id[inj_type == "qc" & nom <= 0]
    cli::cli_abort(
      "QC injections require positive {.field nominal_conc}: {.val {bad_ids}}."
    )
  }

  if ("dilution_factor" %in% names(manifest)) {
    dil <- manifest$dilution_factor
    if (!is.numeric(dil) || !all(is.finite(dil)) || any(dil <= 0)) {
      cli::cli_abort(
        "{.field manifest$dilution_factor} must contain finite positive numbers."
      )
    }
    dil <- as.numeric(dil)
  } else {
    dil <- rep(1, nrow(manifest))
  }

  if ("batch_order" %in% names(manifest)) {
    ord <- manifest$batch_order
    if (!is.numeric(ord) || !all(is.finite(ord))) {
      cli::cli_abort(
        "{.field manifest$batch_order} must contain finite numbers."
      )
    }
    ord <- as.integer(ord)
  } else {
    ord <- seq_len(nrow(manifest))
  }

  tibble::tibble(
    batch_order = ord,
    injection_id = inj_id,
    injection_type = inj_type,
    nominal_conc = nom,
    dilution_factor = dil
  )
}
