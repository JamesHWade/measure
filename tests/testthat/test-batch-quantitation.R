# ==============================================================================
# Tests for measure_record_curves() and measure_quantify_batch() (#203, #81, #84)
# ==============================================================================

.lc_control_columns <- function() {
  c(
    sample_id = "sample_id",
    injection_id = "injection_id",
    channel_id = "channel_id",
    channel_name = "channel_name",
    curve_id = "curve_id",
    axis_name = "axis_name",
    axis_unit = "axis_unit",
    value_name = "value_name",
    value_unit = "value_unit",
    location = "location",
    value = "value"
  )
}

.load_lc_control_batch <- function() {
  data_path <- system.file(
    "extdata",
    "lc_batch_control.csv",
    package = "measure",
    mustWork = TRUE
  )
  manifest_path <- system.file(
    "extdata",
    "lc_batch_manifest.csv",
    package = "measure",
    mustWork = TRUE
  )
  rec <- read_measure_record_delim(
    data_path,
    .lc_control_columns(),
    source_id = "lc-batch-control"
  )
  manifest <- utils::read.csv(manifest_path, stringsAsFactors = FALSE)
  list(record = rec, manifest = manifest, data_path = data_path)
}

# ==============================================================================
# measure_record_curves()
# ==============================================================================

test_that("measure_record_curves links record metadata and source row spans", {
  fixture <- .load_lc_control_batch()
  curves <- measure_record_curves(fixture$record)

  expect_s3_class(curves, "tbl_df")
  expect_equal(nrow(curves), 13L)
  expect_true(is_measure_list(curves$.measures))
  expect_equal(curves$injection_id, sprintf("INJ-%02d", 1:13))
  expect_equal(curves$n_points, rep(226L, 13L))
  expect_equal(curves$source_row_start[[1L]], 1L)
  expect_equal(curves$source_row_end[[1L]], 226L)
  expect_equal(curves$source_row_start[[2L]], 227L)
  expect_equal(curves$source_row_end[[2L]], 452L)
  expect_true(all(
    curves$source_sha256 ==
      "8a9f439a1e4c413d0e6abc138ff8bc26e36282d2210a7f0573c116c3e8769949"
  ))
  expect_equal(unique(curves$axis_unit), "min")
  expect_equal(unique(curves$value_unit), "mAU")
})

test_that("measure_record_curves filters by channel_id and validates input", {
  c1 <- new_measure_tbl(c(1, 2, 3), c(0.1, 0.5, 0.2))
  rec <- measure_record(
    samples = tibble::tibble(sample_id = "S1"),
    injections = tibble::tibble(injection_id = "INJ-1", sample_id = "S1"),
    channels = tibble::tibble(
      channel_id = c("uv254", "uv280"),
      channel_name = c("UV 254", "UV 280")
    ),
    curves = tibble::tibble(
      curve_id = c("C1", "C2"),
      injection_id = c("INJ-1", "INJ-1"),
      channel_id = c("uv254", "uv280"),
      .measures = list(c1, c1)
    )
  )

  sub <- measure_record_curves(rec, channel_id = "uv280")
  expect_equal(nrow(sub), 1L)
  expect_equal(sub$curve_id, "C2")
  expect_equal(sub$channel_name, "UV 280")
  expect_true(is.na(sub$source_row_start))

  expect_error(
    measure_record_curves(rec, channel_id = "missing"),
    "not found"
  )
  expect_error(
    measure_record_curves(rec, channel_id = ""),
    "single non-empty"
  )
})

# ==============================================================================
# measure_quantify_batch(): independent reference qualification (#203)
# ==============================================================================

test_that("measure_quantify_batch reproduces the control batch within declared tolerances", {
  fixture <- .load_lc_control_batch()
  batch <- measure_quantify_batch(
    fixture$record,
    fixture$manifest,
    window = c(4.8, 5.6),
    analyte = "caffeine_control",
    conc_unit = "mg/L",
    calibration_revision = "cal-control-v1",
    min_prominence = 1.0
  )

  expect_s3_class(batch, "measure_batch_results")
  expect_s3_class(batch, "measure_results")
  expect_true(is_measure_batch_results(batch))

  res <- summary(batch)
  expect_identical(tidy(batch), res)
  expect_equal(nrow(res), 13L)

  # Declared pre-comparison tolerances
  tol_area <- 1e-9
  tol_conc <- 1e-9

  # Independent reference values from tools/chromatography/batch-reference.py
  expected_sha <- "8a9f439a1e4c413d0e6abc138ff8bc26e36282d2210a7f0573c116c3e8769949"
  expected_intercept <- 0.5686644570673636
  expected_slope <- 1.4511419544975344
  expected_areas <- c(
    NA_real_,
    7.878024541962506,
    15.02661562262964,
    36.957818290514645,
    72.95416221480001,
    145.74367296996152,
    22.783817403473186,
    106.73893307053662,
    51.34883331610246,
    58.59871587518052,
    196.3491065066145,
    NA_real_,
    NA_real_
  )
  expected_est <- c(
    NA_real_,
    5.036971098686239,
    9.963154273606829,
    25.076219263502185,
    49.88174832475057,
    100.04190703945416,
    15.30873866443889,
    73.16325483142087,
    34.99324700912393,
    39.989231403764606,
    134.91474176097208,
    NA_real_,
    NA_real_
  )
  expected_rep <- c(
    NA_real_,
    5.036971098686239,
    9.963154273606829,
    25.076219263502185,
    49.88174832475057,
    100.04190703945416,
    15.30873866443889,
    73.16325483142087,
    34.99324700912393,
    79.97846280752921,
    NA_real_,
    NA_real_,
    NA_real_
  )
  expected_outcomes <- c(
    "blank_ok",
    "calibrated",
    "calibrated",
    "calibrated",
    "calibrated",
    "calibrated",
    "qc_pass",
    "qc_fail",
    "ok",
    "ok",
    "above_uloq",
    "missing_peak",
    "interfered_peak"
  )
  expected_qc_status <- c(
    "pass",
    "pass",
    "pass",
    "pass",
    "pass",
    "pass",
    "pass",
    "fail",
    "reported",
    "reported",
    "out_of_range",
    "not_quantified",
    "not_quantified"
  )
  expected_peak_counts <- c(0L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 0L, 2L)

  expect_identical(batch$method$source_sha256, expected_sha)
  expect_true(all(res$source_sha256 == expected_sha))
  expect_true(all(res$calibration_revision == "cal-control-v1"))
  expect_true(all(res$conc_unit == "mg/L"))

  coefs <- stats::coef(batch$calibration$model)
  expect_lt(abs(unname(coefs[[1L]]) - expected_intercept), tol_area)
  expect_lt(abs(unname(coefs[[2L]]) - expected_slope), tol_area)

  expect_equal(res$peak_count, expected_peak_counts)
  expect_equal(res$outcome, expected_outcomes)
  expect_equal(res$qc_status, expected_qc_status)
  expect_equal(res$area, expected_areas, tolerance = tol_area)
  expect_equal(res$estimated_conc, expected_est, tolerance = tol_conc)
  expect_equal(res$reported_conc, expected_rep, tolerance = tol_conc)

  # Provenance maps peak intervals back to parsed CSV rows
  expect_equal(res$peak_source_row_start[[2L]], 282L)
  expect_equal(res$peak_source_row_end[[2L]], 362L)
  expect_true(is.na(res$peak_source_row_start[[1L]]))
  expect_true(is.na(res$peak_source_row_start[[12L]]))
  expect_true(is.na(res$peak_source_row_start[[13L]]))

  # QC verification object captures both passing and failing QCs
  expect_s3_class(batch$verification, "measure_calibration_verify")
  expect_equal(attr(batch$verification, "n_total"), 2L)
  expect_equal(attr(batch$verification, "n_pass"), 1L)
  expect_equal(attr(batch$verification, "n_fail"), 1L)
  expect_false(attr(batch$verification, "overall_pass"))
})

test_that("measure_quantify_batch S3 methods and report sections work", {
  fixture <- .load_lc_control_batch()
  batch <- measure_quantify_batch(
    fixture$record,
    fixture$manifest,
    window = c(4.8, 5.6),
    analyte = "caffeine_control",
    conc_unit = "mg/L",
    calibration_revision = "cal-control-v1",
    min_prominence = 1.0
  )

  expect_output(print(batch), "measure_batch_results")
  expect_output(print(batch), "caffeine_control")
  expect_output(print(batch), "cal-control-v1")

  skip_if_not_installed("ggplot2")
  p_chrom <- ggplot2::autoplot(batch, type = "chromatograms")
  p_cal <- ggplot2::autoplot(batch, type = "calibration")
  expect_s3_class(p_chrom, "ggplot")
  expect_s3_class(p_cal, "ggplot")

  sections <- measure_report_sections(batch)
  expect_identical(
    names(sections),
    c(
      "Method & Provenance",
      "Batch Quantitation",
      "Calibration Curve",
      "Chromatograms"
    )
  )
  expect_s3_class(sections[["Method & Provenance"]], "tbl_df")
  expect_s3_class(sections[["Batch Quantitation"]], "tbl_df")
  expect_s3_class(sections[["Calibration Curve"]], "ggplot")
  expect_s3_class(sections[["Chromatograms"]], "ggplot")
})

test_that("measure_quantify_batch flags below_lloq unknowns and blank interference", {
  times <- seq(3, 7, by = 0.05)
  make_trace <- function(peak_area) {
    val <- 0.2 +
      if (peak_area > 0) {
        (peak_area / (0.12 * sqrt(2 * pi))) *
          exp(-0.5 * ((times - 5.0) / 0.12)^2)
      } else {
        0
      }
    new_measure_tbl(times, val)
  }

  ids <- c("BLK-BAD", "STD-1", "STD-2", "STD-3", "UNK-LOW")
  areas <- c(4.5, 10.1, 24.8, 50.2, 4.0)
  rec <- measure_record(
    samples = tibble::tibble(sample_id = ids),
    injections = tibble::tibble(injection_id = ids, sample_id = ids),
    channels = tibble::tibble(channel_id = "uv", channel_name = "UV"),
    curves = tibble::tibble(
      curve_id = paste0("C-", ids),
      injection_id = ids,
      channel_id = rep("uv", length(ids)),
      .measures = lapply(areas, make_trace)
    )
  )
  manifest <- tibble::tibble(
    injection_id = ids,
    injection_type = c("blank", "standard", "standard", "standard", "unknown"),
    nominal_conc = c(NA_real_, 10, 25, 50, NA_real_),
    dilution_factor = c(1, 1, 1, 1, 1)
  )

  batch <- measure_quantify_batch(
    rec,
    manifest,
    window = c(4.5, 5.5),
    min_prominence = 1.0,
    blank_max_pct_lloq = 20
  )
  res <- summary(batch)

  expect_equal(res$outcome[[1L]], "blank_interference")
  expect_equal(res$qc_status[[1L]], "fail")
  expect_equal(res$outcome[[5L]], "below_lloq")
  expect_equal(res$qc_status[[5L]], "out_of_range")
  expect_true(is.na(res$reported_conc[[5L]]))
  expect_true(is.finite(res$estimated_conc[[5L]]))
  expect_lt(res$estimated_conc[[5L]], 10)
})

test_that("measure_quantify_batch validates arguments and manifest strictly", {
  fixture <- .load_lc_control_batch()

  expect_error(
    measure_quantify_batch(
      fixture$record,
      fixture$manifest,
      window = c(5.6, 4.8)
    ),
    "window"
  )

  bad_type <- fixture$manifest
  bad_type$injection_type[[1L]] <- "mystery"
  expect_error(
    measure_quantify_batch(fixture$record, bad_type, window = c(4.8, 5.6)),
    "Unsupported"
  )

  bad_dil <- fixture$manifest
  bad_dil$dilution_factor[[1L]] <- 0
  expect_error(
    measure_quantify_batch(fixture$record, bad_dil, window = c(4.8, 5.6)),
    "dilution_factor"
  )

  bad_inj <- fixture$manifest
  bad_inj$injection_id[[1L]] <- "DOES-NOT-EXIST"
  expect_error(
    measure_quantify_batch(fixture$record, bad_inj, window = c(4.8, 5.6)),
    "not found"
  )

  dup_inj <- rbind(fixture$manifest, fixture$manifest[1L, ])
  expect_error(
    measure_quantify_batch(fixture$record, dup_inj, window = c(4.8, 5.6)),
    "duplicate"
  )

  too_few_std <- fixture$manifest[
    !(fixture$manifest$injection_id %in% c("INJ-02", "INJ-03", "INJ-04")),
    ,
    drop = FALSE
  ]
  expect_error(
    measure_quantify_batch(
      fixture$record,
      too_few_std,
      window = c(4.8, 5.6),
      min_prominence = 1.0
    ),
    "At least 3 standard"
  )
})
