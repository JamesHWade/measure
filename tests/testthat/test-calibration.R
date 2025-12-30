# ==============================================================================
# Tests for calibration-class.R and calibration-fit.R
# ==============================================================================

# Helper data
cal_data <- data.frame(
  nominal_conc = c(0, 10, 25, 50, 100, 200),
  response = c(0.5, 15.2, 35.8, 72.1, 148.3, 295.7)
)

# ==============================================================================
# measure_calibration_fit() tests
# ==============================================================================

test_that("measure_calibration_fit creates valid calibration object", {
  cal <- measure_calibration_fit(cal_data, response ~ nominal_conc)

  expect_s3_class(cal, "measure_calibration")
  expect_true(is_measure_calibration(cal))
  expect_equal(cal$model_type, "linear")
  expect_equal(cal$weights_type, "none")
})

test_that("measure_calibration_fit handles different model types", {
  # Linear
  cal_linear <- measure_calibration_fit(cal_data, response ~ nominal_conc, model = "linear")
  expect_equal(cal_linear$model_type, "linear")

  # Quadratic
  cal_quad <- measure_calibration_fit(cal_data, response ~ nominal_conc, model = "quadratic")
  expect_equal(cal_quad$model_type, "quadratic")
})

test_that("measure_calibration_fit handles weighting schemes", {
  # 1/x weighting
  cal_1x <- measure_calibration_fit(cal_data, response ~ nominal_conc, weights = "1/x")
  expect_equal(cal_1x$weights_type, "1/x")

  # 1/x^2 weighting
  cal_1x2 <- measure_calibration_fit(cal_data, response ~ nominal_conc, weights = "1/x2")
  expect_equal(cal_1x2$weights_type, "1/x2")

  # 1/y weighting
  cal_1y <- measure_calibration_fit(cal_data, response ~ nominal_conc, weights = "1/y")
  expect_equal(cal_1y$weights_type, "1/y")
})

test_that("measure_calibration_fit handles custom weights", {
  custom_weights <- rep(1, nrow(cal_data))
  cal <- measure_calibration_fit(cal_data, response ~ nominal_conc, weights = custom_weights)
  expect_equal(cal$weights_type, "custom")
})

test_that("measure_calibration_fit validates custom weight length", {
  bad_weights <- c(1, 2, 3)  # Wrong length
  expect_error(
    measure_calibration_fit(cal_data, response ~ nominal_conc, weights = bad_weights),
    "length"
  )
})

test_that("measure_calibration_fit calculates diagnostics", {
  cal <- measure_calibration_fit(cal_data, response ~ nominal_conc)

  expect_true(!is.null(cal$diagnostics$r_squared))
  expect_true(cal$diagnostics$r_squared > 0.99)  # Should be very linear
  expect_true(!is.null(cal$diagnostics$sigma))
})

test_that("measure_calibration_fit handles through-origin", {
  cal_origin <- measure_calibration_fit(
    cal_data,
    response ~ nominal_conc,
    origin = TRUE
  )

  # Should have no intercept
  coefs <- stats::coef(cal_origin$model)
  expect_false("(Intercept)" %in% names(coefs))
})

test_that("measure_calibration_fit filters by sample_type", {
  data_with_type <- cal_data
  data_with_type$sample_type <- c("standard", "standard", "standard", "qc", "standard", "standard")

  cal <- measure_calibration_fit(
    data_with_type,
    response ~ nominal_conc,
    sample_type_col = "sample_type"
  )

  # Should have one fewer point (the qc)
  expect_equal(nrow(cal$data), 5)
})

test_that("measure_calibration_fit detects outliers with studentized method", {
  # Add an outlier
  data_outlier <- cal_data
  data_outlier$response[3] <- 100  # Way off

  cal <- measure_calibration_fit(
    data_outlier,
    response ~ nominal_conc,
    outlier_method = "studentized",
    outlier_threshold = 2
  )

  expect_true(!is.null(cal$outliers))
  expect_true(nrow(cal$outliers) > 0)
})

test_that("measure_calibration_fit removes outliers when requested", {
  # Add an outlier
  data_outlier <- cal_data
  data_outlier$response[3] <- 100  # Way off

  cal_flag <- measure_calibration_fit(
    data_outlier,
    response ~ nominal_conc,
    outlier_method = "studentized",
    outlier_threshold = 2,
    outlier_action = "flag"
  )

  cal_remove <- measure_calibration_fit(
    data_outlier,
    response ~ nominal_conc,
    outlier_method = "studentized",
    outlier_threshold = 2,
    outlier_action = "remove"
  )

  # Remove action should have fewer points in fit data
  expect_true(nrow(cal_remove$data) < nrow(cal_flag$data))
})

test_that("measure_calibration_fit validates data input", {
  expect_error(
    measure_calibration_fit("not a data frame", response ~ nominal_conc),
    "data frame"
  )
})

test_that("measure_calibration_fit validates formula", {
  # Formula with unknown column
  expect_error(
    measure_calibration_fit(cal_data, response ~ missing_col),
    "not found"
  )

  expect_error(
    measure_calibration_fit(cal_data, missing_col ~ nominal_conc),
    "not found"
  )
})

test_that("measure_calibration_fit requires minimum points", {
  small_data <- cal_data[1:2, ]
  expect_error(
    measure_calibration_fit(small_data, response ~ nominal_conc),
    "At least 3"
  )
})

# ==============================================================================
# measure_calibration_predict() tests
# ==============================================================================

test_that("measure_calibration_predict returns predictions", {
  cal <- measure_calibration_fit(cal_data, response ~ nominal_conc)
  unknowns <- data.frame(response = c(45, 85, 120))

  preds <- measure_calibration_predict(cal, unknowns)

  expect_s3_class(preds, "tbl_df")
  expect_true(".pred_conc" %in% names(preds))
  expect_equal(nrow(preds), 3)
})

test_that("measure_calibration_predict handles intervals", {
  cal <- measure_calibration_fit(cal_data, response ~ nominal_conc)
  unknowns <- data.frame(response = c(45, 85, 120))

  # Confidence intervals
  preds_ci <- measure_calibration_predict(cal, unknowns, interval = "confidence")
  expect_true(".pred_lower" %in% names(preds_ci))
  expect_true(".pred_upper" %in% names(preds_ci))

  # Prediction intervals (should be wider)
  preds_pi <- measure_calibration_predict(cal, unknowns, interval = "prediction")
  expect_true(all(preds_pi$.pred_upper - preds_pi$.pred_lower >= preds_ci$.pred_upper - preds_ci$.pred_lower))
})

test_that("measure_calibration_predict works with quadratic model", {
  cal_quad <- measure_calibration_fit(cal_data, response ~ nominal_conc, model = "quadratic")
  unknowns <- data.frame(response = c(45, 85, 120))

  preds <- measure_calibration_predict(cal_quad, unknowns)
  expect_equal(nrow(preds), 3)
  expect_true(all(!is.na(preds$.pred_conc)))
})

test_that("measure_calibration_predict validates input", {
  cal <- measure_calibration_fit(cal_data, response ~ nominal_conc)

  expect_error(
    measure_calibration_predict("not a calibration", data.frame(response = 1)),
    "measure_calibration"
  )

  expect_error(
    measure_calibration_predict(cal, data.frame(wrong_col = 1)),
    "not found"
  )
})

# ==============================================================================
# S3 method tests
# ==============================================================================

test_that("tidy.measure_calibration returns coefficient table", {
  cal <- measure_calibration_fit(cal_data, response ~ nominal_conc)
  tidy_result <- tidy(cal)

  expect_s3_class(tidy_result, "tbl_df")
  expect_true("term" %in% names(tidy_result))
  expect_true("estimate" %in% names(tidy_result))
  expect_true("std_error" %in% names(tidy_result))
})

test_that("glance.measure_calibration returns summary stats", {
  cal <- measure_calibration_fit(cal_data, response ~ nominal_conc)
  glance_result <- glance(cal)

  expect_s3_class(glance_result, "tbl_df")
  expect_equal(nrow(glance_result), 1)
  expect_true("r_squared" %in% names(glance_result))
  expect_true("model_type" %in% names(glance_result))
})

test_that("augment.measure_calibration adds fitted values", {
  cal <- measure_calibration_fit(cal_data, response ~ nominal_conc)
  aug_result <- augment(cal)

  expect_s3_class(aug_result, "tbl_df")
  expect_true(".fitted" %in% names(aug_result))
  expect_true(".resid" %in% names(aug_result))
  expect_true(".cooksd" %in% names(aug_result))
})

test_that("autoplot.measure_calibration creates plots", {
  skip_if_not_installed("ggplot2")

  cal <- measure_calibration_fit(cal_data, response ~ nominal_conc)

  p_curve <- autoplot(cal, type = "curve")
  expect_s3_class(p_curve, "ggplot")

  p_resid <- autoplot(cal, type = "residuals")
  expect_s3_class(p_resid, "ggplot")

  p_qq <- autoplot(cal, type = "qq")
  expect_s3_class(p_qq, "ggplot")
})

test_that("print.measure_calibration works", {
  cal <- measure_calibration_fit(cal_data, response ~ nominal_conc)
  expect_output(print(cal), "measure_calibration")
  expect_output(print(cal), "linear")
})

test_that("is_measure_calibration returns correct values", {
  cal <- measure_calibration_fit(cal_data, response ~ nominal_conc)
  expect_true(is_measure_calibration(cal))
  expect_false(is_measure_calibration("not a calibration"))
  expect_false(is_measure_calibration(NULL))
})
