# ==============================================================================
# Tests for lod-loq.R
# ==============================================================================

# Helper data
set.seed(42)
lod_data <- data.frame(
  sample_type = c(rep("blank", 10), rep("standard", 5)),
  response = c(rnorm(10, mean = 0.5, sd = 0.1), c(5, 15, 35, 70, 150)),
  nominal_conc = c(rep(0, 10), c(10, 25, 50, 100, 200))
)

# ==============================================================================
# measure_lod() tests
# ==============================================================================

test_that("measure_lod blank_sd method works", {
  lod <- measure_lod(lod_data, "response", method = "blank_sd")

  expect_s3_class(lod, "measure_lod")
  expect_true(!is.na(lod$value))
  expect_equal(lod$method, "blank_sd")
  expect_equal(lod$k, 3)
})

test_that("measure_lod blank_sd returns expected value", {
  # Create controlled data
  blank_data <- data.frame(
    sample_type = rep("blank", 10),
    response = rep(1.0, 10) # SD = 0, mean = 1
  )
  blank_data$response[1] <- 0.9
  blank_data$response[2] <- 1.1 # Now SD > 0

  lod <- measure_lod(blank_data, "response", method = "blank_sd")

  # LOD should be mean + 3*SD
  expected_lod <- mean(blank_data$response) + 3 * sd(blank_data$response)
  expect_equal(lod$value, expected_lod)
})

test_that("measure_lod calibration method works", {
  # First fit calibration
  cal_data <- lod_data[lod_data$sample_type == "standard", ]
  cal <- measure_calibration_fit(cal_data, response ~ nominal_conc)

  lod <- measure_lod(
    lod_data,
    "response",
    method = "calibration",
    calibration = cal
  )

  expect_s3_class(lod, "measure_lod")
  expect_true(!is.na(lod$value))
  expect_equal(lod$method, "calibration")
})

test_that("measure_lod calibration requires calibration object", {
  expect_error(
    measure_lod(lod_data, "response", method = "calibration"),
    "measure_calibration"
  )
})

test_that("measure_lod validates blank data", {
  # No blanks
  no_blanks <- lod_data[lod_data$sample_type != "blank", ]

  expect_error(
    measure_lod(no_blanks, "response", method = "blank_sd"),
    "At least 2"
  )

  # Only 1 blank
  one_blank <- lod_data
  one_blank$sample_type[1:9] <- "unknown"

  expect_error(
    measure_lod(one_blank, "response", method = "blank_sd"),
    "At least 2"
  )
})

test_that("measure_lod returns parameters", {
  lod <- measure_lod(lod_data, "response", method = "blank_sd")

  expect_true(!is.null(lod$parameters))
  expect_true("blank_mean" %in% names(lod$parameters))
  expect_true("blank_sd" %in% names(lod$parameters))
  expect_true("n_blanks" %in% names(lod$parameters))
})

# ==============================================================================
# measure_loq() tests
# ==============================================================================

test_that("measure_loq blank_sd method works", {
  loq <- measure_loq(lod_data, "response", method = "blank_sd")

  expect_s3_class(loq, "measure_loq")
  expect_true(!is.na(loq$value))
  expect_equal(loq$method, "blank_sd")
  expect_equal(loq$k, 10)
})

test_that("measure_loq is higher than lod", {
  lod <- measure_lod(lod_data, "response", method = "blank_sd")
  loq <- measure_loq(lod_data, "response", method = "blank_sd")

  # LOQ should be higher than LOD (k=10 vs k=3)
  expect_true(loq$value > lod$value)
})

test_that("measure_loq calibration method works", {
  cal_data <- lod_data[lod_data$sample_type == "standard", ]
  cal <- measure_calibration_fit(cal_data, response ~ nominal_conc)

  loq <- measure_loq(
    lod_data,
    "response",
    method = "calibration",
    calibration = cal
  )

  expect_s3_class(loq, "measure_loq")
  expect_true(!is.na(loq$value))
})

# ==============================================================================
# measure_lod_loq() tests
# ==============================================================================

test_that("measure_lod_loq returns both limits", {
  limits <- measure_lod_loq(lod_data, "response", method = "blank_sd")

  expect_s3_class(limits, "measure_lod_loq")
  expect_s3_class(limits$lod, "measure_lod")
  expect_s3_class(limits$loq, "measure_loq")
  expect_true(limits$loq$value > limits$lod$value)
})

test_that("measure_lod_loq calibration method works", {
  cal_data <- lod_data[lod_data$sample_type == "standard", ]
  cal <- measure_calibration_fit(cal_data, response ~ nominal_conc)

  limits <- measure_lod_loq(
    lod_data,
    "response",
    method = "calibration",
    calibration = cal
  )

  expect_s3_class(limits, "measure_lod_loq")
  expect_equal(limits$method, "calibration")
})

# ==============================================================================
# Tidy methods
# ==============================================================================

test_that("tidy.measure_lod works", {
  lod <- measure_lod(lod_data, "response", method = "blank_sd")
  tidy_result <- tidy(lod)

  expect_s3_class(tidy_result, "tbl_df")
  expect_equal(nrow(tidy_result), 1)
  expect_equal(tidy_result$limit_type, "LOD")
  expect_true("value" %in% names(tidy_result))
  expect_true("method" %in% names(tidy_result))
})

test_that("tidy.measure_loq works", {
  loq <- measure_loq(lod_data, "response", method = "blank_sd")
  tidy_result <- tidy(loq)

  expect_s3_class(tidy_result, "tbl_df")
  expect_equal(nrow(tidy_result), 1)
  expect_equal(tidy_result$limit_type, "LOQ")
})

test_that("tidy.measure_lod_loq works", {
  limits <- measure_lod_loq(lod_data, "response", method = "blank_sd")
  tidy_result <- tidy(limits)

  expect_s3_class(tidy_result, "tbl_df")
  expect_equal(nrow(tidy_result), 2)
  expect_equal(sort(tidy_result$limit_type), c("LOD", "LOQ"))
})

# ==============================================================================
# Print methods
# ==============================================================================

test_that("print.measure_lod works", {
  lod <- measure_lod(lod_data, "response", method = "blank_sd")
  expect_output(print(lod), "measure_lod")
  expect_output(print(lod), "blank_sd")
})

test_that("print.measure_loq works", {
  loq <- measure_loq(lod_data, "response", method = "blank_sd")
  expect_output(print(loq), "measure_loq")
})

test_that("print.measure_lod_loq works", {
  limits <- measure_lod_loq(lod_data, "response", method = "blank_sd")
  expect_output(print(limits), "measure_lod_loq")
  expect_output(print(limits), "LOD")
  expect_output(print(limits), "LOQ")
})

# ==============================================================================
# Edge cases
# ==============================================================================

test_that("measure_lod handles NA values in blanks", {
  data_with_na <- lod_data
  data_with_na$response[1] <- NA

  lod <- measure_lod(data_with_na, "response", method = "blank_sd")
  expect_true(!is.na(lod$value))
  expect_equal(lod$parameters$n_blanks, 9) # One NA excluded
})

test_that("measure_lod works with factor sample_type", {
  data_factor <- lod_data
  data_factor$sample_type <- factor(data_factor$sample_type)

  lod <- measure_lod(data_factor, "response", method = "blank_sd")
  expect_s3_class(lod, "measure_lod")
})
