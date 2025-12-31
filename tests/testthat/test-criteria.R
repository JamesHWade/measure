# ==============================================================================
# Tests for criteria.R
# ==============================================================================

# ==============================================================================
# criterion() tests
# ==============================================================================

test_that("criterion creates valid criterion object", {
  crit <- criterion("cv_qc", "<", 15)

  expect_s3_class(crit, "measure_criterion")
  expect_equal(crit$name, "cv_qc")
  expect_equal(crit$operator, "<")
  expect_equal(crit$threshold, 15)
  expect_equal(crit$priority, "major") # default
})

test_that("criterion handles all operators", {
  operators <- c("<", "<=", ">", ">=", "==", "!=")

  for (op in operators) {
    crit <- criterion("test", op, 10)
    expect_equal(crit$operator, op)
  }

  # between and outside require 2-element threshold
  crit_between <- criterion("test", "between", c(5, 15))
  expect_equal(crit_between$operator, "between")
  expect_equal(crit_between$threshold, c(5, 15))

  crit_outside <- criterion("test", "outside", c(5, 15))
  expect_equal(crit_outside$operator, "outside")
})

test_that("criterion validates between/outside thresholds", {
  # Must be length 2
  expect_error(
    criterion("test", "between", 10),
    "length-2"
  )

  # lower must be < upper
  expect_error(
    criterion("test", "between", c(15, 5)),
    "less than"
  )
})

test_that("criterion validates single-value operators", {
  # Single-value operators must have length-1 threshold
  expect_error(
    criterion("test", "<", c(5, 10)),
    "single threshold"
  )
})

test_that("criterion accepts priority levels", {
  crit_critical <- criterion("test", "<", 10, priority = "critical")
  expect_equal(crit_critical$priority, "critical")

  crit_minor <- criterion("test", "<", 10, priority = "minor")
  expect_equal(crit_minor$priority, "minor")
})

test_that("criterion generates description if not provided", {
  crit <- criterion("cv", "<", 15)
  expect_true(!is.null(crit$description))
  expect_match(crit$description, "cv")
  expect_match(crit$description, "15")
})

test_that("criterion uses provided description", {
  crit <- criterion("cv", "<", 15, description = "Custom description")
  expect_equal(crit$description, "Custom description")
})

# ==============================================================================
# measure_criteria() tests
# ==============================================================================

test_that("measure_criteria combines criterion objects", {
  crit1 <- criterion("cv", "<", 15)
  crit2 <- criterion("r_squared", ">=", 0.99)

  criteria <- measure_criteria(crit1, crit2)

  expect_s3_class(criteria, "measure_criteria")
  expect_length(criteria, 2)
  expect_equal(names(criteria), c("cv", "r_squared"))
})

test_that("measure_criteria supports shorthand notation", {
  criteria <- measure_criteria(
    cv = list("<", 15),
    r_squared = list(">=", 0.99)
  )

  expect_s3_class(criteria, "measure_criteria")
  expect_length(criteria, 2)
  expect_equal(criteria$cv$operator, "<")
  expect_equal(criteria$cv$threshold, 15)
})

test_that("measure_criteria supports simple threshold shorthand", {
  # Simple numeric assumes "<="
  criteria <- measure_criteria(cv = 15, rsd = 20)

  expect_equal(criteria$cv$operator, "<=")
  expect_equal(criteria$cv$threshold, 15)
  expect_equal(criteria$rsd$operator, "<=")
  expect_equal(criteria$rsd$threshold, 20)
})

test_that("measure_criteria accepts .list argument", {
  crit_list <- list(
    criterion("cv", "<", 15),
    criterion("rsd", "<", 20)
  )

  criteria <- measure_criteria(.list = crit_list)
  expect_length(criteria, 2)
})

test_that("measure_criteria subsetting preserves class", {
  criteria <- measure_criteria(
    cv = 15,
    rsd = 20,
    bias = list("between", c(-10, 10))
  )

  subset <- criteria[c("cv", "rsd")]
  expect_s3_class(subset, "measure_criteria")
  expect_length(subset, 2)
})

test_that("measure_criteria concatenation works", {
  crit1 <- measure_criteria(cv = 15)
  crit2 <- measure_criteria(rsd = 20)

  combined <- c(crit1, crit2)
  expect_s3_class(combined, "measure_criteria")
  expect_length(combined, 2)
})

# ==============================================================================
# measure_assess() tests
# ==============================================================================

test_that("measure_assess evaluates criteria correctly", {
  criteria <- measure_criteria(
    cv = list("<", 15),
    r_squared = list(">=", 0.99)
  )

  # All pass
  results <- list(cv = 10, r_squared = 0.995)
  assessment <- measure_assess(results, criteria)

  expect_s3_class(assessment, "measure_assessment")
  expect_true(all(assessment$pass))

  # One fails
  results_fail <- list(cv = 18, r_squared = 0.995)
  assessment_fail <- measure_assess(results_fail, criteria)
  expect_false(assessment_fail$pass[assessment_fail$criterion == "cv"])
  expect_true(assessment_fail$pass[assessment_fail$criterion == "r_squared"])
})

test_that("measure_assess handles between operator", {
  criteria <- measure_criteria(
    recovery = list("between", c(80, 120))
  )

  # In range
  assessment <- measure_assess(list(recovery = 95), criteria)
  expect_true(assessment$pass)

  # Below range
  assessment <- measure_assess(list(recovery = 75), criteria)
  expect_false(assessment$pass)

  # Above range
  assessment <- measure_assess(list(recovery = 125), criteria)
  expect_false(assessment$pass)

  # At boundaries (should pass)
  assessment <- measure_assess(list(recovery = 80), criteria)
  expect_true(assessment$pass)
  assessment <- measure_assess(list(recovery = 120), criteria)
  expect_true(assessment$pass)
})

test_that("measure_assess handles outside operator", {
  criteria <- measure_criteria(
    outlier = list("outside", c(-3, 3))
  )

  # Inside range (fails)
  assessment <- measure_assess(list(outlier = 0), criteria)
  expect_false(assessment$pass)

  # Outside range (passes)
  assessment <- measure_assess(list(outlier = 5), criteria)
  expect_true(assessment$pass)
})

test_that("measure_assess handles missing values", {
  criteria <- measure_criteria(cv = 15, rsd = 20)

  # Missing criterion value
  results <- list(cv = 10) # rsd is missing
  assessment <- measure_assess(results, criteria)

  expect_true(assessment$pass[assessment$criterion == "cv"])
  expect_true(is.na(assessment$pass[assessment$criterion == "rsd"]))
})

test_that("measure_assess handles NA values", {
  criteria <- measure_criteria(cv = 15)
  results <- list(cv = NA)

  assessment <- measure_assess(results, criteria)
  expect_true(is.na(assessment$pass))
})

test_that("measure_assess accepts data frames", {
  criteria <- measure_criteria(cv = 15, rsd = 20)
  df <- data.frame(cv = 10, rsd = 15)

  assessment <- measure_assess(df, criteria)
  expect_true(all(assessment$pass))
})

test_that("measure_assess warns on failures", {
  criteria <- measure_criteria(cv = 15)
  results <- list(cv = 20)

  expect_warning(
    measure_assess(results, criteria, action = "warn"),
    "criteria failures"
  )
})

test_that("measure_assess errors on critical failures", {
  criteria <- measure_criteria(
    criterion("cv", "<", 15, priority = "critical")
  )
  results <- list(cv = 20)

  expect_error(
    measure_assess(results, criteria, action = "error"),
    "Critical"
  )
})

# ==============================================================================
# all_pass() tests
# ==============================================================================

test_that("all_pass returns TRUE when all pass", {
  criteria <- measure_criteria(cv = 15, rsd = 20)
  results <- list(cv = 10, rsd = 15)
  assessment <- measure_assess(results, criteria)

  expect_true(all_pass(assessment))
})

test_that("all_pass returns FALSE when any fail", {
  criteria <- measure_criteria(cv = 15, rsd = 20)
  results <- list(cv = 20, rsd = 15) # cv fails
  assessment <- measure_assess(results, criteria)

  expect_false(all_pass(assessment))
})

test_that("all_pass handles NA values", {
  criteria <- measure_criteria(cv = 15)
  results <- list(cv = NA)
  assessment <- measure_assess(results, criteria)

  # Default: NA counts as fail
  expect_false(all_pass(assessment))

  # With na_pass = TRUE
  expect_true(all_pass(assessment, na_pass = TRUE))
})

# ==============================================================================
# get_failures() tests
# ==============================================================================

test_that("get_failures returns only failed criteria", {
  criteria <- measure_criteria(
    cv = 15,
    rsd = 20,
    bias = list("between", c(-10, 10))
  )
  results <- list(cv = 20, rsd = 15, bias = 15) # cv and bias fail

  assessment <- measure_assess(results, criteria)
  failures <- get_failures(assessment)

  expect_s3_class(failures, "measure_assessment")
  expect_equal(nrow(failures), 2)
  expect_equal(sort(failures$criterion), c("bias", "cv"))
})

test_that("get_failures returns empty tibble when all pass", {
  criteria <- measure_criteria(cv = 15)
  results <- list(cv = 10)
  assessment <- measure_assess(results, criteria)

  failures <- get_failures(assessment)
  expect_equal(nrow(failures), 0)
})

# ==============================================================================
# Preset criteria tests
# ==============================================================================

test_that("criteria_bioanalytical creates valid criteria", {
  crit <- criteria_bioanalytical()

  expect_s3_class(crit, "measure_criteria")
  expect_true("cv_qc" %in% names(crit))
  expect_true("r_squared" %in% names(crit))
  expect_true("recovery" %in% names(crit))
})

test_that("criteria_bioanalytical accepts custom values", {
  crit <- criteria_bioanalytical(cv_qc = 20, r_squared = 0.98)

  expect_equal(crit$cv_qc$threshold, 20)
  expect_equal(crit$r_squared$threshold, 0.98)
})

test_that("criteria_ich_q2 creates valid criteria", {
  crit <- criteria_ich_q2()

  expect_s3_class(crit, "measure_criteria")
  expect_true("cv_repeatability" %in% names(crit))
  expect_true("cv_intermediate" %in% names(crit))
})
