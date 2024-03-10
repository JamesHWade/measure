library(testthat)

# Creation
test_that("measure objects can be created", {
  x <- c(1, 2, 3)
  location <- c(0.1, 0.2, 0.3)
  m <- measure(x, location)
  expect_s3_class(m, "measure")
  expect_equal(field(m, "x"), x)
  expect_equal(field(m, "location"), location)
})

test_that("measure constructor checks for valid input", {
  expect_error(measure("a", 1), "must be a numeric vector")
  expect_error(measure(1, "a"), "must be a numeric vector")
  expect_error(measure(1:3, 1:2), "must be the same")
})

# Printing
test_that("measure objects are printed correctly", {
  m <- measure(1:3, c(0.1, 0.2, 0.3))
  expect_output(print(m), "Measure \\[3 × 3\\]")
  expect_equal(vec_ptype_abbr(m), "msre")
  expect_equal(vec_ptype_full(m), "measure<double>")
})

# Coercion
test_that("objects can be coerced to measure", {
  df <- data.frame(x = 1:3, location = c(0.1, 0.2, 0.3))
  m_df <- as_measure(df)
  expect_s3_class(m_df, "measure")

  mat <- matrix(c(1:3, 0.1, 0.2, 0.3), ncol = 2)
  m_mat <- as_measure(mat)
  expect_s3_class(m_mat, "measure")

  expect_error(as_measure(data.frame(1:3)), "must have exactly 2 columns")
  expect_error(as_measure(matrix(1:9, ncol = 3)), "must have exactly 2 columns")
})

# Casting
test_that("measure objects can be cast to other types", {
  m <- measure(1:3, c(0.1, 0.2, 0.3))

  m_df <- vec_cast(m, data.frame())
  expect_equal(m_df, data.frame(x = 1:3, location = c(0.1, 0.2, 0.3)))

  m_mat <- vec_cast(m, matrix())
  expect_equal(m_mat, matrix(c(1:3, 0.1, 0.2, 0.3), ncol = 2))

  df <- data.frame(x = 1:3, location = c(0.1, 0.2, 0.3))
  m_from_df <- vec_cast(df, new_measure())
  expect_s3_class(m_from_df, "measure")

  mat <- matrix(c(1:3, 0.1, 0.2, 0.3), ncol = 2)
  m_from_mat <- vec_cast(mat, new_measure())
  expect_s3_class(m_from_mat, "measure")
})

# Equality and comparison
test_that("measure objects can be compared for equality", {
  m1 <- measure(1:3, c(0.1, 0.2, 0.3))
  m2 <- measure(1:3, c(0.1, 0.2, 0.3))
  m3 <- measure(1:3, c(0.1, 0.2, 0.4))

  expect_true(vec_equal(m1, m2))
  expect_false(vec_equal(m1, m3))
})

# Arithmetic
test_that("arithmetic operations work on measure objects", {
  m <- measure(1:3, c(0.1, 0.2, 0.3))

  expect_equal(field(m + 1, "x"), 2:4)
  expect_equal(field(1 + m, "x"), 2:4)
  expect_equal(field(m - 1, "x"), 0:2)
  expect_equal(field(m * 2, "x"), 2:6)
  expect_equal(field(2 * m, "x"), 2:6)
  expect_equal(field(m / 2, "x"), c(0.5, 1, 1.5))

  m2 <- measure(4:6, c(0.1, 0.2, 0.3))
  expect_equal(field(m + m2, "x"), 5:9)
  expect_equal(field(m - m2, "x"), -3:1)
  expect_equal(m / m2, 1:3 / 4:6)

  expect_error(m * m2, "not permitted")

  expect_equal(field(-m, "x"), -(1:3))
  expect_equal(field(+m, "x"), 1:3)
})

# Extracting underlying data
test_that("underlying data can be extracted from measure objects", {
  m <- measure(1:3, c(0.1, 0.2, 0.3))
  expect_equal(vec_data(m), data.frame(x = 1:3, location = c(0.1, 0.2, 0.3)))
})
