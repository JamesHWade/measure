# ==============================================================================
# Tests for peak algorithm registry
# ==============================================================================

test_that("core peak algorithms are registered on package load", {
  # These should be registered automatically
  expect_true(has_peak_algorithm("prominence"))
  expect_true(has_peak_algorithm("derivative"))
  expect_true(has_peak_algorithm("local_maxima"))
})

test_that("peak_algorithms() returns tibble of registered algorithms", {
  algos <- peak_algorithms()

  expect_s3_class(algos, "tbl_df")
  expect_true(nrow(algos) >= 3) # At least the core algorithms
  expect_true("name" %in% names(algos))
  expect_true("pack_name" %in% names(algos))
  expect_true("description" %in% names(algos))
  expect_true("technique" %in% names(algos))
  expect_true("default_params" %in% names(algos))
})

test_that("peak_algorithms() can filter by pack", {
  algos <- peak_algorithms(packs = "measure")

  expect_s3_class(algos, "tbl_df")
  expect_true(all(algos$pack_name == "measure"))
})

test_that("get_peak_algorithm() retrieves algorithm info", {
  algo <- get_peak_algorithm("prominence")

  expect_type(algo, "list")
  expect_equal(algo$name, "prominence")
  expect_equal(algo$pack_name, "measure")
  expect_true(is.function(algo$algorithm_fn))
  expect_type(algo$default_params, "list")
})

test_that("get_peak_algorithm() returns NULL for non-existent algorithm", {
  expect_null(get_peak_algorithm("nonexistent_algorithm"))
})

test_that("has_peak_algorithm() works correctly", {
  expect_true(has_peak_algorithm("prominence"))
  expect_true(has_peak_algorithm("derivative"))
  expect_false(has_peak_algorithm("nonexistent"))
})

test_that("register_peak_algorithm() adds new algorithm", {
  # Create a simple test algorithm
  test_algo <- function(location, value, threshold = 0) {
    new_peaks_tbl()
  }

  register_peak_algorithm(
    name = "test_algo",
    algorithm_fn = test_algo,
    pack_name = "test",
    description = "Test algorithm",
    default_params = list(threshold = 0)
  )

  expect_true(has_peak_algorithm("test_algo"))

  algo <- get_peak_algorithm("test_algo")
  expect_equal(algo$name, "test_algo")
  expect_equal(algo$pack_name, "test")
  expect_equal(algo$description, "Test algorithm")

  # Clean up
  unregister_peak_algorithm("test_algo")
  expect_false(has_peak_algorithm("test_algo"))
})

test_that("unregister_peak_algorithm() removes algorithm", {
  # Register a temporary algorithm
  register_peak_algorithm(
    name = "temp_algo",
    algorithm_fn = function(location, value) new_peaks_tbl(),
    pack_name = "test",
    description = "Temporary"
  )

  expect_true(has_peak_algorithm("temp_algo"))

  result <- unregister_peak_algorithm("temp_algo")
  expect_true(result)
  expect_false(has_peak_algorithm("temp_algo"))

  # Second unregister should return FALSE
  result2 <- unregister_peak_algorithm("temp_algo")
  expect_false(result2)
})

test_that("register_peak_algorithm() validates inputs", {
  expect_error(
    register_peak_algorithm(
      name = "",
      algorithm_fn = function(x) x,
      pack_name = "test"
    ),
    "non-empty string"
  )

  expect_error(
    register_peak_algorithm(
      name = "test",
      algorithm_fn = "not a function",
      pack_name = "test"
    ),
    "must be a function"
  )

  expect_error(
    register_peak_algorithm(
      name = "test",
      algorithm_fn = function(x) x,
      pack_name = c("a", "b")
    ),
    "must be a string"
  )
})

test_that("registered algorithms can be executed via .run_peak_algorithm", {
  # Create test data
  x <- seq(0, 10, by = 0.1)
  y <- dnorm(x, mean = 5, sd = 1) # Gaussian peak

  # Run the prominence algorithm
  result <- .run_peak_algorithm("prominence", x, y, min_height = 0.1)

  expect_s3_class(result, "peaks_tbl")
  expect_true(nrow(result) >= 1) # Should find the peak
})

test_that(".run_peak_algorithm errors for unknown algorithm", {
  expect_error(
    .run_peak_algorithm("unknown_algorithm", 1:10, 1:10),
    "not found"
  )
})

test_that(".detect_peaks_local_maxima works correctly", {
  # Simple test with known peaks
  x <- seq(0, 10, by = 0.1)
  # Two Gaussian peaks
  y <- dnorm(x, mean = 3, sd = 0.5) + dnorm(x, mean = 7, sd = 0.5)

  result <- .detect_peaks_local_maxima(x, y, min_height = 0.1)

  expect_s3_class(result, "peaks_tbl")
  expect_equal(nrow(result), 2) # Should find both peaks
  # Peaks should be near 3 and 7
  expect_true(any(abs(result$location - 3) < 0.2))
  expect_true(any(abs(result$location - 7) < 0.2))
})

test_that("peak algorithm registry preserves technique info", {
  # Register with technique
  register_peak_algorithm(
    name = "sec_test_algo",
    algorithm_fn = function(location, value) new_peaks_tbl(),
    pack_name = "test.sec",
    description = "SEC-specific test",
    technique = "SEC/GPC"
  )

  algo <- get_peak_algorithm("sec_test_algo")
  expect_equal(algo$technique, "SEC/GPC")

  # Check filtering by technique
  algos_sec <- peak_algorithms(techniques = "SEC/GPC")
  expect_true("sec_test_algo" %in% algos_sec$name)

  # Cleanup
  unregister_peak_algorithm("sec_test_algo")
})
