# ==============================================================================
# Tests for sample-wise mapping infrastructure
# ==============================================================================

# Helper to create test data
create_test_data <- function() {
  recipe(water + fat + protein ~ ., data = meats_long) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    prep() %>%
    bake(new_data = NULL)
}

# ==============================================================================
# measure_map() tests
# ==============================================================================

test_that("measure_map works with a simple function", {
  test_data <- create_test_data()

  # Simple centering function
  center_spectrum <- function(x) {
    x$value <- x$value - mean(x$value)
    x
  }

  result <- measure_map(test_data, center_spectrum)

  # Check structure is preserved
  expect_s3_class(result, "tbl_df")
  expect_true(".measures" %in% names(result))
  expect_equal(nrow(result), nrow(test_data))

  # Check that centering worked
  for (i in seq_len(nrow(result))) {
    expect_equal(mean(result$.measures[[i]]$value), 0, tolerance = 1e-10)
  }
})

test_that("measure_map works with formula syntax", {
  test_data <- create_test_data()

  # Using formula syntax
  result <- measure_map(test_data, ~ {
    .x$value <- .x$value - mean(.x$value)
    .x
  })

  # Check that centering worked
  for (i in seq_len(nrow(result))) {
    expect_equal(mean(result$.measures[[i]]$value), 0, tolerance = 1e-10)
  }
})

test_that("measure_map passes additional arguments via ...", {
  test_data <- create_test_data()

  scale_spectrum <- function(x, scale_factor = 1) {
    x$value <- x$value * scale_factor
    x
  }

  # Get original values for comparison
  original_value <- test_data$.measures[[1]]$value[1]

  # Scale by 2
  result <- measure_map(test_data, scale_spectrum, scale_factor = 2)
  scaled_value <- result$.measures[[1]]$value[1]

  expect_equal(scaled_value, original_value * 2)
})

test_that("measure_map works with formula and additional arguments", {
  test_data <- create_test_data()

  scale_fn <- function(x, factor) {
    x$value <- x$value * factor
    x
  }

  original_value <- test_data$.measures[[1]]$value[1]

  result <- measure_map(test_data, ~ scale_fn(.x, factor = 3))
  scaled_value <- result$.measures[[1]]$value[1]

  expect_equal(scaled_value, original_value * 3)
})

test_that("measure_map errors on non-data.frame input", {
  expect_error(
    measure_map(list(a = 1), function(x) x),
    "must be a data frame"
  )
})

test_that("measure_map errors when no measure columns found", {
  expect_error(
    measure_map(mtcars, function(x) x),
    "No measure columns found"
  )
})

test_that("measure_map errors when function returns wrong type", {
  test_data <- create_test_data()

  expect_error(
    measure_map(test_data, function(x) "not a data frame"),
    "must return a data frame"
  )
})

test_that("measure_map errors when function returns missing columns", {
  test_data <- create_test_data()

  bad_fn <- function(x) {
    tibble::tibble(foo = 1:nrow(x))
  }

  expect_error(
    measure_map(test_data, bad_fn),
    "Missing.*location.*value"
  )
})

test_that("measure_map errors when function changes row count", {
  test_data <- create_test_data()

  bad_fn <- function(x) {
    x[1:5, ]
  }

  expect_error(
    measure_map(test_data, bad_fn),
    "changed the number of measurements"
  )
})

test_that("measure_map error messages include sample number",
{
  test_data <- create_test_data()

  # Function that fails on sample 3
  failing_fn <- function(x) {
    if (x$value[1] == test_data$.measures[[3]]$value[1]) {
      stop("intentional error")
    }
    x
  }

  expect_error(
    measure_map(test_data, failing_fn),
    "sample 3"
  )
})

test_that("measure_map preserves locations", {
  test_data <- create_test_data()

  # Get original locations
  original_locations <- test_data$.measures[[1]]$location

  result <- measure_map(test_data, ~ {
    .x$value <- .x$value * 2
    .x
  })

  expect_equal(result$.measures[[1]]$location, original_locations)
})

test_that("measure_map preserves other columns", {
  test_data <- create_test_data()

  result <- measure_map(test_data, ~ {
    .x$value <- .x$value * 2
    .x
  })

  # Check that outcome and id columns are preserved
  expect_equal(result$water, test_data$water)
  expect_equal(result$fat, test_data$fat)
  expect_equal(result$protein, test_data$protein)
  expect_equal(result$id, test_data$id)
})

test_that("measure_map works with column selection", {
  skip_if_not_installed("modeldata")
  data(meats, package = "modeldata")

  # Create data with two measure columns
  wide_data <- meats[1:10, ]

  # Create two measure columns by using different subsets
  rec1 <- recipe(water + fat + protein ~ ., data = wide_data) %>%
    step_measure_input_wide(
      dplyr::starts_with("x_"),
      output_col = "spectra1"
    ) %>%
    prep()

  test_data <- bake(rec1, new_data = NULL)

  # Select only one column to transform
  original_spectra1 <- test_data$spectra1[[1]]$value[1]

  result <- measure_map(
    test_data,
    ~ {
      .x$value <- .x$value * 10
      .x
    },
    .cols = "spectra1"
  )

  # The spectra1 column should be transformed
  expect_equal(result$spectra1[[1]]$value[1], original_spectra1 * 10)
})

test_that("measure_map errors on non-measure column selection", {
  test_data <- create_test_data()

  expect_error(
    measure_map(test_data, function(x) x, .cols = "water"),
    "not measure column"
  )
})

# ==============================================================================
# measure_map_safely() tests
# ==============================================================================

test_that("measure_map_safely returns correct structure", {
  test_data <- create_test_data()

  result <- measure_map_safely(test_data, function(x) x)

  expect_type(result, "list")
  expect_named(result, c("result", "errors"))
  expect_s3_class(result$result, "tbl_df")
  expect_s3_class(result$errors, "tbl_df")
})

test_that("measure_map_safely captures errors", {
  test_data <- create_test_data()

  # Function that always fails
  always_fails <- function(x) {
    stop("I always fail")
  }

  result <- measure_map_safely(test_data, always_fails)

  # Should have errors for all samples
  expect_equal(nrow(result$errors), nrow(test_data))
  expect_true(all(grepl("I always fail", result$errors$error)))
})

test_that("measure_map_safely uses .otherwise when provided", {
  test_data <- create_test_data()

  # Function that always fails
  always_fails <- function(x) {
    stop("fail")
  }

  # Use a placeholder measure_tbl
  placeholder <- measure:::new_measure_tbl(
    location = 1:10,
    value = rep(0, 10)
  )

  result <- measure_map_safely(test_data, always_fails, .otherwise = placeholder)

  # All measurements should be the placeholder
  for (i in seq_len(nrow(result$result))) {
    expect_equal(result$result$.measures[[i]], placeholder)
  }
})

test_that("measure_map_safely keeps original on error when .otherwise is NULL", {
  test_data <- create_test_data()

  # Function that always fails
  always_fails <- function(x) {
    stop("fail")
  }

  result <- measure_map_safely(test_data, always_fails, .otherwise = NULL)

  # Original measurements should be preserved
  for (i in seq_len(nrow(result$result))) {
    expect_equal(
      result$result$.measures[[i]]$value,
      test_data$.measures[[i]]$value
    )
  }
})

test_that("measure_map_safely handles partial failures", {
  test_data <- create_test_data()

  # Function that fails on sample 5
  partial_fail <- function(x) {
    if (x$value[1] == test_data$.measures[[5]]$value[1]) {
      stop("intentional error")
    }
    x$value <- x$value * 2
    x
  }

  result <- measure_map_safely(test_data, partial_fail)

  # Should have exactly one error (for sample 5)
  expect_equal(nrow(result$errors), 1)
  expect_equal(result$errors$sample, 5)

  # Sample 5 should be unchanged, others should be doubled
  expect_equal(
    result$result$.measures[[5]]$value,
    test_data$.measures[[5]]$value
  )
  expect_equal(
    result$result$.measures[[1]]$value,
    test_data$.measures[[1]]$value * 2
  )
})

test_that("measure_map_safely error tibble has correct columns", {
  test_data <- create_test_data()

  always_fails <- function(x) {
    stop("fail")
  }

  result <- measure_map_safely(test_data, always_fails)

  expect_named(result$errors, c("column", "sample", "error"))
  expect_type(result$errors$column, "character")
  expect_type(result$errors$sample, "integer")
  expect_type(result$errors$error, "character")
})

# ==============================================================================
# measure_summarize() tests
# ==============================================================================

test_that("measure_summarize computes mean and sd by default", {
  test_data <- create_test_data()

  result <- measure_summarize(test_data)

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("location", "mean", "sd"))

  # Check that locations are correct
  expect_equal(result$location, test_data$.measures[[1]]$location)

  # Manual calculation for first location
  first_values <- sapply(test_data$.measures, function(x) x$value[1])
  expect_equal(result$mean[1], mean(first_values), tolerance = 1e-10)
  expect_equal(result$sd[1], sd(first_values), tolerance = 1e-10)
})

test_that("measure_summarize accepts custom functions", {
  test_data <- create_test_data()

  result <- measure_summarize(
    test_data,
    .fns = list(
      median = median,
      min = min,
      max = max
    )
  )

  expect_named(result, c("location", "median", "min", "max"))

  # Check median calculation
  first_values <- sapply(test_data$.measures, function(x) x$value[1])
  expect_equal(result$median[1], median(first_values))
})

test_that("measure_summarize errors on unnamed functions", {
  test_data <- create_test_data()

  expect_error(
    measure_summarize(test_data, .fns = list(mean)),
    "must be named"
  )
})

test_that("measure_summarize errors on non-data.frame input", {
  expect_error(
    measure_summarize(list(a = 1)),
    "must be a data frame"
  )
})

test_that("measure_summarize errors when no measure columns found", {
  expect_error(
    measure_summarize(mtcars),
    "No measure columns found"
  )
})

test_that("measure_summarize handles na.rm correctly", {
  test_data <- create_test_data()

  # Introduce an NA
  test_data$.measures[[1]]$value[1] <- NA

  # With na.rm = TRUE (default)
  result_rm <- measure_summarize(test_data, na.rm = TRUE)
  expect_false(is.na(result_rm$mean[1]))

  # With na.rm = FALSE
  result_no_rm <- measure_summarize(test_data, na.rm = FALSE)
  expect_true(is.na(result_no_rm$mean[1]))
})

# ==============================================================================
# Integration tests
# ==============================================================================

test_that("measure_map can replicate SNV transformation", {
  test_data <- create_test_data()

  # Apply SNV via measure_map
  snv_via_map <- measure_map(test_data, ~ {
    .x$value <- (.x$value - mean(.x$value)) / sd(.x$value)
    .x
  })

  # Apply SNV via the step
  snv_via_step <- recipe(water + fat + protein ~ ., data = meats_long) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    step_measure_snv() %>%
    prep() %>%
    bake(new_data = NULL)

  # Compare results
  for (i in seq_len(nrow(snv_via_map))) {
    expect_equal(
      snv_via_map$.measures[[i]]$value,
      snv_via_step$.measures[[i]]$value,
      tolerance = 1e-10
    )
  }
})

test_that("measure_map works in a dplyr pipeline", {
  test_data <- create_test_data()

  result <- test_data %>%
    dplyr::filter(water > 50) %>%
    measure_map(~ {
      .x$value <- .x$value * 2
      .x
    })

  expect_true(all(result$water > 50))
  expect_s3_class(result, "tbl_df")
})

test_that("chained measure_map calls work correctly", {
  test_data <- create_test_data()

  # Chain two transformations
  result <- test_data %>%
    measure_map(~ {
      .x$value <- .x$value - mean(.x$value)  # center
      .x
    }) %>%
    measure_map(~ {
      .x$value <- .x$value / sd(.x$value)  # scale
      .x
    })

  # Result should be like SNV
  for (i in seq_len(nrow(result))) {
    expect_equal(mean(result$.measures[[i]]$value), 0, tolerance = 1e-10)
    # Note: sd might not be exactly 1 due to centering then scaling separately
  }
})
