# ==============================================================================
# Tests for sample-wise mapping infrastructure
#
# Tests are organized by function type:
# 1. step_measure_map() - Recipe step (PRIMARY interface)
# 2. measure_map() - Exploratory function
# 3. measure_map_safely() - Fault-tolerant exploration
# 4. measure_summarize() - Analysis function
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
# step_measure_map() tests (Recipe Step - PRIMARY INTERFACE)
# ==============================================================================

test_that("step_measure_map works with a named function", {
  my_center <- function(x) {
    x$value <- x$value - mean(x$value)
    x
  }

  rec <- recipe(water + fat + protein ~ ., data = meats_long) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    step_measure_map(my_center) %>%
    prep()

  result <- bake(rec, new_data = NULL)

  # Check that centering worked
  for (i in seq_len(nrow(result))) {
    expect_equal(mean(result$.measures[[i]]$value), 0, tolerance = 1e-10)
  }
})

test_that("step_measure_map works with formula syntax", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    step_measure_map(~ { .x$value <- .x$value * 2; .x }) %>%
    prep()

  result <- bake(rec, new_data = NULL)
  expect_s3_class(result, "tbl_df")
  expect_true(".measures" %in% names(result))
})

test_that("step_measure_map can be chained with other steps", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    step_measure_map(~ { .x$value <- log1p(.x$value); .x }) %>%
    step_measure_snv() %>%
    prep()

  result <- bake(rec, new_data = NULL)

  # After SNV, each sample should have mean 0 and sd 1
  for (i in seq_len(nrow(result))) {
    expect_equal(mean(result$.measures[[i]]$value), 0, tolerance = 1e-10)
    expect_equal(sd(result$.measures[[i]]$value), 1, tolerance = 1e-10)
  }
})

test_that("step_measure_map works on new data", {
  train_ids <- unique(meats_long$id)[1:200]
  test_ids <- unique(meats_long$id)[201:215]

  train_data <- meats_long[meats_long$id %in% train_ids, ]
  test_data <- meats_long[meats_long$id %in% test_ids, ]

  rec <- recipe(water + fat + protein ~ ., data = train_data) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    step_measure_map(~ { .x$value <- .x$value - mean(.x$value); .x }) %>%
    prep()

  result <- bake(rec, new_data = test_data)

  # Check that centering worked on test data
  for (i in seq_len(nrow(result))) {
    expect_equal(mean(result$.measures[[i]]$value), 0, tolerance = 1e-10)
  }
})

test_that("step_measure_map print method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    step_measure_map(~ { .x$value <- .x$value * 2; .x })

  expect_snapshot(rec)

  rec_prep <- prep(rec)
  expect_snapshot(rec_prep)
})

test_that("step_measure_map tidy method works", {
  rec <- recipe(water + fat + protein ~ ., data = meats_long) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    step_measure_map(~ .x, id = "map_test")

  # Before prep
  tidy_before <- tidy(rec, number = 2)
  expect_equal(tidy_before$terms, "<all measure columns>")
  expect_equal(tidy_before$id, "map_test")

  # After prep
  rec_prep <- prep(rec)
  tidy_after <- tidy(rec_prep, number = 2)
  expect_equal(tidy_after$terms, ".measures")
  expect_equal(tidy_after$id, "map_test")
})

test_that("step_measure_map integrates with workflows", {
  skip_if_not_installed("workflows")
  skip_if_not_installed("parsnip")

  library(workflows)
  library(parsnip)

  rec <- recipe(water ~ ., data = meats_long) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    step_measure_map(~ { .x$value <- log1p(.x$value); .x }) %>%
    step_measure_output_wide()

  spec <- linear_reg() %>%
    set_engine("lm")

  wf <- workflow() %>%
    add_recipe(rec) %>%
    add_model(spec)

  # Should fit without error
  fitted <- fit(wf, data = meats_long)
  expect_s3_class(fitted, "workflow")
})

# ==============================================================================
# measure_map() tests (Exploratory Function)
# ==============================================================================

test_that("measure_map works with a simple function", {
  test_data <- create_test_data()

  center_spectrum <- function(x) {
    x$value <- x$value - mean(x$value)
    x
  }

  result <- measure_map(test_data, center_spectrum)

  expect_s3_class(result, "tbl_df")
  expect_true(".measures" %in% names(result))
  expect_equal(nrow(result), nrow(test_data))

  for (i in seq_len(nrow(result))) {
    expect_equal(mean(result$.measures[[i]]$value), 0, tolerance = 1e-10)
  }
})

test_that("measure_map works with formula syntax", {
  test_data <- create_test_data()

  result <- measure_map(test_data, ~ {
    .x$value <- .x$value - mean(.x$value)
    .x
  })

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

  original_value <- test_data$.measures[[1]]$value[1]
  result <- measure_map(test_data, scale_spectrum, scale_factor = 2)
  scaled_value <- result$.measures[[1]]$value[1]

  expect_equal(scaled_value, original_value * 2)
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

test_that("measure_map error messages include sample number", {
  test_data <- create_test_data()

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

  expect_equal(result$water, test_data$water)
  expect_equal(result$fat, test_data$fat)
  expect_equal(result$protein, test_data$protein)
  expect_equal(result$id, test_data$id)
})

test_that("measure_map works with column selection", {
  skip_if_not_installed("modeldata")
  data(meats, package = "modeldata")

  wide_data <- meats[1:10, ]

  rec1 <- recipe(water + fat + protein ~ ., data = wide_data) %>%
    step_measure_input_wide(
      dplyr::starts_with("x_"),
      col_name = "spectra1"
    ) %>%
    prep()

  test_data <- bake(rec1, new_data = NULL)
  original_spectra1 <- test_data$spectra1[[1]]$value[1]

  result <- measure_map(
    test_data,
    ~ { .x$value <- .x$value * 10; .x },
    .cols = "spectra1"
  )

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

  always_fails <- function(x) {
    stop("I always fail")
  }

  result <- measure_map_safely(test_data, always_fails)

  expect_equal(nrow(result$errors), nrow(test_data))
  expect_true(all(grepl("I always fail", result$errors$error)))
})

test_that("measure_map_safely uses .otherwise when provided", {
  test_data <- create_test_data()

  always_fails <- function(x) {
    stop("fail")
  }

  placeholder <- measure:::new_measure_tbl(
    location = 1:10,
    value = rep(0, 10)
  )

  result <- measure_map_safely(test_data, always_fails, .otherwise = placeholder)

  for (i in seq_len(nrow(result$result))) {
    expect_equal(result$result$.measures[[i]], placeholder)
  }
})

test_that("measure_map_safely keeps original on error when .otherwise is NULL", {
  test_data <- create_test_data()

  always_fails <- function(x) {
    stop("fail")
  }

  result <- measure_map_safely(test_data, always_fails, .otherwise = NULL)

  for (i in seq_len(nrow(result$result))) {
    expect_equal(
      result$result$.measures[[i]]$value,
      test_data$.measures[[i]]$value
    )
  }
})

test_that("measure_map_safely handles partial failures", {
  test_data <- create_test_data()

  partial_fail <- function(x) {
    if (x$value[1] == test_data$.measures[[5]]$value[1]) {
      stop("intentional error")
    }
    x$value <- x$value * 2
    x
  }

  result <- measure_map_safely(test_data, partial_fail)

  expect_equal(nrow(result$errors), 1)
  expect_equal(result$errors$sample, 5)

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
  expect_equal(result$location, test_data$.measures[[1]]$location)

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
  test_data$.measures[[1]]$value[1] <- NA

  result_rm <- measure_summarize(test_data, na.rm = TRUE)
  expect_false(is.na(result_rm$mean[1]))

  result_no_rm <- measure_summarize(test_data, na.rm = FALSE)
  expect_true(is.na(result_no_rm$mean[1]))
})

# ==============================================================================
# Integration tests
# ==============================================================================

test_that("measure_map can replicate SNV transformation", {
  test_data <- create_test_data()

  snv_via_map <- measure_map(test_data, ~ {
    .x$value <- (.x$value - mean(.x$value)) / sd(.x$value)
    .x
  })

  snv_via_step <- recipe(water + fat + protein ~ ., data = meats_long) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    step_measure_snv() %>%
    prep() %>%
    bake(new_data = NULL)

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

test_that("prototyping with measure_map transfers to step_measure_map", {

  # This test demonstrates the intended workflow:
  # 1. Prototype with measure_map()
  # 2. Transfer to step_measure_map() for production

  test_data <- create_test_data()

  # Step 1: Prototype a custom transformation
  my_transform <- function(x) {
    x$value <- x$value - min(x$value)  # Shift to zero baseline
    x
  }

  prototype_result <- measure_map(test_data, my_transform)

  # Step 2: Use the same function in a recipe step
  rec <- recipe(water + fat + protein ~ ., data = meats_long) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    step_measure_map(my_transform) %>%
    prep()

  production_result <- bake(rec, new_data = NULL)

  # Results should be identical
  for (i in seq_len(nrow(prototype_result))) {
    expect_equal(
      prototype_result$.measures[[i]]$value,
      production_result$.measures[[i]]$value,
      tolerance = 1e-10
    )
  }
})
