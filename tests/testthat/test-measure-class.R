# Tests for measure class infrastructure

# ==============================================================================
# measure_tbl tests
# ==============================================================================

test_that("new_measure_tbl creates proper structure", {
  mt <- measure:::new_measure_tbl(location = 1:5, value = rnorm(5))

  expect_s3_class(mt, "measure_tbl")
  expect_s3_class(mt, "tbl_df")
  expect_true(is_measure_tbl(mt))
  expect_equal(nrow(mt), 5)
  expect_equal(ncol(mt), 2)
  expect_named(mt, c("location", "value"))
})

test_that("new_measure_tbl validates inputs", {
  # Non-numeric location
 expect_error(
    measure:::new_measure_tbl(location = letters[1:5], value = rnorm(5)),
    "must be numeric"
  )

  # Non-numeric value
  expect_error(
    measure:::new_measure_tbl(location = 1:5, value = letters[1:5]),
    "must be numeric"
  )

  # Mismatched lengths
  expect_error(
    measure:::new_measure_tbl(location = 1:5, value = rnorm(3)),
    "same length"
  )
})

test_that("as_measure_tbl coerces tibbles", {
  # Create a regular tibble
  tbl <- tibble::tibble(location = 1:5, value = rnorm(5))
  expect_false(is_measure_tbl(tbl))

  # Coerce it
  mt <- measure:::as_measure_tbl(tbl)
  expect_true(is_measure_tbl(mt))
  expect_s3_class(mt, "tbl_df")
})

test_that("as_measure_tbl validates structure", {
  # Missing location column
  expect_error(
    measure:::as_measure_tbl(tibble::tibble(value = 1:5)),
    "location"
  )

  # Missing value column
  expect_error(
    measure:::as_measure_tbl(tibble::tibble(location = 1:5)),
    "value"
  )

  # Not a data frame
  expect_error(
    measure:::as_measure_tbl(1:5),
    "data frame"
  )
})

test_that("as_measure_tbl is idempotent", {
  mt <- measure:::new_measure_tbl(location = 1:5, value = rnorm(5))
  mt2 <- measure:::as_measure_tbl(mt)
  expect_identical(mt, mt2)
})

test_that("is_measure_tbl works correctly", {
  mt <- measure:::new_measure_tbl(location = 1:5, value = rnorm(5))
  tbl <- tibble::tibble(location = 1:5, value = rnorm(5))

  expect_true(is_measure_tbl(mt))
  expect_false(is_measure_tbl(tbl))
  expect_false(is_measure_tbl(list()))
  expect_false(is_measure_tbl(NULL))
})

# ==============================================================================
# measure_list tests
# ==============================================================================

test_that("new_measure_list creates proper structure", {
  # Create list of tibbles
  tibbles <- list(
    tibble::tibble(location = 1:5, value = rnorm(5)),
    tibble::tibble(location = 1:5, value = rnorm(5)),
    tibble::tibble(location = 1:5, value = rnorm(5))
  )

  ml <- measure:::new_measure_list(tibbles)

  expect_s3_class(ml, "measure_list")
  expect_true(is_measure_list(ml))
  expect_length(ml, 3)

  # Each element should be measure_tbl
  for (i in seq_along(ml)) {
    expect_true(is_measure_tbl(ml[[i]]))
  }
})

test_that("new_measure_list validates inputs", {
  # Not a list
  expect_error(
    measure:::new_measure_list(1:5),
    "must be a list"
  )

  # Element not a data frame
  expect_error(
    measure:::new_measure_list(list(1:5)),
    "data frame"
  )
})

test_that("as_measure_list coerces lists", {
  tibbles <- list(
    tibble::tibble(location = 1:5, value = rnorm(5)),
    tibble::tibble(location = 1:5, value = rnorm(5))
  )

  ml <- measure:::as_measure_list(tibbles)
  expect_true(is_measure_list(ml))
})

test_that("as_measure_list is idempotent", {
  tibbles <- list(
    tibble::tibble(location = 1:5, value = rnorm(5))
  )
  ml <- measure:::new_measure_list(tibbles)
  ml2 <- measure:::as_measure_list(ml)
  expect_identical(ml, ml2)
})

test_that("is_measure_list works correctly", {
  ml <- measure:::new_measure_list(list(
    tibble::tibble(location = 1:5, value = rnorm(5))
  ))
  plain_list <- list(tibble::tibble(location = 1:5, value = rnorm(5)))

  expect_true(is_measure_list(ml))
  expect_false(is_measure_list(plain_list))
  expect_false(is_measure_list(list()))
  expect_false(is_measure_list(NULL))
})

test_that("measure_list subsetting preserves class", {
  tibbles <- list(
    tibble::tibble(location = 1:5, value = 1:5),
    tibble::tibble(location = 1:5, value = 6:10),
    tibble::tibble(location = 1:5, value = 11:15)
  )
  ml <- measure:::new_measure_list(tibbles)

  # Single bracket subsetting should preserve class
  subset <- ml[1:2]
  expect_true(is_measure_list(subset))
  expect_length(subset, 2)

  # Double bracket should return measure_tbl
  single <- ml[[1]]
  expect_true(is_measure_tbl(single))
})

test_that("measure_list concatenation preserves class", {
  ml1 <- measure:::new_measure_list(list(
    tibble::tibble(location = 1:5, value = 1:5)
  ))
  ml2 <- measure:::new_measure_list(list(
    tibble::tibble(location = 1:5, value = 6:10)
  ))

  combined <- c(ml1, ml2)
  expect_true(is_measure_list(combined))
  expect_length(combined, 2)
})

# ==============================================================================
# Discovery functions
# ==============================================================================

test_that("find_measure_cols finds measure columns", {
  # Create a data frame with a measure_list column
  tibbles <- list(
    tibble::tibble(location = 1:5, value = rnorm(5)),
    tibble::tibble(location = 1:5, value = rnorm(5))
  )
  ml <- measure:::new_measure_list(tibbles)

  data <- tibble::tibble(
    id = 1:2,
    outcome = rnorm(2),
    .measures = ml
  )

  cols <- find_measure_cols(data)
  expect_equal(cols, ".measures")
})

test_that("find_measure_cols returns empty for no measure columns", {
  data <- tibble::tibble(
    id = 1:2,
    outcome = rnorm(2)
  )

  cols <- find_measure_cols(data)
  expect_equal(cols, character(0))
})

test_that("has_measure_col detects measure columns", {
  # With measure_list class
  tibbles <- list(tibble::tibble(location = 1:5, value = rnorm(5)))
  ml <- measure:::new_measure_list(tibbles)
  data <- tibble::tibble(id = 1, .measures = ml)

  result <- has_measure_col(data)
  expect_equal(result, ".measures")
})

test_that("has_measure_col falls back to name check", {
  # With plain list (no class) but named .measures
  data <- tibble::tibble(
    id = 1,
    .measures = list(tibble::tibble(location = 1:5, value = rnorm(5)))
  )

  # Should work via name fallback
  result <- has_measure_col(data)
  expect_equal(result, ".measures")
})

test_that("has_measure_col errors when no measure column", {
  data <- tibble::tibble(id = 1, value = 1)

  expect_error(
    has_measure_col(data),
    "No measure column"
  )
})

# ==============================================================================
# Integration with recipe workflow
# ==============================================================================

test_that("input steps create measure_list", {
  rec <-
    recipe(water + fat + protein ~ ., data = meats_long) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    prep()

  result <- bake(rec, new_data = NULL)

  # .measures should be a measure_list
  expect_true(is_measure_list(result$.measures))

  # Each element should be a measure_tbl
  expect_true(is_measure_tbl(result$.measures[[1]]))
})

test_that("processing steps preserve measure_list class", {
  rec <-
    recipe(water + fat + protein ~ ., data = meats_long) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    step_measure_snv() %>%
    prep()

  result <- bake(rec, new_data = NULL)

  # .measures should still be a measure_list after SNV
  expect_true(is_measure_list(result$.measures))
})

test_that("chained processing steps preserve measure_list class", {
  rec <-
    recipe(water + fat + protein ~ ., data = meats_long) %>%
    update_role(id, new_role = "id") %>%
    step_measure_input_long(transmittance, location = vars(channel)) %>%
    step_measure_snv() %>%
    step_measure_msc() %>%
    prep()

  result <- bake(rec, new_data = NULL)

  # .measures should still be a measure_list after SNV + MSC
  expect_true(is_measure_list(result$.measures))
})

test_that("wide input creates measure_list", {
  skip_if_not_installed("modeldata")
  data(meats, package = "modeldata")

  rec <-
    recipe(water + fat + protein ~ ., data = meats) %>%
    step_measure_input_wide(dplyr::starts_with("x_")) %>%
    prep()

  result <- bake(rec, new_data = NULL)

  expect_true(is_measure_list(result$.measures))
})

# ==============================================================================
# Formatting and printing
# ==============================================================================

test_that("measure_list formats correctly", {
  tibbles <- list(
    tibble::tibble(location = 1:10, value = rnorm(10)),
    tibble::tibble(location = 1:10, value = rnorm(10))
  )
  ml <- measure:::new_measure_list(tibbles)

  formatted <- format(ml)
  expect_length(formatted, 2)
  expect_true(all(grepl("<meas \\[10\\]>", formatted)))
})

test_that("measure_list has vctrs abbreviation", {
  tibbles <- list(
    tibble::tibble(location = 1:5, value = rnorm(5))
  )
  ml <- measure:::new_measure_list(tibbles)

  abbr <- vctrs::vec_ptype_abbr(ml)
  # The abbreviation should contain "meas" or "mesr" (vctrs list_of format)
  expect_true(
    grepl("meas", abbr, ignore.case = TRUE) ||
      grepl("mesr", abbr, ignore.case = TRUE),
    info = paste("Abbreviation was:", abbr)
  )
})

test_that("measure_list prints informatively", {
  tibbles <- list(
    tibble::tibble(location = 1:10, value = rnorm(10)),
    tibble::tibble(location = 1:15, value = rnorm(15)),
    tibble::tibble(location = 1:10, value = rnorm(10))
  )
  ml <- measure:::new_measure_list(tibbles)

  output <- capture.output(print(ml))
  expect_true(any(grepl("measure_list", output)))
  expect_true(any(grepl("3", output)))  # 3 measurements
})
