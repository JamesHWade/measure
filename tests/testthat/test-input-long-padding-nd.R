test_that("nD padding uses the union of observed coordinate tuples", {
  data <- tibble::tibble(
    sample_id = c(rep("a", 3), rep("b", 2)),
    axis_1 = c(1, 1, 2, 1, 2),
    axis_2 = c(1, 2, 1, 1, 1),
    value = c(10, NA_real_, 30, 40, 50),
    outcome = c("A", "A", "A", "B", "B")
  )

  result <- recipes::recipe(outcome ~ ., data = data) |>
    recipes::update_role(sample_id, new_role = "id") |>
    step_measure_input_long(
      value,
      location = dplyr::vars(axis_1, axis_2),
      pad = TRUE
    ) |>
    recipes::prep() |>
    recipes::bake(new_data = NULL)

  first <- result$.measures[[1]]
  second <- result$.measures[[2]]
  expected_coordinates <- tibble::tibble(
    location_1 = c(1, 1, 2),
    location_2 = c(1, 2, 1)
  )

  expect_s3_class(result$.measures, "measure_nd_list")
  expect_equal(first$location_1, expected_coordinates$location_1)
  expect_equal(first$location_2, expected_coordinates$location_2)
  expect_equal(second$location_1, expected_coordinates$location_1)
  expect_equal(second$location_2, expected_coordinates$location_2)
  expect_equal(first$value, c(10, NA_real_, 30))
  expect_equal(second$value, c(40, NA_real_, 50))
  expect_equal(nrow(second), 3L)
  expect_equal(nrow(first), nrow(second))
})

test_that("nD padding preserves metadata and works for three axes", {
  data <- tibble::tibble(
    sample_id = c(rep(1L, 4), rep(2L, 3)),
    axis_1 = c(1L, 1L, 2L, 2L, 1L, 2L, 2L),
    axis_2 = c(1L, 2L, 1L, 2L, 1L, 1L, 2L),
    axis_3 = c(1L, 1L, 1L, 1L, 1L, 1L, 1L),
    value = seq_len(7),
    outcome = rep(c("A", "B"), c(4, 3))
  )

  result <- recipes::recipe(outcome ~ ., data = data) |>
    recipes::update_role(sample_id, new_role = "id") |>
    step_measure_input_long(
      value,
      location = dplyr::vars(axis_1, axis_2, axis_3),
      dim_names = c("first", "second", "third"),
      dim_units = c("a", "b", "c"),
      pad = TRUE
    ) |>
    recipes::prep() |>
    recipes::bake(new_data = NULL)

  expect_equal(vapply(result$.measures, nrow, integer(1)), c(4L, 4L))
  expect_equal(
    measure_dim_names(result$.measures[[1]]),
    c("first", "second", "third")
  )
  expect_equal(measure_dim_units(result$.measures[[2]]), c("a", "b", "c"))
  expect_equal(result$.measures[[1]]$value, seq_len(4))
  expect_equal(result$.measures[[2]]$value, c(5, NA_real_, 6, 7))
})

test_that("nD padding rejects duplicate coordinate tuples", {
  data <- tibble::tibble(
    sample_id = c(1, 1, 2),
    axis_1 = c(1, 1, 1),
    axis_2 = c(1, 1, 2),
    value = c(10, 11, 12),
    outcome = c("A", "A", "B")
  )

  expect_error(
    recipes::recipe(outcome ~ ., data = data) |>
      recipes::update_role(sample_id, new_role = "id") |>
      step_measure_input_long(
        value,
        location = dplyr::vars(axis_1, axis_2),
        pad = TRUE
      ) |>
      recipes::prep() |>
      recipes::bake(new_data = NULL),
    "duplicate coordinate tuples"
  )
})

test_that("nD padding rejects missing and non-finite coordinates", {
  data <- tibble::tibble(
    sample_id = c(1, 1, 2),
    axis_1 = c(1, NA_real_, 1),
    axis_2 = c(1, 1, 1),
    value = c(10, 11, 12),
    outcome = c("A", "A", "B")
  )

  expect_error(
    recipes::recipe(outcome ~ ., data = data) |>
      recipes::update_role(sample_id, new_role = "id") |>
      step_measure_input_long(
        value,
        location = dplyr::vars(axis_1, axis_2),
        pad = TRUE
      ) |>
      recipes::prep() |>
      recipes::bake(new_data = NULL),
    "finite"
  )
})

test_that("nD no-pad keeps sparse samples and emits no padding warning", {
  data <- tibble::tibble(
    sample_id = c(1, 1, 2),
    axis_1 = c(1, 2, 1),
    axis_2 = c(1, 1, 1),
    value = c(10, 20, 30),
    outcome = c("A", "A", "B")
  )

  result <- expect_no_warning(
    recipes::recipe(outcome ~ ., data = data) |>
      recipes::update_role(sample_id, new_role = "id") |>
      step_measure_input_long(
        value,
        location = dplyr::vars(axis_1, axis_2)
      ) |>
      recipes::prep() |>
      recipes::bake(new_data = NULL)
  )

  expect_equal(vapply(result$.measures, nrow, integer(1)), c(2L, 1L))
})

test_that("nD padding applies independently when baking new data", {
  training <- tibble::tibble(
    sample_id = c(1, 1, 2, 2),
    axis_1 = c(1, 2, 1, 2),
    axis_2 = c(1, 1, 1, 1),
    value = c(10, 20, 30, 40),
    outcome = c("A", "A", "B", "B")
  )
  new_data <- tibble::tibble(
    sample_id = c(3, 3, 4),
    axis_1 = c(1, 3, 1),
    axis_2 = c(1, 1, 1),
    value = c(50, 60, 70),
    outcome = c("C", "C", "D")
  )

  recipe <- recipes::recipe(outcome ~ ., data = training) |>
    recipes::update_role(sample_id, new_role = "id") |>
    step_measure_input_long(
      value,
      location = dplyr::vars(axis_1, axis_2),
      pad = TRUE
    ) |>
    recipes::prep()
  result <- recipes::bake(recipe, new_data = new_data)

  expect_equal(vapply(result$.measures, nrow, integer(1)), c(2L, 2L))
  expect_equal(result$.measures[[1]]$value, c(50, 60))
  expect_equal(result$.measures[[2]]$value, c(70, NA_real_))
})

test_that("chained nD input steps keep preserved list columns aligned", {
  data <- tibble::tibble(
    sample_id = c(rep("a", 3), rep("b", 2)),
    axis_1 = c(1, 1, 2, 1, 2),
    axis_2 = c(1, 2, 1, 1, 1),
    first = c(10, 20, 30, 40, 50),
    second = c(11, 21, 31, 41, 51),
    outcome = c("A", "A", "A", "B", "B")
  )

  result <- recipes::recipe(outcome ~ ., data = data) |>
    recipes::update_role(sample_id, new_role = "id") |>
    step_measure_input_long(
      first,
      location = dplyr::vars(axis_1, axis_2),
      col_name = "first_measure",
      pad = TRUE
    ) |>
    step_measure_input_long(
      second,
      location = dplyr::vars(axis_1, axis_2),
      col_name = "second_measure",
      pad = TRUE
    ) |>
    recipes::prep() |>
    recipes::bake(new_data = NULL)

  expect_equal(nrow(result), 2L)
  expect_equal(vapply(result$first_measure, nrow, integer(1)), c(3L, 3L))
  expect_equal(vapply(result$second_measure, nrow, integer(1)), c(3L, 3L))
  expect_equal(result$second_measure[[2]]$value, c(41, NA_real_, 51))
  expect_equal(result$axis_1[[2]], c(1, 2))
  expect_equal(result$axis_2[[2]], c(1, 1))
})

test_that("empty nD bake retains dimensional prototype and metadata", {
  data <- tibble::tibble(
    sample_id = 1:2,
    axis_1 = c(1, 2),
    axis_2 = c(1, 1),
    axis_3 = c(1, 2),
    value = c(10, 20),
    outcome = c("A", "B")
  )
  recipe <- recipes::recipe(outcome ~ ., data = data) |>
    recipes::update_role(sample_id, new_role = "id") |>
    step_measure_input_long(
      value,
      location = dplyr::vars(axis_1, axis_2, axis_3),
      dim_names = c("first", "second", "third"),
      dim_units = c("a", "b", "c")
    ) |>
    recipes::prep()

  result <- recipes::bake(recipe, new_data = data[0, ])
  prototype <- attr(result$.measures, "ptype")

  expect_length(result$.measures, 0L)
  expect_equal(measure_ndim(result$.measures), 3L)
  expect_equal(measure_dim_names(prototype), c("first", "second", "third"))
  expect_equal(measure_dim_units(prototype), c("a", "b", "c"))
})
