test_that("Printing recipe with step_measure_collect works", {
  meats <- modeldata::meats
  rec <- recipe(meats) |>
    update_role(water, protein, fat, new_role = "descriptor") |>
    step_measure_collect(starts_with("x_"), shape = "wide")
  expect_snapshot(rec)
})

test_that("Prepping recipe with step_measure_collect works", {
  meats <- modeldata::meats
  rec <- recipe(meats) |>
    update_role(water, protein, fat, new_role = "descriptor") |>
    step_measure_collect(starts_with("x_"), shape = "wide")
  expect_snapshot(prep(rec))
})

test_that("Baking recipe with step_measure_collect works", {
  meats <- modeldata::meats
  rec <- recipe(meats) |>
    update_role(water, protein, fat, new_role = "descriptor") |>
    step_measure_collect(starts_with("x_"), shape = "wide")
  expect_snapshot(prep(rec) |> bake(new_data = NULL))
})
