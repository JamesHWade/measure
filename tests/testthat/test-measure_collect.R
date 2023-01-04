withr::local_seed(1234)
meats <- modeldata::meats
meats_rec <- recipe(meats) |>
  update_role(water, protein, fat, new_role = "descriptor") |>
  step_measure_collect(starts_with("x_"), shape = "wide")
meats_prep <- prep(meats_rec)
meats_bake <- bake(meats_prep, new_data = NULL)

meats_long_rec <- recipe(meats_long) |>
  update_role(water, protein, fat, id, new_role = "descriptor") |>
  step_measure_collect(channel, transmittance, shape = "long")

meats_long_prep <- prep(meats_long_rec)
meats_long_bake <- bake(meats_long_prep, new_data = NULL)

test_that("Print recipe with step_measure_collect works", {
  expect_snapshot(meats_rec)
  expect_snapshot(meats_long_rec)
})

test_that("Prep recipe with step_measure_collect works",{
  expect_snapshot(meats_prep)
  expect_snapshot(meats_long_prep)
})

test_that("Bake recipe with step_measure_collect works", {
  expect_snapshot(meats_bake)
  expect_snapshot(meats_long_bake)
})

test_that("Tidy recipe with step_measure_collect works", {
  expect_snapshot(tidy(meats_rec))
  expect_snapshot(tidy(meats_long_rec))
})
