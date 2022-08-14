test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("step baseline runs", {
  expect_message(step_baseline())
})

test_that("robust baseline subtraction works", {
  meats_sub <-
    meats_long |>
    dplyr::group_by(id) |>
    subtract_rf_baseline(yvar = transmittance)

  expect_snapshot(dplyr::glimpse(meats_sub))
})
