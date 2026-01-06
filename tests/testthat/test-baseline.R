# test_that("step baseline runs", {
#     rec <- recipe(meats_small) |> step_baseline(transmittance)
#   expect_snapshot(
#     print(rec)
#     )
# })

test_that("robust baseline subtraction works", {
  meats_sub <-
    meats_small |>
    dplyr::group_by(id) |>
    subtract_rf_baseline(yvar = transmittance)

  expect_snapshot(dplyr::glimpse(meats_sub))
})
