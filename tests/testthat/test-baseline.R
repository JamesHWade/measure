test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("step baseline runs", {
  expect_message(step_baseline())
})
