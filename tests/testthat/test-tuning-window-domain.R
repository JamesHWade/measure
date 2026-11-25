test_that("smoothing grids cover the requested odd widths", {
  withr::local_seed(913)
  param <- smooth_window()
  expect_equal(dials::value_seq(param, 4), c(3L, 9L, 15L, 21L))
  expect_equal(dials::value_seq(param, 30), seq(3L, 21L, by = 2L))
  candidates <- function(param) {
    c(
      dials::value_sample(param, 200),
      dials::grid_regular(param, levels = 20)[[1]],
      dials::grid_space_filling(param, size = 8, type = "latin_hypercube")[[1]],
      dials::grid_space_filling(param, size = 7, type = "max_entropy")[[1]]
    )
  }
  expect_true(all(candidates(smooth_window(c(4L, 14L))) %in% seq(5L, 13L, 2L)))
  expect_true(all(candidates(smooth_window(c(4L, 6L))) == 5L))
  expect_true(all(
    candidates(dials::range_set(param, c(7L, 13L))) %in% seq(7L, 13L, 2L)
  ))
  scaled <- smooth_window(log10(c(5, 15)), trans = scales::transform_log10())
  expect_equal(range(dials::value_seq(scaled, 20)), c(5L, 15L))
  expect_true(all(candidates(scaled) %in% seq(5L, 15L, 2L)))
  expect_error(smooth_window(c(4L, 4L)), "odd window")
})
