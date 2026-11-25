# ============================================================================
# Tucker decomposition across two and three measurement axes
# ============================================================================

tucker_three_axis_measure <- function(values = seq_len(8)) {
  grid <- expand.grid(
    location_1 = 1:2,
    location_2 = 1:2,
    location_3 = 1:2
  )

  new_measure_nd_tbl(
    location_1 = grid$location_1,
    location_2 = grid$location_2,
    location_3 = grid$location_3,
    value = values
  )
}

test_that("Tucker projects three content axes in mode order", {
  measures <- new_measure_nd_list(list(tucker_three_axis_measure()))

  # The first loading is the sample mode and is deliberately not used when
  # projecting a held-out sample. The remaining factors make the expected
  # mode and score order unambiguous without relying on a fitted decomposition.
  identity <- diag(2)
  loadings <- list(
    matrix(1, nrow = 1),
    identity,
    cbind(c(1, 1), c(1, -1)),
    identity
  )

  scores <- .project_tucker(
    measures,
    loadings = loadings,
    core = NULL,
    center_values = NULL,
    scale_values = NULL,
    grid_info = NULL
  )

  expect_equal(
    unname(scores[1, ]),
    c(4, 6, -2, -2, 12, 14, -2, -2)
  )
  expect_equal(dim(scores), c(1L, 8L))
})

test_that("Tucker fits a three-axis tensor with independent content ranks", {
  skip_if_not_installed("multiway")

  measures <- new_measure_nd_list(lapply(seq_len(5), function(i) {
    tucker_three_axis_measure(values = seq_len(8) + i)
  }))

  set.seed(141)
  fit <- suppressWarnings({
    .fit_tucker(
      measures,
      ranks = c(1, 2, 1),
      center = FALSE,
      scale = FALSE,
      max_iter = 2L,
      tol = 1e-6
    )
  })

  expect_equal(
    vapply(fit$loadings, ncol, integer(1)),
    c(2L, 1L, 2L, 1L)
  )
  expect_equal(dim(fit$core), c(2L, 1L, 2L, 1L))
  expect_length(fit$grid_info, 3L)
})

test_that("Tucker prep and bake support held-out three-axis samples", {
  skip_if_not_installed("multiway")

  make_data <- function(offset, n = 5L) {
    tibble::tibble(
      id = seq_len(n),
      spectrum = new_measure_nd_list(lapply(seq_len(n), function(i) {
        tucker_three_axis_measure(values = seq_len(8) + offset + i)
      }))
    )
  }

  train <- make_data(offset = 0)
  held_out <- make_data(offset = 100, n = 2L)

  set.seed(118)
  rec <- recipes::recipe(~., data = train) |>
    step_measure_tucker(
      spectrum,
      ranks = 1,
      center = FALSE,
      max_iter = 2L
    ) |>
    recipes::prep()

  baked <- recipes::bake(rec, new_data = held_out)

  factors <- rec$steps[[1L]]$loadings$spectrum[-1L]
  basis <- Reduce(kronecker, rev(factors))
  expected <- vapply(
    held_out$spectrum,
    function(sample) {
      as.numeric(crossprod(basis, sample$value))
    },
    numeric(1)
  )
  expect_equal(baked$tucker_spectrum_1, expected)
  expect_false("spectrum" %in% names(baked))
})
