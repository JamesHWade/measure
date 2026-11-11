whittaker_dense_oracle <- function(y, w, lambda) {
  n <- length(y)
  D <- matrix(0, nrow = n - 2L, ncol = n)
  for (i in seq_len(n - 2L)) {
    D[i, i:(i + 2L)] <- c(1, -2, 1)
  }

  as.numeric(solve(diag(w) + lambda * crossprod(D), w * y))
}

test_that("sparse Whittaker smoothing matches the dense objective", {
  withr::local_seed(136L)

  for (n in c(3L, 4L, 6L, 20L, 101L)) {
    y <- stats::rnorm(n)
    w <- stats::runif(n, min = 0.25, max = 1.75)

    for (lambda in c(0.01, 10, 1e5)) {
      result <- .whittaker_smooth(y, w, lambda)
      expected <- whittaker_dense_oracle(y, w, lambda)
      D <- matrix(0, nrow = n - 2L, ncol = n)
      for (i in seq_len(n - 2L)) {
        D[i, i:(i + 2L)] <- c(1, -2, 1)
      }
      normal_matrix <- diag(w) + lambda * crossprod(D)
      tolerance <- max(1e-8, 20 * kappa(normal_matrix) * .Machine$double.eps)

      expect_lte(
        max(abs(result - expected)),
        tolerance * max(1, max(abs(expected)))
      )
    }
  }
})

test_that("rank deficient weights return the dense minimum norm solution", {
  n <- 9L
  y <- c(1.2, -0.8, 2.1, 4.4, -3.2, 0.7, 2.8, -1.1, 5.3)
  lambda <- 2.5
  D <- matrix(0, nrow = n - 2L, ncol = n)
  for (i in seq_len(n - 2L)) {
    D[i, i:(i + 2L)] <- c(1, -2, 1)
  }

  zero_weights <- numeric(n)
  expect_identical(
    .whittaker_smooth(y, zero_weights, lambda),
    numeric(n)
  )

  for (k in c(1L, 5L, n)) {
    one_weight <- numeric(n)
    one_weight[k] <- 1
    result <- .whittaker_smooth(y, one_weight, lambda)
    t <- seq_len(n) - (n + 1) / 2
    sum_t_squared <- sum(t^2)
    expected_affine <- y[k] *
      (1 / n + t * t[k] / sum_t_squared) /
      (1 / n + t[k]^2 / sum_t_squared)
    normal_matrix <- diag(one_weight) + lambda * crossprod(D)
    expected_ginv <- as.numeric(
      MASS::ginv(normal_matrix) %*% (one_weight * y)
    )

    expect_equal(result, expected_affine, tolerance = 1e-10)
    expect_equal(result, expected_ginv, tolerance = 1e-10)
  }

  two_weights <- numeric(n)
  two_weights[c(2L, 8L)] <- c(0.8, 1.4)
  result <- .whittaker_smooth(y, two_weights, lambda)
  normal_matrix <- diag(two_weights) + lambda * crossprod(D)
  expected_ginv <- as.numeric(
    MASS::ginv(normal_matrix) %*% (two_weights * y)
  )
  expect_equal(result, expected_ginv, tolerance = 1e-10)

  expect_equal(
    .whittaker_smooth(c(2, 7), c(1, 3), lambda),
    c(2, 7)
  )
})

test_that("affine signals remain stable at high lambda", {
  n <- 81L
  x <- seq_len(n)
  signals <- list(
    constant = rep(3.25, n),
    affine = 3.25 + 0.075 * x
  )

  for (weight in c(1e-6, 0.01, 1)) {
    for (signal in signals) {
      result <- .whittaker_smooth(
        signal,
        rep(weight, n),
        lambda = 1e12
      )

      expect_equal(result, signal, tolerance = 1e-8)
    }
  }
})

test_that("tiny positive lambda does not require its reciprocal", {
  withr::local_seed(136L)
  y <- stats::rnorm(20)
  w <- stats::runif(20, min = 0.5, max = 1.5)

  result <- .whittaker_smooth(y, w, lambda = 1e-300)

  expect_equal(result, y, tolerance = 1e-12)
})

test_that("mixed Whittaker solution has small backward and affine moments", {
  withr::local_seed(824L)
  n <- 31L
  y <- stats::rnorm(n)
  w <- stats::runif(n, min = 0.2, max = 1.5)
  D <- matrix(0, nrow = n - 2L, ncol = n)
  for (i in seq_len(n - 2L)) {
    D[i, i:(i + 2L)] <- c(1, -2, 1)
  }
  t <- seq_len(n) - mean(seq_len(n))

  for (lambda in c(0.01, 1e5)) {
    z <- .whittaker_smooth(y, w, lambda)
    if (lambda >= 1) {
      C <- D
      a <- 1 / lambda
    } else {
      C <- sqrt(lambda) * D
      a <- 1
    }
    u <- as.numeric(C %*% z / a)
    mixed_system <- rbind(
      cbind(diag(w), t(C)),
      cbind(C, -a * diag(n - 2L))
    )
    rhs <- c(w * y, numeric(n - 2L))
    solution <- c(z, u)
    backward_error <- max(abs(mixed_system %*% solution - rhs)) /
      (norm(mixed_system, type = "I") * max(abs(solution)) + max(abs(rhs)))

    expect_lt(backward_error, 1e-10)

    weighted_residual <- w * (z - y)
    expect_lt(
      abs(sum(weighted_residual)),
      1e-10 * max(1, sum(abs(weighted_residual)))
    )
    expect_lt(
      abs(sum(t * weighted_residual)),
      1e-10 * max(1, sum(abs(t * weighted_residual)))
    )
  }
})

test_that("large sparse Whittaker smoothing stays finite and affine", {
  n <- 50000L
  y <- 1.5 + 2e-4 * seq_len(n)

  result <- .whittaker_smooth(y, rep(1, n), lambda = 1e5)

  expect_type(result, "double")
  expect_length(result, n)
  expect_true(all(is.finite(result)))
  expect_lt(max(abs(result - y)), 1e-7)
})

test_that("sparse solve failures are reported without a dense fallback", {
  local_mocked_bindings(
    solve = function(...) stop("fixture failure", call. = FALSE),
    .package = "Matrix"
  )

  expect_error(
    .whittaker_smooth(seq_len(10), rep(1, 10), lambda = 1e5),
    "Sparse Whittaker system solve failed: fixture failure",
    fixed = TRUE
  )
})

test_that("extreme weights either preserve affine signals or fail clearly", {
  signal <- as.numeric(seq_len(10))
  result <- tryCatch(
    .whittaker_smooth(signal, rep(1e-100, 10), lambda = 1e5),
    error = identity
  )

  if (inherits(result, "error")) {
    expect_match(
      conditionMessage(result),
      "Sparse Whittaker system solve failed"
    )
  } else {
    expect_true(all(is.finite(result)))
    expect_lt(max(abs(result - signal)), 1e-8)
  }
})
