multiway_grid_sample <- function(shape = c(3L, 2L), sample = 1L, shift = 0) {
  axes <- lapply(shape, seq_len)
  axes[[1L]] <- axes[[1L]] + shift
  names(axes) <- paste0("location_", seq_along(axes))
  grid <- expand.grid(axes)
  signal <- (1 + sample / 10) * rowSums(grid) + sample / 2
  do.call(new_measure_nd_tbl, c(as.list(grid), list(value = signal)))
}

multiway_grid_data <- function(shape = c(3L, 2L)) {
  measures <- lapply(1:6, function(i) multiway_grid_sample(shape, i))
  tibble::tibble(spectrum = new_measure_nd_list(measures))
}

multiway_grid_recipe <- function(kind, data) {
  rec <- recipes::recipe(~., data)
  switch(
    kind,
    mcr = suppressWarnings(step_measure_mcr_als(
      rec,
      spectrum,
      n_components = 1L
    )),
    parafac = step_measure_parafac(
      rec,
      spectrum,
      n_components = 1L,
      center = FALSE
    ),
    tucker = step_measure_tucker(rec, spectrum, ranks = 1L, center = FALSE)
  )
}

test_that("tensor assembly rejects ambiguous or incomplete coordinate axes", {
  complete <- multiway_grid_sample()
  duplicate <- complete[c(1:6, 1), ]
  nonfinite <- complete
  nonfinite$location_1[1L] <- Inf
  missing_level <- complete[complete$location_1 != 1L, ]
  expect_error(
    .nd_list_to_array(list(duplicate)),
    "duplicate coordinate tuples"
  )
  expect_error(.nd_list_to_array(list(nonfinite)), "reference grid")
  expect_error(
    .nd_list_to_array(list(complete, missing_level)),
    "reference grid"
  )
})

test_that("multiway recipes use the trained coordinate grid for held-out scores", {
  skip_if_not_installed("multiway")
  set.seed(742)
  data <- multiway_grid_data()
  point_table <- data$spectrum[[1]]
  shuffled <- tibble::tibble(
    spectrum = new_measure_nd_list(list(
      point_table[nrow(point_table):1L, ]
    ))
  )
  shifted <- tibble::tibble(
    spectrum = new_measure_nd_list(list(
      multiway_grid_sample(shift = 1000)
    ))
  )
  other_shape <- tibble::tibble(
    spectrum = new_measure_nd_list(list(
      multiway_grid_sample(c(2L, 2L))
    ))
  )

  for (kind in c("mcr", "parafac", "tucker")) {
    for (bad in list(shifted, other_shape)) {
      training <- dplyr::bind_rows(data, bad)
      expect_error(
        recipes::prep(multiway_grid_recipe(kind, training)),
        "reference grid"
      )
    }

    fit <- recipes::prep(multiway_grid_recipe(kind, data))
    original <- recipes::bake(fit, new_data = data[1, ])
    expect_named(original, paste0(kind, "_spectrum_1"))
    expect_equal(recipes::bake(fit, new_data = shuffled), original)
    expect_error(recipes::bake(fit, new_data = shifted), "reference grid")
    expect_error(recipes::bake(fit, new_data = other_shape), "reference grid")
  }
})

test_that("MCR-ALS requires two measurement axes and complete training values", {
  rec <- multiway_grid_recipe("mcr", multiway_grid_data(c(2L, 2L, 2L)))
  expect_error(recipes::prep(rec), "MCR-ALS requires 2 measurement axes")
  data <- multiway_grid_data()
  data$spectrum[[1L]]$value[1L] <- Inf
  expect_error(
    recipes::prep(multiway_grid_recipe("mcr", data)),
    "requires finite values"
  )
})

test_that("PARAFAC and Tucker reject four measurement axes before fitting", {
  skip_if_not_installed("multiway")
  data <- multiway_grid_data(c(2L, 2L, 2L, 2L))
  for (kind in c("parafac", "tucker")) {
    expect_error(
      recipes::prep(multiway_grid_recipe(kind, data)),
      "requires 2 or 3 measurement axes"
    )
  }
})
