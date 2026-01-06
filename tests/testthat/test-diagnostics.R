# Tests for diagnostics.R

test_that("fortify.measure_tbl converts to tibble", {
  skip_if_not_installed("ggplot2")

  spec <- new_measure_tbl(location = 1:10, value = rnorm(10))

  result <- fortify.measure_tbl(spec)

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("location", "value"))
  expect_equal(nrow(result), 10)
  expect_equal(result$location, 1:10)
})

test_that("fortify.measure_list converts to tibble with sample column", {
  skip_if_not_installed("ggplot2")

  spec1 <- new_measure_tbl(location = 1:5, value = rnorm(5))
  spec2 <- new_measure_tbl(location = 1:5, value = rnorm(5))
  specs <- new_measure_list(list(spec1, spec2))

  result <- fortify.measure_list(specs)

  expect_s3_class(result, "tbl_df")
  expect_true("sample" %in% names(result))
  expect_true("location" %in% names(result))
  expect_true("value" %in% names(result))
  expect_equal(length(unique(result$sample)), 2)
})

test_that("autoplot.measure_tbl creates ggplot", {
  skip_if_not_installed("ggplot2")

  spec <- new_measure_tbl(location = 1:100, value = sin(1:100 / 10))

  p <- autoplot(spec)

  expect_s3_class(p, "gg")
  expect_s3_class(p, "ggplot")
})

test_that("autoplot.measure_list creates ggplot", {
  skip_if_not_installed("ggplot2")

  spec1 <- new_measure_tbl(location = 1:50, value = sin(1:50 / 5))
  spec2 <- new_measure_tbl(location = 1:50, value = cos(1:50 / 5))
  specs <- new_measure_list(list(spec1, spec2))

  p <- autoplot(specs)

  expect_s3_class(p, "gg")
  expect_s3_class(p, "ggplot")
})

test_that("autoplot.measure_list with summary adds ribbon", {
  skip_if_not_installed("ggplot2")

  spec1 <- new_measure_tbl(location = 1:50, value = sin(1:50 / 5))
  spec2 <- new_measure_tbl(location = 1:50, value = sin(1:50 / 5) + 0.1)
  specs <- new_measure_list(list(spec1, spec2))

  p <- autoplot(specs, summary = TRUE)

  expect_s3_class(p, "ggplot")
  # Check that ribbon layer exists
  layer_classes <- sapply(p$layers, function(l) class(l$geom)[1])
  expect_true("GeomRibbon" %in% layer_classes)
})

test_that("autoplot.measure_list respects max_spectra", {
  skip_if_not_installed("ggplot2")

  # Create 100 spectra
  specs_list <- lapply(1:100, function(i) {
    new_measure_tbl(location = 1:20, value = rnorm(20))
  })
  specs <- new_measure_list(specs_list)

  p <- autoplot(specs, max_spectra = 10)

  expect_s3_class(p, "ggplot")
  # Subtitle should mention subsetting
  expect_true(grepl("10 of 100", p$labels$subtitle, fixed = TRUE))
})

test_that("autoplot.recipe works with prepped recipe", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("recipes")

  data(meats_small, package = "measure")

  rec <- recipes::recipe(water ~ ., data = meats_small) |>
    recipes::update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    step_measure_snv() |>
    recipes::prep(retain = TRUE)

  p <- autoplot(rec, n_samples = 5)

  expect_s3_class(p, "ggplot")
})

test_that("autoplot.recipe errors on unprepped recipe", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("recipes")

  data(meats_small, package = "measure")

  rec <- recipes::recipe(water ~ ., data = meats_small) |>
    step_measure_input_long(transmittance, location = vars(channel))

  expect_error(
    autoplot(rec),
    "must be prepped"
  )
})

test_that("autoplot.recipe summary mode works", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("recipes")

  data(meats_small, package = "measure")

  rec <- recipes::recipe(water ~ ., data = meats_small) |>
    recipes::update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    recipes::prep(retain = TRUE)

  p <- autoplot(rec, which = "summary")

  expect_s3_class(p, "ggplot")
  # Should have ribbon layers
  layer_classes <- sapply(p$layers, function(l) class(l$geom)[1])
  expect_true("GeomRibbon" %in% layer_classes)
})

test_that("plot_measure_comparison works with multiple recipes", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("recipes")

  data(meats_small, package = "measure")

  base_rec <- recipes::recipe(water ~ ., data = meats_small) |>
    recipes::update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel))

  snv_rec <- base_rec |>
    step_measure_snv() |>
    recipes::prep(retain = TRUE)

  raw_rec <- base_rec |>
    recipes::prep(retain = TRUE)

  p <- plot_measure_comparison(
    "Raw" = raw_rec,
    "SNV" = snv_rec,
    n_samples = 3
  )

  expect_s3_class(p, "ggplot")
  # Should have facets
  expect_true(!is.null(p$facet))
})

test_that("plot_measure_comparison errors without recipes", {
  skip_if_not_installed("ggplot2")

  expect_error(
    plot_measure_comparison(),
    "At least one recipe"
  )
})

test_that("plot_measure_comparison errors on unprepped recipe", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("recipes")

  data(meats_small, package = "measure")

  rec <- recipes::recipe(water ~ ., data = meats_small) |>
    step_measure_input_long(transmittance, location = vars(channel))

  expect_error(
    plot_measure_comparison("Unprepped" = rec),
    "must be prepped"
  )
})

test_that("plot_measure_comparison summary_only mode works", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("recipes")

  data(meats_small, package = "measure")

  rec <- recipes::recipe(water ~ ., data = meats_small) |>
    recipes::update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    recipes::prep(retain = TRUE)

  p <- plot_measure_comparison(
    "Test" = rec,
    n_samples = 5,
    summary_only = TRUE
  )

  expect_s3_class(p, "ggplot")
})

test_that("measure_plot_summary creates summary plot", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("recipes")

  data(meats_small, package = "measure")

  rec <- recipes::recipe(water ~ ., data = meats_small) |>
    recipes::update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    recipes::prep(retain = TRUE)

  baked <- recipes::bake(rec, new_data = NULL)
  p <- measure_plot_summary(baked)

  expect_s3_class(p, "ggplot")
})

test_that("measure_plot_summary with range shows min/max", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("recipes")

  data(meats_small, package = "measure")

  rec <- recipes::recipe(water ~ ., data = meats_small) |>
    recipes::update_role(id, new_role = "id") |>
    step_measure_input_long(transmittance, location = vars(channel)) |>
    recipes::prep(retain = TRUE)

  baked <- recipes::bake(rec, new_data = NULL)
  p <- measure_plot_summary(baked, show_range = TRUE)

  expect_s3_class(p, "ggplot")
  # Should have multiple ribbon layers
  layer_classes <- sapply(p$layers, function(l) class(l$geom)[1])
  expect_equal(sum(layer_classes == "GeomRibbon"), 2)
})

test_that("measure_plot_summary errors on non-data.frame", {
  skip_if_not_installed("ggplot2")

  expect_error(
    measure_plot_summary("not a data frame"),
    "must be a data frame"
  )
})

test_that("measure_plot_summary errors when no measure column", {
  skip_if_not_installed("ggplot2")

  df <- data.frame(x = 1:5, y = 1:5)

  expect_error(
    measure_plot_summary(df),
    "No measure column"
  )
})
