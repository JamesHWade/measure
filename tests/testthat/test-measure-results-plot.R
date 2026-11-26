test_that("one-dimensional results use a reproducible signal plot", {
  signal <- new_measure_list(lapply(1:3, function(i) {
    new_measure_tbl(1:3, i + c(0, 1, 2))
  }))
  results <- measure_results(
    tibble::tibble(sample_id = c("A", "B", "C"), signal = signal),
    measures = "signal",
    id_cols = "sample_id"
  )

  first_plot <- ggplot2::autoplot(results)
  second_plot <- ggplot2::autoplot(results)
  first <- ggplot2::ggplot_build(first_plot)$data[[1L]]
  second <- ggplot2::ggplot_build(second_plot)$data[[1L]]

  expect_equal(first$x, second$x)
  expect_equal(first$y, second$y)
  expect_equal(first$y, c(1, 2, 3, 2, 3, 4, 3, 4, 5))
  expect_equal(
    as.character(first_plot$data$.sample),
    rep(c("A", "B", "C"), each = 3)
  )
  expect_equal(length(unique(first$colour)), 3L)

  limited <- ggplot2::autoplot(results, max_spectra = 2L, summary = TRUE)
  layers <- ggplot2::ggplot_build(limited)$data
  expect_identical(levels(limited$data$.sample), c("A", "B"))
  expect_equal(layers[[3L]]$y, c(1.5, 2.5, 3.5))
  expect_match(limited$labels$subtitle, "First 2 of 3")

  single <- ggplot2::autoplot(results, max_spectra = 1L, alpha = 0.4)
  expect_equal(unique(ggplot2::ggplot_build(single)$data[[1L]]$alpha), 0.4)
})


test_that("one-dimensional signal labels use value metadata", {
  signal <- set_measure_value_metadata(
    new_measure_list(list(new_measure_tbl(1:2, c(1, 2)))),
    quantity = "absorbance",
    unit = "AU"
  )
  results <- measure_results(signal)

  plot <- ggplot2::autoplot(results)

  expect_identical(plot$labels$y, "absorbance (AU)")
})


test_that("two-dimensional maps preserve duplicate and irregular coordinates", {
  first <- new_measure_nd_tbl(
    location_1 = c(1, 1, 2, 4),
    location_2 = c(10, 20, 10, 30),
    value = c(1, 2, 3, 4),
    dim_names = c("time", "wavelength"),
    dim_units = c("min", "nm")
  )
  second <- new_measure_nd_tbl(
    location_1 = c(1, 1, 3),
    location_2 = c(10, 10, 40),
    value = c(5, 6, 7),
    dim_names = c("time", "wavelength"),
    dim_units = c("min", "nm")
  )
  results <- measure_results(
    tibble::tibble(
      sample_id = c("A", "B"),
      signal = new_measure_nd_list(list(first, second))
    ),
    measures = "signal",
    id_cols = "sample_id"
  )

  plot <- ggplot2::autoplot(results)
  built <- ggplot2::ggplot_build(plot)
  points <- built$data[[1L]]

  expect_s3_class(plot, "ggplot")
  expect_s3_class(plot$layers[[1L]]$geom, "GeomPoint")
  expect_equal(points$x, c(first$location_1, second$location_1))
  expect_equal(points$y, c(first$location_2, second$location_2))
  expect_equal(length(unique(points$PANEL)), 2L)
  expect_identical(plot$labels$x, "time (min)")
  expect_identical(plot$labels$y, "wavelength (nm)")
})


test_that("two-dimensional maps refuse incompatible axis declarations", {
  first <- new_measure_nd_tbl(
    location_1 = 1:2,
    location_2 = c(10, 20),
    value = c(1, 2),
    dim_names = c("time", "wavelength"),
    dim_units = c("min", "nm")
  )
  second <- new_measure_nd_tbl(
    location_1 = 1:2,
    location_2 = c(10, 20),
    value = c(3, 4),
    dim_names = c("time", "energy"),
    dim_units = c("min", "eV")
  )
  results <- measure_results(
    tibble::tibble(signal = new_measure_nd_list(list(first, second)))
  )

  expect_error(
    ggplot2::autoplot(results),
    "incompatible axis names or units"
  )
})


test_that("higher-dimensional summaries show each sample range and mean", {
  first <- new_measure_nd_tbl(
    location_1 = c(1, 2),
    location_2 = c(10, 20),
    location_3 = c(100, 200),
    value = c(1, 3)
  )
  second <- new_measure_nd_tbl(
    location_1 = c(1, 2),
    location_2 = c(10, 20),
    location_3 = c(100, 200),
    value = c(10, 14)
  )
  results <- measure_results(
    tibble::tibble(
      sample_id = c("A", "B"),
      signal = new_measure_nd_list(list(first, second))
    ),
    measures = "signal",
    id_cols = "sample_id"
  )

  built <- ggplot2::ggplot_build(ggplot2::autoplot(results))
  ranges <- built$data[[1L]]
  means <- built$data[[2L]]

  expect_equal(ranges$ymin, c(1, 10))
  expect_equal(ranges$ymax, c(3, 14))
  expect_equal(means$y, c(2, 12))
  expect_equal(length(unique(ranges$x)), 2L)
})


test_that("empty results and incompatible plot types fail clearly", {
  empty <- measure_results(new_measure_list())
  expect_error(ggplot2::autoplot(empty), "empty")

  signal <- measure_results(
    new_measure_list(list(new_measure_tbl(1:2, c(1, 2))))
  )
  expect_error(
    ggplot2::autoplot(signal, type = "map"),
    "requires two-dimensional"
  )
})
