report_fixture_data <- function() {
  location <- c(8, 9, 10, 11)
  ri <- measure::new_measure_list(list(
    measure::new_measure_tbl(location, c(0.10, 0.55, 0.90, 0.35)),
    measure::new_measure_tbl(location, c(0.08, 0.45, 0.72, 0.28))
  ))
  uv <- measure::new_measure_list(list(
    measure::new_measure_tbl(location, c(0.05, 0.20, 0.31, 0.12)),
    measure::new_measure_tbl(location, c(0.04, 0.17, 0.26, 0.10))
  ))

  data.frame(
    sample_id = c("SEC-001", "SEC-002"),
    batch = c("B-07", "B-07"),
    ri = ri,
    uv = uv,
    stringsAsFactors = FALSE
  )
}

test_that("an installed technique pack extends report sections", {
  lib <- withr::local_tempdir()
  before_libs <- .libPaths()
  install_test_pack("measure.report.fixture", lib)

  was_loaded <- isNamespaceLoaded("measure.report.fixture")
  loadNamespace("measure.report.fixture", lib.loc = lib)
  withr::defer(if (!was_loaded) unloadNamespace("measure.report.fixture"))

  data <- report_fixture_data()
  sec_summary <- data.frame(
    sample_id = data$sample_id,
    number_average_mw = c(42000, 58000),
    dispersity = c(1.82, 1.67),
    stringsAsFactors = FALSE
  )
  results <- getExportedValue("measure.report.fixture", "new_sec_results")(
    data,
    measures = "ri",
    id_cols = c("sample_id", "batch"),
    sec_summary = sec_summary
  )

  expect_s3_class(results, "measure_sec_results")
  expect_s3_class(results, "measure_results")
  method <- getS3method(
    "measure_report_sections",
    "measure_sec_results",
    optional = TRUE
  )
  expect_true(is.function(method))
  expect_identical(environment(method), asNamespace("measure.report.fixture"))

  sections <- measure::measure_report_sections(results, type = "summary")
  expect_named(
    sections,
    c("Sample summary", "Measurements", "SEC summary", "SEC molecular weights")
  )
  expect_s3_class(sections[["Sample summary"]], "data.frame")
  expect_identical(
    sections[["Sample summary"]]$sample_id,
    data$sample_id
  )
  expect_identical(sections[["Sample summary"]]$batch, data$batch)
  expect_equal(sections[["Sample summary"]]$mean, c(0.475, 0.3825))
  expect_s3_class(sections[["Measurements"]], "ggplot")
  expect_identical(
    sections[["SEC summary"]]$sample_id,
    c("SEC-001", "SEC-002")
  )
  expect_equal(
    sections[["SEC summary"]]$number_average_mw,
    c(42000, 58000)
  )
  expect_equal(sections[["SEC summary"]]$dispersity, c(1.82, 1.67))
  expect_s3_class(sections[["SEC molecular weights"]], "ggplot")
  expect_equal(
    ggplot2::ggplot_build(sections[["SEC molecular weights"]])$data[[1]]$y,
    c(42000, 58000)
  )
  expect_identical(.libPaths(), before_libs)
})

test_that("the extension renders one offline HTML report", {
  skip_if_not_installed("quarto")
  skip_if(!nzchar(Sys.which("quarto")), "Quarto CLI not found")

  lib <- withr::local_tempdir()
  install_test_pack("measure.report.fixture", lib)
  loadNamespace("measure.report.fixture", lib.loc = lib)
  withr::defer(unloadNamespace("measure.report.fixture"))

  data <- report_fixture_data()
  results <- getExportedValue("measure.report.fixture", "new_sec_results")(
    data,
    measures = "ri",
    id_cols = c("sample_id", "batch"),
    sec_summary = data.frame(
      sample_id = data$sample_id,
      number_average_mw = c(42000, 58000),
      stringsAsFactors = FALSE
    )
  )
  output_file <- withr::local_tempfile(fileext = ".html")

  rendered <- measure::render_measure_report(
    results,
    output_file = output_file,
    title = "Measurement results",
    open = FALSE,
    quiet = TRUE
  )

  expect_true(file.exists(rendered))
  html <- paste(readLines(rendered, warn = FALSE), collapse = "\n")
  expect_match(html, "Measurement results", fixed = TRUE)
  expect_match(html, "SEC summary", fixed = TRUE)
  expect_match(html, "SEC molecular weights", fixed = TRUE)
  expect_match(html, "SEC-001", fixed = TRUE)
  images <- gregexpr("<img[^>]+src=\"data:image/png;base64,", html, perl = TRUE)
  expect_length(regmatches(html, images)[[1L]], 2L)
})
