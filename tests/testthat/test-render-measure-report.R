test_that("render_measure_report creates a portable HTML report", {
  skip_if_not_installed("quarto")
  skip_if(
    is.null(quarto::quarto_path()) || !file.exists(quarto::quarto_path()),
    "Quarto CLI not found"
  )

  data <- tibble::tibble(
    sample = c("run-A", "run-B"),
    signal = new_measure_list(list(
      new_measure_tbl(c(1, 2, 3), c(1, 2, 3)),
      new_measure_tbl(c(1, 2, 3), c(2, 3, 4))
    ))
  )
  results <- measure_results(data, measures = "signal", id_cols = "sample")
  output_file <- withr::local_tempfile(fileext = ".html")

  rendered <- render_measure_report(
    results,
    output_file,
    title = "<Report & title>",
    type = "summary",
    open = FALSE,
    quiet = TRUE
  )

  expect_identical(rendered, output_file)
  expect_true(file.exists(output_file))
  expect_gt(file.info(output_file)$size, 0)

  html <- paste(readLines(output_file, warn = FALSE), collapse = "\n")
  expect_match(html, "&lt;Report &amp; title&gt;", fixed = TRUE)
  expect_match(html, "Sample summary", fixed = TRUE)
  expect_match(html, "run-A", fixed = TRUE)
  expect_false(grepl("<Report & title>", html, fixed = TRUE))
  resource_tags <- regmatches(
    html,
    gregexpr("<(img|script|link)\\b[^>]*>", html, perl = TRUE)
  )[[1]]
  external_resources <- grepl(
    "(src|href)=\\\"https?://",
    resource_tags,
    perl = TRUE
  )
  expect_false(any(external_resources))
})

test_that("render_measure_report writes Word output", {
  skip_if_not_installed("quarto")
  skip_if(
    is.null(quarto::quarto_path()) || !file.exists(quarto::quarto_path()),
    "Quarto CLI not found"
  )

  data <- tibble::tibble(
    sample = "literal * [label] & $x^2$ ~a~ | next\nline",
    signal = new_measure_list(list(new_measure_tbl(1:3, c(1, 2, 3))))
  )
  results <- measure_results(data, measures = "signal", id_cols = "sample")
  output_file <- withr::local_tempfile(fileext = ".docx")

  rendered <- render_measure_report(
    results,
    output_file,
    title = "Word report",
    type = "summary",
    open = FALSE,
    quiet = TRUE
  )

  expect_identical(rendered, output_file)
  expect_true(file.exists(output_file))
  expect_gt(file.info(output_file)$size, 0)

  if (nzchar(Sys.which("unzip"))) {
    document_xml <- paste(
      system2(
        "unzip",
        c("-p", output_file, "word/document.xml"),
        stdout = TRUE
      ),
      collapse = "\n"
    )
    expect_match(
      document_xml,
      "literal * [label] &amp; $x^2$ ~a~ | next line",
      fixed = TRUE
    )
  }
})

test_that("render_measure_report leaves an existing target when sections fail", {
  skip_if_not_installed("quarto")
  skip_if(
    is.null(quarto::quarto_path()) || !file.exists(quarto::quarto_path()),
    "Quarto CLI not found"
  )

  data <- tibble::tibble(
    sample = "run-A",
    signal = new_measure_list(list(new_measure_tbl(1:3, c(1, 2, 3))))
  )
  results <- measure_results(data, measures = "signal", id_cols = "sample")
  class(results) <- c("failing_measure_results", class(results))
  output_file <- withr::local_tempfile(fileext = ".html")
  writeLines("keep this report", output_file)

  testthat::local_mocked_bindings(
    measure_report_sections = function(x, ...) {
      cli::cli_abort("section generation failed")
    },
    .package = "measure"
  )

  expect_error(
    render_measure_report(results, output_file, open = FALSE, quiet = TRUE),
    "section generation failed"
  )
  expect_identical(readLines(output_file, warn = FALSE), "keep this report")
})
