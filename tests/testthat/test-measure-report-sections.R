test_that("report sections keep the selected signal and sample identifiers", {
  data <- tibble::tibble(
    sample = c("run-B", "run-A"),
    raw = new_measure_list(list(
      new_measure_tbl(1:3, c(100, 200, 300)),
      new_measure_tbl(1:3, c(200, 300, 400))
    )),
    corrected = new_measure_list(list(
      new_measure_tbl(1:3, c(1, NA, 3)),
      new_measure_tbl(1:3, c(-2, 0, 2))
    ))
  )
  result <- measure_results(data, measures = "corrected")
  sections <- measure_report_sections(result, type = "summary")
  expect_named(sections, c("Sample summary", "Measurements"))
  expect_equal(sections[[1]]$sample, data$sample)
  expect_equal(sections[[1]]$mean, c(2, 0))
  expect_equal(sections[[1]]$n_missing, c(1L, 0L))
  expect_s3_class(sections[[2]], "ggplot")
})

test_that("report rendering rejects ambiguous or unsupported sections", {
  table <- data.frame(x = 1)
  for (bad in list(
    list(),
    list(table),
    setNames(list(table, table), c("Area", "Area")),
    setNames(list(table), " "),
    setNames(list(table), NA_character_)
  )) {
    expect_error(check_measure_report_sections(bad), "section names")
  }
  expect_error(
    check_measure_report_sections(list(Area = 12)),
    "Area.*data frame or a ggplot"
  )
})
