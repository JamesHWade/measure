# This fixture intentionally keeps its package dependency out of Imports. The
# test installs it while measure is loaded from the development checkout.

# jarl-ignore unused_function: The test calls this export from the installed fixture.
new_sec_results <- function(
  data,
  measures,
  id_cols,
  sec_summary
) {
  results <- measure::measure_results(
    data = data,
    measures = measures,
    id_cols = id_cols
  )

  attr(results, "sec_summary") <- sec_summary
  class(results) <- c("measure_sec_results", class(results))
  results
}

# jarl-ignore unused_function: Registered by the fixture NAMESPACE for S3 dispatch.
measure_report_sections.measure_sec_results <- function(x, ...) {
  sections <- NextMethod()
  sec_summary <- attr(x, "sec_summary", exact = TRUE)
  sections[["SEC summary"]] <- sec_summary
  sections[["SEC molecular weights"]] <- ggplot2::ggplot(
    sec_summary,
    ggplot2::aes(
      x = sec_sample_label(sample_id),
      y = number_average_mw
    )
  ) +
    ggplot2::geom_col() +
    ggplot2::labs(
      x = "Sample",
      y = "Number-average molecular weight",
      title = "SEC molecular weights"
    ) +
    ggplot2::theme_minimal()
  sections
}

sec_sample_label <- function(sample_id) {
  paste0("SEC sample ", sample_id)
}
