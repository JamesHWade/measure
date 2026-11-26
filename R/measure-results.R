#' Create a processed measurement results object
#'
#' `measure_results()` keeps processed measurements together with the column
#' and metadata selections used to interpret them. The measurement data are
#' retained as supplied.
#'
#' @param data A baked data frame, a `measure_list` or `measure_nd_list`, or a
#'   single `measure_tbl` or `measure_nd_tbl`.
#' @param measures Name of the measurement column in `data`. Required when a
#'   data frame contains more than one measurement column.
#' @param id_cols Names of sample metadata columns to use when summarizing the
#'   results. The default follows [measure_summary_table()] and keeps scalar
#'   columns. Use `character()` to omit them.
#'
#' @return An ordinary S3 list with components `data`, `measures`, and
#'   `id_cols`, and class `measure_results`. Subclasses can be added by
#'   prefixing the class vector. The original measurement objects, including
#'   n-dimensional declarations and value metadata, remain in `data`.
#'
#' @details
#' `summary()` returns one row per sample using [measure_summary_table()].
#' Use [ggplot2::autoplot()] to draw the selected signal and
#' [render_measure_report()] to export the summary and plot.
#'
#' @examples
#' samples <- tibble::tibble(
#'   sample_id = c("A", "B"),
#'   signal = new_measure_list(list(
#'     new_measure_tbl(c(210, 254), c(0.12, 0.18)),
#'     new_measure_tbl(c(210, 254), c(0.20, 0.41))
#'   ))
#' )
#' results <- measure_results(samples, id_cols = "sample_id")
#' summary(results)
#' results
#'
#' @export
measure_results <- function(data, measures = NULL, id_cols = NULL) {
  measurement_table_input(data, measures, id_cols)

  structure(
    list(
      data = data,
      measures = measures,
      id_cols = id_cols
    ),
    class = "measure_results"
  )
}

#' @rdname measure_results
#' @param object A `measure_results` object.
#' @param ... Reserved for method extensions.
#' @param na.rm Whether to omit missing values from the summary statistics.
#' @export
summary.measure_results <- function(object, ..., na.rm = TRUE) {
  measure_summary_table(
    object$data,
    measures = object$measures,
    id_cols = object$id_cols,
    na.rm = na.rm
  )
}

#' @rdname measure_results
#' @param x A `measure_results` object.
#' @export
print.measure_results <- function(x, ...) {
  input <- measurement_table_input(x$data, x$measures, x$id_cols)
  cat("<measure_results>\n")
  cat("  Members: ", length(input$members), "\n", sep = "")
  cat("  Dimensions: ", measure_ndim(input$members), "D\n", sep = "")
  if (is.data.frame(x$data) && !is_measure_tbl(x$data)) {
    cat("  Measure column: ", input$measures, "\n", sep = "")
  }

  invisible(x)
}
