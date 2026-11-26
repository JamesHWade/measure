#' Build report sections for processed measurements
#'
#' A technique pack can register a method for its results subclass, call
#' `NextMethod()` to obtain the core sections, and append its own tables
#' or plots. [render_measure_report()] calls the method in the caller's R
#' session. The same sections can be used directly in a pack's own template.
#'
#' @param x A [measure_results()] object or a technique-pack subclass.
#' @param ... Passed to [ggplot2::autoplot()] by the core method. For example,
#'   use `type = "summary"` for a per-sample summary plot.
#' @return An ordered, uniquely named list. Each name is a section heading.
#'   Each element is a data frame or a ggplot. In the core method,
#'   `Sample summary` contains sample identifiers, point and missing counts,
#'   the mean, standard deviation, range, and declared signal unit.
#'   `Measurements` contains the plot. Use `summary(x)` for the full
#'   measurement summary, including axis metadata.
#' @examples
#' signal <- new_measure_tbl(c(0, 1, 2), c(0.1, 0.8, 0.2))
#' sections <- measure_report_sections(measure_results(signal))
#' sections[["Sample summary"]]
#' @export
measure_report_sections <- function(x, ...) {
  UseMethod("measure_report_sections")
}

#' @export
measure_report_sections.measure_results <- function(x, ...) {
  table <- summary.measure_results(x)
  # measure_summary_table puts the selected sample metadata before row_id.
  ids <- names(table)[seq_len(match("row_id", names(table)) - 1L)]
  if (!length(ids) && "member_name" %in% names(table)) {
    ids <- "member_name"
  }
  if (!length(ids) || anyDuplicated(table[ids]) || anyNA(table[ids])) {
    ids <- c(ids, "row_id")
  }
  fields <- c("n_points", "n_missing", "mean", "sd", "min", "max")
  if (!all(is.na(table$value_unit))) {
    fields <- c(fields, "value_unit")
  }
  list(
    `Sample summary` = table[c(ids, fields)],
    Measurements = ggplot2::autoplot(x, ...)
  )
}

check_measure_report_sections <- function(sections) {
  headings <- names(sections)
  if (
    !is.list(sections) ||
      is.data.frame(sections) ||
      !length(sections) ||
      is.null(headings) ||
      anyNA(headings) ||
      !all(nzchar(trimws(headings))) ||
      anyDuplicated(headings)
  ) {
    cli::cli_abort(
      "{.fn measure_report_sections} must return a list with unique, nonempty section names."
    )
  }
  for (name in headings) {
    section <- sections[[name]]
    if (!is.data.frame(section) && !inherits(section, "ggplot")) {
      cli::cli_abort(
        "Report section {.val {name}} must be a data frame or a ggplot."
      )
    }
  }
  invisible(sections)
}
