#' Export processed measurements as tables
#'
#' After baking a recipe, use `measure_summary_table()` for one row per
#' sample or `measure_slice_table()` for one row per measurement point.
#'
#' @param data A baked data frame, a `measure_list` or `measure_nd_list`, or
#'   a single `measure_tbl` or `measure_nd_tbl`.
#' @param measures Name of the measurement column in `data`. Required when
#'   the data frame contains more than one measurement column.
#' @param id_cols Names of sample metadata columns to carry into the table.
#'   The default keeps scalar columns such as identifiers and outcomes. Use
#'   `character()` to omit them. A metadata name that conflicts with an
#'   output field receives a `.data` suffix and a number if needed.
#' @param na.rm Whether to omit missing values from the summary statistics.
#'   Missing counts always include both `NA` and `NaN`.
#'
#' @return A tibble. Both tables identify each input row by its one-based
#'   `row_id`. Named collections also include `member_name`. Declared signal
#'   quantity and unit appear in `value_quantity` and `value_unit`. For nD
#'   measurements, the `axis_names` and `axis_units` list columns retain
#'   each member's dimension declarations.
#'
#'   The summary reports `n_dimensions`, `n_points`, `n_missing`,
#'   `n_observed`, `mean`,
#'   `sd` (sample standard deviation), `min`, and `max`. Empty members and
#'   members with only missing values yield missing statistics.
#'
#'   The slice table retains point order, including repeated coordinates
#'   and missing values. It gives the one-based `point` within each sample,
#'   the original `location` or `location_1`, `location_2`, ... coordinates,
#'   and the signal `value`. An empty member contributes zero point rows.
#'
#' @seealso [step_measure_output_long()], [step_measure_output_wide()] to
#'   reshape measurements inside a recipe.
#' @examples
#' samples <- tibble::tibble(
#'   sample_id = c("A", "B"),
#'   absorbance = new_measure_list(list(
#'     new_measure_tbl(c(210, 254), c(0.12, NA)),
#'     new_measure_tbl(c(210, 254), c(0.20, 0.41))
#'   ))
#' )
#' measure_summary_table(samples)
#' measure_slice_table(samples)
#' @export
measure_summary_table <- function(
  data,
  measures = NULL,
  id_cols = NULL,
  na.rm = TRUE
) {
  rlang::check_bool(na.rm)
  input <- measurement_table_input(data, measures, id_cols)
  values <- lapply(input$members, "[[", "value")
  n_points <- lengths(values, use.names = FALSE)
  n_missing <- vapply(
    values,
    function(x) sum(is.na(x)),
    integer(1),
    USE.NAMES = FALSE
  )
  statistics <- vapply(
    values,
    function(x) {
      if (na.rm) {
        x <- x[!is.na(x)]
      }
      if (!length(x) || anyNA(x)) {
        return(rep(NA_real_, 4))
      }
      c(mean(x), stats::sd(x), min(x), max(x))
    },
    numeric(4),
    USE.NAMES = FALSE
  )
  fields <- tibble::tibble(
    n_dimensions = rep(measure_ndim(input$members), length(values)),
    n_points,
    n_missing,
    n_observed = n_points - n_missing,
    mean = statistics[1, ],
    sd = statistics[2, ],
    min = statistics[3, ],
    max = statistics[4, ]
  )
  measurement_table_bind(input$ids, dplyr::bind_cols(input$labels, fields))
}

#' @rdname measure_summary_table
#' @export
measure_slice_table <- function(data, measures = NULL, id_cols = NULL) {
  input <- measurement_table_input(data, measures, id_cols)
  members <- input$members
  sizes <- vapply(members, nrow, integer(1), USE.NAMES = FALSE)
  rows <- rep(seq_along(members), sizes)
  # Strip measurement classes before row-binding: dimension declarations
  # belong to individual members and are carried separately in the labels.
  points <- lapply(members, tibble::as_tibble)
  if (!length(points)) {
    points <- list(tibble::as_tibble(attr(members, "ptype", exact = TRUE)))
  }
  fields <- dplyr::bind_cols(
    input$labels[rows, ],
    tibble::tibble(point = sequence(sizes)),
    dplyr::bind_rows(unname(points))
  )
  measurement_table_bind(input$ids[rows, ], fields)
}

measurement_table_input <- function(data, measures, id_cols) {
  if (is_measure_nd_tbl(data)) {
    data <- new_measure_nd_list(list(data))
  } else if (is_measure_tbl(data)) {
    data <- new_measure_list(list(data))
  }
  if (is_measure_list(data)) {
    data <- tibble::tibble(.measures = data)
  }
  candidates <- find_measure_cols(data)
  if (is.null(measures)) {
    if (!length(candidates)) {
      cli::cli_abort("No measurement column was found in {.arg data}.")
    }
    if (length(candidates) > 1L) {
      cli::cli_abort(
        "Multiple measure columns found. Select one with {.arg measures}."
      )
    }
    measures <- candidates
  }
  if (!rlang::is_string(measures) || !measures %in% candidates) {
    cli::cli_abort(
      "{.arg measures} must name one measurement column in {.arg data}."
    )
  }
  members <- data[[measures]]
  if (is.null(id_cols)) {
    scalar <- vapply(
      data,
      function(x) !is.list(x) && is.null(dim(x)),
      logical(1)
    )
    id_cols <- names(data)[scalar]
  }
  if (
    !is.character(id_cols) ||
      anyDuplicated(id_cols) ||
      !all(id_cols %in% setdiff(names(data), candidates))
  ) {
    cli::cli_abort(
      "{.arg id_cols} must contain unique names of metadata columns in {.arg data}."
    )
  }

  metadata <- measure_value_metadata(members)
  labels <- tibble::tibble(
    row_id = seq_along(members),
    member_name = names(members),
    value_quantity = rep(metadata$quantity %||% NA_character_, length(members)),
    value_unit = rep(metadata$unit %||% NA_character_, length(members))
  )
  if (is_measure_nd_list(members)) {
    labels$axis_names <- unname(lapply(members, measure_dim_names))
    labels$axis_units <- unname(lapply(members, measure_dim_units))
  }
  list(
    measures = measures,
    members = members,
    ids = tibble::as_tibble(data)[id_cols],
    labels = labels
  )
}

measurement_table_bind <- function(ids, fields) {
  clashes <- names(ids) %in% names(fields)
  if (any(clashes)) {
    replacements <- make.unique(c(
      names(ids)[!clashes],
      names(fields),
      paste0(names(ids)[clashes], ".data")
    ))
    names(ids)[clashes] <- utils::tail(replacements, sum(clashes))
  }
  dplyr::bind_cols(ids, fields)
}
