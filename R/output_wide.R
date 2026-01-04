#' Reorganize Measurements to Separate Columns
#'
#' `step_measure_output_wide` creates a *specification* of a recipe
#'  step that converts measures to multiple columns (i.e., "wide" format).
#' @family input/output steps
#' @inheritParams recipes::step_center
#' @param measures An optional single character string specifying which measure
#'   column to output. If `NULL` (the default) and only one measure column
#'   exists, that column will be used. If multiple measure columns exist and
#'   `measures` is `NULL`, an error will be thrown prompting you to specify
#'   which column to output.
#' @param prefix A character string used to name the new columns.
#' @details
#' This step is designed convert analytical measurements from their internal
#' data structure to separate columns.
#'
#' Wide outputs can be helpful when you want to use standard recipes steps with
#' the measuresments, such as [recipes::step_pca()], [recipes::step_pls()], and
#' so on.
#' @examples
#' library(dplyr)
#'
#' data(glucose_bioreactors)
#' bioreactors_small$batch_sample <- NULL
#'
#' small_tr <- bioreactors_small[1:200, ]
#' small_te <- bioreactors_small[201:210, ]
#'
#' small_rec <-
#'   recipe(glucose ~ ., data = small_tr) |>
#'   update_role(batch_id, day, new_role = "id columns") |>
#'   step_measure_input_wide(`400`:`3050`) |>
#'   prep()
#'
#' # Before reformatting:
#'
#' small_rec |> bake(new_data = small_te)
#'
#' # After reformatting:
#'
#' output_rec <-
#'   small_rec |>
#'   step_measure_output_wide() |>
#'   prep()
#'
#' output_rec |> bake(new_data = small_te)
#'
#' @export

step_measure_output_wide <-
  function(
    recipe,
    prefix = "measure_",
    measures = NULL,
    role = "predictor",
    trained = FALSE,
    skip = FALSE,
    id = rand_id("measure_output_wide")
  ) {
    add_step(
      recipe,
      step_measure_output_wide_new(
        measures = measures,
        prefix = prefix,
        trained = trained,
        role = role,
        skip = skip,
        id = id
      )
    )
  }

step_measure_output_wide_new <-
  function(measures, prefix, role, trained, skip, id) {
    step(
      subclass = "measure_output_wide",
      measures = measures,
      prefix = prefix,
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_measure_output_wide <- function(x, training, info = NULL, ...) {
  check_has_measure(training, match.call())

  # Resolve which column to output
  measure_cols <- find_measure_cols(training)

  if (is.null(x$measures)) {
    if (length(measure_cols) > 1) {
      cli::cli_abort(c(
        "Multiple measure columns found: {.field {measure_cols}}",
        "i" = "Use {.arg measures} to specify which column to output."
      ))
    }
    col <- measure_cols[1]
  } else {
    col <- x$measures
    if (!col %in% measure_cols) {
      cli::cli_abort(
        "Column {.field {col}} is not a measure column. Available: {.field {measure_cols}}"
      )
    }
  }

  step_measure_output_wide_new(
    measures = col,
    prefix = x$prefix,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_output_wide <- function(object, new_data, ...) {
  col <- object$measures

  # Drop list columns that were preserved by step_measure_input_long.

  # In wide format, list columns interfere with pivot_wider (which requires
  # scalar id_cols). Location data is already inside the measure tibble.
  measure_cols <- find_measure_cols(new_data)
  list_cols <- names(new_data)[vapply(new_data, is.list, logical(1))]
  non_measure_list_cols <- setdiff(list_cols, measure_cols)
  if (length(non_measure_list_cols) > 0) {
    cli::cli_inform(
      "Dropping {length(non_measure_list_cols)} list column{?s} for wide output: {.field {non_measure_list_cols}}"
    )
    new_data <- new_data[, !names(new_data) %in% non_measure_list_cols]
  }

  non_meas <- names(new_data)
  non_meas <- non_meas[non_meas != col]

  res <-
    new_data |>
    tidyr::unnest(cols = dplyr::all_of(col)) |>
    dplyr::mutate(location = gsub(" ", "0", format(location), fixed = TRUE)) |>
    # remove NA values that are introduced by padding
    tidyr::drop_na("value") |>
    tidyr::pivot_wider(
      id_cols = c(dplyr::all_of(non_meas)),
      names_from = "location",
      values_from = "value"
    ) |>
    dplyr::rename_with(~ paste0(object$prefix, .x), c(-dplyr::all_of(non_meas)))
}

#' @export
print.step_measure_output_wide <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Restructure analytical measurements to wide format"
    print_step(
      rlang::quos("<internal data>"),
      rlang::quos("<internal data>"),
      x$trained,
      title,
      width
    )
    invisible(x)
  }

#' @rdname tidy.recipe
#' @export
tidy.step_measure_output_wide <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(
      terms = na_chr,
      value = na_dbl
    )
  } else {
    res <- tibble(
      terms = na_chr,
      value = na_dbl
    )
  }
  res$id <- x$id
  res
}
