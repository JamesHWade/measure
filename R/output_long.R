#' Reorganize Measurements to Long Format
#'
#' `step_measure_output_long` creates a *specification* of a recipe
#'
#'  step that converts measures to a format with columns for the measurement and
#'  the corresponding location (i.e., "long" format).
#'
#' @family input/output steps
#' @inheritParams recipes::step_center
#' @param measures An optional single character string specifying which measure
#'   column to output. If `NULL` (the default) and only one measure column
#'   exists, that column will be used. If multiple measure columns exist and
#'   `measures` is `NULL`, an error will be thrown prompting you to specify
#'   which column to output.
#' @param values_to A single character string for the column containing the
#'   analytical measurement.
#' @param location_to A single character string for the column name prefix for
#'   location columns. For 1D data, this becomes the column name (default:
#'   `.location`). For nD data, this becomes a prefix with dimension suffixes
#'   (e.g., `.location_1`, `.location_2`).
#' @details
#' This step is designed convert analytical measurements from their internal
#' data structure to a long format with explicit location columns.
#'
#' For 1D data, the output has two columns: the measurement value and a single
#' location column.
#'
#' For n-dimensional data (2D, 3D, etc.), the output has n+1 columns: the
#' measurement value and n location columns named with the `location_to` prefix
#' followed by dimension numbers (e.g., `.location_1`, `.location_2`).
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
#'   step_measure_output_long() |>
#'   prep()
#'
#' output_rec |> bake(new_data = small_te)
#'
#' @export

step_measure_output_long <-
  function(
    recipe,
    values_to = ".measure",
    location_to = ".location",
    measures = NULL,
    role = "predictor",
    trained = FALSE,
    skip = FALSE,
    id = rand_id("measure_output_long")
  ) {
    add_step(
      recipe,
      step_measure_output_long_new(
        measures = measures,
        values_to = values_to,
        location_to = location_to,
        trained = trained,
        role = role,
        skip = skip,
        id = id
      )
    )
  }

step_measure_output_long_new <-
  function(measures, values_to, location_to, role, trained, skip, id) {
    step(
      subclass = "measure_output_long",
      measures = measures,
      values_to = values_to,
      location_to = location_to,
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_measure_output_long <- function(x, training, info = NULL, ...) {
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

  step_measure_output_long_new(
    measures = col,
    values_to = x$values_to,
    location_to = x$location_to,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_output_long <- function(object, new_data, ...) {
  col <- object$measures

  # Find list columns that may have been preserved by step_measure_input_long
  measure_cols <- find_measure_cols(new_data)
  list_cols <- names(new_data)[vapply(new_data, is.list, logical(1))]
  non_measure_list_cols <- setdiff(list_cols, measure_cols)

  # Detect if this is an nD measure column
  ndim <- get_measure_col_ndim(new_data, col)

  # Get the expected length after unnesting the measure column
  measure_lengths <- vapply(new_data[[col]], nrow, integer(1))

  # Identify which list columns are safe to unnest (same length as measure)
  # These are likely location columns preserved by step_measure_input_long
  safe_to_unnest <- character(0)
  unsafe_list_cols <- character(0)
  for (lc in non_measure_list_cols) {
    lc_lengths <- lengths(new_data[[lc]])
    if (all(lc_lengths == measure_lengths)) {
      safe_to_unnest <- c(safe_to_unnest, lc)
    } else {
      unsafe_list_cols <- c(unsafe_list_cols, lc)
    }
  }

  # Drop unsafe list columns (those with incompatible lengths)
  # These are user-defined list columns that weren't created by input_long
  if (length(unsafe_list_cols) > 0) {
    cli::cli_inform(
      "Dropping {length(unsafe_list_cols)} list column{?s} with incompatible
       lengths for long output: {.field {unsafe_list_cols}}"
    )
    new_data <- new_data[, !names(new_data) %in% unsafe_list_cols]
  }

  # Unnest measure column + safe list columns together
  cols_to_unnest <- c(col, safe_to_unnest)

  if (ndim == 1L) {
    # 1D case
    rnm <- c(paste0(col, ".value"), paste0(col, ".location"))
    names(rnm) <- c(object$values_to, object$location_to)
    result <- new_data |>
      tidyr::unnest(cols = dplyr::all_of(cols_to_unnest), names_sep = ".") |>
      dplyr::rename(!!rnm)

    # Drop columns that are redundant with the location column
    # After unnest with names_sep=".", columns become "colname." (single element)
    # or remain as "colname" if they were scalar
    loc_col_name <- object$location_to
    loc_values <- result[[loc_col_name]]
    redundant_cols <- character(0)

    # Check both original names and unnested names (with "." suffix)
    for (orig_col in safe_to_unnest) {
      # Try the unnested name first (e.g., "elution_time.")
      unnested_name <- paste0(orig_col, ".")
      check_name <- if (unnested_name %in% names(result)) {
        unnested_name
      } else {
        orig_col
      }

      if (
        check_name %in%
          names(result) &&
          is.numeric(result[[check_name]]) &&
          length(result[[check_name]]) == length(loc_values) &&
          identical(result[[check_name]], loc_values)
      ) {
        redundant_cols <- c(redundant_cols, check_name)
      }
    }
    if (length(redundant_cols) > 0) {
      result <- result[, !names(result) %in% redundant_cols]
    }
    result
  } else {
    # nD case: unnest and rename location_1, location_2, etc.
    old_names <- c(
      paste0(col, ".value"),
      paste0(col, ".location_", seq_len(ndim))
    )
    new_names <- c(
      object$values_to,
      paste0(object$location_to, "_", seq_len(ndim))
    )
    rnm <- stats::setNames(old_names, new_names)

    result <- new_data |>
      tidyr::unnest(cols = dplyr::all_of(cols_to_unnest), names_sep = ".") |>
      dplyr::rename(dplyr::all_of(rnm))

    # Drop columns that are redundant with location columns
    loc_col_names <- paste0(object$location_to, "_", seq_len(ndim))
    redundant_cols <- character(0)
    for (i in seq_len(ndim)) {
      loc_values <- result[[loc_col_names[i]]]
      for (orig_col in safe_to_unnest) {
        unnested_name <- paste0(orig_col, ".")
        check_name <- if (unnested_name %in% names(result)) {
          unnested_name
        } else {
          orig_col
        }

        if (
          check_name %in%
            names(result) &&
            is.numeric(result[[check_name]]) &&
            length(result[[check_name]]) == length(loc_values) &&
            identical(result[[check_name]], loc_values)
        ) {
          redundant_cols <- c(redundant_cols, check_name)
        }
      }
    }
    if (length(redundant_cols) > 0) {
      result <- result[, !names(result) %in% redundant_cols]
    }
    result
  }
}

#' @export
print.step_measure_output_long <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Restructure analytical measurements to long format"
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
tidy.step_measure_output_long <- function(x, ...) {
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

value_to_tibble <- function(x, prefix = "measure_") {
  x <- matrix(x$value, nrow = 1)
  colnames(x) <- recipes::names0(ncol(x), prefix = prefix)
  dplyr::as_tibble(x)
}
