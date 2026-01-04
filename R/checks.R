check_missing_measures <- function(.data, loc) {
  if (!is.na(loc)) {
    return(invisible(NULL))
  }
  has_na <- !all(vctrs::vec_detect_complete(.data))
  if (has_na) {
    cli::cli_abort(
      "Missing data are only allowed when the wave number is supplied."
    )
  }
  invisible(NULL)
}

check_single_selector <- function(res, arg) {
  if (length(res) != 1) {
    cli::cli_abort(
      "The selection for {.arg {arg}} should only select a single column
       ({length(res)} columns were selected)."
    )
  }
}

check_measure_dims <- function(x, col = ".measures") {
  num_rows <- purrr::map_int(x[[col]], nrow)
  num_unique <- sort(table(num_rows), decreasing = TRUE)
  most_freq <- as.integer(names(num_unique)[1])
  if (length(num_unique) != 1) {
    which_rows <- which(num_rows != most_freq)
    n_bad <- length(which_rows)
    chr_rows <- paste(which_rows, collapse = ", ")
    cli::cli_abort(
      "The number of rows in each measure should be the same.
                   Most samples have {most_freq} rows and these do not:
                   {chr_rows}. Please pad the input with missing values."
    )
  }
  invisible(NULL)
}

pad_measure_dims <- function(x, col = ".measures") {
  # Determine the most frequent number of rows
  num_rows <- purrr::map_int(x[[col]], nrow)
  num_unique <- sort(table(num_rows), decreasing = TRUE)
  most_freq <- as.integer(names(num_unique)[1])

  # Pad each measure so they all have 'most_freq' rows
  x[[col]] <- purrr::map(
    x[[col]],
    ~ {
      df <- .x
      if (nrow(df) < most_freq) {
        # Calculate how many rows to add
        rows_to_add <- most_freq - nrow(df)
        # Create a data frame with the required number of missing rows
        missing_rows <-
          purrr::map_dfc(names(df), ~ rep(NA_real_, rows_to_add)) |>
          tibble::as_tibble() |>
          setNames(names(df)) |>
          suppressMessages() # suppress message about new column names
        # Bind the missing rows to the original data frame
        df <- bind_rows(df, missing_rows)
        df
      }
      df
    }
  )
  x
}

#' Check if a column is numeric or a list of numeric vectors
#'
#' This helper function validates that a column is either:
#' 1. A numeric vector (double or integer)
#' 2. A list column where each element is a numeric vector
#'
#' List columns occur when a previous step_measure_input_long has run,
#' preserving columns as list columns for subsequent input steps.
#'
#' @param x The column vector to check
#' @param col_name The name of the column (for error messages)
#' @noRd
check_type_or_list_numeric <- function(x, col_name) {
  # Accept numeric vectors directly

  if (is.numeric(x)) {
    return(invisible(NULL))
  }

  # Accept list columns where all elements are numeric
  if (is.list(x)) {
    all_numeric <- all(vapply(x, is.numeric, logical(1)))
    if (all_numeric) {
      return(invisible(NULL))
    }
  }

  # If neither, throw an error
  cli::cli_abort(
    "Column {.field {col_name}} must be numeric (double or integer) or
     a list column containing numeric vectors.
     Found type: {.cls {class(x)[1]}}."
  )
}
