check_missing_measures <- function(.data, loc) {
  if (!is.na(loc)) {
    return(invisible(NULL))
  }
  has_na <- any(!vctrs::vec_detect_complete(.data))
  if (has_na) {
    rlang::abort("Missing data are only allowed when the wave number is supplied.")
  }
  invisible(NULL)
}

check_single_selector <- function(res, arg) {
  if (length(res) != 1) {
    msg <- paste0("The selection for `", arg, "` should only select a single ",
                  "column (", length(res), " columns were selected).")
    rlang::abort(msg)
  }
}

check_measure_dims <- function(x) {
  num_rows <- purrr::map_int(x$.measures, nrow)
  num_unique <- sort(table(num_rows), decreasing = TRUE)
  most_freq <- as.integer(names(num_unique)[1])
  if (length(num_unique) != 1) {
    which_rows <- which(num_rows != most_freq)
    n_bad <- length(which_rows)
    chr_rows <- paste(which_rows, collapse = ", ")
    cli::cli_abort("The number of rows in each measure should be the same.
                   Most samples have {most_freq} rows and these do not:
                   {chr_rows}. Please pad the input with missing values.")
  }
  invisible(NULL)
}

pad_measure_dims <- function(x) {
  # Determine the most frequent number of rows
  num_rows <- purrr::map_int(x$.measures, nrow)
  num_unique <- sort(table(num_rows), decreasing = TRUE)
  most_freq <- as.integer(names(num_unique)[1])

  # Pad each measure so they all have 'most_freq' rows
  x$.measures <- purrr::map(x$.measures, ~{
    df <- .x
    if (nrow(df) < most_freq) {
      # Calculate how many rows to add
      rows_to_add <- most_freq - nrow(df)
      # Create a data frame with the required number of missing rows
      missing_rows <-
        purrr::map_dfc(names(df), ~rep(NA_real_, rows_to_add)) %>%
        tibble::as_tibble() %>%
        setNames(names(df)) %>%
        suppressMessages() # suppress message about new column names
      # Bind the missing rows to the original data frame
      df <- bind_rows(df, missing_rows)
      df
    }
    df
  })
  x
}
