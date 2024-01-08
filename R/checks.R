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
