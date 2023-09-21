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
