# ------------------------------------------------------------------------------
# Add location columns

add_location_col <- function(x, loc) {
  x$location <- loc
  x
}

add_location <- function(.data, loc) {
  dplyr::mutate(
    .data,
    .measures = purrr::map(.measures, add_location_col, loc = loc)
  )
}

# ------------------------------------------------------------------------------
# Make sue that data are in the correct format.

check_has_measure <- function(x, cl) {
  step_fn <- as.character(cl[[1]])
  step_fn <- gsub("prep\\.", "", step_fn)
  step_fn <- paste0("`", step_fn, "()`.")


  if (!any(names(x) == ".measures")) {
    msg <-
      paste0("It appears that the measurements have not been converted ",
             "for the inernal format. See `step_measure_input_long()` ",
             "and `step_measure_input_wide()` and use these prior to ",
             step_fn)
    rlang::abort(msg)
  }
}
