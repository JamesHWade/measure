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
