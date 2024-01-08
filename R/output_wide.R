#' Reorganize Measurements to Separate Columns
#'
#' `step_measure_output_wide` creates a *specification* of a recipe
#'  step that converts measures to multiple columns (i.e., "wide" format).
#' @family input/output steps
#' @inheritParams recipes::step_center
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
#' small_tr <- bioreactors_small[  1:200,]
#' small_te <- bioreactors_small[201:210,]
#'
#' small_rec <-
#'   recipe(glucose ~ . , data = small_tr) %>%
#'   update_role(batch_id, day, new_role = "id columns") %>%
#'   step_measure_input_wide(`400`:`3050`) %>%
#'   prep()
#'
#' # Before reformatting:
#'
#' small_rec %>% bake(new_data = small_te)
#'
#' # After reformatting:
#'
#' output_rec <-
#'   small_rec %>%
#'   step_measure_output_wide() %>%
#'   prep()
#'
#' output_rec %>% bake(new_data = small_te)
#'
#' @export

step_measure_output_wide <-
  function(recipe,
           prefix = "measure_",
           role = "predictor",
           trained = FALSE,
           skip = FALSE,
           id = rand_id("measure_output_wide")) {
    add_step(
      recipe,
      step_measure_output_wide_new(
        prefix = prefix,
        trained = trained,
        role = role,
        skip = skip,
        id = id
      )
    )
  }

step_measure_output_wide_new <-
  function(prefix, role, trained, skip, id) {
    step(
      subclass = "measure_output_wide",
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
  step_measure_output_wide_new(
    prefix = x$prefix,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_output_wide <- function(object, new_data, ...) {

  non_meas <- names(new_data)
  non_meas <- non_meas[non_meas != ".measures"]

  res <-
    new_data %>%
    tidyr::unnest(cols = c(.measures)) %>%
    dplyr::mutate(location = gsub(" ", "0", format(location))) %>%
    tidyr::pivot_wider(
      id_cols = c(dplyr::all_of(non_meas)),
      names_from = "location",
      values_from = "value"
    ) %>%
    dplyr::rename_with(~ paste0(object$prefix, .x), c(-dplyr::all_of(non_meas)))
}

#' @export
print.step_measure_output_wide <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Restructure analytical measurements to wide format"
    print_step(rlang::quos("<internal data>"), rlang::quos("<internal data>"), x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @export
tidy.step_measure_output_wide <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = na_chr,
                  value = na_dbl)
  } else {
    res <- tibble(terms = na_chr,
                  value = na_dbl)
  }
  res$id <- x$id
  res
}

