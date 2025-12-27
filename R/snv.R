#' Standard Normal Variate (SNV) Transformation
#'
#' `step_measure_snv()` creates a *specification* of a recipe step that applies
#' Standard Normal Variate transformation to spectral data. SNV normalizes each
#' spectrum to have zero mean and unit standard deviation.
#'
#' @param recipe A recipe object. The step will be added to the sequence of
#'   operations for this recipe.
#' @param role Not used by this step since no new variables are created.
#' @param trained A logical to indicate if the quantities for preprocessing
#'   have been estimated.
#' @param skip A logical. Should the step be skipped when the recipe is baked
#'   by [bake()]? While all operations are baked when [prep()] is run, some
#'   operations may not be able to be conducted on new data (e.g. processing
#'   the outcome variable(s)). Care should be taken when using `skip = TRUE`
#'   as it may affect the computations for subsequent operations.
#' @param id A character string that is unique to this step to identify it.
#'
#' @return An updated version of `recipe` with the new step added to the
#'   sequence of any existing operations.
#'
#' @details
#' Standard Normal Variate (SNV) is a row-wise transformation that normalizes
#' each spectrum independently. For a spectrum \eqn{x}, the transformation is:
#'
#' \deqn{SNV(x) = \frac{x - \bar{x}}{s_x}}{SNV(x) = (x - mean(x)) / sd(x)}
#'
#' where \eqn{\bar{x}}{mean(x)} is the mean and \eqn{s_x}{sd(x)} is the standard
#' deviation of the spectrum values.
#'
#' SNV is commonly used to remove multiplicative effects of scatter and particle
#' size in NIR spectroscopy. After SNV transformation, each spectrum will have a
#' mean of zero and a standard deviation of one.
#'
#' **No selectors should be supplied to this step function**. The data should be
#' in the internal format produced by [step_measure_input_wide()] or
#' [step_measure_input_long()].
#'
#' The measurement locations are preserved unchanged.
#'
#' # Tidying
#'
#' When you [`tidy()`][recipes::tidy.recipe()] this step, a tibble with column
#' `terms` (set to `".measures"`) and `id` is returned.
#'
#' @family measure-preprocessing
#' @export
#'
#' @examples
#' library(recipes)
#'
#' rec <-
#'   recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_snv() |>
#'   prep()
#'
#' bake(rec, new_data = NULL)
step_measure_snv <- function(
  recipe,
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_snv")
) {
  recipes::add_step(
    recipe,
    step_measure_snv_new(
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_measure_snv_new <- function(role, trained, skip, id) {
  recipes::step(
    subclass = "measure_snv",
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_snv <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  step_measure_snv_new(
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_snv <- function(object, new_data, ...) {
  result <- .compute_snv(new_data$.measures)
  # Preserve measure_list class
  new_data$.measures <- new_measure_list(result)
  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_snv <- function(
  x,
  width = max(20, options()$width - 30),
  ...
) {
  title <- "SNV transformation on "

  if (x$trained) {
    cat(title, "<internal measurements>", sep = "")
  } else {
    cat(title)
  }
  cat("\n")

  invisible(x)
}

#' @rdname tidy.recipe
#' @export
#' @keywords internal
tidy.step_measure_snv <- function(x, ...) {
  tibble::tibble(
    terms = ".measures",
    id = x$id
  )
}

# ------------------------------------------------------------------------------
# Internal computation

#' Compute SNV transformation
#'
#' @param dat A list of tibbles with `location` and `value` columns.
#' @return A list of tibbles with SNV-transformed values.
#' @noRd
.compute_snv <- function(dat) {
  purrr::map(dat, .snv_single)
}

#' Apply SNV to a single spectrum
#'
#' @param x A tibble with `location` and `value` columns.
#' @return A tibble with SNV-transformed values.
#' @noRd
.snv_single <- function(x) {
  values <- x$value
  x_mean <- mean(values, na.rm = TRUE)
  x_sd <- stats::sd(values, na.rm = TRUE)

  if (is.na(x_sd) || x_sd == 0) {
    cli::cli_warn(
      "Standard deviation is zero or NA for a spectrum. SNV cannot be applied;
       returning centered values."
    )
    x$value <- values - x_mean
  } else {
    x$value <- (values - x_mean) / x_sd
  }

  x
}
