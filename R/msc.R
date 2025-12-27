#' Multiplicative Scatter Correction (MSC)
#'
#' `step_measure_msc()` creates a *specification* of a recipe step that applies
#' Multiplicative Scatter Correction to spectral data. MSC removes physical
#' light scatter by accounting for additive and multiplicative effects.
#'
#' @param recipe A recipe object. The step will be added to the sequence of
#'   operations for this recipe.
#' @param role Not used by this step since no new variables are created.
#' @param trained A logical to indicate if the quantities for preprocessing
#'   have been estimated.
#' @param ref_spectrum A numeric vector containing the reference spectrum
#'   computed during training. This is `NULL` until the step is trained.
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
#' Multiplicative Scatter Correction (MSC) is a normalization method that
#' attempts to account for additive and multiplicative effects by aligning
#' each spectrum to a reference spectrum. For a spectrum \eqn{x_i} and
#' reference \eqn{x_r}, the transformation is:
#'
#' \deqn{x_i = m_i \cdot x_r + a_i}
#' \deqn{MSC(x_i) = \frac{x_i - a_i}{m_i}}
#'
#' where \eqn{a_i} and \eqn{m_i} are the additive (intercept) and multiplicative
#' (slope) terms from regressing \eqn{x_i} on \eqn{x_r}.
#'
#' The reference spectrum is computed as the mean of all training spectra during
#' `prep()` and stored for use when applying the transformation to new data.
#'
#' MSC is commonly used to remove physical light scatter effects in NIR
#' spectroscopy caused by differences in particle size or path length.
#'
#' **No selectors should be supplied to this step function**. The data should be
#' in the internal format produced by [step_measure_input_wide()] or
#' [step_measure_input_long()].
#'
#' The measurement locations are preserved unchanged.
#'
#' # Tidying
#'
#' When you [`tidy()`][recipes::tidy.recipe()] this step, a tibble with columns
#' `terms` (set to `".measures"`) and `id` is returned.
#'
#' @references
#' Geladi, P., MacDougall, D., and Martens, H. 1985. Linearization and
#' Scatter-Correction for Near-Infrared Reflectance Spectra of Meat.
#' Applied Spectroscopy, 39(3):491-500.
#'
#' @family measure-preprocessing
#' @seealso [step_measure_snv()] for a simpler scatter correction method
#' @export
#'
#' @examples
#' library(recipes)
#'
#' rec <-
#'   recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_msc() |>
#'   prep()
#'
#' bake(rec, new_data = NULL)
step_measure_msc <- function(recipe,
                             role = NA,
                             trained = FALSE,
                             ref_spectrum = NULL,
                             skip = FALSE,
                             id = recipes::rand_id("measure_msc")) {
 recipes::add_step(
    recipe,
    step_measure_msc_new(
      role = role,
      trained = trained,
      ref_spectrum = ref_spectrum,
      skip = skip,
      id = id
    )
  )
}

step_measure_msc_new <- function(role, trained, ref_spectrum, skip, id) {
  recipes::step(
    subclass = "measure_msc",
    role = role,
    trained = trained,
    ref_spectrum = ref_spectrum,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_msc <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

 # Compute reference spectrum as mean of all training spectra
  ref_spectrum <- .compute_reference_spectrum(training$.measures)

  step_measure_msc_new(
    role = x$role,
    trained = TRUE,
    ref_spectrum = ref_spectrum,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_msc <- function(object, new_data, ...) {
  new_data$.measures <- .compute_msc(new_data$.measures, object$ref_spectrum)
  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_msc <- function(x, width = max(20, options()$width - 30), ...) {
  title <- "MSC transformation on "

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
tidy.step_measure_msc <- function(x, ...) {
  tibble::tibble(
    terms = ".measures",
    id = x$id
  )
}

# ------------------------------------------------------------------------------
# Internal computation

#' Compute reference spectrum from training data
#'
#' @param dat A list of tibbles with `location` and `value` columns.
#' @return A numeric vector of the mean spectrum.
#' @noRd
.compute_reference_spectrum <- function(dat) {
 # Convert to matrix and compute column means
  mat <- measure_to_matrix(dat)
  colMeans(mat, na.rm = TRUE)
}

#' Compute MSC transformation
#'
#' @param dat A list of tibbles with `location` and `value` columns.
#' @param ref_spectrum A numeric vector of the reference spectrum.
#' @return A list of tibbles with MSC-transformed values.
#' @noRd
.compute_msc <- function(dat, ref_spectrum) {
  purrr::map(dat, .msc_single, ref_spectrum = ref_spectrum)
}

#' Apply MSC to a single spectrum
#'
#' @param x A tibble with `location` and `value` columns.
#' @param ref_spectrum A numeric vector of the reference spectrum.
#' @return A tibble with MSC-transformed values.
#' @noRd
.msc_single <- function(x, ref_spectrum) {
  values <- x$value

  # Fit linear regression: values = slope * ref_spectrum + intercept
  # Using lm.fit for efficiency
  fit <- stats::lm.fit(cbind(1, ref_spectrum), values)
  intercept <- fit$coefficients[1]
  slope <- fit$coefficients[2]

  # Handle edge case where slope is zero or NA
  if (is.na(slope) || abs(slope) < .Machine$double.eps) {
    cli::cli_warn(
      "Multiplicative coefficient is zero or NA for a spectrum.
       MSC cannot be applied; returning centered values."
    )
    x$value <- values - intercept
  } else {
    # Apply MSC correction: (values - intercept) / slope
    x$value <- (values - intercept) / slope
  }

  x
}
