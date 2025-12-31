# ==============================================================================
# Spectral Math Transformations
#
# This file contains steps for common spectroscopic transformations:
# - step_measure_absorbance: Transmittance → Absorbance
# - step_measure_transmittance: Absorbance → Transmittance
# - step_measure_log: Log transformation
# - step_measure_kubelka_munk: Kubelka-Munk transformation
# - step_measure_derivative: Simple finite difference derivatives
# - step_measure_derivative_gap: Gap (Norris-Williams) derivatives
# ==============================================================================

# ==============================================================================
# step_measure_absorbance
# ==============================================================================

#' Convert Transmittance to Absorbance
#'
#' `step_measure_absorbance()` creates a *specification* of a recipe step that
#' converts transmittance values to absorbance using the Beer-Lambert law.
#'
#' @param recipe A recipe object. The step will be added to the sequence of
#'   operations for this recipe.
#' @param measures An optional character vector of measure column names to
#'   process. If `NULL` (the default), all measure columns will be processed.
#' @param role Not used by this step since no new variables are created.
#' @param trained A logical to indicate if the step has been trained.
#' @param skip A logical. Should the step be skipped when baking?
#' @param id A character string that is unique to this step.
#'
#' @return An updated version of `recipe` with the new step added.
#'
#' @details
#' This step applies the Beer-Lambert law transformation:
#'
#' \deqn{A = -\log_{10}(T)}{A = -log10(T)}
#'
#' where \eqn{T} is transmittance and \eqn{A} is absorbance.
#'
#' **Important**: Transmittance values should be in the range (0, 1] or (0, 100].
#' Zero or negative values will produce `-Inf` or `NaN` with a warning.
#'
#' The measurement locations are preserved unchanged.
#'
#' @family measure-preprocessing
#' @seealso [step_measure_transmittance()] for the inverse transformation
#' @export
#'
#' @examples
#' library(recipes)
#'
#' rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_absorbance() |>
#'   prep()
#'
#' bake(rec, new_data = NULL)
step_measure_absorbance <- function(
  recipe,
  measures = NULL,
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_absorbance")
) {
  recipes::add_step(
    recipe,
    step_measure_absorbance_new(
      measures = measures,
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_measure_absorbance_new <- function(measures, role, trained, skip, id) {
  recipes::step(
    subclass = "measure_absorbance",
    measures = measures,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_absorbance <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  step_measure_absorbance_new(
    measures = measure_cols,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_absorbance <- function(object, new_data, ...) {
  for (col in object$measures) {
    result <- purrr::map(new_data[[col]], .absorbance_single)
    new_data[[col]] <- new_measure_list(result)
  }

  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_absorbance <- function(
  x,
  width = max(20, options()$width - 30),
  ...
) {
  title <- "Transmittance to absorbance"

  if (x$trained) {
    cat(title, " on <internal measurements>", sep = "")
  } else {
    cat(title)
  }
  cat("\n")

  invisible(x)
}

#' @rdname tidy.recipe
#' @export
#' @keywords internal
tidy.step_measure_absorbance <- function(x, ...) {
  if (is_trained(x)) {
    terms <- x$measures
  } else {
    terms <- "<all measure columns>"
  }

  tibble::tibble(
    terms = terms,
    id = x$id
  )
}

#' @export
required_pkgs.step_measure_absorbance <- function(x, ...) {
  c("measure")
}

# ==============================================================================
# step_measure_transmittance
# ==============================================================================

#' Convert Absorbance to Transmittance
#'
#' `step_measure_transmittance()` creates a *specification* of a recipe step
#' that converts absorbance values to transmittance.
#'
#' @inheritParams step_measure_absorbance
#'
#' @return An updated version of `recipe` with the new step added.
#'
#' @details
#' This step applies the inverse Beer-Lambert law transformation:
#'
#' \deqn{T = 10^{-A}}{T = 10^(-A)}
#'
#' where \eqn{A} is absorbance and \eqn{T} is transmittance.
#'
#' The measurement locations are preserved unchanged.
#'
#' @family measure-preprocessing
#' @seealso [step_measure_absorbance()] for the inverse transformation
#' @export
#'
#' @examples
#' library(recipes)
#'
#' # Convert to absorbance then back to transmittance (round-trip)
#' rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_absorbance() |>
#'   step_measure_transmittance() |>
#'   prep()
step_measure_transmittance <- function(
  recipe,
  measures = NULL,
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_transmittance")
) {
  recipes::add_step(
    recipe,
    step_measure_transmittance_new(
      measures = measures,
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_measure_transmittance_new <- function(measures, role, trained, skip, id) {
  recipes::step(
    subclass = "measure_transmittance",
    measures = measures,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_transmittance <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  step_measure_transmittance_new(
    measures = measure_cols,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_transmittance <- function(object, new_data, ...) {
  for (col in object$measures) {
    result <- purrr::map(new_data[[col]], .transmittance_single)
    new_data[[col]] <- new_measure_list(result)
  }

  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_transmittance <- function(
  x,
  width = max(20, options()$width - 30),
  ...
) {
  title <- "Absorbance to transmittance"

  if (x$trained) {
    cat(title, " on <internal measurements>", sep = "")
  } else {
    cat(title)
  }
  cat("\n")

  invisible(x)
}

#' @rdname tidy.recipe
#' @export
#' @keywords internal
tidy.step_measure_transmittance <- function(x, ...) {
  if (is_trained(x)) {
    terms <- x$measures
  } else {
    terms <- "<all measure columns>"
  }

  tibble::tibble(
    terms = terms,
    id = x$id
  )
}

#' @export
required_pkgs.step_measure_transmittance <- function(x, ...) {
  c("measure")
}

# ==============================================================================
# step_measure_log
# ==============================================================================

#' Log Transformation
#'
#' `step_measure_log()` creates a *specification* of a recipe step that applies
#' a logarithmic transformation to measurement values.
#'
#' @inheritParams step_measure_absorbance
#' @param base The base of the logarithm. Default is `exp(1)` (natural log).
#'   Use `10` for log10 transformation.
#' @param offset A numeric offset added to values before taking the log.
#'   Default is `0`. Use a small positive value (e.g., `1` for `log1p`)
#'   to handle zero or near-zero values.
#'
#' @return An updated version of `recipe` with the new step added.
#'
#' @details
#' This step applies the transformation:
#'
#' \deqn{y' = \log_b(y + \text{offset})}{y' = log_b(y + offset)}
#'
#' where \eqn{b} is the base.
#'
#' Log transformation is commonly used for:
#' - Variance stabilization
#' - Normalizing skewed distributions
#' - Converting multiplicative relationships to additive
#'
#' **Warning**: Non-positive values (after offset) will produce `-Inf` or `NaN`.
#'
#' The measurement locations are preserved unchanged.
#'
#' @family measure-preprocessing
#' @export
#'
#' @examples
#' library(recipes)
#'
#' # Natural log transformation
#' rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_log(offset = 1) |>
#'   prep()
#'
#' # Log10 transformation
#' rec2 <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_log(base = 10) |>
#'   prep()
step_measure_log <- function(
  recipe,
  base = exp(1),
  offset = 0,
  measures = NULL,
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_log")
) {
  recipes::add_step(
    recipe,
    step_measure_log_new(
      base = base,
      offset = offset,
      measures = measures,
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_measure_log_new <- function(
  base,
  offset,
  measures,
  role,
  trained,
  skip,
  id
) {
  recipes::step(
    subclass = "measure_log",
    base = base,
    offset = offset,
    measures = measures,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_log <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  step_measure_log_new(
    base = x$base,
    offset = x$offset,
    measures = measure_cols,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_log <- function(object, new_data, ...) {
  for (col in object$measures) {
    result <- purrr::map(
      new_data[[col]],
      .log_single,
      base = object$base,
      offset = object$offset
    )
    new_data[[col]] <- new_measure_list(result)
  }

  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_log <- function(
  x,
  width = max(20, options()$width - 30),
  ...
) {
  base_str <- if (x$base == exp(1)) {
    "natural"
  } else {
    paste0("base ", round(x$base, 2))
  }
  offset_str <- if (x$offset != 0) paste0(", offset=", x$offset) else ""
  title <- paste0("Log transformation (", base_str, offset_str, ")")

  if (x$trained) {
    cat(title, " on <internal measurements>", sep = "")
  } else {
    cat(title)
  }
  cat("\n")

  invisible(x)
}

#' @rdname tidy.recipe
#' @export
#' @keywords internal
tidy.step_measure_log <- function(x, ...) {
  if (is_trained(x)) {
    terms <- x$measures
  } else {
    terms <- "<all measure columns>"
  }

  tibble::tibble(
    terms = terms,
    base = x$base,
    offset = x$offset,
    id = x$id
  )
}

#' @export
required_pkgs.step_measure_log <- function(x, ...) {
  c("measure")
}

# ==============================================================================
# step_measure_kubelka_munk
# ==============================================================================

#' Kubelka-Munk Transformation
#'
#' `step_measure_kubelka_munk()` creates a *specification* of a recipe step
#' that applies the Kubelka-Munk transformation for diffuse reflectance data.
#'
#' @inheritParams step_measure_absorbance
#'
#' @return An updated version of `recipe` with the new step added.
#'
#' @details
#' The Kubelka-Munk transformation is used for diffuse reflectance spectroscopy
#' to convert reflectance to a quantity proportional to concentration:
#'
#' \deqn{f(R) = \frac{(1-R)^2}{2R}}{f(R) = (1-R)^2 / (2R)}
#'
#' where \eqn{R} is the reflectance (0 to 1).
#'
#' **Important**: Reflectance values should be in the range (0, 1).
#' Values at the boundaries will produce extreme values or `Inf`.
#'
#' This transformation is commonly used in:
#' - NIR diffuse reflectance spectroscopy
#' - Analysis of powders and solid samples
#' - When Beer-Lambert law doesn't apply directly
#'
#' The measurement locations are preserved unchanged.
#'
#' @family measure-preprocessing
#' @export
#'
#' @examples
#' library(recipes)
#'
#' # Assuming reflectance data in (0, 1) range
#' # Note: meats_long has transmittance, this is illustrative
#' rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_kubelka_munk()
step_measure_kubelka_munk <- function(
  recipe,
  measures = NULL,
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_kubelka_munk")
) {
  recipes::add_step(
    recipe,
    step_measure_kubelka_munk_new(
      measures = measures,
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_measure_kubelka_munk_new <- function(measures, role, trained, skip, id) {
  recipes::step(
    subclass = "measure_kubelka_munk",
    measures = measures,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_kubelka_munk <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  step_measure_kubelka_munk_new(
    measures = measure_cols,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_kubelka_munk <- function(object, new_data, ...) {
  for (col in object$measures) {
    result <- purrr::map(new_data[[col]], .kubelka_munk_single)
    new_data[[col]] <- new_measure_list(result)
  }

  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_kubelka_munk <- function(
  x,
  width = max(20, options()$width - 30),
  ...
) {
  title <- "Kubelka-Munk transformation"

  if (x$trained) {
    cat(title, " on <internal measurements>", sep = "")
  } else {
    cat(title)
  }
  cat("\n")

  invisible(x)
}

#' @rdname tidy.recipe
#' @export
#' @keywords internal
tidy.step_measure_kubelka_munk <- function(x, ...) {
  if (is_trained(x)) {
    terms <- x$measures
  } else {
    terms <- "<all measure columns>"
  }

  tibble::tibble(
    terms = terms,
    id = x$id
  )
}

#' @export
required_pkgs.step_measure_kubelka_munk <- function(x, ...) {
  c("measure")
}

# ==============================================================================
# step_measure_derivative
# ==============================================================================

#' Simple Finite Difference Derivatives
#'
#' `step_measure_derivative()` creates a *specification* of a recipe step that
#' computes derivatives using simple finite differences.
#'
#' @inheritParams step_measure_absorbance
#' @param order The order of the derivative (1 or 2). Default is `1` (first
#'   derivative).
#'
#' @return An updated version of `recipe` with the new step added.
#'
#' @details
#' This step computes derivatives using forward finite differences:
#'
#' \deqn{\frac{dy}{dx} \approx \frac{y_{i+1} - y_i}{x_{i+1} - x_i}}{dy/dx ≈ (y[i+1] - y[i]) / (x[i+1] - x[i])}
#'
#' For each derivative order, the spectrum length is reduced by 1.
#' - First derivative: n-1 points
#' - Second derivative: n-2 points
#'
#' The location values are updated to the left point of each difference.
#'
#' **Note**: For smoothed derivatives, consider using [step_measure_savitzky_golay()]
#' with `differentiation_order > 0` instead.
#'
#' @family measure-preprocessing
#' @seealso [step_measure_derivative_gap()] for gap derivatives,
#'   [step_measure_savitzky_golay()] for smoothed derivatives
#' @export
#'
#' @examples
#' library(recipes)
#'
#' # First derivative
#' rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_derivative(order = 1) |>
#'   prep()
#'
#' # Second derivative
#' rec2 <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_derivative(order = 2) |>
#'   prep()
step_measure_derivative <- function(
  recipe,
  order = 1L,
  measures = NULL,
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_derivative")
) {
  order <- as.integer(order)
  if (order < 1 || order > 2) {
    cli::cli_abort("{.arg order} must be 1 or 2, not {order}.")
  }

  recipes::add_step(
    recipe,
    step_measure_derivative_new(
      order = order,
      measures = measures,
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_measure_derivative_new <- function(
  order,
  measures,
  role,
  trained,
  skip,
  id
) {
  recipes::step(
    subclass = "measure_derivative",
    order = order,
    measures = measures,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_derivative <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  step_measure_derivative_new(
    order = x$order,
    measures = measure_cols,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_derivative <- function(object, new_data, ...) {
  for (col in object$measures) {
    result <- purrr::map(
      new_data[[col]],
      .derivative_single,
      order = object$order
    )
    new_data[[col]] <- new_measure_list(result)
  }

  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_derivative <- function(
  x,
  width = max(20, options()$width - 30),
  ...
) {
  order_str <- if (x$order == 1) "1st" else "2nd"
  title <- paste0(order_str, " derivative (finite difference)")

  if (x$trained) {
    cat(title, " on <internal measurements>", sep = "")
  } else {
    cat(title)
  }
  cat("\n")

  invisible(x)
}

#' @rdname tidy.recipe
#' @export
#' @keywords internal
tidy.step_measure_derivative <- function(x, ...) {
  if (is_trained(x)) {
    terms <- x$measures
  } else {
    terms <- "<all measure columns>"
  }

  tibble::tibble(
    terms = terms,
    order = x$order,
    id = x$id
  )
}

#' @export
required_pkgs.step_measure_derivative <- function(x, ...) {
  c("measure")
}

# ==============================================================================
# step_measure_derivative_gap
# ==============================================================================

#' Gap (Norris-Williams) Derivatives
#'
#' `step_measure_derivative_gap()` creates a *specification* of a recipe step
#' that computes gap derivatives using the Norris-Williams method.
#'
#' @inheritParams step_measure_absorbance
#' @param gap The gap size (number of points to skip on each side). Default is
#'   `2`. The derivative at point i is computed from points i-gap and i+gap.
#' @param segment The segment size for averaging. Default is `1` (no averaging).
#'   When greater than 1, multiple points are averaged on each side before
#'   computing the difference.
#'
#' @return An updated version of `recipe` with the new step added.
#'
#' @details
#' Gap derivatives compute the difference between points separated by a gap:
#'
#' \deqn{\frac{dy}{dx} \approx \frac{y_{i+g} - y_{i-g}}{x_{i+g} - x_{i-g}}}{dy/dx ≈ (y[i+g] - y[i-g]) / (x[i+g] - x[i-g])}
#'
#' where \eqn{g} is the gap size.
#'
#' When `segment > 1`, the Norris-Williams method is used, which averages
#' `segment` points on each side before computing the difference.
#'
#' The spectrum length is reduced by `2 * gap` points.
#'
#' Gap derivatives are often used in NIR chemometrics as an alternative to

#' Savitzky-Golay derivatives when less smoothing is desired.
#'
#' @family measure-preprocessing
#' @seealso [step_measure_derivative()] for simple finite differences,
#'   [step_measure_savitzky_golay()] for smoothed derivatives
#' @export
#'
#' @examples
#' library(recipes)
#'
#' # Gap derivative with gap=2
#' rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_derivative_gap(gap = 2) |>
#'   prep()
#'
#' # Norris-Williams with gap=3, segment=2
#' rec2 <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_derivative_gap(gap = 3, segment = 2) |>
#'   prep()
step_measure_derivative_gap <- function(
  recipe,
  gap = 2L,
  segment = 1L,
  measures = NULL,
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_derivative_gap")
) {
  gap <- as.integer(gap)
  segment <- as.integer(segment)

  if (gap < 1) {
    cli::cli_abort("{.arg gap} must be at least 1, not {gap}.")
  }
  if (segment < 1) {
    cli::cli_abort("{.arg segment} must be at least 1, not {segment}.")
  }

  recipes::add_step(
    recipe,
    step_measure_derivative_gap_new(
      gap = gap,
      segment = segment,
      measures = measures,
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_measure_derivative_gap_new <- function(
  gap,
  segment,
  measures,
  role,
  trained,
  skip,
  id
) {
  recipes::step(
    subclass = "measure_derivative_gap",
    gap = gap,
    segment = segment,
    measures = measures,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_derivative_gap <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  step_measure_derivative_gap_new(
    gap = x$gap,
    segment = x$segment,
    measures = measure_cols,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_derivative_gap <- function(object, new_data, ...) {
  for (col in object$measures) {
    result <- purrr::map(
      new_data[[col]],
      .derivative_gap_single,
      gap = object$gap,
      segment = object$segment
    )
    new_data[[col]] <- new_measure_list(result)
  }

  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_derivative_gap <- function(
  x,
  width = max(20, options()$width - 30),
  ...
) {
  seg_str <- if (x$segment > 1) paste0(", segment=", x$segment) else ""
  title <- paste0("Gap derivative (gap=", x$gap, seg_str, ")")

  if (x$trained) {
    cat(title, " on <internal measurements>", sep = "")
  } else {
    cat(title)
  }
  cat("\n")

  invisible(x)
}

#' @rdname tidy.recipe
#' @export
#' @keywords internal
tidy.step_measure_derivative_gap <- function(x, ...) {
  if (is_trained(x)) {
    terms <- x$measures
  } else {
    terms <- "<all measure columns>"
  }

  tibble::tibble(
    terms = terms,
    gap = x$gap,
    segment = x$segment,
    id = x$id
  )
}

#' @export
required_pkgs.step_measure_derivative_gap <- function(x, ...) {
  c("measure")
}

# ==============================================================================
# Internal helper functions
# ==============================================================================

#' Convert transmittance to absorbance for a single spectrum
#' @param x A measure_tbl with location and value columns
#' @return Modified measure_tbl with absorbance values
#' @noRd
.absorbance_single <- function(x) {
  if (any(x$value <= 0, na.rm = TRUE)) {
    cli::cli_warn(
      "Non-positive transmittance values found; absorbance will contain -Inf/NaN."
    )
  }
  x$value <- -log10(x$value)
  x
}

#' Convert absorbance to transmittance for a single spectrum
#' @param x A measure_tbl with location and value columns
#' @return Modified measure_tbl with transmittance values
#' @noRd
.transmittance_single <- function(x) {
  x$value <- 10^(-x$value)
  x
}

#' Apply log transformation to a single spectrum
#' @param x A measure_tbl with location and value columns
#' @param base Log base
#' @param offset Offset to add before log
#' @return Modified measure_tbl with log-transformed values
#' @noRd
.log_single <- function(x, base = exp(1), offset = 0) {
  adjusted <- x$value + offset
  if (any(adjusted <= 0, na.rm = TRUE)) {
    cli::cli_warn(
      "Non-positive values after offset; log will contain -Inf/NaN."
    )
  }
  x$value <- log(adjusted, base = base)
  x
}

#' Apply Kubelka-Munk transformation to a single spectrum
#' @param x A measure_tbl with location and value columns (reflectance)
#' @return Modified measure_tbl with K-M transformed values
#' @noRd
.kubelka_munk_single <- function(x) {
  R <- x$value
  if (any(R <= 0 | R >= 1, na.rm = TRUE)) {
    cli::cli_warn(
      "Reflectance values outside (0, 1); Kubelka-Munk may produce extreme values."
    )
  }
  x$value <- (1 - R)^2 / (2 * R)
  x
}

#' Compute simple finite difference derivative for a single spectrum
#' @param x A measure_tbl with location and value columns
#' @param order Derivative order (1 or 2)
#' @return Modified measure_tbl with derivative values
#' @noRd
.derivative_single <- function(x, order = 1L) {
  loc <- x$location
  val <- x$value

  for (i in seq_len(order)) {
    n <- length(val)
    if (n < 2) {
      cli::cli_warn("Cannot compute derivative: fewer than 2 points.")
      return(x)
    }

    # Forward difference with location-aware spacing
    dx <- diff(loc)
    dy <- diff(val)
    val <- dy / dx

    # Assign to left location (drop last point)
    loc <- loc[-n]
  }

  # Create new measure_tbl with correct dimensions
  new_measure_tbl(location = loc, value = val)
}

#' Compute gap derivative for a single spectrum
#' @param x A measure_tbl with location and value columns
#' @param gap Gap size
#' @param segment Segment size for averaging
#' @return Modified measure_tbl with derivative values
#' @noRd
.derivative_gap_single <- function(x, gap = 2L, segment = 1L) {
  n <- length(x$value)

  # For segment averaging, we need extra points
  if (segment > 1L) {
    effective_gap <- gap + segment - 1
    required <- 2 * effective_gap + 1
  } else {
    required <- 2 * gap + 1
  }

  if (n < required) {
    cli::cli_warn(
      "Spectrum has {n} points; need >= {required} for gap={gap}, segment={segment}. Returning unchanged."
    )
    return(x)
  }

  # Center indices where we can compute the derivative
  if (segment > 1L) {
    effective_gap <- gap + segment - 1
    idx <- (effective_gap + 1):(n - effective_gap)
  } else {
    idx <- (gap + 1):(n - gap)
  }

  if (segment == 1L) {
    # Simple gap derivative
    values_plus <- x$value[idx + gap]
    values_minus <- x$value[idx - gap]
  } else {
    # Norris-Williams: average segment points on each side
    values_plus <- vapply(
      idx,
      function(i) {
        plus_range <- i + gap + seq(0, segment - 1)
        mean(x$value[plus_range], na.rm = TRUE)
      },
      numeric(1)
    )
    values_minus <- vapply(
      idx,
      function(i) {
        minus_range <- i - gap - seq(0, segment - 1)
        mean(x$value[minus_range], na.rm = TRUE)
      },
      numeric(1)
    )
  }

  dx <- x$location[idx + gap] - x$location[idx - gap]
  new_values <- (values_plus - values_minus) / dx
  new_locations <- x$location[idx]

  # Create new measure_tbl with correct dimensions
  new_measure_tbl(location = new_locations, value = new_values)
}
