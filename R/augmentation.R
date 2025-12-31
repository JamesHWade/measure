# ==============================================================================
# Data Augmentation Steps
#
# This file contains steps for augmenting training data:
# - step_measure_augment_noise: Add random noise
# - step_measure_augment_shift: Random x-axis shifts
# - step_measure_augment_scale: Random intensity scaling
#
# Key pattern: All augmentation steps default to skip=TRUE so they only
# apply during training (prep), not during prediction (bake on new data).
# ==============================================================================

# ==============================================================================
# Helper for reproducible row-based seeding
# ==============================================================================

#' Generate deterministic seed from row data
#' @noRd
.row_seed <- function(values, base_seed = 12345L) {
  # Use hash of values to create deterministic but unique seed per row
  hash_val <- sum(values * seq_along(values), na.rm = TRUE)
  as.integer(abs(base_seed + round(hash_val * 1000)) %% .Machine$integer.max)
}

# ==============================================================================
# step_measure_augment_noise
# ==============================================================================

#' Add Random Noise to Measurements
#'
#' `step_measure_augment_noise()` creates a *specification* of a recipe step that
#' adds controlled random noise to spectral data for data augmentation.
#'
#' @param recipe A recipe object.
#' @param sd Standard deviation of noise. If `relative = TRUE` (default),
#'   this is relative to the signal range (0.01 = 1% of range). If
#'   `relative = FALSE`, this is the absolute noise level.
#' @param distribution Noise distribution: `"gaussian"` (default) or `"uniform"`.
#' @param relative Logical. If `TRUE` (default), `sd` is relative to signal range.
#' @param measures An optional character vector of measure column names.
#' @param role Not used.
#' @param trained Logical indicating if the step has been trained.
#' @param skip Logical. Should the step be skipped when baking? Default is `TRUE`,
#'   meaning augmentation only applies during training.
#' @param id Unique step identifier.
#'
#' @return An updated recipe with the new step added.
#'
#' @details
#' Data augmentation adds variability to training data to improve model
#' robustness. Adding noise simulates measurement uncertainty and helps
#' models generalize better.
#'
#' **Default behavior (`skip = TRUE`):**
#' The augmentation is only applied during `prep()` on training data.
#' When `bake()` is called on new data, the step is skipped.
#'
#' **Reproducibility:**
#' The noise is deterministic based on the row content, so the same input
#' always produces the same augmented output within a session.
#'
#' @family measure-augmentation
#' @export
#'
#' @examples
#' library(recipes)
#'
#' rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_augment_noise(sd = 0.02) |>
#'   prep()
#'
#' # Noise only applied to training data
#' bake(rec, new_data = NULL)
step_measure_augment_noise <- function(
  recipe,
  sd = 0.01,
  distribution = c("gaussian", "uniform"),
  relative = TRUE,
  measures = NULL,
  role = NA,
  trained = FALSE,
  skip = TRUE,
  id = recipes::rand_id("measure_augment_noise")
) {
  distribution <- rlang::arg_match(distribution)

  if (!is.numeric(sd) || length(sd) != 1 || sd < 0) {
    cli::cli_abort("{.arg sd} must be a non-negative number.")
  }

  recipes::add_step(
    recipe,
    step_measure_augment_noise_new(
      sd = sd,
      distribution = distribution,
      relative = relative,
      measures = measures,
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_measure_augment_noise_new <- function(
  sd,
  distribution,
  relative,
  measures,
  role,
  trained,
  skip,
  id
) {
  recipes::step(
    subclass = "measure_augment_noise",
    sd = sd,
    distribution = distribution,
    relative = relative,
    measures = measures,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_augment_noise <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  step_measure_augment_noise_new(
    sd = x$sd,
    distribution = x$distribution,
    relative = x$relative,
    measures = measure_cols,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' Add noise to a single spectrum
#' @noRd
.add_noise_single <- function(m, sd, distribution, relative) {
  values <- m$value
  n <- length(values)

  # Get seed from row data for reproducibility
  seed <- .row_seed(values)
  old_seed <- get0(".Random.seed", envir = globalenv())
  on.exit({
    if (!is.null(old_seed)) {
      assign(".Random.seed", old_seed, envir = globalenv())
    } else if (exists(".Random.seed", envir = globalenv())) {
      rm(".Random.seed", envir = globalenv())
    }
  })
  set.seed(seed)

  # Calculate actual noise SD
  if (relative) {
    value_range <- diff(range(values, na.rm = TRUE))
    if (value_range < .Machine$double.eps) {
      value_range <- 1
    }
    noise_sd <- sd * value_range
  } else {
    noise_sd <- sd
  }

  # Generate noise
  if (distribution == "gaussian") {
    noise <- stats::rnorm(n, mean = 0, sd = noise_sd)
  } else {
    # Uniform in [-noise_sd, noise_sd] (same variance as Gaussian would need sqrt(3)*sd)
    noise <- stats::runif(
      n,
      min = -noise_sd * sqrt(3),
      max = noise_sd * sqrt(3)
    )
  }

  m$value <- values + noise
  m
}

#' @export
bake.step_measure_augment_noise <- function(object, new_data, ...) {
  sd <- object$sd
  distribution <- object$distribution
  relative <- object$relative

  for (col in object$measures) {
    result <- purrr::map(new_data[[col]], function(m) {
      .add_noise_single(m, sd, distribution, relative)
    })
    new_data[[col]] <- new_measure_list(result)
  }

  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_augment_noise <- function(
  x,
  width = max(20, options()$width - 30),
  ...
) {
  rel_str <- if (x$relative) " (relative)" else ""
  title <- paste0("Add ", x$distribution, " noise (sd=", x$sd, rel_str, ")")
  if (x$trained) {
    cat(title, " on <internal measurements>", sep = "")
  } else {
    cat(title)
  }
  if (x$skip) {
    cat(" [training only]")
  }
  cat("\n")
  invisible(x)
}

#' @rdname tidy.recipe
#' @export
#' @keywords internal
tidy.step_measure_augment_noise <- function(x, ...) {
  tibble::tibble(
    sd = x$sd,
    distribution = x$distribution,
    relative = x$relative,
    skip = x$skip,
    id = x$id
  )
}

#' @rdname required_pkgs.recipe
#' @export
#' @keywords internal
required_pkgs.step_measure_augment_noise <- function(x, ...) {
  c("measure")
}

# ==============================================================================
# step_measure_augment_shift
# ==============================================================================

#' Add Random X-axis Shifts
#'
#' `step_measure_augment_shift()` creates a *specification* of a recipe step that
#' applies random shifts along the x-axis for shift invariance training.
#'
#' @param recipe A recipe object.
#' @param max_shift Maximum shift amount in location units. The actual shift
#'   is uniformly sampled from `[-max_shift, max_shift]`.
#' @param measures An optional character vector of measure column names.
#' @param role Not used.
#' @param trained Logical indicating if the step has been trained.
#' @param skip Logical. Should the step be skipped when baking? Default is `TRUE`.
#' @param id Unique step identifier.
#'
#' @return An updated recipe with the new step added.
#'
#' @details
#' This step adds random x-axis shifts to help models become invariant to
#' small retention time or wavelength shifts. This is particularly useful
#' for chromatographic data where peak positions may vary slightly.
#'
#' The spectrum is interpolated to the shifted positions using linear
#' interpolation. Values outside the original range use boundary values.
#'
#' **Default behavior (`skip = TRUE`):**
#' The shift is only applied during training. When predicting on new data,
#' the step is skipped.
#'
#' @family measure-augmentation
#' @export
#'
#' @examples
#' library(recipes)
#'
#' rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_augment_shift(max_shift = 2) |>
#'   prep()
#'
#' bake(rec, new_data = NULL)
step_measure_augment_shift <- function(
  recipe,
  max_shift = 1,
  measures = NULL,
  role = NA,
  trained = FALSE,
  skip = TRUE,
  id = recipes::rand_id("measure_augment_shift")
) {
  if (!is.numeric(max_shift) || length(max_shift) != 1 || max_shift < 0) {
    cli::cli_abort("{.arg max_shift} must be a non-negative number.")
  }

  recipes::add_step(
    recipe,
    step_measure_augment_shift_new(
      max_shift = max_shift,
      measures = measures,
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_measure_augment_shift_new <- function(
  max_shift,
  measures,
  role,
  trained,
  skip,
  id
) {
  recipes::step(
    subclass = "measure_augment_shift",
    max_shift = max_shift,
    measures = measures,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_augment_shift <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  step_measure_augment_shift_new(
    max_shift = x$max_shift,
    measures = measure_cols,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' Apply shift to a single spectrum
#' @noRd
.apply_shift_single <- function(m, max_shift) {
  location <- m$location
  values <- m$value

  # Get seed from row data for reproducibility
  seed <- .row_seed(values)
  old_seed <- get0(".Random.seed", envir = globalenv())
  on.exit({
    if (!is.null(old_seed)) {
      assign(".Random.seed", old_seed, envir = globalenv())
    } else if (exists(".Random.seed", envir = globalenv())) {
      rm(".Random.seed", envir = globalenv())
    }
  })
  set.seed(seed)

  # Generate random shift
  shift <- stats::runif(1, min = -max_shift, max = max_shift)

  # Shift the location values
  shifted_location <- location + shift

  # Interpolate to original locations
  new_values <- stats::approx(
    x = shifted_location,
    y = values,
    xout = location,
    rule = 2 # Use boundary values for extrapolation
  )$y

  m$value <- new_values
  m
}

#' @export
bake.step_measure_augment_shift <- function(object, new_data, ...) {
  max_shift <- object$max_shift

  for (col in object$measures) {
    result <- purrr::map(new_data[[col]], function(m) {
      .apply_shift_single(m, max_shift)
    })
    new_data[[col]] <- new_measure_list(result)
  }

  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_augment_shift <- function(
  x,
  width = max(20, options()$width - 30),
  ...
) {
  title <- paste0("Random x-shift (max=", x$max_shift, ")")
  if (x$trained) {
    cat(title, " on <internal measurements>", sep = "")
  } else {
    cat(title)
  }
  if (x$skip) {
    cat(" [training only]")
  }
  cat("\n")
  invisible(x)
}

#' @rdname tidy.recipe
#' @export
#' @keywords internal
tidy.step_measure_augment_shift <- function(x, ...) {
  tibble::tibble(
    max_shift = x$max_shift,
    skip = x$skip,
    id = x$id
  )
}

#' @rdname required_pkgs.recipe
#' @export
#' @keywords internal
required_pkgs.step_measure_augment_shift <- function(x, ...) {
  c("measure")
}

# ==============================================================================
# step_measure_augment_scale
# ==============================================================================

#' Random Intensity Scaling
#'
#' `step_measure_augment_scale()` creates a *specification* of a recipe step that
#' applies random intensity scaling for scale invariance training.
#'
#' @param recipe A recipe object.
#' @param range A numeric vector of length 2 specifying the range of scaling
#'   factors. Default is `c(0.9, 1.1)`, meaning 90%-110% of original.
#' @param measures An optional character vector of measure column names.
#' @param role Not used.
#' @param trained Logical indicating if the step has been trained.
#' @param skip Logical. Should the step be skipped when baking? Default is `TRUE`.
#' @param id Unique step identifier.
#'
#' @return An updated recipe with the new step added.
#'
#' @details
#' This step multiplies spectrum values by a random scaling factor sampled
#' uniformly from the specified range. This helps models become robust to
#' variations in signal intensity.
#'
#' **Common use cases:**
#' - Simulating concentration variations
#' - Compensating for detector sensitivity differences
#' - Making models robust to sample preparation variability
#'
#' **Default behavior (`skip = TRUE`):**
#' The scaling is only applied during training. When predicting on new data,
#' the step is skipped.
#'
#' @family measure-augmentation
#' @export
#'
#' @examples
#' library(recipes)
#'
#' rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_augment_scale(range = c(0.8, 1.2)) |>
#'   prep()
#'
#' bake(rec, new_data = NULL)
step_measure_augment_scale <- function(
  recipe,
  range = c(0.9, 1.1),
  measures = NULL,
  role = NA,
  trained = FALSE,
  skip = TRUE,
  id = recipes::rand_id("measure_augment_scale")
) {
  if (!is.numeric(range) || length(range) != 2) {
    cli::cli_abort("{.arg range} must be a numeric vector of length 2.")
  }

  if (range[1] >= range[2]) {
    cli::cli_abort("{.arg range} must have min < max.")
  }

  if (any(range <= 0)) {
    cli::cli_abort("{.arg range} values must be positive.")
  }

  recipes::add_step(
    recipe,
    step_measure_augment_scale_new(
      range = range,
      measures = measures,
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_measure_augment_scale_new <- function(
  range,
  measures,
  role,
  trained,
  skip,
  id
) {
  recipes::step(
    subclass = "measure_augment_scale",
    range = range,
    measures = measures,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_augment_scale <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  step_measure_augment_scale_new(
    range = x$range,
    measures = measure_cols,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' Apply scale to a single spectrum
#' @noRd
.apply_scale_single <- function(m, range) {
  values <- m$value

  # Get seed from row data for reproducibility
  seed <- .row_seed(values)
  old_seed <- get0(".Random.seed", envir = globalenv())
  on.exit({
    if (!is.null(old_seed)) {
      assign(".Random.seed", old_seed, envir = globalenv())
    } else if (exists(".Random.seed", envir = globalenv())) {
      rm(".Random.seed", envir = globalenv())
    }
  })
  set.seed(seed)

  # Generate random scaling factor
  scale_factor <- stats::runif(1, min = range[1], max = range[2])

  m$value <- values * scale_factor
  m
}

#' @export
bake.step_measure_augment_scale <- function(object, new_data, ...) {
  range <- object$range

  for (col in object$measures) {
    result <- purrr::map(new_data[[col]], function(m) {
      .apply_scale_single(m, range)
    })
    new_data[[col]] <- new_measure_list(result)
  }

  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_augment_scale <- function(
  x,
  width = max(20, options()$width - 30),
  ...
) {
  title <- paste0("Random scaling [", x$range[1], ", ", x$range[2], "]")
  if (x$trained) {
    cat(title, " on <internal measurements>", sep = "")
  } else {
    cat(title)
  }
  if (x$skip) {
    cat(" [training only]")
  }
  cat("\n")
  invisible(x)
}

#' @rdname tidy.recipe
#' @export
#' @keywords internal
tidy.step_measure_augment_scale <- function(x, ...) {
  tibble::tibble(
    range_min = x$range[1],
    range_max = x$range[2],
    skip = x$skip,
    id = x$id
  )
}

#' @rdname required_pkgs.recipe
#' @export
#' @keywords internal
required_pkgs.step_measure_augment_scale <- function(x, ...) {
  c("measure")
}
