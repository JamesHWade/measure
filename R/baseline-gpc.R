#' GPC/SEC Baseline Correction
#'
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' `step_measure_baseline_gpc()` creates a *specification* of a recipe step
#' that applies baseline correction optimized for Gel Permeation Chromatography
#' (GPC) or Size Exclusion Chromatography (SEC) data. This method estimates the
#' baseline by interpolating between baseline regions at the start and end of
#' the chromatogram.
#'
#' **This step has been superseded by `measure.sec::step_sec_baseline()`.**
#' For new code, we recommend using the `measure.sec` package which provides
#' more complete SEC/GPC analysis functionality.
#'
#' @param recipe A recipe object. The step will be added to the sequence of
#'   operations for this recipe.
#' @param measures An optional character vector of measure column names to
#'   process. If `NULL` (the default), all measure columns (columns with class
#'   `measure_list`) will be processed.
#' @param left_frac Fraction of points from the beginning to use as the left
#'   baseline region. Default is `0.05` (first 5% of data points).
#' @param right_frac Fraction of points from the end to use as the right
#'   baseline region. Default is `0.05` (last 5% of data points).
#' @param method Method for baseline estimation. One of:
#'   - `"linear"` (default): Linear interpolation between left and right means
#'   - `"median"`: Uses median of baseline regions (more robust to outliers)
#'   - `"spline"`: Smooth spline through baseline regions
#' @param role Not used by this step since no new variables are created.
#' @param trained A logical to indicate if the quantities for preprocessing
#'   have been estimated.
#' @param skip A logical. Should the step be skipped when the recipe is baked?
#' @param id A character string that is unique to this step to identify it.
#'
#' @return An updated version of `recipe` with the new step added to the
#'   sequence of any existing operations.
#'
#' @details
#' GPC/SEC chromatograms typically have distinct baseline regions at the
#' beginning and end where no polymer elutes. This step leverages this
#' characteristic by:
#'
#' 1
#' 2. Computing a representative baseline value for each region (mean or median)
#' 3. Interpolating between these values to estimate the full baseline
#' 4. Subtracting the estimated baseline from the signal
#'
#' The `left_frac` and `right_frac` parameters control how much of the
#' chromatogram is considered "baseline". Choose values that:
#' - Include only the flat, signal-free regions
#' - Exclude any polymer peaks or system peaks
#' - Are large enough to average out noise
#'
#' Unlike general-purpose baseline methods like ALS or polynomial fitting,
#' this approach is specifically designed for the characteristic shape of
#' GPC/SEC chromatograms and is computationally very fast.
#'
#' **No selectors should be supplied to this step function**. The data should be
#' in the internal format produced by [step_measure_input_wide()] or
#' [step_measure_input_long()].
#'
#' # Tidying
#'
#' When you [`tidy()`][recipes::tidy.recipe()] this step, a tibble with columns
#' `terms`, `left_frac`, `right_frac`, `method`, and `id` is returned.
#'
#' @seealso [step_measure_baseline_als()] for general-purpose baseline
#'   correction, [step_measure_detrend()] for simple trend removal.
#'
#' @family measure-baseline
#' @export
#'
#' @examples
#' library(recipes)
#'
#' # Using meats_long as example (works on any measurement data)
#' rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_baseline_gpc(left_frac = 0.1, right_frac = 0.1) |>
#'   prep()
#'
#' bake(rec, new_data = NULL)
step_measure_baseline_gpc <- function(
  recipe,
  measures = NULL,
  left_frac = 0.05,
  right_frac = 0.05,
  method = "linear",
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_baseline_gpc")
) {
  recipes::add_step(
    recipe,
    step_measure_baseline_gpc_new(
      measures = measures,
      left_frac = left_frac,
      right_frac = right_frac,
      method = method,
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_measure_baseline_gpc_new <- function(
  measures,
  left_frac,
  right_frac,
  method,
  role,
  trained,
  skip,
  id
) {
  recipes::step(
    subclass = "measure_baseline_gpc",
    measures = measures,
    left_frac = left_frac,
    right_frac = right_frac,
    method = method,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_baseline_gpc <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  # Validate parameters
  if (
    !is.numeric(x$left_frac) ||
      length(x$left_frac) != 1 ||
      x$left_frac <= 0 ||
      x$left_frac >= 0.5
  ) {
    cli::cli_abort(
      "{.arg left_frac} must be a number between 0 and 0.5, not {.val {x$left_frac}}."
    )
  }

  if (
    !is.numeric(x$right_frac) ||
      length(x$right_frac) != 1 ||
      x$right_frac <= 0 ||
      x$right_frac >= 0.5
  ) {
    cli::cli_abort(
      "{.arg right_frac} must be a number between 0 and 0.5, not {.val {x$right_frac}}."
    )
  }

  if (x$left_frac + x$right_frac >= 0.5) {
    cli::cli_abort(
      "{.arg left_frac} + {.arg right_frac} must be less than 0.5."
    )
  }

  valid_methods <- c("linear", "median", "spline")
  if (!x$method %in% valid_methods) {
    cli::cli_abort(
      "{.arg method} must be one of {.or {.val {valid_methods}}}, not {.val {x$method}}."
    )
  }

  # Resolve which columns to process
  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  step_measure_baseline_gpc_new(
    measures = measure_cols,
    left_frac = x$left_frac,
    right_frac = x$right_frac,
    method = x$method,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_baseline_gpc <- function(object, new_data, ...) {
  for (col in object$measures) {
    result <- .compute_baseline_gpc(
      new_data[[col]],
      left_frac = object$left_frac,
      right_frac = object$right_frac,
      method = object$method
    )
    new_data[[col]] <- new_measure_list(result)
  }
  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_baseline_gpc <- function(
  x,
  width = max(20, options()$width - 30),
  ...
) {
  title <- "GPC/SEC baseline correction on "

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
tidy.step_measure_baseline_gpc <- function(x, ...) {
  if (recipes::is_trained(x)) {
    terms_val <- x$measures
  } else {
    terms_val <- "<all measure columns>"
  }
  tibble::tibble(
    terms = terms_val,
    left_frac = x$left_frac,
    right_frac = x$right_frac,
    method = x$method,
    id = x$id
  )
}

#' @export
required_pkgs.step_measure_baseline_gpc <- function(x, ...) {
  "measure"
}

# ------------------------------------------------------------------------------
# Internal computation

#' Compute GPC baseline correction for a measure_list
#'
#' @param dat A measure_list (list of measure_tbl objects).
#' @param left_frac Fraction for left baseline region.
#' @param right_frac Fraction for right baseline region.
#' @param method Baseline estimation method.
#' @return A list of measure_tbl objects with baseline-corrected values.
#' @noRd
.compute_baseline_gpc <- function(dat, left_frac, right_frac, method) {
  purrr::map(
    dat,
    .gpc_baseline_single,
    left_frac = left_frac,
    right_frac = right_frac,
    method = method
  )
}

#' Apply GPC baseline correction to a single chromatogram
#'
#' @param x A measure_tbl with location and value columns.
#' @param left_frac Fraction for left baseline region.
#' @param right_frac Fraction for right baseline region.
#' @param method Baseline estimation method.
#' @return A measure_tbl with baseline-corrected values.
#' @noRd
.gpc_baseline_single <- function(x, left_frac, right_frac, method) {
  y <- x$value
  n <- length(y)

  # Handle edge cases
  if (n < 10) {
    cli::cli_warn(
      "Chromatogram has fewer than 10 points; returning unchanged."
    )
    return(x)
  }

  if (all(is.na(y))) {
    cli::cli_warn("Chromatogram is all NA; returning unchanged.")
    return(x)
  }

  # Calculate region indices
  n_left <- max(1, floor(n * left_frac))
  n_right <- max(1, floor(n * right_frac))

  left_idx <- seq_len(n_left)
  right_idx <- seq(n - n_right + 1, n)

  left_values <- y[left_idx]
  right_values <- y[right_idx]

  # Compute baseline based on method
  if (method == "linear") {
    # Linear interpolation between mean of left and right regions
    left_mean <- mean(left_values, na.rm = TRUE)
    right_mean <- mean(right_values, na.rm = TRUE)

    # Linear baseline from left_mean at left center to right_mean at right center
    left_center <- mean(left_idx)
    right_center <- mean(right_idx)

    slope <- (right_mean - left_mean) / (right_center - left_center)
    intercept <- left_mean - slope * left_center

    baseline <- intercept + slope * seq_len(n)
  } else if (method == "median") {
    # Use medians for robustness
    left_med <- stats::median(left_values, na.rm = TRUE)
    right_med <- stats::median(right_values, na.rm = TRUE)

    left_center <- mean(left_idx)
    right_center <- mean(right_idx)

    slope <- (right_med - left_med) / (right_center - left_center)
    intercept <- left_med - slope * left_center

    baseline <- intercept + slope * seq_len(n)
  } else if (method == "spline") {
    # Smooth spline through baseline regions
    baseline_idx <- c(left_idx, right_idx)
    baseline_y <- c(left_values, right_values)

    # Remove NAs for spline fitting
    valid <- !is.na(baseline_y)
    if (sum(valid) < 4) {
      cli::cli_warn(
        "Too few valid baseline points for spline; using linear method."
      )
      # Fall back to linear
      left_mean <- mean(left_values, na.rm = TRUE)
      right_mean <- mean(right_values, na.rm = TRUE)
      left_center <- mean(left_idx)
      right_center <- mean(right_idx)
      slope <- (right_mean - left_mean) / (right_center - left_center)
      intercept <- left_mean - slope * left_center
      baseline <- intercept + slope * seq_len(n)
    } else {
      fit <- tryCatch(
        stats::smooth.spline(
          x = baseline_idx[valid],
          y = baseline_y[valid],
          spar = 0.8 # Strong smoothing
        ),
        error = function(e) {
          cli::cli_warn(
            "Spline fitting failed: {e$message}; using linear method."
          )
          return(NULL)
        }
      )

      if (is.null(fit)) {
        # Fall back to linear
        left_mean <- mean(left_values, na.rm = TRUE)
        right_mean <- mean(right_values, na.rm = TRUE)
        left_center <- mean(left_idx)
        right_center <- mean(right_idx)
        slope <- (right_mean - left_mean) / (right_center - left_center)
        intercept <- left_mean - slope * left_center
        baseline <- intercept + slope * seq_len(n)
      } else {
        baseline <- stats::predict(fit, x = seq_len(n))$y
      }
    }
  }

  # Subtract baseline
  x$value <- y - baseline
  x
}
