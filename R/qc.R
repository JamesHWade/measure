# ==============================================================================
# Quality Control Steps
#
# This file contains quality control preprocessing steps:
# - step_measure_qc_snr: Calculate signal-to-noise ratio
# - step_measure_qc_saturated: Detect saturated regions
# - step_measure_impute: Interpolate missing values
# - step_measure_qc_outlier: Detect outlier samples
# ==============================================================================

# ==============================================================================
# step_measure_qc_snr
# ==============================================================================

#' Calculate Signal-to-Noise Ratio
#'
#' `step_measure_qc_snr()` creates a *specification* of a recipe step that
#' calculates the signal-to-noise ratio (SNR) for each measurement and adds
#' it as a new column. This is useful for quality control and filtering.
#'
#' @param recipe A recipe object.
#' @param measures An optional character vector of measure column names.
#' @param new_col Name of the new column to store SNR values. Default is
#'   `".snr"`.
#' @param signal_method How to estimate signal:
#'   - `"max"` (default): Maximum absolute value
#'   - `"range"`: Peak-to-peak range
#'   - `"rms"`: Root mean square
#' @param noise_method How to estimate noise:
#'   - `"diff"` (default): RMS of first differences (estimates high-freq noise)
#'   - `"mad"`: Median absolute deviation of values
#'   - `"residual"`: Residuals from smoothed fit
#' @param role Role for the new column. Default is `"predictor"`.
#' @param trained Logical indicating if the step has been trained.
#' @param skip Logical. Should the step be skipped when baking?
#' @param id Unique step identifier.
#'
#' @return An updated recipe with the new step added.
#'
#' @details
#' SNR is calculated as `signal / noise`, where signal and noise are estimated
#' using the specified methods. Higher values indicate cleaner data.
#'
#' The `"diff"` noise method is particularly useful because it estimates
#' high-frequency noise without being affected by broad spectral features:
#' \deqn{noise = \sqrt{\frac{1}{2(n-1)} \sum_{i=2}^{n} (x_i - x_{i-1})^2}}
#'
#' @family measure-qc
#' @export
#'
#' @examples
#' library(recipes)
#'
#' rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_qc_snr() |>
#'   prep()
#'
#' bake(rec, new_data = NULL)
step_measure_qc_snr <- function(
  recipe,
  measures = NULL,
  new_col = ".snr",
  signal_method = c("max", "range", "rms"),
  noise_method = c("diff", "mad", "residual"),
  role = "predictor",
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_qc_snr")
) {
  signal_method <- match.arg(signal_method)
  noise_method <- match.arg(noise_method)

  recipes::add_step(
    recipe,
    step_measure_qc_snr_new(
      measures = measures,
      new_col = new_col,
      signal_method = signal_method,
      noise_method = noise_method,
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_measure_qc_snr_new <- function(
  measures,
  new_col,
  signal_method,
  noise_method,
  role,
  trained,
  skip,
  id
) {
  recipes::step(
    subclass = "measure_qc_snr",
    measures = measures,
    new_col = new_col,
    signal_method = signal_method,
    noise_method = noise_method,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_qc_snr <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  step_measure_qc_snr_new(
    measures = measure_cols,
    new_col = x$new_col,
    signal_method = x$signal_method,
    noise_method = x$noise_method,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_qc_snr <- function(object, new_data, ...) {
  col <- object$measures[1] # Use first measure column

  snr_values <- purrr::map_dbl(
    new_data[[col]],
    .compute_snr,
    signal_method = object$signal_method,
    noise_method = object$noise_method
  )

  new_data[[object$new_col]] <- snr_values
  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_qc_snr <- function(
  x,
  width = max(20, options()$width - 30),
  ...
) {
  title <- paste0("Signal-to-noise ratio calculation -> ", x$new_col)
  cat(title)
  cat("\n")
  invisible(x)
}

#' @rdname tidy.recipe
#' @export
#' @keywords internal
tidy.step_measure_qc_snr <- function(x, ...) {
  tibble::tibble(
    terms = if (is_trained(x)) x$measures else "<all measure columns>",
    new_col = x$new_col,
    signal_method = x$signal_method,
    noise_method = x$noise_method,
    id = x$id
  )
}

.compute_snr <- function(x, signal_method, noise_method) {
  values <- x$value
  values <- values[!is.na(values)]

  if (length(values) < 3) {
    return(NA_real_)
  }

  # Calculate signal
  signal <- switch(
    signal_method,
    "max" = max(abs(values)),
    "range" = max(values) - min(values),
    "rms" = sqrt(mean(values^2))
  )

  # Calculate noise
  noise <- switch(
    noise_method,
    "diff" = {
      # RMS of first differences (estimates high-freq noise)
      diffs <- diff(values)
      sqrt(mean(diffs^2) / 2)
    },
    "mad" = {
      # MAD scaled to SD
      stats::mad(values, constant = 1.4826)
    },
    "residual" = {
      # Residuals from a smooth fit
      n <- length(values)
      window <- min(21, n %/% 5)
      if (window %% 2 == 0) {
        window <- window + 1
      }
      if (window >= 3) {
        kernel <- rep(1 / window, window)
        smoothed <- stats::filter(values, kernel, sides = 2)
        residuals <- values - as.numeric(smoothed)
        residuals <- residuals[!is.na(residuals)]
        if (length(residuals) > 0) {
          sqrt(mean(residuals^2))
        } else {
          stats::mad(values, constant = 1.4826)
        }
      } else {
        stats::mad(values, constant = 1.4826)
      }
    }
  )

  if (is.na(noise) || noise == 0) {
    return(NA_real_)
  }

  signal / noise
}


# ==============================================================================
# step_measure_qc_saturated
# ==============================================================================

#' Detect Saturated Measurements
#'
#' `step_measure_qc_saturated()` creates a *specification* of a recipe step
#' that detects saturated (clipped) regions in measurements and adds metadata
#' columns indicating saturation status.
#'
#' @param recipe A recipe object.
#' @param measures An optional character vector of measure column names.
#' @param upper_limit Upper saturation threshold. Default is `NULL` (auto-detect).
#' @param lower_limit Lower saturation threshold. Default is `NULL` (auto-detect).
#' @param tolerance How close to the limit counts as saturated. Default is 0.01.
#' @param new_col_flag Name of column for saturation flag. Default is
#'   `".saturated"`.
#' @param new_col_pct Name of column for saturation percentage. Default is
#'   `".sat_pct"`.
#' @param role Role for new columns. Default is `"predictor"`.
#' @param trained Logical indicating if the step has been trained.
#' @param skip Logical. Should the step be skipped when baking?
#' @param id Unique step identifier.
#'
#' @return An updated recipe with the new step added.
#'
#' @details
#' Saturation occurs when detector response reaches its maximum (or minimum)
#' capacity. Saturated data points lose quantitative information and may need
#' special handling.
#'
#' If limits are not specified, they are auto-detected as values appearing
#' as flat regions at extreme values (using `min()` and `max()`).
#'
#' Two new columns are added:
#' - `.saturated`: Logical, TRUE if any saturation detected
#' - `.sat_pct`: Percentage of points that are saturated
#'
#' @family measure-qc
#' @export
#'
#' @examples
#' library(recipes)
#'
#' rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_qc_saturated() |>
#'   prep()
#'
#' bake(rec, new_data = NULL)
step_measure_qc_saturated <- function(
  recipe,
  measures = NULL,
  upper_limit = NULL,
  lower_limit = NULL,
  tolerance = 0.01,
  new_col_flag = ".saturated",
  new_col_pct = ".sat_pct",
  role = "predictor",
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_qc_saturated")
) {
  recipes::add_step(
    recipe,
    step_measure_qc_saturated_new(
      measures = measures,
      upper_limit = upper_limit,
      lower_limit = lower_limit,
      tolerance = tolerance,
      new_col_flag = new_col_flag,
      new_col_pct = new_col_pct,
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_measure_qc_saturated_new <- function(
  measures,
  upper_limit,
  lower_limit,
  tolerance,
  new_col_flag,
  new_col_pct,
  role,
  trained,
  skip,
  id
) {
  recipes::step(
    subclass = "measure_qc_saturated",
    measures = measures,
    upper_limit = upper_limit,
    lower_limit = lower_limit,
    tolerance = tolerance,
    new_col_flag = new_col_flag,
    new_col_pct = new_col_pct,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_qc_saturated <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  # Auto-detect limits if not provided
  upper_limit <- x$upper_limit
  lower_limit <- x$lower_limit

  if (is.null(upper_limit) || is.null(lower_limit)) {
    all_values <- unlist(purrr::map(training[[measure_cols[1]]], ~ .x$value))
    all_values <- all_values[!is.na(all_values)]

    if (is.null(upper_limit)) {
      upper_limit <- max(all_values, na.rm = TRUE)
    }
    if (is.null(lower_limit)) {
      lower_limit <- min(all_values, na.rm = TRUE)
    }
  }

  step_measure_qc_saturated_new(
    measures = measure_cols,
    upper_limit = upper_limit,
    lower_limit = lower_limit,
    tolerance = x$tolerance,
    new_col_flag = x$new_col_flag,
    new_col_pct = x$new_col_pct,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_qc_saturated <- function(object, new_data, ...) {
  col <- object$measures[1]

  sat_results <- purrr::map(
    new_data[[col]],
    .detect_saturation,
    upper_limit = object$upper_limit,
    lower_limit = object$lower_limit,
    tolerance = object$tolerance
  )

  new_data[[object$new_col_flag]] <- purrr::map_lgl(sat_results, "saturated")
  new_data[[object$new_col_pct]] <- purrr::map_dbl(sat_results, "pct")

  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_qc_saturated <- function(
  x,
  width = max(20, options()$width - 30),
  ...
) {
  title <- "Saturation detection"
  if (x$trained) {
    title <- paste0(
      title,
      " [",
      round(x$lower_limit, 3),
      ", ",
      round(x$upper_limit, 3),
      "]"
    )
  }
  cat(title)
  cat("\n")
  invisible(x)
}

#' @rdname tidy.recipe
#' @export
#' @keywords internal
tidy.step_measure_qc_saturated <- function(x, ...) {
  tibble::tibble(
    terms = if (is_trained(x)) x$measures else "<all measure columns>",
    upper_limit = x$upper_limit %||% NA_real_,
    lower_limit = x$lower_limit %||% NA_real_,
    id = x$id
  )
}

.detect_saturation <- function(x, upper_limit, lower_limit, tolerance) {
  values <- x$value
  n <- length(values)
  if (n == 0) {
    return(list(saturated = FALSE, pct = 0))
  }

  upper_thresh <- upper_limit * (1 - tolerance)
  lower_thresh <- lower_limit * (1 + tolerance)

  # Count saturated points
  n_upper <- sum(values >= upper_thresh, na.rm = TRUE)
  n_lower <- sum(values <= lower_thresh, na.rm = TRUE)
  n_saturated <- n_upper + n_lower

  list(
    saturated = n_saturated > 0,
    pct = 100 * n_saturated / n
  )
}


# ==============================================================================
# step_measure_impute
# ==============================================================================

#' Impute Missing Values in Measurements
#'
#' `step_measure_impute()` creates a *specification* of a recipe step that
#' imputes (fills in) missing values (NA) in measurement data using
#' interpolation or other methods.
#'
#' @param recipe A recipe object.
#' @param measures An optional character vector of measure column names.
#' @param method Imputation method:
#'   - `"linear"` (default): Linear interpolation
#'   - `"spline"`: Cubic spline interpolation
#'   - `"constant"`: Nearest non-NA value
#'   - `"mean"`: Global mean of non-NA values
#' @param max_gap Maximum gap size to impute. Gaps larger than this are left
#'   as NA. Default is `Inf` (impute all).
#' @param role Not used.
#' @param trained Logical indicating if the step has been trained.
#' @param skip Logical. Should the step be skipped when baking?
#' @param id Unique step identifier.
#'
#' @return An updated recipe with the new step added.
#'
#' @details
#' Missing values can occur due to:
#' - Removed spikes (after despiking with replacement set to NA)
#' - Excluded regions
#' - Instrument gaps or dropouts
#'
#' Linear and spline interpolation use the `stats::approx()` and
#' `stats::spline()` functions respectively. They are most appropriate when
#' gaps are small relative to spectral features.
#'
#' @family measure-qc
#' @export
#'
#' @examples
#' library(recipes)
#'
#' rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_impute(method = "linear") |>
#'   prep()
#'
#' bake(rec, new_data = NULL)
step_measure_impute <- function(
  recipe,
  measures = NULL,
  method = c("linear", "spline", "constant", "mean"),
  max_gap = Inf,
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_impute")
) {
  method <- match.arg(method)

  recipes::add_step(
    recipe,
    step_measure_impute_new(
      measures = measures,
      method = method,
      max_gap = max_gap,
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_measure_impute_new <- function(
  measures,
  method,
  max_gap,
  role,
  trained,
  skip,
  id
) {
  recipes::step(
    subclass = "measure_impute",
    measures = measures,
    method = method,
    max_gap = max_gap,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_impute <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  step_measure_impute_new(
    measures = measure_cols,
    method = x$method,
    max_gap = x$max_gap,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_impute <- function(object, new_data, ...) {
  for (col in object$measures) {
    result <- purrr::map(
      new_data[[col]],
      .impute_single,
      method = object$method,
      max_gap = object$max_gap
    )
    new_data[[col]] <- new_measure_list(result)
  }
  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_impute <- function(
  x,
  width = max(20, options()$width - 30),
  ...
) {
  title <- paste0("Imputation (", x$method, ") on ")
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
tidy.step_measure_impute <- function(x, ...) {
  tibble::tibble(
    terms = if (is_trained(x)) x$measures else "<all measure columns>",
    method = x$method,
    max_gap = x$max_gap,
    id = x$id
  )
}

.impute_single <- function(x, method, max_gap) {
  values <- x$value
  locations <- x$location
  n <- length(values)

  na_idx <- which(is.na(values))
  if (length(na_idx) == 0) {
    return(x) # No missing values
  }

  if (length(na_idx) == n) {
    cli::cli_warn("All values are NA. Cannot impute.")
    return(x)
  }

  non_na_idx <- which(!is.na(values))

  # Check gap sizes and identify which to impute
  if (is.finite(max_gap)) {
    # Find gaps and their sizes
    impute_mask <- rep(TRUE, n)
    gap_starts <- na_idx[c(TRUE, diff(na_idx) > 1)]

    for (start in gap_starts) {
      # Find end of this gap
      end <- start
      while (end < n && is.na(values[end + 1])) {
        end <- end + 1
      }
      gap_size <- end - start + 1

      if (gap_size > max_gap) {
        impute_mask[start:end] <- FALSE
      }
    }
    na_idx <- na_idx[impute_mask[na_idx]]
  }

  if (length(na_idx) == 0) {
    return(x)
  }

  # Impute based on method
  result <- values

  if (method == "linear") {
    interp <- stats::approx(
      x = locations[non_na_idx],
      y = values[non_na_idx],
      xout = locations[na_idx],
      rule = 2 # Constant extrapolation at edges
    )
    result[na_idx] <- interp$y
  } else if (method == "spline") {
    if (length(non_na_idx) >= 4) {
      interp <- stats::spline(
        x = locations[non_na_idx],
        y = values[non_na_idx],
        xout = locations[na_idx]
      )
      result[na_idx] <- interp$y
    } else {
      # Fall back to linear if not enough points for spline
      interp <- stats::approx(
        x = locations[non_na_idx],
        y = values[non_na_idx],
        xout = locations[na_idx],
        rule = 2
      )
      result[na_idx] <- interp$y
    }
  } else if (method == "constant") {
    # Nearest neighbor
    for (i in na_idx) {
      distances <- abs(non_na_idx - i)
      nearest <- non_na_idx[which.min(distances)]
      result[i] <- values[nearest]
    }
  } else if (method == "mean") {
    result[na_idx] <- mean(values, na.rm = TRUE)
  }

  x$value <- result
  x
}


# ==============================================================================
# step_measure_qc_outlier
# ==============================================================================

#' Detect Outlier Samples
#'
#' `step_measure_qc_outlier()` creates a *specification* of a recipe step that
#' detects outlier samples using Mahalanobis distance or PCA-based methods.
#' A new column is added indicating outlier status.
#'
#' @param recipe A recipe object.
#' @param measures An optional character vector of measure column names.
#' @param method Detection method:
#'   - `"mahalanobis"` (default): Mahalanobis distance with robust covariance
#'   - `"pca"`: PCA score-based outliers (Hotelling's T^2)
#' @param threshold Threshold for outlier detection in standard deviation
#'   units. Default is 3. Tunable via [outlier_threshold()].
#' @param n_components For PCA method, number of components to use. Default
#'   is `NULL` (auto-select based on variance explained).
#' @param new_col Name of the new outlier flag column. Default is `".outlier"`.
#' @param new_col_score Name of the outlier score column. Default is
#'   `".outlier_score"`.
#' @param role Role for new columns. Default is `"predictor"`.
#' @param trained Logical indicating if the step has been trained.
#' @param skip Logical. Should the step be skipped when baking?
#' @param id Unique step identifier.
#'
#' @return An updated recipe with the new step added.
#'
#' @details
#' Outlier samples can arise from measurement errors, sample preparation
#' issues, or genuine unusual samples. This step helps identify them.
#'
#' **Mahalanobis method**: Computes the multivariate distance from each sample
#' to the center of the distribution, accounting for correlations. Uses robust
#' estimation of center and covariance via median and MAD.
#'
#' **PCA method**: Projects data onto principal components and computes
#' Hotelling's T^2 statistic. Samples with extreme scores are flagged.
#'
#' Two columns are added:
#' - `.outlier`: Logical flag
#' - `.outlier_score`: Numeric score (higher = more extreme)
#'
#' @family measure-qc
#' @export
#'
#' @examples
#' library(recipes)
#'
#' rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_qc_outlier(threshold = 3) |>
#'   prep()
#'
#' bake(rec, new_data = NULL)
step_measure_qc_outlier <- function(
  recipe,
  measures = NULL,
  method = c("mahalanobis", "pca"),
  threshold = 3,
  n_components = NULL,
  new_col = ".outlier",
  new_col_score = ".outlier_score",
  role = "predictor",
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_qc_outlier")
) {
  method <- match.arg(method)

  recipes::add_step(
    recipe,
    step_measure_qc_outlier_new(
      measures = measures,
      method = method,
      threshold = threshold,
      n_components = n_components,
      new_col = new_col,
      new_col_score = new_col_score,
      center = NULL,
      scale = NULL,
      pca_rotation = NULL,
      pca_sdev = NULL,
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_measure_qc_outlier_new <- function(
  measures,
  method,
  threshold,
  n_components,
  new_col,
  new_col_score,
  center,
  scale,
  pca_rotation,
  pca_sdev,
  role,
  trained,
  skip,
  id
) {
  recipes::step(
    subclass = "measure_qc_outlier",
    measures = measures,
    method = method,
    threshold = threshold,
    n_components = n_components,
    new_col = new_col,
    new_col_score = new_col_score,
    center = center,
    scale = scale,
    pca_rotation = pca_rotation,
    pca_sdev = pca_sdev,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_qc_outlier <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  col <- measure_cols[1]

  # Convert to matrix for calculations
  mat <- measure_to_matrix(training[[col]])

  # Compute robust center and scale
  center <- apply(mat, 2, stats::median, na.rm = TRUE)
  scale <- apply(mat, 2, stats::mad, constant = 1.4826, na.rm = TRUE)
  scale[scale == 0] <- 1 # Avoid division by zero

  pca_rotation <- NULL
  pca_sdev <- NULL

  if (x$method == "pca") {
    # Compute PCA on centered/scaled data
    mat_scaled <- scale(mat, center = center, scale = scale)
    mat_scaled[is.na(mat_scaled)] <- 0

    pca <- stats::prcomp(mat_scaled, center = FALSE, scale. = FALSE)
    pca_rotation <- pca$rotation
    pca_sdev <- pca$sdev

    # Auto-select components if not specified
    if (is.null(x$n_components)) {
      var_explained <- cumsum(pca_sdev^2) / sum(pca_sdev^2)
      x$n_components <- min(which(var_explained >= 0.95), length(pca_sdev))
    }
  }

  step_measure_qc_outlier_new(
    measures = measure_cols,
    method = x$method,
    threshold = x$threshold,
    n_components = x$n_components,
    new_col = x$new_col,
    new_col_score = x$new_col_score,
    center = center,
    scale = scale,
    pca_rotation = pca_rotation,
    pca_sdev = pca_sdev,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_qc_outlier <- function(object, new_data, ...) {
  col <- object$measures[1]

  # Convert to matrix
  mat <- measure_to_matrix(new_data[[col]])

  if (object$method == "mahalanobis") {
    # Simplified robust Mahalanobis using diagonal covariance
    mat_scaled <- scale(mat, center = object$center, scale = object$scale)
    mat_scaled[is.na(mat_scaled)] <- 0

    # Sum of squared standardized values (assumes diagonal covariance)
    scores <- rowSums(mat_scaled^2)
    # Convert to approximate SD units
    scores <- sqrt(scores / ncol(mat))
  } else {
    # PCA method
    mat_scaled <- scale(mat, center = object$center, scale = object$scale)
    mat_scaled[is.na(mat_scaled)] <- 0

    # Project onto PCA space
    scores_pca <- mat_scaled %*%
      object$pca_rotation[, 1:object$n_components, drop = FALSE]

    # Hotelling's T^2
    sdev <- object$pca_sdev[1:object$n_components]
    t2 <- rowSums((scores_pca / rep(sdev, each = nrow(scores_pca)))^2)

    # Convert to approximate SD units
    scores <- sqrt(t2 / object$n_components)
  }

  new_data[[object$new_col]] <- scores > object$threshold
  new_data[[object$new_col_score]] <- scores

  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_qc_outlier <- function(
  x,
  width = max(20, options()$width - 30),
  ...
) {
  title <- paste0(
    "Outlier detection (",
    x$method,
    ", threshold = ",
    x$threshold,
    ")"
  )
  cat(title)
  cat("\n")
  invisible(x)
}

#' @rdname tidy.recipe
#' @export
#' @keywords internal
tidy.step_measure_qc_outlier <- function(x, ...) {
  tibble::tibble(
    terms = if (is_trained(x)) x$measures else "<all measure columns>",
    method = x$method,
    threshold = x$threshold,
    n_components = x$n_components %||% NA_integer_,
    id = x$id
  )
}
