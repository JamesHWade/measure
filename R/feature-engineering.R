# ==============================================================================
# Feature Engineering Steps
#
# This file contains steps for extracting features from measurement data:
# - step_measure_integrals: Calculate integrated areas for specified regions
# - step_measure_ratios: Calculate ratios between integrated regions
# - step_measure_moments: Calculate statistical moments from spectra
# - step_measure_bin: Reduce spectrum to fewer points via binning
# ==============================================================================

# ==============================================================================
# step_measure_integrals
# ==============================================================================

#' Calculate Region Integrals
#'
#' `step_measure_integrals()` creates a *specification* of a recipe step that
#' calculates integrated areas for specified x-axis regions.
#'
#' @param recipe A recipe object.
#' @param regions A named or unnamed list of numeric vectors, each of length 2
#'   specifying regions as `c(min, max)`. For example:
#'   `list(peak1 = c(1000, 1100), peak2 = c(1500, 1600))`.
#' @param method Integration method: `"trapezoid"` (default) or `"simpson"`.
#' @param measures An optional character vector of measure column names.
#' @param prefix Prefix for output column names. Default is `"integral_"`.
#' @param role Role for generated columns. Default is `"predictor"`.
#' @param trained Logical indicating if the step has been trained.
#' @param skip Logical. Should the step be skipped when baking?
#' @param id Unique step identifier.
#'
#' @return An updated recipe with the new step added.
#'
#' @details
#' This step calculates the integrated area under the curve for each specified
#' region. The result is added as new predictor columns, one per region.
#'
#' **Column naming:**
#' - If regions are named: `prefix + name` (e.g., `"integral_peak1"`)
#' - If regions are unnamed: `prefix + index` (e.g., `"integral_1"`)
#'
#' **Integration methods:**
#' - `"trapezoid"`: Trapezoidal rule, fast and accurate for smooth data
#' - `"simpson"`: Simpson's rule, more accurate for smooth curves
#'
#' @family measure-features
#' @export
#'
#' @examples
#' library(recipes)
#'
#' rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_integrals(
#'     regions = list(low = c(1, 30), mid = c(40, 60), high = c(70, 100))
#'   ) |>
#'   prep()
#'
#' bake(rec, new_data = NULL)
step_measure_integrals <- function(
    recipe,
    regions,
    method = c("trapezoid", "simpson"),
    measures = NULL,
    prefix = "integral_",
    role = "predictor",
    trained = FALSE,
    skip = FALSE,
    id = recipes::rand_id("measure_integrals")) {

  method <- rlang::arg_match(method)

  if (missing(regions)) {
    cli::cli_abort("{.arg regions} must be provided.")
  }

  # Validate regions
  if (!is.list(regions)) {
    cli::cli_abort("{.arg regions} must be a list of numeric vectors.")
  }

  for (i in seq_along(regions)) {
    r <- regions[[i]]
    if (!is.numeric(r) || length(r) != 2) {
      cli::cli_abort(
        "Each region must be a numeric vector of length 2. Region {i} is invalid."
      )
    }
    if (r[1] >= r[2]) {
      cli::cli_abort(
        "Each region must have min < max. Region {i}: c({r[1]}, {r[2]})."
      )
    }
  }

  # Ensure regions have names
  if (is.null(names(regions))) {
    names(regions) <- as.character(seq_along(regions))
  } else {
    # Fill in empty names
    empty_names <- names(regions) == ""
    names(regions)[empty_names] <- as.character(which(empty_names))
  }

  recipes::add_step(
    recipe,
    step_measure_integrals_new(
      regions = regions,
      method = method,
      measures = measures,
      prefix = prefix,
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_measure_integrals_new <- function(
    regions, method, measures, prefix, role, trained, skip, id) {
  recipes::step(
    subclass = "measure_integrals",
    regions = regions,
    method = method,
    measures = measures,
    prefix = prefix,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_integrals <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  step_measure_integrals_new(
    regions = x$regions,
    method = x$method,
    measures = measure_cols,
    prefix = x$prefix,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' Calculate integral for a single region
#' @noRd
.integrate_region <- function(location, value, region, method) {
  # Filter to region
  idx <- location >= region[1] & location <= region[2]

  if (sum(idx) < 2) {
    return(NA_real_)
  }

  x <- location[idx]
  y <- value[idx]

  if (method == "trapezoid") {
    # Trapezoidal rule: sum of (y[i] + y[i+1])/2 * (x[i+1] - x[i])
    n <- length(x)
    dx <- diff(x)
    area <- sum((y[-n] + y[-1]) / 2 * dx)
  } else if (method == "simpson") {
    # Simpson's rule (requires odd number of points for pure Simpson)
    n <- length(x)
    if (n < 3) {
      # Fall back to trapezoid
      dx <- diff(x)
      area <- sum((y[-n] + y[-1]) / 2 * dx)
    } else {
      # Composite Simpson's rule
      h <- (x[n] - x[1]) / (n - 1)
      # For non-uniform spacing, use weighted sum
      area <- 0
      for (i in seq(1, n - 2, by = 2)) {
        if (i + 2 <= n) {
          h1 <- x[i + 1] - x[i]
          h2 <- x[i + 2] - x[i + 1]
          area <- area + (h1 + h2) / 6 * (
            y[i] * (2 - h2 / h1) +
              y[i + 1] * (h1 + h2)^2 / (h1 * h2) +
              y[i + 2] * (2 - h1 / h2)
          )
        }
      }
      # Handle last segment if odd number of intervals
      if (n %% 2 == 0) {
        area <- area + (x[n] - x[n - 1]) * (y[n] + y[n - 1]) / 2
      }
    }
  }

  area
}

#' @export
bake.step_measure_integrals <- function(object, new_data, ...) {
  regions <- object$regions
  method <- object$method
  prefix <- object$prefix
  measure_col <- object$measures[1]

  # Calculate integrals for each sample and region
  all_results <- purrr::map(new_data[[measure_col]], function(m) {
    purrr::map_dbl(regions, function(region) {
      .integrate_region(m$location, m$value, region, method)
    })
  })

  # Convert to data frame
  result_df <- do.call(rbind, all_results)
  result_df <- tibble::as_tibble(result_df)

  # Add prefix to column names
  names(result_df) <- paste0(prefix, names(result_df))

  # Bind to original data
  new_data <- dplyr::bind_cols(new_data, result_df)

  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_integrals <- function(x, width = max(20, options()$width - 30), ...) {
  n_regions <- length(x$regions)
  title <- paste0("Calculate integrals for ", n_regions, " region(s)")
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
tidy.step_measure_integrals <- function(x, ...) {
  tibble::tibble(
    regions = list(x$regions),
    method = x$method,
    prefix = x$prefix,
    id = x$id
  )
}

#' @rdname required_pkgs.recipe
#' @export
#' @keywords internal
required_pkgs.step_measure_integrals <- function(x, ...) {
  c("measure")
}

# ==============================================================================
# step_measure_ratios
# ==============================================================================

#' Calculate Region Ratios
#'
#' `step_measure_ratios()` creates a *specification* of a recipe step that
#' calculates ratios between integrated regions.
#'
#' @param recipe A recipe object.
#' @param numerator A numeric vector of length 2 specifying the numerator region.
#' @param denominator A numeric vector of length 2 specifying the denominator region.
#' @param name Output column name. If `NULL`, auto-generated from prefix.
#' @param method Integration method: `"trapezoid"` (default) or `"simpson"`.
#' @param measures An optional character vector of measure column names.
#' @param prefix Prefix for output column name if `name` is NULL. Default is `"ratio_"`.
#' @param role Role for generated column. Default is `"predictor"`.
#' @param trained Logical indicating if the step has been trained.
#' @param skip Logical. Should the step be skipped when baking?
#' @param id Unique step identifier.
#'
#' @return An updated recipe with the new step added.
#'
#' @details
#' This step calculates the ratio of integrated areas between two regions:
#' `ratio = integral(numerator) / integral(denominator)`
#'
#' This is useful for calculating peak ratios in spectroscopy, or relative
#' concentrations in chromatography.
#'
#' If the denominator integral is zero or NA, the ratio will be NA.
#'
#' @family measure-features
#' @export
#'
#' @examples
#' library(recipes)
#'
#' rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_ratios(
#'     numerator = c(1, 30),
#'     denominator = c(70, 100),
#'     name = "low_high_ratio"
#'   ) |>
#'   prep()
#'
#' bake(rec, new_data = NULL)
step_measure_ratios <- function(
    recipe,
    numerator,
    denominator,
    name = NULL,
    method = c("trapezoid", "simpson"),
    measures = NULL,
    prefix = "ratio_",
    role = "predictor",
    trained = FALSE,
    skip = FALSE,
    id = recipes::rand_id("measure_ratios")) {

  method <- rlang::arg_match(method)

  # Validate numerator
  if (missing(numerator) || !is.numeric(numerator) || length(numerator) != 2) {
    cli::cli_abort("{.arg numerator} must be a numeric vector of length 2.")
  }
  if (numerator[1] >= numerator[2]) {
    cli::cli_abort("{.arg numerator} must have min < max.")
  }

  # Validate denominator
  if (missing(denominator) || !is.numeric(denominator) || length(denominator) != 2) {
    cli::cli_abort("{.arg denominator} must be a numeric vector of length 2.")
  }
  if (denominator[1] >= denominator[2]) {
    cli::cli_abort("{.arg denominator} must have min < max.")
  }

  # Generate name if not provided
  if (is.null(name)) {
    name <- paste0(prefix, "1")
  }

  recipes::add_step(
    recipe,
    step_measure_ratios_new(
      numerator = numerator,
      denominator = denominator,
      name = name,
      method = method,
      measures = measures,
      prefix = prefix,
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_measure_ratios_new <- function(
    numerator, denominator, name, method, measures, prefix, role, trained, skip, id) {
  recipes::step(
    subclass = "measure_ratios",
    numerator = numerator,
    denominator = denominator,
    name = name,
    method = method,
    measures = measures,
    prefix = prefix,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_ratios <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  step_measure_ratios_new(
    numerator = x$numerator,
    denominator = x$denominator,
    name = x$name,
    method = x$method,
    measures = measure_cols,
    prefix = x$prefix,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_ratios <- function(object, new_data, ...) {
  numerator <- object$numerator
  denominator <- object$denominator
  method <- object$method
  name <- object$name
  measure_col <- object$measures[1]

  # Calculate ratios for each sample
  ratios <- purrr::map_dbl(new_data[[measure_col]], function(m) {
    num_int <- .integrate_region(m$location, m$value, numerator, method)
    den_int <- .integrate_region(m$location, m$value, denominator, method)

    if (is.na(den_int) || abs(den_int) < .Machine$double.eps) {
      NA_real_
    } else {
      num_int / den_int
    }
  })

  # Create result column
  result_df <- tibble::tibble(!!name := ratios)

  # Bind to original data
  new_data <- dplyr::bind_cols(new_data, result_df)

  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_ratios <- function(x, width = max(20, options()$width - 30), ...) {
  title <- paste0(
    "Calculate ratio [", x$numerator[1], ",", x$numerator[2], "] / [",
    x$denominator[1], ",", x$denominator[2], "]"
  )
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
tidy.step_measure_ratios <- function(x, ...) {
  tibble::tibble(
    name = x$name,
    numerator_min = x$numerator[1],
    numerator_max = x$numerator[2],
    denominator_min = x$denominator[1],
    denominator_max = x$denominator[2],
    method = x$method,
    id = x$id
  )
}

#' @rdname required_pkgs.recipe
#' @export
#' @keywords internal
required_pkgs.step_measure_ratios <- function(x, ...) {
  c("measure")
}

# ==============================================================================
# step_measure_moments
# ==============================================================================

#' Calculate Statistical Moments
#'
#' `step_measure_moments()` creates a *specification* of a recipe step that
#' calculates statistical moments from spectra.
#'
#' @param recipe A recipe object.
#' @param moments Character vector specifying which moments to calculate.
#'   Options: `"mean"`, `"sd"`, `"skewness"`, `"kurtosis"`, `"entropy"`.
#'   Default is `c("mean", "sd", "skewness", "kurtosis")`.
#' @param weighted Logical. If `TRUE`, moments are weighted by location values.
#'   Default is `FALSE`.
#' @param measures An optional character vector of measure column names.
#' @param prefix Prefix for output column names. Default is `"moment_"`.
#' @param role Role for generated columns. Default is `"predictor"`.
#' @param trained Logical indicating if the step has been trained.
#' @param skip Logical. Should the step be skipped when baking?
#' @param id Unique step identifier.
#'
#' @return An updated recipe with the new step added.
#'
#' @details
#' This step calculates statistical moments that summarize the distribution
#' of values in each spectrum:
#'
#' | Moment | Description |
#' |--------|-------------|
#' | mean | Mean value of the spectrum |
#' | sd | Standard deviation of values |
#' | skewness | Asymmetry of the distribution |
#' | kurtosis | "Tailedness" of the distribution |
#' | entropy | Shannon entropy (requires positive values) |
#'
#' When `weighted = TRUE`, the location (x-axis) values are used as weights,
#' which can be useful for calculating center of mass or weighted statistics.
#'
#' @family measure-features
#' @export
#'
#' @examples
#' library(recipes)
#'
#' rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_moments(moments = c("mean", "sd", "skewness")) |>
#'   prep()
#'
#' bake(rec, new_data = NULL)
step_measure_moments <- function(
    recipe,
    moments = c("mean", "sd", "skewness", "kurtosis"),
    weighted = FALSE,
    measures = NULL,
    prefix = "moment_",
    role = "predictor",
    trained = FALSE,
    skip = FALSE,
    id = recipes::rand_id("measure_moments")) {

  valid_moments <- c("mean", "sd", "skewness", "kurtosis", "entropy")
  if (!all(moments %in% valid_moments)) {
    invalid <- setdiff(moments, valid_moments)
    cli::cli_abort(
      "Invalid moments: {.val {invalid}}. Must be one of: {.val {valid_moments}}"
    )
  }

  recipes::add_step(
    recipe,
    step_measure_moments_new(
      moments = moments,
      weighted = weighted,
      measures = measures,
      prefix = prefix,
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_measure_moments_new <- function(
    moments, weighted, measures, prefix, role, trained, skip, id) {
  recipes::step(
    subclass = "measure_moments",
    moments = moments,
    weighted = weighted,
    measures = measures,
    prefix = prefix,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_moments <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  step_measure_moments_new(
    moments = x$moments,
    weighted = x$weighted,
    measures = measure_cols,
    prefix = x$prefix,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' Calculate moments for a single spectrum
#' @noRd
.calc_moments <- function(location, value, moments, weighted) {
  result <- numeric(length(moments))
  names(result) <- moments

  n <- length(value)
  if (n < 2) {
    return(rep(NA_real_, length(moments)))
  }

  if (weighted) {
    # Use location as weights
    w <- abs(location)
    w <- w / sum(w)
  } else {
    w <- rep(1 / n, n)
  }

  if ("mean" %in% moments) {
    result["mean"] <- sum(w * value)
  }

  if ("sd" %in% moments) {
    m <- sum(w * value)
    result["sd"] <- sqrt(sum(w * (value - m)^2))
  }

  if ("skewness" %in% moments) {
    m <- sum(w * value)
    s <- sqrt(sum(w * (value - m)^2))
    if (s > 0) {
      result["skewness"] <- sum(w * ((value - m) / s)^3)
    } else {
      result["skewness"] <- NA_real_
    }
  }

  if ("kurtosis" %in% moments) {
    m <- sum(w * value)
    s <- sqrt(sum(w * (value - m)^2))
    if (s > 0) {
      result["kurtosis"] <- sum(w * ((value - m) / s)^4) - 3
    } else {
      result["kurtosis"] <- NA_real_
    }
  }

  if ("entropy" %in% moments) {
    # Shannon entropy - requires positive values
    p <- pmax(value, 0)
    p <- p / sum(p)
    p <- p[p > 0]  # Remove zeros to avoid log(0)
    if (length(p) > 0) {
      result["entropy"] <- -sum(p * log(p))
    } else {
      result["entropy"] <- NA_real_
    }
  }

  result
}

#' @export
bake.step_measure_moments <- function(object, new_data, ...) {
  moments <- object$moments
  weighted <- object$weighted
  prefix <- object$prefix
  measure_col <- object$measures[1]

  # Calculate moments for each sample
  all_results <- purrr::map(new_data[[measure_col]], function(m) {
    .calc_moments(m$location, m$value, moments, weighted)
  })

  # Convert to data frame
  result_df <- do.call(rbind, all_results)
  result_df <- tibble::as_tibble(result_df)

  # Add prefix to column names
  names(result_df) <- paste0(prefix, names(result_df))

  # Bind to original data
  new_data <- dplyr::bind_cols(new_data, result_df)

  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_moments <- function(x, width = max(20, options()$width - 30), ...) {
  moments_str <- paste(x$moments, collapse = ", ")
  title <- paste0("Calculate moments: ", moments_str)
  if (x$weighted) {
    title <- paste0(title, " (weighted)")
  }
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
tidy.step_measure_moments <- function(x, ...) {
  tibble::tibble(
    moments = list(x$moments),
    weighted = x$weighted,
    prefix = x$prefix,
    id = x$id
  )
}

#' @rdname required_pkgs.recipe
#' @export
#' @keywords internal
required_pkgs.step_measure_moments <- function(x, ...) {
  c("measure")
}

# ==============================================================================
# step_measure_bin
# ==============================================================================

#' Spectral Binning
#'
#' `step_measure_bin()` creates a *specification* of a recipe step that
#' reduces a spectrum to fewer points by averaging within bins.
#'
#' @param recipe A recipe object.
#' @param n_bins Number of bins (mutually exclusive with `bin_width`).
#' @param bin_width Width of each bin in location units (mutually exclusive
#'   with `n_bins`).
#' @param method Aggregation method: `"mean"` (default), `"sum"`, `"median"`,
#'   or `"max"`.
#' @param measures An optional character vector of measure column names.
#' @param role Not used (modifies existing data).
#' @param trained Logical indicating if the step has been trained.
#' @param bin_breaks The computed bin breaks (after training).
#' @param skip Logical. Should the step be skipped when baking?
#' @param id Unique step identifier.
#'
#' @return An updated recipe with the new step added.
#'
#' @details
#' This step reduces the number of points in each spectrum by dividing the
#' x-axis into bins and aggregating values within each bin. The result
#' replaces the `.measures` column with the binned data.
#'
#' This is useful for:
#' - Reducing data dimensionality
#' - Decreasing noise through averaging
#' - Speeding up downstream processing
#' - Aligning data from different resolutions
#'
#' The bin boundaries are determined during `prep()` from the training data
#' and stored for consistent application to new data.
#'
#' @family measure-features
#' @export
#'
#' @examples
#' library(recipes)
#'
#' # Bin to 20 points
#' rec <- recipe(water + fat + protein ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_bin(n_bins = 20) |>
#'   prep()
#'
#' bake(rec, new_data = NULL)
step_measure_bin <- function(
    recipe,
    n_bins = NULL,
    bin_width = NULL,
    method = c("mean", "sum", "median", "max"),
    measures = NULL,
    role = NA,
    trained = FALSE,
    bin_breaks = NULL,
    skip = FALSE,
    id = recipes::rand_id("measure_bin")) {

  method <- rlang::arg_match(method)

  # Must specify exactly one of n_bins or bin_width
  has_n <- !is.null(n_bins)
  has_width <- !is.null(bin_width)

  if (!has_n && !has_width) {
    cli::cli_abort("Either {.arg n_bins} or {.arg bin_width} must be specified.")
  }
  if (has_n && has_width) {
    cli::cli_abort("{.arg n_bins} and {.arg bin_width} are mutually exclusive.")
  }

  if (has_n) {
    if (!is.numeric(n_bins) || length(n_bins) != 1 || n_bins < 2 || n_bins != round(n_bins)) {
      cli::cli_abort("{.arg n_bins} must be a positive integer >= 2.")
    }
  }

  if (has_width) {
    if (!is.numeric(bin_width) || length(bin_width) != 1 || bin_width <= 0) {
      cli::cli_abort("{.arg bin_width} must be a positive number.")
    }
  }

  recipes::add_step(
    recipe,
    step_measure_bin_new(
      n_bins = n_bins,
      bin_width = bin_width,
      method = method,
      measures = measures,
      role = role,
      trained = trained,
      bin_breaks = bin_breaks,
      skip = skip,
      id = id
    )
  )
}

step_measure_bin_new <- function(
    n_bins, bin_width, method, measures, role, trained, bin_breaks, skip, id) {
  recipes::step(
    subclass = "measure_bin",
    n_bins = n_bins,
    bin_width = bin_width,
    method = method,
    measures = measures,
    role = role,
    trained = trained,
    bin_breaks = bin_breaks,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_bin <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  if (is.null(x$measures)) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- x$measures
  }

  # Determine bin breaks from training data
  all_locs <- unlist(purrr::map(training[[measure_cols[1]]], ~ .x$location))
  loc_min <- min(all_locs, na.rm = TRUE)
  loc_max <- max(all_locs, na.rm = TRUE)

  if (!is.null(x$n_bins)) {
    bin_breaks <- seq(loc_min, loc_max, length.out = x$n_bins + 1)
  } else {
    bin_breaks <- seq(loc_min, loc_max, by = x$bin_width)
    # Ensure we include the endpoint
    if (utils::tail(bin_breaks, 1) < loc_max) {
      bin_breaks <- c(bin_breaks, loc_max)
    }
  }

  step_measure_bin_new(
    n_bins = x$n_bins,
    bin_width = x$bin_width,
    method = x$method,
    measures = measure_cols,
    role = x$role,
    trained = TRUE,
    bin_breaks = bin_breaks,
    skip = x$skip,
    id = x$id
  )
}

#' Bin a single spectrum
#' @noRd
.bin_spectrum <- function(location, value, bin_breaks, method) {
  n_bins <- length(bin_breaks) - 1

  # Compute bin centers
  bin_centers <- (bin_breaks[-1] + bin_breaks[-length(bin_breaks)]) / 2

  # Assign each point to a bin
  bin_idx <- findInterval(location, bin_breaks, all.inside = TRUE)

  # Aggregate by bin
  agg_fn <- switch(
    method,
    mean = function(x) if (length(x) > 0) mean(x, na.rm = TRUE) else NA_real_,
    sum = function(x) if (length(x) > 0) sum(x, na.rm = TRUE) else NA_real_,
    median = function(x) if (length(x) > 0) stats::median(x, na.rm = TRUE) else NA_real_,
    max = function(x) if (length(x) > 0) max(x, na.rm = TRUE) else NA_real_
  )

  binned_values <- numeric(n_bins)
  for (i in seq_len(n_bins)) {
    vals_in_bin <- value[bin_idx == i]
    binned_values[i] <- agg_fn(vals_in_bin)
  }

  new_measure_tbl(
    location = bin_centers,
    value = binned_values
  )
}

#' @export
bake.step_measure_bin <- function(object, new_data, ...) {
  bin_breaks <- object$bin_breaks
  method <- object$method

  for (col in object$measures) {
    result <- purrr::map(new_data[[col]], function(m) {
      .bin_spectrum(m$location, m$value, bin_breaks, method)
    })
    new_data[[col]] <- new_measure_list(result)
  }

  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_bin <- function(x, width = max(20, options()$width - 30), ...) {
  if (x$trained) {
    n_bins <- length(x$bin_breaks) - 1
    title <- paste0("Bin to ", n_bins, " points (", x$method, ")")
    cat(title, " on <internal measurements>", sep = "")
  } else {
    if (!is.null(x$n_bins)) {
      title <- paste0("Bin to ", x$n_bins, " bins (", x$method, ")")
    } else {
      title <- paste0("Bin with width ", x$bin_width, " (", x$method, ")")
    }
    cat(title)
  }
  cat("\n")
  invisible(x)
}

#' @rdname tidy.recipe
#' @export
#' @keywords internal
tidy.step_measure_bin <- function(x, ...) {
  if (is_trained(x)) {
    tibble::tibble(
      n_bins = length(x$bin_breaks) - 1,
      method = x$method,
      id = x$id
    )
  } else {
    tibble::tibble(
      n_bins = x$n_bins %||% NA_integer_,
      bin_width = x$bin_width %||% NA_real_,
      method = x$method,
      id = x$id
    )
  }
}

#' @rdname required_pkgs.recipe
#' @export
#' @keywords internal
required_pkgs.step_measure_bin <- function(x, ...) {
  c("measure")
}

#' @rdname tunable_measure
#' @export
# Note: n_bins and bin_width are mutually exclusive parameters.
# Both are listed as tunable so users can choose which to tune.
# When using tune(), only specify tune() for one of them.
tunable.step_measure_bin <- function(x, ...) {
  tibble::tibble(
    name = c("n_bins", "bin_width"),
    call_info = list(
      list(pkg = "dials", fun = "num_bins"),
      list(pkg = "measure", fun = "bin_width")
    ),
    source = "recipe",
    component = "step_measure_bin",
    component_id = x$id
  )
}
