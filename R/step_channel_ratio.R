# ==============================================================================
# Multi-Channel Ratio Step
#
# Computes ratios between measurement channels.
# ==============================================================================

#' Compute Ratios Between Channels
#'
#' `step_measure_channel_ratio()` creates a *specification* of a recipe step
#' that computes ratios between pairs of measurement channels.
#'
#' @param recipe A recipe object.
#' @param numerator Column name(s) for the numerator channel(s).
#' @param denominator Column name(s) for the denominator channel(s).
#'   Must have same length as `numerator` (paired ratios).
#' @param output_prefix Prefix for output column names. Default is `"ratio_"`.
#' @param epsilon Small value added to denominator to avoid division by zero.
#'   Default is `1e-10`.
#' @param log_transform Logical. Should the ratio be log-transformed?
#'   Default is `FALSE`.
#' @param remove_original Logical. Should original channel columns be removed?
#'   Default is `FALSE`.
#' @param role Not used.
#' @param trained Logical indicating if the step has been trained.
#' @param skip Logical. Should the step be skipped when baking?
#' @param id Unique step identifier.
#'
#' @return An updated recipe with the new step added.
#'
#' @details
#' Channel ratios are useful in analytical chemistry for:
#'
#' - **Normalization**: UV/RI ratios normalize for concentration variations
#' - **Identification**: Characteristic ratios help identify compounds
#' - **Quality control**: Ratio stability indicates system performance
#'
#' ## Output Columns
#'
#' For each numerator/denominator pair, creates a new measure column named
#' `{output_prefix}{numerator}_{denominator}` (e.g., `"ratio_uv_ri"`).
#'
#' ## Log Transform
#'
#' When `log_transform = TRUE`, computes `log(numerator / denominator)` which
#' can be useful for:
#'
#' - Normalizing skewed distributions
#' - Converting multiplicative relationships to additive
#' - Working with absorbance ratios
#'
#' @note
#' Channels must be aligned to the same grid before computing ratios. Use
#' [step_measure_channel_align()] first if grids differ.
#'
#' @family measure-channel
#' @export
#'
#' @examples
#' library(recipes)
#' library(tibble)
#'
#' # Create sample multi-channel data
#' df <- tibble(
#'   id = rep(1:3, each = 10),
#'   time = rep(seq(0, 9, by = 1), 3),
#'   uv = rnorm(30, 100, 10),
#'   ri = rnorm(30, 50, 5),
#'   concentration = rep(c(10, 25, 50), each = 10)
#' )
#'
#' # Compute UV/RI ratio
#' rec <- recipe(concentration ~ ., data = df) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(uv, location = vars(time)) |>
#'   step_measure_input_long(ri, location = vars(time)) |>
#'   step_measure_channel_ratio(numerator = "uv", denominator = "ri")
step_measure_channel_ratio <- function(
  recipe,
  numerator,
  denominator,
  output_prefix = "ratio_",
  epsilon = 1e-10,
  log_transform = FALSE,
  remove_original = FALSE,
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_channel_ratio")
) {
  if (length(numerator) != length(denominator)) {
    cli::cli_abort(
      c(
        "{.arg numerator} and {.arg denominator} must have the same length.",
        "i" = "Got {length(numerator)} numerator{?s} and {length(denominator)} denominator{?s}."
      )
    )
  }

  recipes::add_step(
    recipe,
    step_measure_channel_ratio_new(
      numerator = numerator,
      denominator = denominator,
      output_prefix = output_prefix,
      epsilon = epsilon,
      log_transform = log_transform,
      remove_original = remove_original,
      output_cols = NULL,
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_measure_channel_ratio_new <- function(
  numerator,
  denominator,
  output_prefix,
  epsilon,
  log_transform,
  remove_original,
  output_cols,
  role,
  trained,
  skip,
  id
) {
  recipes::step(
    subclass = "measure_channel_ratio",
    numerator = numerator,
    denominator = denominator,
    output_prefix = output_prefix,
    epsilon = epsilon,
    log_transform = log_transform,
    remove_original = remove_original,
    output_cols = output_cols,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_channel_ratio <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  # Validate columns exist and are measure columns
  measure_cols <- find_measure_cols(training)

  for (col in unique(c(x$numerator, x$denominator))) {
    if (!col %in% measure_cols) {
      cli::cli_abort(
        "Column {.field {col}} is not a measure column."
      )
    }
  }

  # Generate output column names
  output_cols <- paste0(
    x$output_prefix,
    x$numerator,
    "_",
    x$denominator
  )

  # Check for conflicts
  existing <- intersect(output_cols, names(training))
  if (length(existing) > 0) {
    cli::cli_warn(
      "Output column{?s} {.field {existing}} already exist{?s} and will be overwritten."
    )
  }

  # Validate grids match (check first sample)
  for (i in seq_along(x$numerator)) {
    num_grid <- training[[x$numerator[i]]][[1]]$location
    den_grid <- training[[x$denominator[i]]][[1]]$location
    if (!identical(num_grid, den_grid)) {
      cli::cli_warn(
        c(
          "Grids for {.field {x$numerator[i]}} and {.field {x$denominator[i]}} differ.",
          "i" = "Use {.fn step_measure_channel_align} first for accurate ratios."
        )
      )
    }
  }

  step_measure_channel_ratio_new(
    numerator = x$numerator,
    denominator = x$denominator,
    output_prefix = x$output_prefix,
    epsilon = x$epsilon,
    log_transform = x$log_transform,
    remove_original = x$remove_original,
    output_cols = output_cols,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_channel_ratio <- function(object, new_data, ...) {
  n_samples <- nrow(new_data)

  for (i in seq_along(object$numerator)) {
    num_col <- object$numerator[i]
    den_col <- object$denominator[i]
    out_col <- object$output_cols[i]

    result <- vector("list", n_samples)
    for (j in seq_len(n_samples)) {
      num_m <- new_data[[num_col]][[j]]
      den_m <- new_data[[den_col]][[j]]

      result[[j]] <- .compute_ratio(
        num_m,
        den_m,
        object$epsilon,
        object$log_transform
      )
    }

    new_data[[out_col]] <- new_measure_list(result)
  }

  # Remove original columns if requested
  if (object$remove_original) {
    cols_to_remove <- unique(c(object$numerator, object$denominator))
    new_data <- new_data[,
      setdiff(names(new_data), cols_to_remove),
      drop = FALSE
    ]
  }

  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_channel_ratio <- function(
  x,
  width = max(20, options()$width - 30),
  ...
) {
  if (x$trained) {
    pairs <- paste0(x$numerator, "/", x$denominator)
    cat("Channel ratio for ", paste(pairs, collapse = ", "), sep = "")
  } else {
    cat("Channel ratio <pending>")
  }
  if (x$log_transform) {
    cat(" [log-transformed]")
  }
  cat("\n")
  invisible(x)
}

#' @rdname tidy.recipe
#' @export
#' @keywords internal
tidy.step_measure_channel_ratio <- function(x, ...) {
  if (is_trained(x)) {
    tibble::tibble(
      numerator = x$numerator,
      denominator = x$denominator,
      output_col = x$output_cols,
      log_transform = x$log_transform,
      epsilon = x$epsilon,
      id = x$id
    )
  } else {
    tibble::tibble(
      numerator = x$numerator,
      denominator = x$denominator,
      output_col = NA_character_,
      log_transform = x$log_transform,
      epsilon = x$epsilon,
      id = x$id
    )
  }
}

# ------------------------------------------------------------------------------
# Internal helper
# ------------------------------------------------------------------------------

.compute_ratio <- function(num_m, den_m, epsilon, log_transform) {
  # Handle mismatched grids by using numerator's grid
  if (!identical(num_m$location, den_m$location)) {
    # Interpolate denominator to numerator's grid
    den_values <- stats::approx(
      den_m$location,
      den_m$value,
      xout = num_m$location,
      rule = 2
    )$y
  } else {
    den_values <- den_m$value
  }

  # Compute ratio with epsilon to avoid division by zero
  ratio_values <- num_m$value / (den_values + epsilon)

  if (log_transform) {
    # Handle negative values before log
    ratio_values <- ifelse(ratio_values > 0, log(ratio_values), NA_real_)
  }

  new_measure_tbl(
    location = num_m$location,
    value = ratio_values
  )
}
