# ==============================================================================
# Multi-Channel Combination Step
#
# Combines multiple measure columns into one using various strategies.
# ==============================================================================

#' Combine Multiple Channels
#'
#' `step_measure_channel_combine()` creates a *specification* of a recipe step
#' that combines multiple measurement channels into a single representation.
#'
#' @param recipe A recipe object.
#' @param ... One or more selector functions to choose measure columns.
#'   If empty, all measure columns are used.
#' @param strategy How to combine channels:
#'
#'   - `"stack"`: Stack channels into an nD measurement with channel as a dimension
#'   - `"concat"`: Concatenate channels into a single 1D measurement
#'   - `"weighted_sum"`: Compute weighted sum across channels
#'   - `"mean"`: Average across channels (equal weights)
#'
#' @param weights For `strategy = "weighted_sum"`, a numeric vector of weights.
#'   Must have same length as number of channels. Default is equal weights.
#' @param output_col Name of the output measure column. Default is `".measures"`.
#' @param remove_original Logical. Should original channel columns be removed?
#'   Default is `TRUE`.
#' @param role Not used.
#' @param trained Logical indicating if the step has been trained.
#' @param skip Logical. Should the step be skipped when baking?
#' @param id Unique step identifier.
#'
#' @return An updated recipe with the new step added.
#'
#' @details
#' After aligning multiple channels to a common grid with [step_measure_channel_align()],
#' this step combines them for downstream analysis. The choice of strategy depends
#' on the analysis goal:
#'
#' ## Strategies
#'
#' - **stack**: Creates an n-dimensional measurement where channel becomes a
#'   dimension. Useful for multi-way analysis (PARAFAC, Tucker).
#' - **concat**: Concatenates all channels end-to-end into a single long vector.
#'   Useful for PLS or other models that expect 1D input.
#' - **weighted_sum**: Computes a weighted combination of channel values at each
#'   location. Useful when channels should be fused into a single signal.
#' - **mean**: Simple average across channels (special case of weighted_sum).
#'
#' @note
#' Channels must be aligned to the same grid before combining. Use
#' [step_measure_channel_align()] first if grids differ.
#'
#' @family measure-channel
#' @export
#'
#' @examples
#' library(recipes)
#' library(tibble)
#'
#' # Create sample multi-channel data (already aligned)
#' df <- tibble(
#'   id = rep(1:3, each = 10),
#'   time = rep(seq(0, 9, by = 1), 3),
#'   uv = rnorm(30, 100, 10),
#'   ri = rnorm(30, 50, 5),
#'   concentration = rep(c(10, 25, 50), each = 10)
#' )
#'
#' # Ingest and combine with stacking
#' rec <- recipe(concentration ~ ., data = df) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(uv, location = vars(time)) |>
#'   step_measure_input_long(ri, location = vars(time)) |>
#'   step_measure_channel_combine(strategy = "stack")
step_measure_channel_combine <- function(
  recipe,
  ...,
  strategy = c("stack", "concat", "weighted_sum", "mean"),
  weights = NULL,
  output_col = ".measures",
  remove_original = TRUE,
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_channel_combine")
) {
  strategy <- match.arg(strategy)

  recipes::add_step(
    recipe,
    step_measure_channel_combine_new(
      terms = rlang::enquos(...),
      strategy = strategy,
      weights = weights,
      output_col = output_col,
      remove_original = remove_original,
      measure_cols = NULL,
      channel_names = NULL,
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_measure_channel_combine_new <- function(
  terms,
  strategy,
  weights,
  output_col,
  remove_original,
  measure_cols,
  channel_names,
  role,
  trained,
  skip,
  id
) {
  recipes::step(
    subclass = "measure_channel_combine",
    terms = terms,
    strategy = strategy,
    weights = weights,
    output_col = output_col,
    remove_original = remove_original,
    measure_cols = measure_cols,
    channel_names = channel_names,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_channel_combine <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  # Get measure columns
  if (length(x$terms) == 0) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- recipes::recipes_eval_select(x$terms, training, info)
    actual_measure <- find_measure_cols(training)
    invalid <- setdiff(measure_cols, actual_measure)
    if (length(invalid) > 0) {
      cli::cli_abort(
        "Column{?s} {.field {invalid}} {?is/are} not measure column{?s}."
      )
    }
  }

  if (length(measure_cols) < 2) {
    cli::cli_abort(
      "Channel combination requires at least 2 measure columns, found {length(measure_cols)}."
    )
  }

  # Validate weights for weighted_sum
  weights <- x$weights
  if (x$strategy == "weighted_sum") {
    if (is.null(weights)) {
      weights <- rep(1 / length(measure_cols), length(measure_cols))
    } else if (length(weights) != length(measure_cols)) {
      cli::cli_abort(
        c(
          "Weights length must match number of channels.",
          "i" = "Got {length(weights)} weights for {length(measure_cols)} channels."
        )
      )
    }
  } else if (x$strategy == "mean") {
    weights <- rep(1 / length(measure_cols), length(measure_cols))
  }

  # Validate grids are aligned (check first sample)
  grids <- lapply(measure_cols, function(col) training[[col]][[1]]$location)
  if (!all(sapply(grids[-1], function(g) identical(g, grids[[1]])))) {
    cli::cli_warn(
      c(
        "Channels have different grids.",
        "i" = "Use {.fn step_measure_channel_align} first for best results."
      )
    )
  }

  # Store channel names for stacking
  channel_names <- gsub("^\\.measures_?", "", measure_cols)
  channel_names <- ifelse(channel_names == "", measure_cols, channel_names)

  step_measure_channel_combine_new(
    terms = x$terms,
    strategy = x$strategy,
    weights = weights,
    output_col = x$output_col,
    remove_original = x$remove_original,
    measure_cols = measure_cols,
    channel_names = channel_names,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_channel_combine <- function(object, new_data, ...) {
  n_samples <- nrow(new_data)

  combined <- switch(
    object$strategy,
    "stack" = .combine_stack(new_data, object$measure_cols, object$channel_names),
    "concat" = .combine_concat(new_data, object$measure_cols),
    "weighted_sum" = .combine_weighted(new_data, object$measure_cols, object$weights),
    "mean" = .combine_weighted(new_data, object$measure_cols, object$weights)
  )

  # Add combined column
  new_data[[object$output_col]] <- combined

  # Remove original columns if requested
  if (object$remove_original) {
    new_data <- new_data[, setdiff(names(new_data), object$measure_cols), drop = FALSE]
  }

  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_channel_combine <- function(
  x,
  width = max(20, options()$width - 30),
  ...
) {
  title <- paste0("Channel combination (strategy = ", x$strategy, ") ")
  if (x$trained) {
    cat(title, "from ", paste(x$measure_cols, collapse = ", "),
        " to ", x$output_col, sep = "")
  } else {
    cat(title, "<pending>", sep = "")
  }
  cat("\n")
  invisible(x)
}

#' @rdname tidy.recipe
#' @export
#' @keywords internal
tidy.step_measure_channel_combine <- function(x, ...) {
  if (is_trained(x)) {
    tibble::tibble(
      terms = x$measure_cols,
      strategy = x$strategy,
      output_col = x$output_col,
      id = x$id
    )
  } else {
    tibble::tibble(
      terms = rlang::na_chr,
      strategy = x$strategy,
      output_col = x$output_col,
      id = x$id
    )
  }
}

# ------------------------------------------------------------------------------
# Internal combine functions
# ------------------------------------------------------------------------------

.combine_stack <- function(data, measure_cols, channel_names) {
  n_samples <- nrow(data)

  result <- vector("list", n_samples)
  for (i in seq_len(n_samples)) {
    # Get all channels for this sample
    channels <- lapply(measure_cols, function(col) data[[col]][[i]])

    # Stack into nD (location × channel)
    result[[i]] <- .stack_channels(channels, channel_names)
  }

  new_measure_nd_list(result)
}

.stack_channels <- function(channels, channel_names) {
  # Use first channel's locations
  locations <- channels[[1]]$location
  n_locs <- length(locations)
  n_channels <- length(channels)

  # Build nD tibble
  location_1 <- rep(locations, n_channels)
  location_2 <- rep(seq_len(n_channels), each = n_locs)
  values <- unlist(lapply(channels, function(ch) ch$value))

  new_measure_nd_tbl(
    location_1 = location_1,
    location_2 = location_2,
    value = values,
    dim_names = c("location", "channel"),
    dim_units = c(NA_character_, NA_character_)
  )
}

.combine_concat <- function(data, measure_cols) {
  n_samples <- nrow(data)

  result <- vector("list", n_samples)
  for (i in seq_len(n_samples)) {
    # Concatenate all channels
    all_locs <- c()
    all_vals <- c()
    offset <- 0

    for (col in measure_cols) {
      m <- data[[col]][[i]]
      # Offset locations to make them unique
      all_locs <- c(all_locs, m$location + offset)
      all_vals <- c(all_vals, m$value)
      offset <- max(all_locs) + 1
    }

    result[[i]] <- new_measure_tbl(
      location = all_locs,
      value = all_vals
    )
  }

  new_measure_list(result)
}

.combine_weighted <- function(data, measure_cols, weights) {
  n_samples <- nrow(data)

  result <- vector("list", n_samples)
  for (i in seq_len(n_samples)) {
    # Get reference locations from first channel
    ref <- data[[measure_cols[1]]][[i]]
    locations <- ref$location

    # Weighted sum of values
    values <- rep(0, length(locations))
    for (j in seq_along(measure_cols)) {
      m <- data[[measure_cols[j]]][[i]]
      values <- values + weights[j] * m$value
    }

    result[[i]] <- new_measure_tbl(
      location = locations,
      value = values
    )
  }

  new_measure_list(result)
}
