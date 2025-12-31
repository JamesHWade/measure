# ==============================================================================
# Multi-Channel Alignment Step
#
# Aligns multiple measure columns or channels within nD data to a common grid.
# ==============================================================================

#' Align Multiple Channels to a Common Grid
#'
#' `step_measure_channel_align()` creates a *specification* of a recipe step that
#' aligns multiple measurement channels to a common location grid.
#'
#' @param recipe A recipe object.
#' @param ... One or more selector functions to choose measure columns.
#'   If empty, all measure columns are used.
#' @param method How to determine the common grid:
#'
#'   - `"union"` (default): Use all unique locations from all channels
#'   - `"intersection"`: Use only locations present in all channels
#'   - `"reference"`: Use the grid from the reference channel
#'
#' @param reference For `method = "reference"`, which channel to use as reference.
#'   Can be a column name (character) or column index (integer). Default is 1
#'   (first channel).
#' @param interpolation Interpolation method for missing values:
#'
#'   - `"linear"` (default): Linear interpolation
#'   - `"spline"`: Cubic spline interpolation
#'   - `"constant"`: Nearest neighbor (constant)
#'
#' @param role Not used.
#' @param trained Logical indicating if the step has been trained.
#' @param skip Logical. Should the step be skipped when baking?
#' @param id Unique step identifier.
#'
#' @return An updated recipe with the new step added.
#'
#' @details
#' Multi-channel analytical instruments (e.g., LC-DAD, SEC with multiple detectors)

#' often produce measurements at slightly different location grids for each channel.
#' This step aligns all channels to a common grid, enabling:
#'
#' - Direct comparison between channels
#' - Channel combination or ratio calculations
#' - Modeling with consistent feature dimensions
#'
#' ## Grid Methods
#'
#' - **Union**: Creates a grid containing all unique locations from all channels.
#'   Values are interpolated where channels don't have data.
#' - **Intersection**: Uses only locations where all channels have data.
#'   No interpolation needed but may lose data at edges.
#' - **Reference**: Uses one channel's grid as the target. Other channels are
#'   interpolated to match.
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
#'   time_uv = rep(seq(0, 9, by = 1), 3),
#'   absorbance_uv = rnorm(30, 100, 10),
#'   time_ri = rep(seq(0.5, 9.5, by = 1), 3),
#'   absorbance_ri = rnorm(30, 50, 5),
#'   concentration = rep(c(10, 25, 50), each = 10)
#' )
#'
#' # Ingest as separate channels, then align
#' rec <- recipe(concentration ~ ., data = df) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(absorbance_uv, location = vars(time_uv)) |>
#'   step_measure_input_long(absorbance_ri, location = vars(time_ri)) |>
#'   step_measure_channel_align(method = "union")
step_measure_channel_align <- function(
  recipe,
  ...,
  method = c("union", "intersection", "reference"),
  reference = 1L,
  interpolation = c("linear", "spline", "constant"),
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = recipes::rand_id("measure_channel_align")
) {
  method <- match.arg(method)
  interpolation <- match.arg(interpolation)

  recipes::add_step(
    recipe,
    step_measure_channel_align_new(
      terms = rlang::enquos(...),
      method = method,
      reference = reference,
      interpolation = interpolation,
      common_grid = NULL,
      measure_cols = NULL,
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_measure_channel_align_new <- function(
  terms,
  method,
  reference,
  interpolation,
  common_grid,
  measure_cols,
  role,

  trained,
  skip,
  id
) {
  recipes::step(
    subclass = "measure_channel_align",
    terms = terms,
    method = method,
    reference = reference,
    interpolation = interpolation,
    common_grid = common_grid,
    measure_cols = measure_cols,
    role = role,
    trained = trained,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_measure_channel_align <- function(x, training, info = NULL, ...) {
  check_for_measure(training)

  # Get measure columns - either from terms or find all

  if (length(x$terms) == 0) {
    measure_cols <- find_measure_cols(training)
  } else {
    measure_cols <- recipes::recipes_eval_select(x$terms, training, info)
    # Validate they are measure columns
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
      "Channel alignment requires at least 2 measure columns, found {length(measure_cols)}."
    )
  }

  # Determine reference column
  ref_col <- .resolve_reference(x$reference, measure_cols)

  # Compute common grid from training data
  common_grid <- .compute_common_grid(
    training,
    measure_cols,
    x$method,
    ref_col
  )

  step_measure_channel_align_new(
    terms = x$terms,
    method = x$method,
    reference = ref_col,
    interpolation = x$interpolation,
    common_grid = common_grid,
    measure_cols = measure_cols,
    role = x$role,
    trained = TRUE,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_measure_channel_align <- function(object, new_data, ...) {
  for (col in object$measure_cols) {
    result <- purrr::map(
      new_data[[col]],
      .align_to_grid,
      target_grid = object$common_grid,
      interpolation = object$interpolation
    )
    new_data[[col]] <- new_measure_list(result)
  }
  tibble::as_tibble(new_data)
}

#' @export
print.step_measure_channel_align <- function(
  x,
  width = max(20, options()$width - 30),
  ...
) {
  title <- paste0(
    "Channel alignment (method = ", x$method,
    ", interp = ", x$interpolation, ") on "
  )
  if (x$trained) {
    cat(title, paste(x$measure_cols, collapse = ", "), sep = "")
  } else {
    cat(title, "<pending>", sep = "")
  }
  cat("\n")
  invisible(x)
}

#' @rdname tidy.recipe
#' @export
#' @keywords internal
tidy.step_measure_channel_align <- function(x, ...) {
  if (is_trained(x)) {
    tibble::tibble(
      terms = x$measure_cols,
      method = x$method,
      reference = x$reference,
      interpolation = x$interpolation,
      n_grid_points = length(x$common_grid),
      id = x$id
    )
  } else {
    tibble::tibble(
      terms = rlang::na_chr,
      method = x$method,
      reference = as.character(x$reference),
      interpolation = x$interpolation,
      n_grid_points = NA_integer_,
      id = x$id
    )
  }
}

# ------------------------------------------------------------------------------
# Internal helpers
# ------------------------------------------------------------------------------

.resolve_reference <- function(ref, measure_cols) {
  if (is.numeric(ref)) {
    if (ref < 1 || ref > length(measure_cols)) {
      cli::cli_abort(
        "{.arg reference} index {ref} is out of range (1 to {length(measure_cols)})."
      )
    }
    return(measure_cols[ref])
  }

  if (is.character(ref)) {
    if (!ref %in% measure_cols) {
      cli::cli_abort(
        "{.arg reference} column {.field {ref}} not found in measure columns."
      )
    }
    return(ref)
  }

  cli::cli_abort("{.arg reference} must be a column name or index.")
}

.compute_common_grid <- function(data, measure_cols, method, ref_col) {
  # Collect all unique locations from each column
  all_grids <- lapply(measure_cols, function(col) {
    locations <- unique(unlist(lapply(data[[col]], function(m) m$location)))
    sort(unique(locations))
  })
  names(all_grids) <- measure_cols

  switch(
    method,
    "union" = sort(unique(unlist(all_grids))),
    "intersection" = Reduce(intersect, all_grids),
    "reference" = all_grids[[ref_col]]
  )
}

.align_to_grid <- function(m, target_grid, interpolation) {
  if (!is_measure_tbl(m)) {
    cli::cli_warn("Skipping non-measure_tbl element.")
    return(m)
  }

  # If grids match exactly, return as-is
  if (identical(m$location, target_grid)) {
    return(m)
  }

  # Interpolate to target grid
  interp_fn <- switch(
    interpolation,
    "linear" = function(x, y, xout) stats::approx(x, y, xout, rule = 2)$y,
    "spline" = function(x, y, xout) stats::spline(x, y, xout = xout)$y,
    "constant" = function(x, y, xout) stats::approx(x, y, xout, method = "constant", rule = 2)$y
  )

  new_values <- interp_fn(m$location, m$value, target_grid)

  new_measure_tbl(
    location = target_grid,
    value = new_values
  )
}
