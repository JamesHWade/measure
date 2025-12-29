#' Diagnostics and Visualization for Measure Data
#'
#' Autoplot and fortify methods for visualizing spectral/chromatographic data
#' and recipe transformations.
#'
#' @name diagnostics
#' @keywords internal
#' @importFrom ggplot2 autoplot
NULL


# ggplot2 availability check ---------------------------------------------

check_ggplot2 <- function(call = rlang::caller_env()) {
  rlang::check_installed(
    "ggplot2",
    reason = "for plotting measure data.",
    call = call
  )
}


# fortify methods --------------------------------------------------------

#' Convert Measure Objects to Data Frames for Plotting
#'
#' These methods convert measure objects to data frames suitable for
#' use with ggplot2.
#'
#' @param model A `measure_tbl` or `measure_list` object.
#' @param data Ignored. Present for compatibility with generic.
#' @param ... Additional arguments (currently unused).
#'
#' @return A tibble with columns `location` and `value` (for `measure_tbl`)
#'   or `location`, `value`, and `sample` (for `measure_list`).
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' # Single spectrum
#' spec <- new_measure_tbl(location = 1:100, value = rnorm(100))
#' ggplot(fortify(spec), aes(location, value)) + geom_line()
#'
#' # Multiple spectra (from recipe output)
#' rec <- recipe(water ~ ., data = meats_long) |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   prep()
#' baked <- bake(rec, new_data = NULL)
#' ggplot(fortify(baked$.measures), aes(location, value, group = sample)) +
#'   geom_line(alpha = 0.5)
#' }
#'
#' @name fortify-measure
#' @exportS3Method ggplot2::fortify
fortify.measure_tbl <- function(model, data = NULL, ...) {
  tibble::tibble(
    location = model$location,
    value = model$value
  )
}

#' @rdname fortify-measure
#' @exportS3Method ggplot2::fortify
fortify.measure_list <- function(model, data = NULL, ...) {
  # Use existing helper function
  result <- measure_to_tibble(model)
  names(result)[names(result) == "sample_num"] <- "sample"
  result
}


# autoplot.measure_tbl ---------------------------------------------------

#' Autoplot Methods for Measure Objects
#'
#' Create ggplot2 visualizations of spectral/chromatographic data stored
#' in measure objects.
#'
#' @param object A `measure_tbl`, `measure_list`, or `recipe` object.
#' @param ... Additional arguments passed to specific plot types.
#'
#' @details
#' For `measure_tbl` (single spectrum):
#' - Plots location vs value as a line
#'
#' For `measure_list` (multiple spectra):
#' - Plots all spectra with optional summary ribbon
#' - Use `summary = TRUE` for mean +/- SD ribbon
#' - Use `max_spectra` to limit number of individual lines
#'
#' For `recipe`:
#' - Shows before/after comparison of preprocessing
#' - Requires a prepped recipe
#' - Use `n_samples` to control number of samples shown
#'
#' @return A ggplot2 object.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' # Single spectrum
#' spec <- new_measure_tbl(location = 1:100, value = sin(1:100 / 10) + rnorm(100, sd = 0.1))
#' autoplot(spec)
#'
#' # Multiple spectra with summary
#' rec <- recipe(water ~ ., data = meats_long) |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   prep()
#' baked <- bake(rec, new_data = NULL)
#' autoplot(baked$.measures, summary = TRUE)
#'
#' # Recipe before/after comparison
#' rec <- recipe(water ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_snv() |>
#'   prep()
#' autoplot(rec, n_samples = 10)
#' }
#'
#' @name autoplot-measure
#' @export
autoplot.measure_tbl <- function(object, ...) {
  check_ggplot2()

  plot_data <- fortify.measure_tbl(object)

  ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$location, y = .data$value)) +
    ggplot2::geom_line() +
    ggplot2::labs(
      x = "Location",
      y = "Value",
      title = "Spectrum"
    ) +
    ggplot2::theme_minimal()
}


# autoplot.measure_list --------------------------------------------------

#' @rdname autoplot-measure
#' @param summary Logical. If TRUE, add mean +/- SD ribbon. Default FALSE.
#' @param max_spectra Maximum number of individual spectra to plot.
#'   Default 50. Set to NULL for no limit.
#' @param alpha Transparency for individual spectrum lines. Default 0.3.
#' @param color_by Optional column name for coloring spectra by group.
#' @export
autoplot.measure_list <- function(object,
                                  summary = FALSE,
                                  max_spectra = 50,
                                  alpha = 0.3,
                                  color_by = NULL,
                                  ...) {
  check_ggplot2()

  plot_data <- fortify.measure_list(object)
  n_samples <- length(unique(plot_data$sample))

  # Subset if too many spectra
  if (!is.null(max_spectra) && n_samples > max_spectra) {
    sample_subset <- sample(unique(plot_data$sample), max_spectra)
    plot_data <- plot_data[plot_data$sample %in% sample_subset, ]
    subtitle <- paste0("Showing ", max_spectra, " of ", n_samples, " spectra")
  } else {
    subtitle <- paste0(n_samples, " spectra")
  }

  # Build base plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$location, y = .data$value))

  # Add individual lines
  p <- p + ggplot2::geom_line(
    ggplot2::aes(group = .data$sample),
    alpha = alpha,
    linewidth = 0.3
  )

  # Add summary ribbon if requested
  if (summary) {
    # Compute summary statistics
    summary_data <- plot_data |>
      dplyr::group_by(.data$location) |>
      dplyr::summarize(
        mean_value = mean(.data$value, na.rm = TRUE),
        sd_value = stats::sd(.data$value, na.rm = TRUE),
        .groups = "drop"
      )

    p <- p +
      ggplot2::geom_ribbon(
        data = summary_data,
        ggplot2::aes(
          y = .data$mean_value,
          ymin = .data$mean_value - .data$sd_value,
          ymax = .data$mean_value + .data$sd_value
        ),
        alpha = 0.3,
        fill = "steelblue"
      ) +
      ggplot2::geom_line(
        data = summary_data,
        ggplot2::aes(y = .data$mean_value),
        color = "steelblue",
        linewidth = 1
      )
  }

  p <- p +
    ggplot2::labs(
      x = "Location",
      y = "Value",
      title = "Spectra",
      subtitle = subtitle
    ) +
    ggplot2::theme_minimal()

  p
}


# autoplot.recipe --------------------------------------------------------

#' @rdname autoplot-measure
#' @param n_samples Number of samples to show in before/after comparison.
#'   Default 10.
#' @param which Which comparison to show: "before_after" (default),
#'   "steps" (show each step), or "summary" (summary statistics).
#' @export
autoplot.recipe <- function(object,
                            n_samples = 10,
                            which = c("before_after", "summary"),
                            ...) {
  check_ggplot2()

  which <- match.arg(which)

  if (!recipes::fully_trained(object)) {
    cli::cli_abort(c(
      "Recipe must be prepped before plotting.",
      "i" = "Use {.code prep(recipe)} first."
    ))
  }

  # Get original and processed data
  original_data <- object$template

  # Check if original data has measure column or needs input processing
  processed_data <- recipes::bake(object, new_data = NULL)

  meas_cols <- find_measure_cols(processed_data)
  if (length(meas_cols) == 0) {
    cli::cli_abort(c(
      "Processed data does not contain measure columns.",
      "i" = "Ensure recipe includes {.fn step_measure_input_wide} or {.fn step_measure_input_long}.",
      "i" = "Do not include output steps when using autoplot()."
    ))
  }

  meas_col <- meas_cols[1]
  processed_measures <- processed_data[[meas_col]]

  # Subset samples
  n_available <- length(processed_measures)
  n_show <- min(n_samples, n_available)
  sample_idx <- seq_len(n_show)

  processed_subset <- processed_measures[sample_idx]

  if (which == "before_after") {
    # Need to get "before" data by applying only the input step
    before_data <- apply_input_step_only(object)

    if (!is.null(before_data)) {
      before_col <- find_measure_cols(before_data)[1]
      before_measures <- before_data[[before_col]][sample_idx]

      # Combine for faceted plot
      before_df <- fortify.measure_list(new_measure_list(before_measures))
      before_df$stage <- "Before"

      after_df <- fortify.measure_list(new_measure_list(processed_subset))
      after_df$stage <- "After"

      plot_data <- rbind(before_df, after_df)
      plot_data$stage <- factor(plot_data$stage, levels = c("Before", "After"))

      p <- ggplot2::ggplot(plot_data,
                           ggplot2::aes(x = .data$location, y = .data$value,
                                        group = .data$sample)) +
        ggplot2::geom_line(alpha = 0.5, linewidth = 0.3) +
        ggplot2::facet_wrap(~ stage, scales = "free_y") +
        ggplot2::labs(
          x = "Location",
          y = "Value",
          title = "Recipe Preprocessing Effect",
          subtitle = paste(n_show, "samples shown")
        ) +
        ggplot2::theme_minimal()

    } else {
      # Can't extract before, just show after
      plot_data <- fortify.measure_list(new_measure_list(processed_subset))
      p <- ggplot2::ggplot(plot_data,
                           ggplot2::aes(x = .data$location, y = .data$value,
                                        group = .data$sample)) +
        ggplot2::geom_line(alpha = 0.5, linewidth = 0.3) +
        ggplot2::labs(
          x = "Location",
          y = "Value",
          title = "Processed Spectra",
          subtitle = paste(n_show, "samples shown")
        ) +
        ggplot2::theme_minimal()
    }
  } else if (which == "summary") {
    # Summary statistics comparison
    plot_data <- fortify.measure_list(new_measure_list(processed_subset))

    summary_data <- plot_data |>
      dplyr::group_by(.data$location) |>
      dplyr::summarize(
        mean = mean(.data$value, na.rm = TRUE),
        sd = stats::sd(.data$value, na.rm = TRUE),
        min = min(.data$value, na.rm = TRUE),
        max = max(.data$value, na.rm = TRUE),
        .groups = "drop"
      )

    p <- ggplot2::ggplot(summary_data, ggplot2::aes(x = .data$location)) +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = .data$min, ymax = .data$max),
        alpha = 0.2, fill = "gray60"
      ) +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = .data$mean - .data$sd, ymax = .data$mean + .data$sd),
        alpha = 0.3, fill = "steelblue"
      ) +
      ggplot2::geom_line(ggplot2::aes(y = .data$mean), color = "steelblue", linewidth = 1) +
      ggplot2::labs(
        x = "Location",
        y = "Value",
        title = "Processed Spectra Summary",
        subtitle = paste0("Mean +/- SD (blue), min/max (gray) from ", n_show, " samples")
      ) +
      ggplot2::theme_minimal()
  }

  p
}


# Helper: extract input-only recipe --------------------------------------

#' Apply only input step to get "before" data
#' @noRd
apply_input_step_only <- function(recipe) {
  # Find input step index
  step_classes <- vapply(recipe$steps, function(s) class(s)[1], character(1))
  input_idx <- grep("^step_measure_input", step_classes)

  if (length(input_idx) == 0) {
    return(NULL)
  }

  tryCatch({
    # Get the trained input step
    input_step <- recipe$steps[[input_idx[1]]]

    # Get template data (training data)
    template_data <- recipe$template

    # Apply just the input step's bake method to the template
    result <- recipes::bake(input_step, new_data = template_data)
    result
  }, error = function(e) {
    NULL
  })
}


# plot_measure_comparison ------------------------------------------------

#' Compare Multiple Preprocessing Recipes
#'
#' Visualize the effect of different preprocessing recipes side-by-side.
#' Useful for comparing different parameter settings or preprocessing
#' strategies.
#'
#' @param ... Named recipe objects to compare. Each must be a prepped recipe.
#' @param data Data to apply recipes to. If NULL, uses the training data
#'   from the first recipe.
#' @param n_samples Number of samples to show. Default 5.
#' @param summary_only If TRUE, only show summary statistics (mean +/- SD).
#'   Default FALSE shows individual spectra.
#'
#' @return A ggplot2 object with faceted comparison.
#'
#' @examples
#' \dontrun{
#' library(recipes)
#' library(ggplot2)
#'
#' # Compare SNV vs MSC preprocessing
#' base_rec <- recipe(water ~ ., data = meats_long) |>
#'   update_role(id, new_role = "id") |>
#'   step_measure_input_long(transmittance, location = vars(channel))
#'
#' snv_rec <- base_rec |>
#'   step_measure_snv() |>
#'   prep()
#'
#' msc_rec <- base_rec |>
#'   step_measure_msc() |>
#'   prep()
#'
#' plot_measure_comparison(
#'   "SNV" = snv_rec,
#'   "MSC" = msc_rec,
#'   n_samples = 10
#' )
#' }
#'
#' @export
plot_measure_comparison <- function(...,
                                    data = NULL,
                                    n_samples = 5,
                                    summary_only = FALSE) {
  check_ggplot2()

  recipes_list <- list(...)

  if (length(recipes_list) == 0) {
    cli::cli_abort("At least one recipe must be provided.")
  }

  # Get names
  recipe_names <- names(recipes_list)
  if (is.null(recipe_names) || any(recipe_names == "")) {
    recipe_names <- paste0("Recipe_", seq_along(recipes_list))
  }

  # Validate all are prepped recipes
  for (i in seq_along(recipes_list)) {
    if (!inherits(recipes_list[[i]], "recipe")) {
      cli::cli_abort("{.arg {recipe_names[i]}} must be a recipe object.")
    }
    if (!recipes::fully_trained(recipes_list[[i]])) {
      cli::cli_abort("{.arg {recipe_names[i]}} must be prepped. Use {.code prep()} first.")
    }
  }

  # Process each recipe
  all_data <- tibble::tibble()

  for (i in seq_along(recipes_list)) {
    rec <- recipes_list[[i]]
    rec_name <- recipe_names[i]

    # Bake the recipe
    baked <- if (is.null(data)) {
      recipes::bake(rec, new_data = NULL)
    } else {
      recipes::bake(rec, new_data = data)
    }

    # Extract measure column
    meas_col <- find_measure_cols(baked)
    if (length(meas_col) == 0) {
      cli::cli_warn("Recipe {.val {rec_name}} has no measure column. Skipping.")
      next
    }

    measures <- baked[[meas_col[1]]]

    # Subset samples
    n_show <- min(n_samples, length(measures))
    measures_subset <- measures[seq_len(n_show)]

    # Convert to plot data
    plot_df <- fortify.measure_list(new_measure_list(measures_subset))
    plot_df$recipe <- rec_name

    all_data <- rbind(all_data, plot_df)
  }

  if (nrow(all_data) == 0) {
    cli::cli_abort("No valid data to plot.")
  }

  # Create plot
  all_data$recipe <- factor(all_data$recipe, levels = recipe_names)

  if (summary_only) {
    # Summary statistics
    summary_data <- all_data |>
      dplyr::group_by(.data$recipe, .data$location) |>
      dplyr::summarize(
        mean = mean(.data$value, na.rm = TRUE),
        sd = stats::sd(.data$value, na.rm = TRUE),
        .groups = "drop"
      )

    p <- ggplot2::ggplot(summary_data, ggplot2::aes(x = .data$location)) +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = .data$mean - .data$sd, ymax = .data$mean + .data$sd),
        alpha = 0.3, fill = "steelblue"
      ) +
      ggplot2::geom_line(
        ggplot2::aes(y = .data$mean),
        color = "steelblue", linewidth = 0.8
      )
  } else {
    p <- ggplot2::ggplot(all_data,
                         ggplot2::aes(x = .data$location, y = .data$value,
                                      group = .data$sample)) +
      ggplot2::geom_line(alpha = 0.5, linewidth = 0.3)
  }

  p <- p +
    ggplot2::facet_wrap(~ recipe, scales = "free_y") +
    ggplot2::labs(
      x = "Location",
      y = "Value",
      title = "Preprocessing Comparison",
      subtitle = paste(n_samples, "samples per method")
    ) +
    ggplot2::theme_minimal()

  p
}


# measure_plot_summary ---------------------------------------------------

#' Plot Summary Statistics for Measure Data
#'
#' Create a summary plot showing mean +/- standard deviation across
#' all samples at each measurement location.
#'
#' @param data A data frame with a measure column (`.measures`).
#' @param measure_col Name of the measure column. If NULL, auto-detected.
#' @param show_range Logical. If TRUE, also show min/max range. Default FALSE.
#'
#' @return A ggplot2 object.
#'
#' @examples
#' \dontrun{
#' rec <- recipe(water ~ ., data = meats_long) |>
#'   step_measure_input_long(transmittance, location = vars(channel)) |>
#'   step_measure_snv() |>
#'   prep()
#'
#' baked <- bake(rec, new_data = NULL)
#' measure_plot_summary(baked)
#' }
#'
#' @export
measure_plot_summary <- function(data, measure_col = NULL, show_range = FALSE) {
  check_ggplot2()

  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame.")
  }

  # Find measure column
  if (is.null(measure_col)) {
    meas_cols <- find_measure_cols(data)
    if (length(meas_cols) == 0) {
      cli::cli_abort("No measure column found in data.")
    }
    measure_col <- meas_cols[1]
  }

  measures <- data[[measure_col]]

  # Use measure_summarize if available, otherwise compute manually
  summary_data <- tryCatch({
    measure_summarize(measures)
  }, error = function(e) {
    # Manual computation
    plot_df <- fortify.measure_list(new_measure_list(measures))
    plot_df |>
      dplyr::group_by(.data$location) |>
      dplyr::summarize(
        mean = mean(.data$value, na.rm = TRUE),
        sd = stats::sd(.data$value, na.rm = TRUE),
        min = min(.data$value, na.rm = TRUE),
        max = max(.data$value, na.rm = TRUE),
        n = dplyr::n(),
        .groups = "drop"
      )
  })

  p <- ggplot2::ggplot(summary_data, ggplot2::aes(x = .data$location))

  if (show_range) {
    p <- p + ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .data$min, ymax = .data$max),
      alpha = 0.15, fill = "gray50"
    )
  }

  p <- p +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .data$mean - .data$sd, ymax = .data$mean + .data$sd),
      alpha = 0.3, fill = "steelblue"
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = .data$mean),
      color = "steelblue", linewidth = 1
    ) +
    ggplot2::labs(
      x = "Location",
      y = "Value",
      title = "Spectral Summary",
      subtitle = paste0("Mean +/- SD (n = ", nrow(data), " samples)")
    ) +
    ggplot2::theme_minimal()

  p
}
