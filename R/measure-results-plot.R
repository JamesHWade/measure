#' Plot processed measurement results
#'
#' @param object A [measure_results()] object.
#' @param type The default, `"auto"`, displays one-dimensional signals as
#'   lines, two-dimensional signals as maps, and higher-dimensional signals
#'   as per-sample ranges and means. Choose `"signal"`, `"map"`, or
#'   `"summary"` to select a view explicitly.
#' @param max_spectra Maximum number of samples to include in a signal
#'   overlay, taken in input order. Use `NULL` to show all samples.
#' @param summary For a signal overlay, add the mean and standard-deviation
#'   ribbon for the displayed samples at each location.
#' @param alpha Opacity of the signal traces or map points.
#' @param point_size Size of the points in a map.
#' @param ... Reserved for method extensions.
#' @details Maps draw a point at each measured coordinate pair. Each sample
#'   has a separate panel. Axis names and units must agree across panels.
#' @return A [ggplot2::ggplot()] object.
#' @export
autoplot.measure_results <- function(
  object,
  type = c("auto", "signal", "map", "summary"),
  max_spectra = 12L,
  summary = FALSE,
  alpha = 0.8,
  point_size = 2.5,
  ...
) {
  type <- rlang::arg_match(type)
  input <- measurement_table_input(object$data, object$measures, object$id_cols)
  members <- input$members
  n <- length(members)
  if (!n) {
    cli::cli_abort("Cannot plot an empty measurement collection.")
  }
  ndim <- measure_ndim(members)
  if (type == "auto") {
    type <- switch(as.character(ndim), "1" = "signal", "2" = "map", "summary")
  }
  labels <- result_sample_labels(input)
  metadata <- measure_value_metadata(members)
  signal <- result_axis_label(metadata$quantity %||% "Signal", metadata$unit)

  if (type == "signal") {
    if (ndim != 1L) {
      cli::cli_abort("A signal plot requires one-dimensional measurements.")
    }
    rlang::check_number_whole(max_spectra, min = 1, allow_null = TRUE)
    rlang::check_bool(summary)
    keep <- seq_len(min(n, max_spectra %||% n))
    data <- fortify.measure_list(members[keep])
    data$.sample <- factor(labels[keep][data$sample], levels = labels[keep])
    if (length(keep) == 1L) {
      lines <- ggplot2::aes(group = .data$sample)
      subtitle <- labels[keep]
    } else {
      lines <- ggplot2::aes(group = .data$sample, colour = .data$.sample)
      subtitle <- paste(n, "samples")
    }
    if (length(keep) < n) {
      subtitle <- paste("First", length(keep), "of", n, "samples")
    }
    plot <- plot_measure_lines(
      data,
      lines,
      summary && length(keep) > 1L,
      alpha,
      subtitle
    )
    return(
      plot +
        ggplot2::labs(
          title = "Processed signals",
          colour = "Sample",
          y = signal
        )
    )
  }

  if (type == "map") {
    if (ndim != 2L) {
      cli::cli_abort("A map requires two-dimensional measurements.")
    }
    axes <- unique(input$labels[c("axis_names", "axis_units")])
    if (nrow(axes) != 1L) {
      cli::cli_abort(
        "Cannot plot samples with incompatible axis names or units together."
      )
    }
    axis_names <- axes$axis_names[[1L]] %||% c("Location 1", "Location 2")
    units <- axes$axis_units[[1L]]
    data <- measure_slice_table(
      object$data,
      object$measures,
      id_cols = character()
    )
    data$.sample <- factor(labels[data$row_id], levels = labels)
    return(
      ggplot2::ggplot(
        data,
        ggplot2::aes(.data$location_1, .data$location_2, colour = .data$value)
      ) +
        ggplot2::geom_point(size = point_size, alpha = alpha) +
        ggplot2::scale_colour_viridis_c(name = signal, na.value = "grey75") +
        ggplot2::facet_wrap(~.sample, scales = "free", drop = FALSE) +
        ggplot2::labs(
          title = "Two-dimensional signal map",
          x = result_axis_label(axis_names[1], units[1]),
          y = result_axis_label(axis_names[2], units[2])
        ) +
        ggplot2::theme_minimal()
    )
  }

  data <- summary.measure_results(object)
  data$.sample <- factor(labels, levels = labels)
  ggplot2::ggplot(data, ggplot2::aes(x = .data$.sample)) +
    ggplot2::geom_linerange(
      ggplot2::aes(ymin = .data$min, ymax = .data$max),
      na.rm = TRUE
    ) +
    ggplot2::geom_point(ggplot2::aes(y = .data$mean), na.rm = TRUE) +
    ggplot2::scale_x_discrete(drop = FALSE) +
    ggplot2::labs(x = "Sample", y = signal, title = "Observed range and mean") +
    ggplot2::theme_minimal()
}

result_axis_label <- function(name, unit) {
  if (is.null(unit) || is.na(unit) || !nzchar(unit)) {
    name
  } else {
    paste0(name, " (", unit, ")")
  }
}

result_sample_labels <- function(input) {
  n <- length(input$members)
  fallback <- names(input$members) %||% rep("", n)
  missing <- is.na(fallback) | !nzchar(fallback)
  fallback[missing] <- paste("Sample", which(missing))
  ids <- input$ids
  labels <- vapply(
    seq_len(n),
    function(i) {
      values <- unlist(
        lapply(ids, function(col) as.character(col[[i]])),
        use.names = FALSE
      )
      values <- values[!is.na(values) & nzchar(values)]
      if (length(values)) paste(values, collapse = " / ") else fallback[i]
    },
    character(1)
  )
  make.unique(labels, sep = " #")
}
