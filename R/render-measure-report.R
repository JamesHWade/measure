#' Render processed measurement results
#'
#' Renders the sections returned by [measure_report_sections()] to a portable
#' HTML or Word document. The section generic is evaluated in the calling R
#' session, which lets technique packs provide methods for their result
#' subclasses, including plots that call functions in the pack namespace.
#'
#' @param results A `measure_results` object or a subclass supplied by a
#'   technique pack.
#' @param output_file Destination path. The extension must be `.html` or
#'   `.docx`; it determines the output format.
#' @param title Report title. Dynamic text is escaped by the built-in
#'   template before it is written to the document.
#' @param open Logical; open the rendered document after it is written?
#'   Defaults to `interactive()`.
#' @param quiet Logical; suppress Quarto messages?
#' @param ... Arguments passed to [measure_report_sections()].
#'
#' @return Invisibly, `output_file`.
#' @export
render_measure_report <- function(
  results,
  output_file,
  title = "Measurement results",
  open = interactive(),
  quiet = FALSE,
  ...
) {
  if (!inherits(results, "measure_results")) {
    cli::cli_abort(
      "{.arg results} must be a {.cls measure_results} object or subclass."
    )
  }

  if (!rlang::is_string(output_file) || !nzchar(output_file)) {
    cli::cli_abort("{.arg output_file} must be a non-empty path.")
  }

  output_ext <- tolower(tools::file_ext(output_file))
  if (!output_ext %in% c("html", "docx")) {
    cli::cli_abort(
      "{.arg output_file} must end in {.file .html} or {.file .docx}."
    )
  }

  if (!rlang::is_string(title) || is.na(title)) {
    cli::cli_abort("{.arg title} must be a single, non-missing string.")
  }

  rlang::check_bool(open)
  rlang::check_bool(quiet)

  rlang::check_installed("quarto", reason = "to render measurement reports")
  quarto_path <- quarto::quarto_path()
  if (is.null(quarto_path) || !file.exists(quarto_path)) {
    cli::cli_abort(
      c(
        "Quarto CLI is required to render measurement reports.",
        "i" = "Install Quarto from {.url https://quarto.org/docs/get-started/}."
      )
    )
  }

  sections <- measure_report_sections(results, ...)
  check_measure_report_sections(sections)

  destination <- fs::path_abs(output_file)
  if (dir.exists(destination)) {
    cli::cli_abort("{.arg output_file} points to a directory.")
  }
  fs::dir_create(dirname(destination))
  template_file <- system.file(
    "templates",
    "measure-results.qmd",
    package = "measure",
    mustWork = TRUE
  )

  # Stage beside the destination so the completed report can replace it by rename.
  render_dir <- tempfile(".measure-report-", tmpdir = dirname(destination))
  if (!dir.create(render_dir, showWarnings = FALSE)) {
    cli::cli_abort("Could not create a temporary report directory.")
  }
  on.exit(unlink(render_dir, recursive = TRUE, force = TRUE), add = TRUE)

  # Build plots while their extension methods and namespace helpers are available.
  for (i in which(vapply(sections, inherits, logical(1), "ggplot"))) {
    filename <- paste0("plot-", i, ".png")
    ggplot2::ggsave(
      file.path(render_dir, filename),
      plot = sections[[i]],
      width = 8,
      height = 5,
      dpi = 144,
      bg = "white"
    )
    sections[[i]] <- filename
  }
  input_file <- file.path(render_dir, "measure-results.qmd")
  fs::file_copy(template_file, input_file)
  payload <- file.path(render_dir, "sections.rds")
  saveRDS(sections, payload)
  rendered_file <- file.path(render_dir, paste0("rendered.", output_ext))
  quarto::quarto_render(
    input = input_file,
    output_format = output_ext,
    output_file = basename(rendered_file),
    execute_params = list(
      sections = payload,
      title = title,
      library_paths = .libPaths()
    ),
    quiet = quiet
  )
  fs::file_move(rendered_file, destination)

  if (isTRUE(open)) {
    utils::browseURL(destination)
  }

  invisible(output_file)
}
