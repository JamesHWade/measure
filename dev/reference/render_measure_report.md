# Render processed measurement results

Renders the sections returned by
[`measure_report_sections()`](https://jameshwade.github.io/measure/dev/reference/measure_report_sections.md)
to a portable HTML or Word document. The section generic is evaluated in
the calling R session, which lets technique packs provide methods for
their result subclasses, including plots that call functions in the pack
namespace.

## Usage

``` r
render_measure_report(
  results,
  output_file,
  title = "Measurement results",
  open = interactive(),
  quiet = FALSE,
  ...
)
```

## Arguments

- results:

  A `measure_results` object or a subclass supplied by a technique pack.

- output_file:

  Destination path. The extension must be `.html` or `.docx`; it
  determines the output format.

- title:

  Report title. Dynamic text is escaped by the built-in template before
  it is written to the document.

- open:

  Logical; open the rendered document after it is written? Defaults to
  [`interactive()`](https://rdrr.io/r/base/interactive.html).

- quiet:

  Logical; suppress Quarto messages?

- ...:

  Arguments passed to
  [`measure_report_sections()`](https://jameshwade.github.io/measure/dev/reference/measure_report_sections.md).

## Value

Invisibly, `output_file`.
