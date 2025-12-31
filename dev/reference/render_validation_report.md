# Render a Validation Report to Document Format

Renders a `measure_validation_report` object to HTML, PDF, or Word
format using standardized Quarto templates. Templates follow either ICH
Q2(R2) or USP \<1225\> validation report structures.

## Usage

``` r
render_validation_report(
  report,
  output_file = NULL,
  output_format = c("html", "pdf", "docx"),
  template = c("ich_q2", "usp_1225"),
  output_dir = ".",
  include_plots = TRUE,
  include_raw_data = FALSE,
  open = interactive(),
  quiet = FALSE,
  ...
)
```

## Arguments

- report:

  A `measure_validation_report` object created by
  [`measure_validation_report()`](https://jameshwade.github.io/measure/dev/reference/measure_validation_report.md).

- output_file:

  Output file path. If NULL, uses the report title with appropriate
  extension.

- output_format:

  Output format: "html" (default), "pdf", or "docx". PDF requires a
  LaTeX installation (e.g., TinyTeX).

- template:

  Template style: "ich_q2" (default) for ICH Q2(R2) layout, or
  "usp_1225" for USP \<1225\> compendial layout.

- output_dir:

  Directory for output file. Default: current directory.

- include_plots:

  Logical; include diagnostic plots? Default: TRUE.

- include_raw_data:

  Logical; include raw data tables in appendix?

  Default: FALSE.

- open:

  Logical; open the rendered document? Default: TRUE in interactive
  sessions.

- quiet:

  Logical; suppress Quarto rendering messages? Default: FALSE.

- ...:

  Additional arguments passed to
  [`quarto::quarto_render()`](https://quarto-dev.github.io/quarto-r/reference/quarto_render.html).

## Value

Invisibly returns the path to the rendered document.

## Details

### Template Styles

**ICH Q2(R2) Template** (`template = "ich_q2"`):

- Organized by validation characteristic (specificity, linearity, etc.)

- Includes performance-based lifecycle considerations

- Structured for regulatory submission

**USP \<1225\> Template** (`template = "usp_1225"`):

- Compendial validation structure

- Category-based organization (I, II, III, IV)

- Emphasis on system suitability

### Requirements

- **HTML output**: Requires `quarto` package

- **PDF output**: Requires `quarto` package and LaTeX (TinyTeX
  recommended)

- **DOCX output**: Requires `quarto` package

Install Quarto from <https://quarto.org/docs/get-started/>. Install
TinyTeX with `quarto::quarto_install_tinytex()`.

## See also

[`measure_validation_report()`](https://jameshwade.github.io/measure/dev/reference/measure_validation_report.md)
to create the report object.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create a validation report (see measure_validation_report examples)
report <- measure_validation_report(
  title = "Method Validation Report",
  method_name = "HPLC Assay",
  analyst = "J. Smith"
)

# Render to HTML with ICH Q2 template
render_validation_report(report, output_format = "html")

# Render to PDF with USP template
render_validation_report(
  report,
  output_format = "pdf",
  template = "usp_1225",
  output_file = "validation_report.pdf"
)

# Render to Word for editing
render_validation_report(report, output_format = "docx")
} # }
```
